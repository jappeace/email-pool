{-# LANGUAGE DeriveAnyClass #-}

module Network.Mail.Pool
  ( SmtpCred(..)
  , smtpHost
  , smtpLogin
  , smtpPassword
  , smtpPort
  , module X
  , emailOptions
  , sendEmail
  , smtpPool
  , defSettings
  , poolCred
  , poolConnf
  , poolStripes
  , poolUnused
  , poolStripeMax
  , PoolSettings(..)
  , openTls
  , openPlain
  , openTls'
  ) where

import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Pool                   as X
import           Data.Time                   (NominalDiffTime)
import           Lens.Micro
import           Network.HaskellNet.SMTP     as X
import           Network.HaskellNet.SMTP.SSL as X
import           Network.Mail.Mime
import           Network.Socket
import           Options.Applicative
import           Type.Reflection             (Typeable)

-- | Failed to authetnicate with some upstream service (smtp for example)
newtype ServiceAuthFailure a = ServiceAuthFailure a
  deriving (Typeable, Show)
  deriving anyclass Exception

-- | We use smtp because it's an incredibly stable and well supported protocol
--   this prevents vendorlocking.
data SmtpCred = SmtpCred
  { _smtpPassword :: String
  , _smtpLogin    :: String
  , _smtpHost     :: String
  , _smtpPort     :: PortNumber
  } deriving (Show)

smtpHost :: Lens' SmtpCred String
smtpHost = lens _smtpHost (\a b -> a{_smtpHost= b})
smtpLogin :: Lens' SmtpCred String
smtpLogin = lens _smtpLogin (\a b -> a{_smtpLogin= b})
smtpPassword :: Lens' SmtpCred String
smtpPassword = lens _smtpPassword (\a b -> a{_smtpPassword= b})
smtpPort :: Lens' SmtpCred PortNumber
smtpPort = lens _smtpPort (\a b -> a{_smtpPort= b})

data PoolSettings = PoolSettings
  { _poolCred      :: SmtpCred -- ^ credentials for smtp connection
  , _poolConnf     :: SmtpCred -> IO SMTPConnection -- ^ smtpcred can't be factored out.
  , _poolStripes   :: Int -- ^ stripe, see docs, I think I just need 1: https://hackage.haskell.org/package/resource-pool-0.2.3.2/docs/Data-Pool.html
  , _poolUnused    :: NominalDiffTime -- ^ unused connections are kept open for a minute
  , _poolStripeMax :: Int -- ^ max. 10 connections open per stripe
  }

poolCred      :: Lens' PoolSettings SmtpCred
poolCred      = lens _poolCred (\a b -> a{_poolCred=b})
poolConnf     :: Lens' PoolSettings (SmtpCred -> IO SMTPConnection)
poolConnf      = lens _poolConnf (\a b -> a{_poolConnf=b})
poolStripes   :: Lens' PoolSettings Int
poolStripes      = lens _poolStripes (\a b -> a{_poolStripes=b})
poolUnused    :: Lens' PoolSettings NominalDiffTime
poolUnused      = lens _poolUnused (\a b -> a{_poolUnused=b})
poolStripeMax :: Lens' PoolSettings Int
poolStripeMax      = lens _poolStripeMax (\a b -> a{_poolStripeMax=b})

defSettings :: SmtpCred -> PoolSettings
defSettings cred = PoolSettings
  { _poolCred = cred
  , _poolConnf = openPlain
  , _poolStripes = 1
  , _poolUnused = 60
  , _poolStripeMax = 5
  }

openPlain :: SmtpCred -> IO SMTPConnection
openPlain smtp = connectSMTPPort (smtp ^. smtpHost) (smtp ^. smtpPort)

openTls :: SmtpCred -> IO SMTPConnection
openTls = openTls' defaultSettingsSMTPSTARTTLS

openTls' :: Settings -> SmtpCred -> IO SMTPConnection
openTls' def smtp = connectSMTPSTARTTLSWithSettings (smtp ^. smtpHost) $ def {
    sslPort = (smtp ^. smtpPort)
  }


smtpPool :: PoolSettings -> IO (Pool SMTPConnection)
smtpPool smtp =
    createPool
      (do
        conn <- smtp ^. poolConnf $ smtp ^. poolCred
        authorize conn (smtp ^. poolCred)
        pure conn
      )
      closeSMTP
      (smtp ^. poolStripes)
      (smtp ^. poolUnused)
      5

handleAny :: (SomeException -> IO a) -> IO a -> IO a
handleAny = handle

-- | we need to auth only once per connection.
--   this is annoying because we want to crash on failure to auth.
authorize :: SMTPConnection -> SmtpCred -> IO ()
authorize conn smtp = do
  handleAny
    (\ex -> do
       closeSMTP conn -- don't leak
       throwIO ex) $ do
    isSuccess <-
      authenticate LOGIN (smtp ^. smtpLogin) (smtp ^. smtpPassword) conn
    if isSuccess
      then pure ()
      else throwIO $
           ServiceAuthFailure $
           smtpPassword .~ "obfuscated, see the running instance CLI" $ smtp

emailOptions :: Parser SmtpCred
emailOptions =
  SmtpCred <$>
  strOption
    (long "smtp-pass" <> metavar "SMTP-PASS" <>
     help
       "the smtp password, in case of mailjet: https://app.mailjet.com/transactional/smtp") <*>
  strOption
    (long "smtp-login" <> metavar "SMTP-LOGIN" <>
     help
       "the smtp login name, in case of mailjet: https://app.mailjet.com/transactional/smtp") <*>
  strOption
    (long "smtp-host" <> metavar "SMTP-HOST" <> value "in-v3.mailjet.com" <>
     showDefault <>
     help "the smtp host, excluding port") <*>
  option
    auto
    (long "smtp-port" <> help "The port on which the smtp server listens" <>
     showDefault <>
     value 587 <>
     metavar "SMTP-PORT")

sendEmail :: MonadIO m => Pool SMTPConnection -> Mail -> m ()
sendEmail pool = liftIO . withResource pool . sendMimeMail2
