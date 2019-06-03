{-# LANGUAGE DeriveAnyClass #-}

module Network.Mail.Pool(
  module Network.Mail.Pool,
  module X
                        ) where

import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Pool               as X
import           Lens.Micro
import           Network.HaskellNet.SMTP as X
import           Network.Mail.Mime
import           Network.Socket
import           Options.Applicative
import           Type.Reflection         (Typeable)

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


defSmtpPool :: MonadIO m => SmtpCred -> m (Pool SMTPConnection)
defSmtpPool smtp = do
  liftIO $
    createPool
      (authAndConnect smtp)
      closeSMTP
      1 -- stripe, see docs, I think I just need 1: https://hackage.haskell.org/package/resource-pool-0.2.3.2/docs/Data-Pool.html
      60 -- unused connections are kept open for a minute
      5 -- max. 10 connections open per stripe

handleAny :: (SomeException -> IO a) -> IO a -> IO a
handleAny = handle

-- | we need to auth only once per connection.
--   this is annoying because we want to crash on failure to auth.
authAndConnect :: SmtpCred -> IO SMTPConnection
authAndConnect smtp = do
  conn <- connectSMTPPort (smtp ^. smtpHost) (smtp ^. smtpPort)
  handleAny
    (\ex -> do
       closeSMTP conn -- don't leak
       throwIO ex) $ do
    isSuccess <-
      authenticate LOGIN (smtp ^. smtpLogin) (smtp ^. smtpPassword) conn
    if isSuccess
      then pure conn
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
