{ mkDerivation, base, HaskellNet, hpack, microlens, mime-mail
, network, optparse-applicative, resource-pool, stdenv
}:
mkDerivation {
  pname = "mail-pool";
  version = "1.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base HaskellNet microlens mime-mail network optparse-applicative
    resource-pool
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base HaskellNet microlens mime-mail network optparse-applicative
    resource-pool
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/jappeace/email-pool#readme";
  description = "Preconfigured email connection pool on top of smtp";
  license = stdenv.lib.licenses.mit;
}
