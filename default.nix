{ mkDerivation, base, HaskellNet, HaskellNet-SSL, hpack, microlens
, mime-mail, network, optparse-applicative, resource-pool, stdenv
, time
}:
mkDerivation {
  pname = "mail-pool";
  version = "2.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base HaskellNet HaskellNet-SSL microlens mime-mail network
    optparse-applicative resource-pool time
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base HaskellNet HaskellNet-SSL microlens mime-mail network
    optparse-applicative resource-pool time
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/jappeace/email-pool#readme";
  description = "Preconfigured email connection pool on top of smtp";
  license = stdenv.lib.licenses.mit;
}
