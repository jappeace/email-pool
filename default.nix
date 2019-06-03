{ mkDerivation, base, HaskellNet, hpack, microlens, mime-mail
, network, optparse-applicative, resource-pool, stdenv
}:
mkDerivation {
  pname = "mail-pool";
  version = "1.0.0";
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
  description = "Preconfigured email connection pool on top of smtp";
  license = stdenv.lib.licenses.mit;
}
