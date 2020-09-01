{ mkDerivation, base, pure, pure-elm, pure-prop, pure-lifted, stdenv }:
mkDerivation {
  pname = "pure-modal";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure pure-elm pure-prop pure-lifted ];
  homepage = "github.com/grumply/pure-modal";
  description = "";
  license = stdenv.lib.licenses.bsd3;
}
