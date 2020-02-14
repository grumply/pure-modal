{ mkDerivation, base, pure, pure-cond, pure-prop, pure-portal, pure-theme, pure-css, pure-lifted, stdenv }:
mkDerivation {
  pname = "pure-modal";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure pure-cond pure-prop pure-portal pure-theme pure-css pure-lifted ];
  homepage = "github.com/grumply/pure-modal";
  description = "";
  license = stdenv.lib.licenses.bsd3;
}
