{ mkDerivation, base, mtl, pure, stdenv, transformers }:
mkDerivation {
  pname = "pure-state";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base mtl pure transformers ];
  homepage = "github.com/grumply/pure-state";
  description = "A stateful View monad";
  license = stdenv.lib.licenses.bsd3;
}
