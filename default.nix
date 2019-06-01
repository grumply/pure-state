{ mkDerivation, base, exceptions, mtl, pure-core, pure-default, pure-dom, pure-lifted, stdenv, transformers }:
mkDerivation {
  pname = "pure-state";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base exceptions mtl pure-core pure-default pure-dom pure-lifted transformers ];
  homepage = "github.com/grumply/pure-state";
  description = "A stateful View monad";
  license = stdenv.lib.licenses.bsd3;
}
