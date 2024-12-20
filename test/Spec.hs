import Relude
import Test.Hspec
import qualified LibSpec

main :: IO ()
main = hspec $ do
  describe "LibSpec" LibSpec.spec