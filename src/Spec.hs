module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Tests del enunciado" $ do
    it "Importe en Huber" $ do
      importeEnHuber ubicacion1 ubicacion2 `shouldBe` 5

