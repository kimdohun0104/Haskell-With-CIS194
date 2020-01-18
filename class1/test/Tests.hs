
import Test.Hspec
import Class1

main :: IO()
main = hspec $ do
    describe "Class1 testing" $ do
        it "toDigits" $
            toDigits 1234 `shouldBe` [1,2,3,4]
        
        it "doubleEveryOtherFromLeft" $
            doubleEveryOtherFromLeft [1,2,3,4] `shouldBe` [1,4,3,8]

        it "doubleEveryOther" $
            doubleEveryOther [1,2,3,4] `shouldBe` [2,2,6,4] 

        it "sumEachDigit" $
            sumEachDigit 5 `shouldBe` 6

        it "sumTenDigit" $
            sumTenDigit 10 `shouldBe` 1

        it "sumDigits" $
            sumDigits [1,2,3,4] `shouldBe` 10