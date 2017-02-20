import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck (Property, Arbitrary(..), CoArbitrary(..), oneof)
import Test.QuickCheck.Classes (functor, applicative, alternative, monad)
import Test.QuickCheck.Checkers (EqProp(..), eq)
import JsonParser (Result(..), ParserM(..))

main :: IO ()
main = defaultMain tests

instance Arbitrary a => Arbitrary (Result a) where
    arbitrary = oneof [Success <$> arbitrary <*> arbitrary, pure Error]

instance CoArbitrary a => CoArbitrary (Result a)

instance CoArbitrary a => CoArbitrary (ParserM a)
instance Arbitrary a => Arbitrary (ParserM a) where
    arbitrary = ParserM <$> arbitrary

instance Show (ParserM a) where
    show _ = "Parser"

instance Eq a => EqProp (Result a) where
    (=-=) = eq

instance Eq a => EqProp (ParserM a) where
    ParserM p =-= ParserM q = p =-= q

batchToGroup :: (String, [(String, Property)]) -> Test
batchToGroup (name, tests) = testGroup name $ map testToTestProperty tests

testToTestProperty :: (String, Property) -> Test
testToTestProperty (name, prop) = testProperty name prop

tests = [ testGroup "Result" [ batchToGroup (functor (undefined :: Result (String, String, String)))
                             , batchToGroup (applicative (undefined :: Result (String, String, String)))
                             , batchToGroup (alternative (undefined :: Result (String, String, String)))
                             ]
        , testGroup "ParserM" [ batchToGroup (functor (undefined :: ParserM (String, String, String)))
                              , batchToGroup (alternative (undefined :: ParserM (String, String, String)))
                              , batchToGroup (applicative (undefined :: ParserM (String, String, String)))
                              , batchToGroup (monad (undefined :: ParserM (String, String, String)))
                              ]
        ]
