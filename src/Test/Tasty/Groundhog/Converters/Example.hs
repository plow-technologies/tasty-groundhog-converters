{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE QuasiQuotes #-}

{- |
Module      : Test.Tasty.Groundhog.Converters.Example
Description : An example of creating a Converter test
Copyright   : Plow Technologies LLC
License     : MIT License

Maintainer  : Scott Murphy

Database entries are persisted state, which means they are a serialization and should be tested for change.
Here is an example of doing that.
 -}


module Test.Tasty.Groundhog.Converters.Example  where

import Database.Groundhog.Converters
import Data.Int (Int64)
import   Data.Map.Strict (Map)
import qualified  Data.Map.Strict  as Map
import Database.Groundhog.TH
import Test.Tasty.Groundhog.Converters
import Test.Tasty
import Test.Tasty.QuickCheck


-- | Sample DataType 'Group' proviides a Map between an Integer and  a 'Person'
-- However, the person is embedded in the Datatype relative to the SQL database

data Group = Group {
     _people :: Map Integer Person
         }
 deriving (Eq)


-- | A wrapped representation of a Person
data Person = Person { _unPerson :: String}
  deriving (Eq)


-- | To Build up the converter we have to have an arbitrary instance
instance Arbitrary Person where
  arbitrary = Person <$> arbitrary

-- | An Isomorphism between the representation that is pleasent to use in haskell
-- and the one that makes sense to store i.e. 'PersistEntity' 
personMapConverter :: Converter (Map Integer Person) [(Int64,String)]
personMapConverter = mapConverter `composeConverter` fmapConverter (bicomposeConverter integerConverter personConverter)

-- | This converter is embedded in 'personMapConverter'
personConverter :: Converter Person String
personConverter = (_unPerson,Person)


-- | A declaration for group.
mkPersist defaultCodegenConfig [groundhog|
- entity: Group
  constructors:
  - name: Group
    fields:
      - name: _people
        dbName: people
        exprName: MappedIdToPerson
        converter: personMapConverter
- primitive: Person
  converter: personConverter


|]


-- | build a golden test (a single test designed to make sure a representation stays constant over time).
-- The aGroup provided is only used the first time the test is used.  The converter at the top level here
-- is just (id, id) and (==) is used because there is an Eq instance on Group.
exampleGoldenSqlConverter :: TestTree
exampleGoldenSqlConverter = goldenSqlConverter "Test The test GoldenSqlConverter" "TestGolden" aGroup (==) (id,id) 
  where
    aGroup = Group somePeople
    somePeople = (Map.fromList . zip [1 ..] . fmap Person ) ["Margret"]

-- | There are no database hits on a round trip test
-- Converter makes the claim that a Converter is an Isomorphism between the two DataTypes.
-- Round trip tests should verify this.
exampleRoundTripTest :: TestTree
exampleRoundTripTest = roundTripConverter "roundtrip personMapConverter" (==) personMapConverter 

-- | call the example test
tastyTest :: IO ()
tastyTest = defaultMain allTests
 where
    allTests = testGroup "all example groundhog converter tests" [ exampleGoldenSqlConverter
                                                                 , exampleRoundTripTest]
