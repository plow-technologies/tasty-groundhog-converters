module Test.Tasty.Groundhog.Converters where
--------------------------------------------------
-- Imports TO TEST SUITE
--------------------------------------------------
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Database.Groundhog.Core
import Database.Groundhog.Sqlite
import Database.Groundhog.Converters


{- TO TEST SUITE-}

-- | Test round trip property of a converter,
-- >>> roundTripConverter "should convert a RecordBiMap to a StorableList" (==) myRecordConverter
-- You will need to create an Arbitrary instance for the incoming item
roundTripConverter :: Arbitrary a => TestName -> (a -> a -> Bool) -> (Converter a b) -> TestTree
roundTripConverter testName toBool converterToTest = testProperty testName  runRoundTripTest 
  where
    (f,g) = converterToTest
    runRoundTripTest = do
       val <- arbitrary
       (return . toBool val . g . f) val

-- | goldenSqlConverter takes advantage of the file nature of SQLlite to read in your data
-- using groundhog.  Even if you store your data in a different database
-- if the serialization of a converter works in SQLite it will probably work in
-- your given DB.  Obviously that isn't perfect but this makes writing a quick test easy
-- The 'a' you give the function will only be active for the first insert.
-- The rest of the time, the conversion and insertion are self contained.


goldenSqlConverter :: (PersistEntity b) =>  TestName ->
                       FilePath -> a -> 
                       (b -> b -> Bool) -> Converter a b  ->
                       TestTree
goldenSqlConverter testName fp someA  bToBool converter  = testCase testName (runSqlTest >>= assertBool "SQLite insertion and conversion should match original")
  where
     (toB,toA) = converter
     runSqlTest = withSqliteConn fp $ runDbConn $ do
       runMigration $ migrate (toB someA)
       bs <- selectAll
       case bs of
         [] -> do
            _ <- insert (toB someA)
            bs' <- selectAll
            return (and $ (\(_,b) -> (bToBool b . toB.toA) b) <$> bs' )
         _ -> return (and $ (\(_,b) -> (bToBool b . toB.toA ) b) <$> bs) 


--------------------------------------------------
