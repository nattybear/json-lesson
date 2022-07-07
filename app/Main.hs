module Main where

import Control.Monad
import Data.Aeson
import Data.Text             as T
import Data.ByteString.Lazy  as B
import Data.ByteString.Char8 as BC
import GHC.Generics

data NOAAResult = NOAAResult { uid          :: T.Text
                             , mindate      :: T.Text
                             , maxdate      :: T.Text
                             , name         :: T.Text
                             , datacoverage :: Int
                             , resultId     :: T.Text
                             } deriving Show

instance FromJSON NOAAResult where
  parseJSON (Object v) = NOAAResult <$> v .: "uid"
                                    <*> v .: "mindate"
                                    <*> v .: "maxdate"
                                    <*> v .: "name"
                                    <*> v .: "datacoverage"
                                    <*> v .: "id"

data ResultSet = ResultSet { offset :: Int
                           , count  :: Int
                           , limit  :: Int
                           } deriving (Show, Generic) 

instance FromJSON ResultSet

data Metadata = Metadata { resultset :: ResultSet
                         } deriving (Show, Generic)

instance FromJSON Metadata

data NOAAResponse = NOAAResponse { metadata :: Metadata
                                 , results  :: [NOAAResult]
                                 } deriving (Show, Generic)

instance FromJSON NOAAResponse

printResults :: Maybe [NOAAResult] -> IO ()
printResults Nothing        = print "error loading data"
printResults (Just results) = forM_ results (print . name)

main :: IO ()
main = do
  jsonData <- B.readFile "data.json"
  let noaaResponse = decode jsonData :: Maybe NOAAResponse
      noaaResults  = results <$> noaaResponse
  printResults noaaResults
