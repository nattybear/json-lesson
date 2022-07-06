module Main where

import Data.Aeson
import Data.Text             as T
import Data.ByteString.Lazy  as B
import Data.ByteString.Char8 as BC
import GHC.Generics

data Book = Book { title  :: T.Text
                 , author :: T.Text
                 , year   :: Int
                 } deriving (Show, Generic)

instance FromJSON Book
instance ToJSON Book

myBook :: Book
myBook = Book { author = "Will Kurt"
              , title  = "Learn Haskell"
              , year   = 2017 }

myBookJSON :: B.ByteString
myBookJSON = encode myBook

rawJSON :: B.ByteString
rawJSON = "{\"author\":\"Emil Ciroan\",\"title\":\"A Short History of Decay\",\"year\":1949}"

bookFromJSON :: Maybe Book
bookFromJSON = decode rawJSON

wrongJSON :: B.ByteString
wrongJSON = "{\"writer\":\"Emil Cioran\",\"title\":\"A Short History of Decay\",\"year\":1949}"

bookFromWrongJSON :: Maybe Book
bookFromWrongJSON = decode wrongJSON

data Name = Name { firstName :: T.Text
                 , lastName  :: T.Text
                 } deriving (Show, Generic)

instance FromJSON Name
instance ToJSON Name

sampleError :: B.ByteString
sampleError = "{\"message\":\"oops!\",\"error\": 123}"

data ErrorMessage = ErrorMessage { message   :: T.Text
                                 , errorCode :: Int
                                 } deriving Show

instance FromJSON ErrorMessage where
  parseJSON (Object v) = ErrorMessage <$> v .: "message"
                                      <*> v .: "error"

main :: IO ()
main = print "hi"
