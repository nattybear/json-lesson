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
                 } deriving Show

instance FromJSON Name where
  parseJSON (Object v) = Name <$> v .: "firstName"
                              <*> v .: "lastName"


sampleError :: B.ByteString
sampleError = "{\"message\":\"oops!\",\"error\": 123}"

data ErrorMessage = ErrorMessage { message   :: T.Text
                                 , errorCode :: Int
                                 } deriving Show

instance FromJSON ErrorMessage where
  parseJSON (Object v) = ErrorMessage <$> v .: "message"
                                      <*> v .: "error"

sampleErrorMessage :: Maybe ErrorMessage
sampleErrorMessage = decode sampleError

instance ToJSON ErrorMessage where
  toJSON (ErrorMessage message errorCode) = object [ "message" .= message
                                                   , "error"   .= errorCode ]

instance ToJSON Name where
  toJSON (Name firstName lastName) = object [ "firstName" .= firstName
                                            , "lastName"  .= lastName ]

anErrorMessage :: ErrorMessage
anErrorMessage = ErrorMessage "Everything is Okay" 0

data NOAAResult = NOAAResult { uid          :: T.Text
                             , mindate      :: T.Text
                             , maxdate      :: T.Text
                             , name         :: T.Text
                             , datacoverage :: Int
                             , resultId     :: T.Text
                             } deriving Show

main :: IO ()
main = print "hi"
