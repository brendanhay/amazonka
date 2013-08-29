{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

-- Module      : Test.Common
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.Common
    (
    -- * Group Tests
      testVersion

    -- * Properties
    , Rq
    , Rs
    , prop

    -- * Re-used Imports
    , module Test
    , module Common
    ) where

import qualified Algorithms.NaturalSort               as Nat
import           Data.Aeson                           as Common (Value(..), ToJSON(..), FromJSON(..))
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Char8                as BS
import           Data.List                            ((\\), sortBy)
import           Data.Maybe
import           Data.Monoid
import           Data.Text.Encoding
import           Network.AWS.Internal                 as Common hiding (Query)
import           System.IO.Unsafe                     (unsafePerformIO)
import           Test.Arbitrary                       ()
import           Test.Framework                       as Test
import           Test.Framework.Providers.QuickCheck2 as Test
import           Test.QuickCheck                      as Test hiding (within)
import           Test.TH                              as Test
import           Text.Hastache.Aeson

testVersion :: ByteString -> [Test] -> Test
testVersion ver = plusTestOptions opts . testGroup (BS.unpack ver)
  where
    opts = mempty
        { topt_maximum_generated_tests = Just 10
        , topt_maximum_test_size       = Just 10
        }

class TestProperty a where
    prop :: a -> Bool

type Rq a = Request  a -> Bool
type Rs a = Response a -> Bool

data Request a where
    Request :: AWSRequest s a b
            => { trqRequest  :: a
               , trqRaw      :: RawRequest s b
               , trqEncoded  :: ByteString
               , trqTemplate :: ByteString
               , trqDiff     :: [String]
               , trqJSON     :: Value
               }
            -> Request a

instance (Eq a, Arbitrary a) => TestProperty (Request a) where
    prop = all null . trqDiff

instance (Eq a, Show a, Arbitrary a, Template a, ToJSON a,
          AWSService s, AWSRequest s a b)
         => Arbitrary (Request a) where
    arbitrary = do
        rq  <- arbitrary
        let raw  = request rq
            enc  = encode raw
            tmpl = render' rq raw
            diff = difference tmpl enc
        return $ Request rq raw enc tmpl diff (toJSON rq)
      where
        encode RawRequest{..} = BS.unlines $ filter (not . BS.null)
            [ BS.pack (show rqMethod) <> " " <> fromMaybe "/" rqPath
            , maybe "" ("Action=" <>) rqAction
            , BS.intercalate "\n" . map join $ sortBy sort rqQuery
            , maybe "" (const $ toBS rqContent) rqBody
            , fromMaybe "" rqBody
            ]

        join (k, v) = k <> "=" <> v
        sort x y    = Nat.compare (decodeUtf8 $ fst x) (decodeUtf8 $ fst y)

        render' x y = unsafePerformIO $
            render (readTemplate x) (concatJSON (toJSON x) (toJSON y))

        concatJSON (Object x) (Object y) = Object $ x <> y
        concatJSON _          y          = y

instance Show a => Show (Request a) where
    show Request{..} = unlines $
        [ "[Request]"
        , show trqRequest
        , ""
        , "[JSON]"
        , show trqJSON
        , ""
        , "[Raw]"
        , show trqRaw
        , ""
        , "[Actual]"
        , formatBS trqEncoded
        , "[Expected]"
        , formatBS trqTemplate
        , "[Diff]"
        , if all null trqDiff then "<identical>" else formatLines trqDiff
        ]

data Response a = Response
    { trsResponse :: a
    , trsParsed   :: Either String a
    , trsTemplate :: ByteString
    , trsXML      :: ByteString
    , trsDiff     :: [String]
    , trsJSON     :: Value
    }

instance (Eq a, Arbitrary a) => TestProperty (Response a) where
    prop Response{..} = either (const False) (== trsResponse) trsParsed

instance (Eq a, Show a, Arbitrary a, Template a, IsXML a, ToJSON a)
         => Arbitrary (Response a) where
    arbitrary = do
        rsp <- arbitrary
        let xml  = toIndentedXML 2 rsp
            json = toJSON rsp
            tmpl = unsafePerformIO $ render (readTemplate rsp) json
            diff = difference tmpl xml
        return $ Response rsp (fromXML tmpl) tmpl xml diff json

instance Show a => Show (Response a) where
    show Response{..} = unlines
        [ "[Response]"
        , show trsResponse
        , ""
        , "[Parsed]"
        , show trsParsed
        , ""
        , "[JSON]"
        , show trsJSON
        , ""
        , "[Actual]"
        , formatBS trsXML
        , "[Expected]"
        , formatBS trsTemplate
        , "[Diff]"
        , if all null trsDiff then "<identical>" else formatLines trsDiff
        ]

formatBS :: ByteString -> String
formatBS = formatLines . lines . BS.unpack

formatLines :: [String] -> String
formatLines = concatMap fmt
    . takeWhile (not . null . snd)
    . dropWhile (null . snd)
    . zipWith (,) ([1..] :: [Int])
  where
    fmt (n, s) = show n ++ ": " ++ s ++ "\n"

difference :: ByteString -> ByteString -> [String]
difference x y = zipWithTail (normalise x) (normalise y)
  where
    normalise = map (BS.unpack . BS.unwords . BS.words) . BS.lines

    zipWithTail (a:as) (b:bs) = twoWay b a : zipWithTail as bs
    zipWithTail []     bs     = bs
    zipWithTail as     _      = as

    twoWay a b = (a \\ b) ++ (b \\ a)
