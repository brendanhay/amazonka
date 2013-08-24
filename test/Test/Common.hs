{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances             #-}


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
    , Query
    , XML
    , Rq
    , Rs
    , prop

    -- * Re-used Imports
    , module Test
    , module Common
    ) where

import           Control.Applicative
import           Data.Aeson                           as Common (Value(..), ToJSON(..), FromJSON(..))
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Char8                as BS
import qualified Data.ByteString.Lazy.Char8           as LBS
import           Data.List                            ((\\))
import           Data.Maybe
import           Data.Monoid
import           Network.AWS.Internal                 as Common hiding (Query)
import           Network.HTTP.Types                   (urlEncode, urlDecode)
import           System.IO.Unsafe                     (unsafePerformIO)
import           Test.Arbitrary                       ()
import           Test.Framework                       as Test
import           Test.Framework.Providers.QuickCheck2 as Test
import           Test.QuickCheck                      as Test
import           Test.TH                              as Test
import           Text.Hastache
import           Text.Hastache.Aeson


testVersion :: ByteString -> [Test] -> Test
testVersion ver = plusTestOptions
    (mempty { topt_maximum_test_size = Just 50 }) . testGroup (BS.unpack ver)

class TestProperty a where
    prop :: a -> Bool

data Plain
data Query
data XML

type Rq a = Request  a -> Bool
type Rs a = Response a -> Bool

data Request a where
    Request :: AWSRequest s a b
            => { trqRequest  :: a
               , trqRaw      :: RawRequest s b
               , trqEncoded  :: ByteString
               , trqTemplate :: ByteString
               , trqDiff     :: [String]
               }
            -> Request a

instance (Eq a, Arbitrary a) => TestProperty (Request a) where
    prop = all null . trqDiff

instance (Eq a, Show a, Arbitrary a, Template a, ToJSON a, AWSRequest s a b)
         => Arbitrary (Request a) where
    arbitrary = do
        rq <- arbitrary
        let raw  = request rq
            enc  = encode raw
            tmpl = render rq raw
            diff = difference tmpl enc
        return $ Request rq raw enc tmpl diff
      where
        encode RawRequest{..} = BS.unlines $ filter (not . BS.null)
            [ BS.pack (show rqMethod) <> " " <> fromMaybe "/" rqPath
            , fromMaybe "" rqBody
            ]

        render x y = unsafePerformIO $
            LBS.toStrict <$> hastacheStr defaultConfig
                (readTemplate x)
                (jsonValueContext $ concatJSON (toJSON x) (toJSON y))

        concatJSON (Object x) (Object y) = Object $ x <> y
        concatJSON _          y          = y

instance Show a => Show (Request a) where
    show Request{..} = unlines $
        [ "[Request]"
        , show trqRequest
        , ""
        , "[Raw]"
        , show trqRaw
        , ""
        , "[Encoded]"
        , formatBS trqEncoded
        , "[Template]"
        , formatBS trqTemplate
        , "[Diff]"
        , if all null trqDiff then "<identical>" else formatLines trqDiff
        ]

data Response a = Response
    { rsContent  :: a
    , rsTemplate :: ByteString
    , rsXML      :: ByteString
    , rsParsed   :: Either String a
    }

instance (Eq a, Arbitrary a) => TestProperty (Response a) where
    prop Response{..} = either (const False) (== rsContent) rsParsed

instance (Eq a, Show a, Arbitrary a, Template a, IsXML a, ToJSON a)
         => Arbitrary (Response a) where
    arbitrary = do
        rsp <- arbitrary
        let tmpl = render rsp
            xml  = toIndentedXML 2 rsp
        return . Response rsp tmpl xml $ fromXML tmpl
      where
        render x = unsafePerformIO $
            LBS.toStrict <$> hastacheStr defaultConfig
                (readTemplate x)
                (jsonValueContext $ toJSON x)

instance Show a => Show (Response a) where
    show Response{..} = unlines
        [ "[Response]"
        , show rsContent
        , ""
        , "[Parsed]"
        , show rsParsed
        , ""
        , "[Template]"
        , formatBS rsTemplate
        , "[Encoded]"
        , formatBS rsXML
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

    zipWithTail (a:as) (b:bs) = (\\) a b : zipWithTail as bs
    zipWithTail []     bs     = bs
    zipWithTail as     _      = as
