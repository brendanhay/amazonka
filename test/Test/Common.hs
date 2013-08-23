{-# LANGUAGE RecordWildCards #-}

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
    , XMLRq
    , XMLRs
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
import           Data.Monoid
import           Network.AWS.Internal                 as Common
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

type XMLRq a = XMLRequest  a -> Bool
type XMLRs a = XMLResponse a -> Bool

class TestProperty a where
    prop :: a -> Bool

data XMLRequest a = XMLRequest
    { rqBody     :: a
    , rqTemplate :: ByteString
    , rqXML      :: ByteString
    , rqDiff     :: [String]
    , rqParsed   :: Either String a
    }

instance (Eq a, Arbitrary a) => TestProperty (XMLRequest a) where
    prop XMLRequest{..} = (&&)
        (either (const False) (== rqBody) rqParsed)
        (all null rqDiff)

instance (Eq a, Show a, Arbitrary a, Template a, IsXML a, ToJSON a)
         => Arbitrary (XMLRequest a) where
    arbitrary = do
        rq <- arbitrary
        let tmpl = render rq
            xml  = toIndentedXML 2 rq
            diff = difference xml tmpl
        return . XMLRequest rq tmpl xml diff $ fromXML tmpl

instance Show a => Show (XMLRequest a) where
    show XMLRequest{..} = unlines
        [ "[Request]"
        , show rqBody
        , ""
        , "[Parsed]"
        , show rqParsed
        , ""
        , "[Template]"
        , formatBS rqTemplate
        , "[Encoded]"
        , formatBS rqXML
        , "[Diff]"
        , formatLines rqDiff
        ]

data XMLResponse a = XMLResponse
    { rsResponse :: a
    , rsTemplate :: ByteString
    , rsXML      :: ByteString
    , rsParsed   :: Either String a
    }

instance (Eq a, Arbitrary a) => TestProperty (XMLResponse a) where
    prop XMLResponse{..} = either (const False) (== rsResponse) rsParsed

instance (Eq a, Show a, Arbitrary a, Template a, IsXML a, ToJSON a)
         => Arbitrary (XMLResponse a) where
    arbitrary = do
        rsp <- arbitrary
        let tmpl = render rsp
            xml  = toIndentedXML 2 rsp
        return . XMLResponse rsp tmpl xml $ fromXML tmpl

instance Show a => Show (XMLResponse a) where
    show XMLResponse{..} = unlines
        [ "[Response]"
        , show rsResponse
        , ""
        , "[Parsed]"
        , show rsParsed
        , ""
        , "[Template]"
        , formatBS rsTemplate
        , "[Encoded]"
        , formatBS rsXML
        ]

render :: (Template a, ToJSON a) => a -> ByteString
render x = unsafePerformIO $
    LBS.toStrict <$> hastacheStr defaultConfig
        (readTemplate x)
        (jsonValueContext $ toJSON x)

formatBS :: ByteString -> String
formatBS = formatLines . lines . BS.unpack

formatLines :: [String] -> String
formatLines = concatMap fmt
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
