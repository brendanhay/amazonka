{-# LANGUAGE RecordWildCards #-}

-- |
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
    -- * Test Case Setup
      testVersion

    -- * Request Tests
    , Body
    , body

    -- * Response Tests
    , Res
    , res

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

type Body a = Bdy a -> Bool

body :: (Eq a, Arbitrary a) => Body a
body (Bdy b _ _ d p) = (either (const False) (== b) p) &&  (all (null . snd) d)

type Res a = Response a -> Bool

res :: (Eq a, Arbitrary a) => Res a
res (Response r _ _ p) = either (const False) (== r) p

data Bdy a = Bdy
    { bdyBody     :: a
    , bdyTemplate :: ByteString
    , bdyXML      :: ByteString
    , bdyDiff     :: [(Integer, String)]
    , bdyParsed   :: Either String a
    }

instance (Eq a, Show a, Arbitrary a, Template a, IsXML a, ToJSON a)
         => Arbitrary (Bdy a) where
    arbitrary = do
        bdy <- arbitrary
        let tmpl = render bdy
            xml  = toIndentedXML 2 bdy
            diff = difference xml tmpl
        return . Bdy bdy tmpl xml diff $ fromXML tmpl

instance Show a => Show (Bdy a) where
    show Bdy{..} = unlines
        [ "[Body]"
        , show bdyBody
        , ""
        , "[Parsed]"
        , show bdyParsed
        , ""
        , "[Template]"
        , BS.unpack bdyTemplate
        , "[Encoded]"
        , BS.unpack bdyXML
        , ""
        , "[Line Diff]"
        , concatMap (\(n, s) -> show n ++ ": " ++ s ++ "\n") bdyDiff
        ]

data Response a = Response
    { resResponse :: a
    , resTemplate :: ByteString
    , resXML      :: ByteString
    , resParsed   :: Either String a
    }

instance (Eq a, Show a, Arbitrary a, Template a, IsXML a, ToJSON a)
         => Arbitrary (Response a) where
    arbitrary = do
        resp <- arbitrary
        let tmpl = render resp
            xml  = toIndentedXML 2 resp
        return . Response resp tmpl xml $ fromXML tmpl

instance Show a => Show (Response a) where
    show Response{..} = unlines
        [ "[Response]"
        , show resResponse
        , ""
        , "[Parsed]"
        , show resParsed
        , ""
        , "[Template]"
        , BS.unpack resTemplate
        , "[Encoded]"
        , BS.unpack resXML
        ]

render :: (Template a, ToJSON a) => a -> ByteString
render x = unsafePerformIO $
    LBS.toStrict <$> hastacheStr defaultConfig
        (readTemplate x)
        (jsonValueContext $ toJSON x)

difference :: ByteString -> ByteString -> [(Integer, String)]
difference x y = dropWhile (null . snd)
     . zipWith (,) [1..]
     $ zipWithTail (normalise x) (normalise y)
  where
    normalise = map (BS.unpack . BS.unwords . BS.words) . BS.lines

    zipWithTail (a:as) (b:bs) = (\\) a b : zipWithTail as bs
    zipWithTail []     bs     = bs
    zipWithTail as     _      = as
