{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

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

data Query
data XML

type Rq t a = Request t a -> Bool
type Rs a   = Response a  -> Bool

data Request t a = Request
    { rqRequest  :: a
    , rqTemplate :: ByteString
    , rqEncoded  :: ByteString
    , rqDiff     :: [String]
    , rqParsed   :: Either String a
    }

instance (Eq a, Arbitrary a) => TestProperty (Request t a) where
    prop Request{..} = (&&)
        (either (const False) (== rqRequest) rqParsed)
        (all null rqDiff)

instance (Eq a, Show a, Arbitrary a, Template a, ToJSON a, IsXML a)
         => Arbitrary (Request XML a) where
    arbitrary = do
        xml <- arbitrary
        let tmpl = render xml
            enc  = toIndentedXML 2 xml
            diff = difference enc tmpl
        return . Request xml tmpl enc diff $ fromXML tmpl

instance (Eq a, Show a, Arbitrary a, Template a, ToJSON a, IsQuery a)
         => Arbitrary (Request Query a) where
    arbitrary = do
        qry <- arbitrary
        let tmpl = render qry
            enc  = encodeQuery (urlEncode True) $ toQuery qry
            dec  = fromQuery $ decodeQuery (urlDecode True) tmpl
            diff = difference enc tmpl
        return $ Request qry tmpl enc diff dec

instance Show a => Show (Request t a) where
    show Request{..} = unlines
        [ "[Request]"
        , show rqRequest
        , ""
        , "[Parsed]"
        , show rqParsed
        , ""
        , "[Template]"
        , formatBS rqTemplate
        , "[Encoded]"
        , formatBS rqEncoded
        , "[Diff]"
        , formatLines rqDiff
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
