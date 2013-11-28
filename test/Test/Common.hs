{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
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
    , TRq
    , TRs
    , prop
    , qc

    -- * Aeson
    , ToJSON    (..)
    , FromJSON  (..)
    , Value     (..)
    , stringify

    -- * Re-exported Modules
    , module Test.TH
    , module Test.Tasty
    , module Test.Tasty.QuickCheck
    ) where

import qualified Algorithms.NaturalSort  as Nat
import           Control.Applicative
import           Control.Monad           ((<=<))
import           Data.Aeson              (ToJSON(..), FromJSON(..), Value(..))
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as BS
import qualified Data.ByteString.Lazy    as LBS
import           Data.List               ((\\), sortBy)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import qualified Data.Text.Lazy.Builder  as LText
import qualified Data.Text.Lazy.Encoding as LText
import qualified Data.Text.Lazy.IO       as LText
import           Network.AWS.Internal
import qualified Network.Http.Internal   as Client
import           System.IO.Unsafe        (unsafePerformIO)
import           Test.Arbitrary         ()
import           Test.TH
import           Test.Tasty
import           Test.Tasty.QuickCheck
import qualified Text.EDE                as EDE

testVersion :: Service -> [TestTree] -> TestTree
testVersion svc = testGroup . BS.unpack $ mconcat
    ["***** [", svcName svc, "] [", svcVersion svc, "] *****"]

qc :: Testable a => TestName -> a -> TestTree
qc = testProperty

class TestProperty a where
    prop :: a -> Bool

type TRq a = TestRequest  a -> Bool
type TRs a = TestResponse a -> Bool

data TestRequest a = TestRequest
    { trqRequest  :: !a
    , trqSigned   :: !Signed
    , trqEncoded  :: !ByteString
    , trqTemplate :: Either String ByteString
    , trqDiff     :: [String]
    , trqJSON     :: !Value
    }

instance (Eq a, Arbitrary a) => TestProperty (TestRequest a) where
    prop = all null . trqDiff

instance (Eq a, Show a, Arbitrary a, Template a, ToJSON a, Rq a)
         => Arbitrary (TestRequest a) where
    arbitrary = do
        rq <- arbitrary
        let Right raw = unsafePerformIO . runAWS creds False $ request rq
            enc  = encode raw
            tmpl = render (template rq) (toJSON rq)
            diff = difference tmpl enc
        return $ TestRequest rq raw enc tmpl diff (toJSON rq)
      where
        encode Signed{..} = BS.unlines $ filter (not . BS.null)
            [ BS.pack (show rqMethod) <> " " <> rqPath
            , BS.intercalate "\n" . map join $ sortBy sort rqQuery
            , fromMaybe "" $ Client.lookupHeader (Client.qHeaders sRequest) "content-type"
            , body
            ]
          where
            Request{..} = sMeta
            body = case sBody of
                Strict bs -> bs
                _         -> ""

        creds = FromKeys "access" "secret"

        join (k, v) = k <> "=" <> v
        sort x y    = Nat.compare (Text.decodeUtf8 $ fst x) (Text.decodeUtf8 $ fst y)

instance Show a => Show (TestRequest a) where
    show TestRequest{..} = unlines $
        [ "[Request]"
        , show trqRequest
        , ""
        , "[JSON]"
        , show trqJSON
        , ""
        , "[Signed]"
        , show trqSigned
        , ""
        , "[Actual]"
        , formatBS trqEncoded
        , "[Expected]"
        , formatBS $ either BS.pack id trqTemplate
        , "[Diff]"
        , if all null trqDiff then "<identical>" else formatLines trqDiff
        ]

data TestResponse a = TestResponse
    { trsResponse :: !a
    , trsParsed   :: Either String a
    , trsTemplate :: Either String ByteString
    , trsXML      :: ByteString
    , trsDiff     :: [String]
    , trsJSON     :: !Value
    }

instance (Eq a, Arbitrary a) => TestProperty (TestResponse a) where
    prop TestResponse{..} = either (const False) (== trsResponse) trsParsed

instance (Eq a, Show a, Arbitrary a, Template a, IsXML a, ToJSON a)
         => Arbitrary (TestResponse a) where
    arbitrary = do
        rsp <- arbitrary
        let xml  = toIndentedXML 2 rsp
            json = toJSON rsp
            tmpl = render (template rsp) json
            diff = difference tmpl xml
        return $ TestResponse rsp (tmpl >>= fromXML) tmpl xml diff json

instance Show a => Show (TestResponse a) where
    show TestResponse{..} = unlines
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
        , formatBS $ either BS.pack id trsTemplate
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

difference :: Either String ByteString -> ByteString -> [String]
difference x y = zipWithTail (normalise $ either (const "") id x) (normalise y)
  where
    normalise = map (BS.unpack . BS.unwords . BS.words) . BS.lines

    zipWithTail (a:as) (b:bs) = twoWay b a : zipWithTail as bs
    zipWithTail []     bs     = bs
    zipWithTail as     _      = as

    twoWay a b = (a \\ b) ++ (b \\ a)

stringify :: Show a => a -> Value
stringify = String . Text.pack . show

render :: FilePath -> Value -> Either String ByteString
render tmpl (Object o) = f <$> unsafePerformIO g
  where
    f = LBS.toStrict . LText.encodeUtf8 . LText.toLazyText
    g = (EDE.eitherRender o <=< EDE.eitherParse) <$> LText.readFile tmpl
render tmpl _ = Right . unsafePerformIO $ BS.readFile tmpl
