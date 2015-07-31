{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : Test.AWS.Sign.V$
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Sign.V4 (tests) where

import           Control.Lens
import qualified Data.ByteString.Char8    as BS8
import qualified Data.Foldable            as Fold
import           Data.List                (sort)
import           Data.Monoid
import           Data.String
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4
import           Test.AWS.Arbitrary
import           Test.QuickCheck.Property
import           Test.Tasty
import           Test.Tasty.QuickCheck

-- Write some V4 signing properties:
--  test canonical query
--  test query
--  test canonical headers
--  test signed headers
--  test time format
--  test host, x-amz-date, content-sha256 headers exist
--  test empty path
--  test empty query

tests :: TestTree
tests = testGroup "v4"
    [ testGroup "canonical query"
        [ testProperty "empty" $ queryEmpty
        , testProperty "empty key values" $ queryEmptyKeyValues
        ]
    ]

queryEmpty :: Request () -> Signer -> Property
queryEmpty rq sg = property $
    mempty == toBS (metaCanonicalQuery . sign sg $ rq & rqQuery .~ mempty)

queryEmptyKeyValues :: [NonEmptyList Char] -> Property
queryEmptyKeyValues xs = property $ do
    rq <- arbitrary
    sg <- arbitrary

    let v4 = sign sg $ rq & rqQuery .~ Fold.foldMap (fromString . getNonEmpty) xs

        e  = sort $ map ((<> "=") . urlEncode True . BS8.pack . getNonEmpty) xs
        a  = BS8.split '&' . toBS $ metaCanonicalQuery v4

        m  = "Expected: " ++ show e ++ "\n" ++
              "Actual: "  ++ show a

    return . counterexample m $ e == a

newtype Signer = Signer { sign :: Request () -> Meta V4 }

instance Show Signer where
    show = const "V4"

instance Arbitrary Signer where
    arbitrary = do
        a   <- arbitrary
        reg <- arbitrary
        ts  <- arbitrary
        return $ Signer (_sgMeta . signed auth reg ts (testV4 a))

auth :: AuthEnv
auth = AuthEnv "access-key" "super-secret-key" Nothing Nothing

data TestV4

instance AWSService TestV4 where
    type Sg TestV4 = V4

    service = error "service not defined for TestV4."

testV4 :: Abbrev -> Service TestV4
testV4 a = svc
  where
    svc = Service
        { _svcAbbrev   = a
        , _svcPrefix   = Text.encodeUtf8 . Text.toLower $ toText a
        , _svcVersion  = "2012-01-01"
        , _svcEndpoint = defaultEndpoint svc
        , _svcTimeout  = Nothing
        , _svcStatus   = const False
        , _svcError    = error "_svcError not defined."
        , _svcRetry    = error "_svcRetry not defined."
        }
