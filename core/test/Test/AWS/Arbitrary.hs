{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Test.AWS.Arbitrary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Arbitrary where

import qualified Data.ByteString.Char8   as BS8
import           Data.CaseInsensitive    (CI, FoldCase)
import qualified Data.CaseInsensitive    as CI
import           Data.String
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import           Data.Time
import           Network.AWS.Data.Query
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4
import           Network.HTTP.Types
import           Test.QuickCheck.Gen     as QC
import qualified Test.QuickCheck.Unicode as Unicode
import           Test.Tasty.QuickCheck

instance Show (Signed a) where
    show = const "Signed { <Meta> <ClientRequest> }"

instance Arbitrary Service where
  arbitrary = svc <$> arbitrary
    where
      svc abbrev = Service
        { _svcAbbrev   = abbrev
        , _svcSigner   = v4
        , _svcPrefix   = Text.encodeUtf8 . Text.toLower $ toText abbrev
        , _svcVersion  = "2012-01-01"
        , _svcEndpoint = defaultEndpoint (svc abbrev)
        , _svcTimeout  = Nothing
        , _svcCheck    = const False
        , _svcRetry    = Exponential 1 2 3 (Just . Text.pack . show)
        , _svcError    = \status hdrs _ ->
            ServiceError $ ServiceError'
                { _serviceAbbrev    = abbrev
                , _serviceStatus    = status
                , _serviceHeaders   = hdrs
                , _serviceCode      = ErrorCode "Arbitrary.Service"
                , _serviceMessage   = Nothing
                , _serviceRequestId = Nothing
                }
        }

instance Arbitrary (Request ()) where
    arbitrary = Request
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary Abbrev where
    arbitrary = fromString <$> QC.elements
        [ "IAM"
        , "SDB"
        , "STS"
        , "S3"
        , "EC2"
        , "RDS"
        , "EMR"
        , "SQS"
        , "SNS"
        , "ImportExport"
        , "CloudFront"
        , "SES"
        , "Route53"
        ]

instance Arbitrary StdMethod where
    arbitrary = QC.elements
        [ GET
        , POST
        , HEAD
        , PUT
        , DELETE
        , TRACE
        , CONNECT
        , OPTIONS
        , PATCH
        ]

instance Arbitrary Region where
    arbitrary = QC.elements
        [ Ireland
        , Frankfurt
        , Tokyo
        , Singapore
        , Sydney
        , Beijing
        , NorthVirginia
        , NorthCalifornia
        , Oregon
        , GovCloud
        , GovCloudFIPS
        , SaoPaulo
        ]

instance Arbitrary RqBody where
    arbitrary = toBody <$> (arbitrary :: Gen ByteString)

instance Arbitrary RawPath where
    arbitrary = do
        xs <- listOf (BS8.pack . getNonEmpty <$> arbitrary)
        return $! rawPath (BS8.intercalate "/" xs)

instance Arbitrary QueryString where
    arbitrary = sized arbitraryQS

-- | Used to limit the recursion depth.
arbitraryQS :: Int -> Gen QueryString
arbitraryQS = \case
    0 -> QPair  <$> arbitrary <*> (QValue <$> arbitrary)
    n -> oneof
        [ QValue <$> arbitrary
        , QPair  <$> arbitrary <*> arbitraryQS (n - 1)
        , QList  <$> (take 4 <$> QC.listOf (arbitraryQS (n `div` 2)))
        ]

instance (Arbitrary a, FoldCase a) => Arbitrary (CI a) where
    arbitrary = CI.mk <$> arbitrary

instance Arbitrary Text where
    arbitrary = Text.pack <$> suchThat Unicode.string (not . null)

instance Arbitrary ByteString where
    arbitrary = BS8.pack <$> suchThat Unicode.string (not . null)

instance Arbitrary DiffTime where
    arbitrary = arbitrarySizedFractional
    shrink    = shrinkRealFrac

instance Arbitrary UTCTime where
    arbitrary = UTCTime
        <$> arbitrary
        <*> (fromRational . toRational <$> choose (0 :: Double, 86400))

    shrink u@(UTCTime day dayTime) =
           [ u { utctDay     = d } | d <- shrink day     ]
        ++ [ u { utctDayTime = t } | t <- shrink dayTime ]

instance Arbitrary Day where
    arbitrary = ModifiedJulianDay . (2000 +) <$> arbitrary
    shrink    = fmap ModifiedJulianDay . shrink . toModifiedJulianDay

instance Arbitrary (Time a) where
    arbitrary = Time <$> arbitrary

instance Arbitrary AccessKey where
    arbitrary = AccessKey <$> arbitrary

instance (Arbitrary a) => Arbitrary (Sensitive a) where
    arbitrary = Sensitive <$> arbitrary

instance Arbitrary SecretKey where
    arbitrary = SecretKey . fromString <$> suchThat Unicode.string (not . null)

instance Arbitrary SessionToken where
    arbitrary = SessionToken <$> arbitrary

instance Arbitrary AuthEnv where
    arbitrary = AuthEnv <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
