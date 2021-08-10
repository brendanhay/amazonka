{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Test.AWS.Arbitrary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Test.AWS.Arbitrary where

import qualified Data.ByteString.Char8 as BS8
import Data.CaseInsensitive (FoldCase)
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (Day (..), UTCTime (..))
import Network.AWS.Core
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.HTTP.Types (StdMethod (..))
import Test.QuickCheck.Gen as QC
import qualified Test.QuickCheck.Unicode as Unicode
import Test.Tasty.QuickCheck

instance Show (Signed a) where
  show = const "Signed { <Meta> <ClientRequest> }"

instance Arbitrary Service where
  arbitrary = svc <$> arbitrary
    where
      svc abbrev =
        Service
          { _serviceAbbrev = abbrev,
            _serviceSigner = v4,
            _serviceSigningName = Text.encodeUtf8 . Text.toLower $ toText abbrev,
            _serviceVersion = "2012-01-01",
            _serviceEndpointPrefix = Text.encodeUtf8 . Text.toLower $ toText abbrev,
            _serviceEndpoint = defaultEndpoint (svc abbrev),
            _serviceTimeout = Nothing,
            _serviceCheck = const False,
            _serviceRetry = Exponential 1 2 3 (Just . Text.pack . show),
            _serviceError = \status hdrs _ ->
              ServiceError $
                ServiceError'
                  { _serviceErrorAbbrev = abbrev,
                    _serviceErrorStatus = status,
                    _serviceErrorHeaders = hdrs,
                    _serviceErrorCode = ErrorCode "Arbitrary.Service",
                    _serviceErrorMessage = Nothing,
                    _serviceErrorRequestId = Nothing
                  }
          }

instance Arbitrary (Request ()) where
  arbitrary =
    Request
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary Abbrev where
  arbitrary =
    fromString
      <$> QC.elements
        [ "IAM",
          "SDB",
          "STS",
          "S3",
          "EC2",
          "RDS",
          "EMR",
          "SQS",
          "SNS",
          "ImportExport",
          "CloudFront",
          "SES",
          "Route53"
        ]

instance Arbitrary StdMethod where
  arbitrary =
    QC.elements
      [ GET,
        POST,
        HEAD,
        PUT,
        DELETE,
        TRACE,
        CONNECT,
        OPTIONS,
        PATCH
      ]

instance Arbitrary Region where
  arbitrary =
    QC.elements
      [ Ireland,
        Frankfurt,
        Tokyo,
        Singapore,
        Sydney,
        Beijing,
        NorthVirginia,
        NorthCalifornia,
        Oregon,
        GovCloudEast,
        GovCloudWest,
        SaoPaulo
      ]

instance Arbitrary RequestBody where
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
  0 -> QPair <$> arbitrary <*> (QValue <$> arbitrary)
  n ->
    oneof
      [ QValue <$> arbitrary,
        QPair <$> arbitrary <*> arbitraryQS (n - 1),
        QList <$> (take 4 <$> QC.listOf (arbitraryQS (n `div` 2)))
      ]

instance (Arbitrary a, FoldCase a) => Arbitrary (CI a) where
  arbitrary = CI.mk <$> arbitrary

instance Arbitrary Text where
  arbitrary = Text.pack <$> suchThat Unicode.string (not . null)

instance Arbitrary ByteString where
  arbitrary = BS8.pack <$> suchThat Unicode.string (not . null)

instance Arbitrary DiffTime where
  arbitrary = arbitrarySizedFractional
  shrink = shrinkRealFrac

instance Arbitrary UTCTime where
  arbitrary =
    UTCTime
      <$> arbitrary
      <*> (fromRational . toRational <$> choose (0 :: Double, 86400))

  shrink u@(UTCTime day dayTime) =
    [u {utctDay = d} | d <- shrink day]
      ++ [u {utctDayTime = t} | t <- shrink dayTime]

instance Arbitrary Day where
  arbitrary = ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = fmap ModifiedJulianDay . shrink . toModifiedJulianDay

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
