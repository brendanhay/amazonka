{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Test.Amazonka.Arbitrary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Arbitrary where

import Amazonka.Core
import Amazonka.Data
import Amazonka.Prelude
import Amazonka.Sign.V4
import qualified Data.ByteString.Char8 as BS8
import Data.CaseInsensitive (FoldCase)
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (Day (..), UTCTime (..))
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
          { abbrev = abbrev,
            signer = v4,
            signingName = Text.encodeUtf8 . Text.toLower $ toText abbrev,
            version = "2012-01-01",
            s3AddressingStyle = S3AddressingStyleAuto,
            endpointPrefix = Text.encodeUtf8 . Text.toLower $ toText abbrev,
            endpoint = defaultEndpoint (svc abbrev),
            timeout = Nothing,
            check = const False,
            retry = Exponential 1 2 3 (Just . Text.pack . show),
            error = \status hdrs _ ->
              ServiceError $
                ServiceError'
                  { abbrev = abbrev,
                    status = status,
                    headers = hdrs,
                    code = ErrorCode "Arbitrary.Service",
                    message = Nothing,
                    requestId = Nothing
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
