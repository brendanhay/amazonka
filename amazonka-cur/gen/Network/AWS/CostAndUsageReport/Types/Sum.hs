{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostAndUsageReport.Types.Sum where

import Network.AWS.Prelude

-- | Region of customer S3 bucket.
data AWSRegion
  = ApNortheast1
  | ApSoutheast1
  | ApSoutheast2
  | EuCentral1
  | EuWest1
  | UsEast1
  | UsWest1
  | UsWest2
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AWSRegion where
    parser = takeLowerText >>= \case
        "ap-northeast-1" -> pure ApNortheast1
        "ap-southeast-1" -> pure ApSoutheast1
        "ap-southeast-2" -> pure ApSoutheast2
        "eu-central-1" -> pure EuCentral1
        "eu-west-1" -> pure EuWest1
        "us-east-1" -> pure UsEast1
        "us-west-1" -> pure UsWest1
        "us-west-2" -> pure UsWest2
        e -> fromTextError $ "Failure parsing AWSRegion from value: '" <> e
           <> "'. Accepted values: ap-northeast-1, ap-southeast-1, ap-southeast-2, eu-central-1, eu-west-1, us-east-1, us-west-1, us-west-2"

instance ToText AWSRegion where
    toText = \case
        ApNortheast1 -> "ap-northeast-1"
        ApSoutheast1 -> "ap-southeast-1"
        ApSoutheast2 -> "ap-southeast-2"
        EuCentral1 -> "eu-central-1"
        EuWest1 -> "eu-west-1"
        UsEast1 -> "us-east-1"
        UsWest1 -> "us-west-1"
        UsWest2 -> "us-west-2"

instance Hashable     AWSRegion
instance NFData       AWSRegion
instance ToByteString AWSRegion
instance ToQuery      AWSRegion
instance ToHeader     AWSRegion

instance ToJSON AWSRegion where
    toJSON = toJSONText

instance FromJSON AWSRegion where
    parseJSON = parseJSONText "AWSRegion"

-- | Enable support for Redshift and/or QuickSight.
data AdditionalArtifact
  = Quicksight
  | Redshift
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AdditionalArtifact where
    parser = takeLowerText >>= \case
        "quicksight" -> pure Quicksight
        "redshift" -> pure Redshift
        e -> fromTextError $ "Failure parsing AdditionalArtifact from value: '" <> e
           <> "'. Accepted values: quicksight, redshift"

instance ToText AdditionalArtifact where
    toText = \case
        Quicksight -> "QUICKSIGHT"
        Redshift -> "REDSHIFT"

instance Hashable     AdditionalArtifact
instance NFData       AdditionalArtifact
instance ToByteString AdditionalArtifact
instance ToQuery      AdditionalArtifact
instance ToHeader     AdditionalArtifact

instance ToJSON AdditionalArtifact where
    toJSON = toJSONText

instance FromJSON AdditionalArtifact where
    parseJSON = parseJSONText "AdditionalArtifact"

-- | Preferred compression format for report.
data CompressionFormat
  = Gzip
  | Zip
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CompressionFormat where
    parser = takeLowerText >>= \case
        "gzip" -> pure Gzip
        "zip" -> pure Zip
        e -> fromTextError $ "Failure parsing CompressionFormat from value: '" <> e
           <> "'. Accepted values: gzip, zip"

instance ToText CompressionFormat where
    toText = \case
        Gzip -> "GZIP"
        Zip -> "ZIP"

instance Hashable     CompressionFormat
instance NFData       CompressionFormat
instance ToByteString CompressionFormat
instance ToQuery      CompressionFormat
instance ToHeader     CompressionFormat

instance ToJSON CompressionFormat where
    toJSON = toJSONText

instance FromJSON CompressionFormat where
    parseJSON = parseJSONText "CompressionFormat"

-- | Preferred format for report.
data ReportFormat =
  TextORcsv
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReportFormat where
    parser = takeLowerText >>= \case
        "textorcsv" -> pure TextORcsv
        e -> fromTextError $ "Failure parsing ReportFormat from value: '" <> e
           <> "'. Accepted values: textorcsv"

instance ToText ReportFormat where
    toText = \case
        TextORcsv -> "textORcsv"

instance Hashable     ReportFormat
instance NFData       ReportFormat
instance ToByteString ReportFormat
instance ToQuery      ReportFormat
instance ToHeader     ReportFormat

instance ToJSON ReportFormat where
    toJSON = toJSONText

instance FromJSON ReportFormat where
    parseJSON = parseJSONText "ReportFormat"

-- | Preference of including Resource IDs. You can include additional details about individual resource IDs in your report.
data SchemaElement =
  Resources
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SchemaElement where
    parser = takeLowerText >>= \case
        "resources" -> pure Resources
        e -> fromTextError $ "Failure parsing SchemaElement from value: '" <> e
           <> "'. Accepted values: resources"

instance ToText SchemaElement where
    toText = \case
        Resources -> "RESOURCES"

instance Hashable     SchemaElement
instance NFData       SchemaElement
instance ToByteString SchemaElement
instance ToQuery      SchemaElement
instance ToHeader     SchemaElement

instance ToJSON SchemaElement where
    toJSON = toJSONText

instance FromJSON SchemaElement where
    parseJSON = parseJSONText "SchemaElement"

-- | The frequency on which report data are measured and displayed.
data TimeUnit
  = Daily
  | Hourly
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TimeUnit where
    parser = takeLowerText >>= \case
        "daily" -> pure Daily
        "hourly" -> pure Hourly
        e -> fromTextError $ "Failure parsing TimeUnit from value: '" <> e
           <> "'. Accepted values: daily, hourly"

instance ToText TimeUnit where
    toText = \case
        Daily -> "DAILY"
        Hourly -> "HOURLY"

instance Hashable     TimeUnit
instance NFData       TimeUnit
instance ToByteString TimeUnit
instance ToQuery      TimeUnit
instance ToHeader     TimeUnit

instance ToJSON TimeUnit where
    toJSON = toJSONText

instance FromJSON TimeUnit where
    parseJSON = parseJSONText "TimeUnit"
