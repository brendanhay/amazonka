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

-- | The region of the S3 bucket that AWS delivers the report into.
--
--
data AWSRegion
  = ApNortheast1
  | ApNortheast3
  | ApSoutheast1
  | ApSoutheast2
  | EuCentral1
  | EuNorth1
  | EuWest1
  | UsEast1
  | UsWest1
  | UsWest2
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AWSRegion where
    parser = takeLowerText >>= \case
        "ap-northeast-1" -> pure ApNortheast1
        "ap-northeast-3" -> pure ApNortheast3
        "ap-southeast-1" -> pure ApSoutheast1
        "ap-southeast-2" -> pure ApSoutheast2
        "eu-central-1" -> pure EuCentral1
        "eu-north-1" -> pure EuNorth1
        "eu-west-1" -> pure EuWest1
        "us-east-1" -> pure UsEast1
        "us-west-1" -> pure UsWest1
        "us-west-2" -> pure UsWest2
        e -> fromTextError $ "Failure parsing AWSRegion from value: '" <> e
           <> "'. Accepted values: ap-northeast-1, ap-northeast-3, ap-southeast-1, ap-southeast-2, eu-central-1, eu-north-1, eu-west-1, us-east-1, us-west-1, us-west-2"

instance ToText AWSRegion where
    toText = \case
        ApNortheast1 -> "ap-northeast-1"
        ApNortheast3 -> "ap-northeast-3"
        ApSoutheast1 -> "ap-southeast-1"
        ApSoutheast2 -> "ap-southeast-2"
        EuCentral1 -> "eu-central-1"
        EuNorth1 -> "eu-north-1"
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

-- | The types of manifest that you want AWS to create for this report.
--
--
data AdditionalArtifact
  = Athena
  | Quicksight
  | Redshift
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AdditionalArtifact where
    parser = takeLowerText >>= \case
        "athena" -> pure Athena
        "quicksight" -> pure Quicksight
        "redshift" -> pure Redshift
        e -> fromTextError $ "Failure parsing AdditionalArtifact from value: '" <> e
           <> "'. Accepted values: athena, quicksight, redshift"

instance ToText AdditionalArtifact where
    toText = \case
        Athena -> "ATHENA"
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

-- | The compression format that AWS uses for the report.
--
--
data CompressionFormat
  = CFGzip
  | CFParquet
  | CFZip
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CompressionFormat where
    parser = takeLowerText >>= \case
        "gzip" -> pure CFGzip
        "parquet" -> pure CFParquet
        "zip" -> pure CFZip
        e -> fromTextError $ "Failure parsing CompressionFormat from value: '" <> e
           <> "'. Accepted values: gzip, parquet, zip"

instance ToText CompressionFormat where
    toText = \case
        CFGzip -> "GZIP"
        CFParquet -> "Parquet"
        CFZip -> "ZIP"

instance Hashable     CompressionFormat
instance NFData       CompressionFormat
instance ToByteString CompressionFormat
instance ToQuery      CompressionFormat
instance ToHeader     CompressionFormat

instance ToJSON CompressionFormat where
    toJSON = toJSONText

instance FromJSON CompressionFormat where
    parseJSON = parseJSONText "CompressionFormat"

-- | The format that AWS saves the report in.
--
--
data ReportFormat
  = Parquet
  | TextORcsv
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReportFormat where
    parser = takeLowerText >>= \case
        "parquet" -> pure Parquet
        "textorcsv" -> pure TextORcsv
        e -> fromTextError $ "Failure parsing ReportFormat from value: '" <> e
           <> "'. Accepted values: parquet, textorcsv"

instance ToText ReportFormat where
    toText = \case
        Parquet -> "Parquet"
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

data ReportVersioning
  = CreateNewReport
  | OverwriteReport
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReportVersioning where
    parser = takeLowerText >>= \case
        "create_new_report" -> pure CreateNewReport
        "overwrite_report" -> pure OverwriteReport
        e -> fromTextError $ "Failure parsing ReportVersioning from value: '" <> e
           <> "'. Accepted values: create_new_report, overwrite_report"

instance ToText ReportVersioning where
    toText = \case
        CreateNewReport -> "CREATE_NEW_REPORT"
        OverwriteReport -> "OVERWRITE_REPORT"

instance Hashable     ReportVersioning
instance NFData       ReportVersioning
instance ToByteString ReportVersioning
instance ToQuery      ReportVersioning
instance ToHeader     ReportVersioning

instance ToJSON ReportVersioning where
    toJSON = toJSONText

instance FromJSON ReportVersioning where
    parseJSON = parseJSONText "ReportVersioning"

-- | Whether or not AWS includes resource IDs in the report.
--
--
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

-- | The length of time covered by the report.
--
--
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
