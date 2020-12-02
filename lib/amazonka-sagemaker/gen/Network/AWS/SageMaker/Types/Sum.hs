{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.Sum where

import Network.AWS.Prelude

data CompressionType
  = CTGzip
  | CTNone
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CompressionType where
    parser = takeLowerText >>= \case
        "gzip" -> pure CTGzip
        "none" -> pure CTNone
        e -> fromTextError $ "Failure parsing CompressionType from value: '" <> e
           <> "'. Accepted values: gzip, none"

instance ToText CompressionType where
    toText = \case
        CTGzip -> "Gzip"
        CTNone -> "None"

instance Hashable     CompressionType
instance NFData       CompressionType
instance ToByteString CompressionType
instance ToQuery      CompressionType
instance ToHeader     CompressionType

instance ToJSON CompressionType where
    toJSON = toJSONText

instance FromJSON CompressionType where
    parseJSON = parseJSONText "CompressionType"

data DirectInternetAccess
  = Disabled
  | Enabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DirectInternetAccess where
    parser = takeLowerText >>= \case
        "disabled" -> pure Disabled
        "enabled" -> pure Enabled
        e -> fromTextError $ "Failure parsing DirectInternetAccess from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText DirectInternetAccess where
    toText = \case
        Disabled -> "Disabled"
        Enabled -> "Enabled"

instance Hashable     DirectInternetAccess
instance NFData       DirectInternetAccess
instance ToByteString DirectInternetAccess
instance ToQuery      DirectInternetAccess
instance ToHeader     DirectInternetAccess

instance ToJSON DirectInternetAccess where
    toJSON = toJSONText

instance FromJSON DirectInternetAccess where
    parseJSON = parseJSONText "DirectInternetAccess"

data EndpointConfigSortKey
  = CreationTime
  | Name
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EndpointConfigSortKey where
    parser = takeLowerText >>= \case
        "creationtime" -> pure CreationTime
        "name" -> pure Name
        e -> fromTextError $ "Failure parsing EndpointConfigSortKey from value: '" <> e
           <> "'. Accepted values: creationtime, name"

instance ToText EndpointConfigSortKey where
    toText = \case
        CreationTime -> "CreationTime"
        Name -> "Name"

instance Hashable     EndpointConfigSortKey
instance NFData       EndpointConfigSortKey
instance ToByteString EndpointConfigSortKey
instance ToQuery      EndpointConfigSortKey
instance ToHeader     EndpointConfigSortKey

instance ToJSON EndpointConfigSortKey where
    toJSON = toJSONText

data EndpointSortKey
  = ESKCreationTime
  | ESKName
  | ESKStatus
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EndpointSortKey where
    parser = takeLowerText >>= \case
        "creationtime" -> pure ESKCreationTime
        "name" -> pure ESKName
        "status" -> pure ESKStatus
        e -> fromTextError $ "Failure parsing EndpointSortKey from value: '" <> e
           <> "'. Accepted values: creationtime, name, status"

instance ToText EndpointSortKey where
    toText = \case
        ESKCreationTime -> "CreationTime"
        ESKName -> "Name"
        ESKStatus -> "Status"

instance Hashable     EndpointSortKey
instance NFData       EndpointSortKey
instance ToByteString EndpointSortKey
instance ToQuery      EndpointSortKey
instance ToHeader     EndpointSortKey

instance ToJSON EndpointSortKey where
    toJSON = toJSONText

data EndpointStatus
  = Creating
  | Deleting
  | Failed
  | InService
  | OutOfService
  | RollingBack
  | Updating
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EndpointStatus where
    parser = takeLowerText >>= \case
        "creating" -> pure Creating
        "deleting" -> pure Deleting
        "failed" -> pure Failed
        "inservice" -> pure InService
        "outofservice" -> pure OutOfService
        "rollingback" -> pure RollingBack
        "updating" -> pure Updating
        e -> fromTextError $ "Failure parsing EndpointStatus from value: '" <> e
           <> "'. Accepted values: creating, deleting, failed, inservice, outofservice, rollingback, updating"

instance ToText EndpointStatus where
    toText = \case
        Creating -> "Creating"
        Deleting -> "Deleting"
        Failed -> "Failed"
        InService -> "InService"
        OutOfService -> "OutOfService"
        RollingBack -> "RollingBack"
        Updating -> "Updating"

instance Hashable     EndpointStatus
instance NFData       EndpointStatus
instance ToByteString EndpointStatus
instance ToQuery      EndpointStatus
instance ToHeader     EndpointStatus

instance ToJSON EndpointStatus where
    toJSON = toJSONText

instance FromJSON EndpointStatus where
    parseJSON = parseJSONText "EndpointStatus"

data InstanceType
  = Ml_M4_10XLarge
  | Ml_M4_16XLarge
  | Ml_M4_2XLarge
  | Ml_M4_4XLarge
  | Ml_M4_XLarge
  | Ml_P2_16XLarge
  | Ml_P2_8XLarge
  | Ml_P2_XLarge
  | Ml_P3_16XLarge
  | Ml_P3_2XLarge
  | Ml_P3_8XLarge
  | Ml_T2_2XLarge
  | Ml_T2_Large
  | Ml_T2_Medium
  | Ml_T2_XLarge
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceType where
    parser = takeLowerText >>= \case
        "ml.m4.10xlarge" -> pure Ml_M4_10XLarge
        "ml.m4.16xlarge" -> pure Ml_M4_16XLarge
        "ml.m4.2xlarge" -> pure Ml_M4_2XLarge
        "ml.m4.4xlarge" -> pure Ml_M4_4XLarge
        "ml.m4.xlarge" -> pure Ml_M4_XLarge
        "ml.p2.16xlarge" -> pure Ml_P2_16XLarge
        "ml.p2.8xlarge" -> pure Ml_P2_8XLarge
        "ml.p2.xlarge" -> pure Ml_P2_XLarge
        "ml.p3.16xlarge" -> pure Ml_P3_16XLarge
        "ml.p3.2xlarge" -> pure Ml_P3_2XLarge
        "ml.p3.8xlarge" -> pure Ml_P3_8XLarge
        "ml.t2.2xlarge" -> pure Ml_T2_2XLarge
        "ml.t2.large" -> pure Ml_T2_Large
        "ml.t2.medium" -> pure Ml_T2_Medium
        "ml.t2.xlarge" -> pure Ml_T2_XLarge
        e -> fromTextError $ "Failure parsing InstanceType from value: '" <> e
           <> "'. Accepted values: ml.m4.10xlarge, ml.m4.16xlarge, ml.m4.2xlarge, ml.m4.4xlarge, ml.m4.xlarge, ml.p2.16xlarge, ml.p2.8xlarge, ml.p2.xlarge, ml.p3.16xlarge, ml.p3.2xlarge, ml.p3.8xlarge, ml.t2.2xlarge, ml.t2.large, ml.t2.medium, ml.t2.xlarge"

instance ToText InstanceType where
    toText = \case
        Ml_M4_10XLarge -> "ml.m4.10xlarge"
        Ml_M4_16XLarge -> "ml.m4.16xlarge"
        Ml_M4_2XLarge -> "ml.m4.2xlarge"
        Ml_M4_4XLarge -> "ml.m4.4xlarge"
        Ml_M4_XLarge -> "ml.m4.xlarge"
        Ml_P2_16XLarge -> "ml.p2.16xlarge"
        Ml_P2_8XLarge -> "ml.p2.8xlarge"
        Ml_P2_XLarge -> "ml.p2.xlarge"
        Ml_P3_16XLarge -> "ml.p3.16xlarge"
        Ml_P3_2XLarge -> "ml.p3.2xlarge"
        Ml_P3_8XLarge -> "ml.p3.8xlarge"
        Ml_T2_2XLarge -> "ml.t2.2xlarge"
        Ml_T2_Large -> "ml.t2.large"
        Ml_T2_Medium -> "ml.t2.medium"
        Ml_T2_XLarge -> "ml.t2.xlarge"

instance Hashable     InstanceType
instance NFData       InstanceType
instance ToByteString InstanceType
instance ToQuery      InstanceType
instance ToHeader     InstanceType

instance ToJSON InstanceType where
    toJSON = toJSONText

instance FromJSON InstanceType where
    parseJSON = parseJSONText "InstanceType"

data ModelSortKey
  = MSKCreationTime
  | MSKName
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ModelSortKey where
    parser = takeLowerText >>= \case
        "creationtime" -> pure MSKCreationTime
        "name" -> pure MSKName
        e -> fromTextError $ "Failure parsing ModelSortKey from value: '" <> e
           <> "'. Accepted values: creationtime, name"

instance ToText ModelSortKey where
    toText = \case
        MSKCreationTime -> "CreationTime"
        MSKName -> "Name"

instance Hashable     ModelSortKey
instance NFData       ModelSortKey
instance ToByteString ModelSortKey
instance ToQuery      ModelSortKey
instance ToHeader     ModelSortKey

instance ToJSON ModelSortKey where
    toJSON = toJSONText

data NotebookInstanceLifecycleConfigSortKey
  = NILCSKCreationTime
  | NILCSKLastModifiedTime
  | NILCSKName
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NotebookInstanceLifecycleConfigSortKey where
    parser = takeLowerText >>= \case
        "creationtime" -> pure NILCSKCreationTime
        "lastmodifiedtime" -> pure NILCSKLastModifiedTime
        "name" -> pure NILCSKName
        e -> fromTextError $ "Failure parsing NotebookInstanceLifecycleConfigSortKey from value: '" <> e
           <> "'. Accepted values: creationtime, lastmodifiedtime, name"

instance ToText NotebookInstanceLifecycleConfigSortKey where
    toText = \case
        NILCSKCreationTime -> "CreationTime"
        NILCSKLastModifiedTime -> "LastModifiedTime"
        NILCSKName -> "Name"

instance Hashable     NotebookInstanceLifecycleConfigSortKey
instance NFData       NotebookInstanceLifecycleConfigSortKey
instance ToByteString NotebookInstanceLifecycleConfigSortKey
instance ToQuery      NotebookInstanceLifecycleConfigSortKey
instance ToHeader     NotebookInstanceLifecycleConfigSortKey

instance ToJSON NotebookInstanceLifecycleConfigSortKey where
    toJSON = toJSONText

data NotebookInstanceLifecycleConfigSortOrder
  = NILCSOAscending
  | NILCSODescending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NotebookInstanceLifecycleConfigSortOrder where
    parser = takeLowerText >>= \case
        "ascending" -> pure NILCSOAscending
        "descending" -> pure NILCSODescending
        e -> fromTextError $ "Failure parsing NotebookInstanceLifecycleConfigSortOrder from value: '" <> e
           <> "'. Accepted values: ascending, descending"

instance ToText NotebookInstanceLifecycleConfigSortOrder where
    toText = \case
        NILCSOAscending -> "Ascending"
        NILCSODescending -> "Descending"

instance Hashable     NotebookInstanceLifecycleConfigSortOrder
instance NFData       NotebookInstanceLifecycleConfigSortOrder
instance ToByteString NotebookInstanceLifecycleConfigSortOrder
instance ToQuery      NotebookInstanceLifecycleConfigSortOrder
instance ToHeader     NotebookInstanceLifecycleConfigSortOrder

instance ToJSON NotebookInstanceLifecycleConfigSortOrder where
    toJSON = toJSONText

data NotebookInstanceSortKey
  = NISKCreationTime
  | NISKName
  | NISKStatus
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NotebookInstanceSortKey where
    parser = takeLowerText >>= \case
        "creationtime" -> pure NISKCreationTime
        "name" -> pure NISKName
        "status" -> pure NISKStatus
        e -> fromTextError $ "Failure parsing NotebookInstanceSortKey from value: '" <> e
           <> "'. Accepted values: creationtime, name, status"

instance ToText NotebookInstanceSortKey where
    toText = \case
        NISKCreationTime -> "CreationTime"
        NISKName -> "Name"
        NISKStatus -> "Status"

instance Hashable     NotebookInstanceSortKey
instance NFData       NotebookInstanceSortKey
instance ToByteString NotebookInstanceSortKey
instance ToQuery      NotebookInstanceSortKey
instance ToHeader     NotebookInstanceSortKey

instance ToJSON NotebookInstanceSortKey where
    toJSON = toJSONText

data NotebookInstanceSortOrder
  = Ascending
  | Descending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NotebookInstanceSortOrder where
    parser = takeLowerText >>= \case
        "ascending" -> pure Ascending
        "descending" -> pure Descending
        e -> fromTextError $ "Failure parsing NotebookInstanceSortOrder from value: '" <> e
           <> "'. Accepted values: ascending, descending"

instance ToText NotebookInstanceSortOrder where
    toText = \case
        Ascending -> "Ascending"
        Descending -> "Descending"

instance Hashable     NotebookInstanceSortOrder
instance NFData       NotebookInstanceSortOrder
instance ToByteString NotebookInstanceSortOrder
instance ToQuery      NotebookInstanceSortOrder
instance ToHeader     NotebookInstanceSortOrder

instance ToJSON NotebookInstanceSortOrder where
    toJSON = toJSONText

data NotebookInstanceStatus
  = NISDeleting
  | NISFailed
  | NISInService
  | NISPending
  | NISStopped
  | NISStopping
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NotebookInstanceStatus where
    parser = takeLowerText >>= \case
        "deleting" -> pure NISDeleting
        "failed" -> pure NISFailed
        "inservice" -> pure NISInService
        "pending" -> pure NISPending
        "stopped" -> pure NISStopped
        "stopping" -> pure NISStopping
        e -> fromTextError $ "Failure parsing NotebookInstanceStatus from value: '" <> e
           <> "'. Accepted values: deleting, failed, inservice, pending, stopped, stopping"

instance ToText NotebookInstanceStatus where
    toText = \case
        NISDeleting -> "Deleting"
        NISFailed -> "Failed"
        NISInService -> "InService"
        NISPending -> "Pending"
        NISStopped -> "Stopped"
        NISStopping -> "Stopping"

instance Hashable     NotebookInstanceStatus
instance NFData       NotebookInstanceStatus
instance ToByteString NotebookInstanceStatus
instance ToQuery      NotebookInstanceStatus
instance ToHeader     NotebookInstanceStatus

instance ToJSON NotebookInstanceStatus where
    toJSON = toJSONText

instance FromJSON NotebookInstanceStatus where
    parseJSON = parseJSONText "NotebookInstanceStatus"

data OrderKey
  = OKAscending
  | OKDescending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OrderKey where
    parser = takeLowerText >>= \case
        "ascending" -> pure OKAscending
        "descending" -> pure OKDescending
        e -> fromTextError $ "Failure parsing OrderKey from value: '" <> e
           <> "'. Accepted values: ascending, descending"

instance ToText OrderKey where
    toText = \case
        OKAscending -> "Ascending"
        OKDescending -> "Descending"

instance Hashable     OrderKey
instance NFData       OrderKey
instance ToByteString OrderKey
instance ToQuery      OrderKey
instance ToHeader     OrderKey

instance ToJSON OrderKey where
    toJSON = toJSONText

data ProductionVariantInstanceType
  = PVITMl_C4_2XLarge
  | PVITMl_C4_4XLarge
  | PVITMl_C4_8XLarge
  | PVITMl_C4_Large
  | PVITMl_C4_XLarge
  | PVITMl_C5_18XLarge
  | PVITMl_C5_2XLarge
  | PVITMl_C5_4XLarge
  | PVITMl_C5_9XLarge
  | PVITMl_C5_Large
  | PVITMl_C5_XLarge
  | PVITMl_M4_10XLarge
  | PVITMl_M4_16XLarge
  | PVITMl_M4_2XLarge
  | PVITMl_M4_4XLarge
  | PVITMl_M4_XLarge
  | PVITMl_M5_12XLarge
  | PVITMl_M5_24XLarge
  | PVITMl_M5_2XLarge
  | PVITMl_M5_4XLarge
  | PVITMl_M5_Large
  | PVITMl_M5_XLarge
  | PVITMl_P2_16XLarge
  | PVITMl_P2_8XLarge
  | PVITMl_P2_XLarge
  | PVITMl_P3_16XLarge
  | PVITMl_P3_2XLarge
  | PVITMl_P3_8XLarge
  | PVITMl_T2_2XLarge
  | PVITMl_T2_Large
  | PVITMl_T2_Medium
  | PVITMl_T2_XLarge
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProductionVariantInstanceType where
    parser = takeLowerText >>= \case
        "ml.c4.2xlarge" -> pure PVITMl_C4_2XLarge
        "ml.c4.4xlarge" -> pure PVITMl_C4_4XLarge
        "ml.c4.8xlarge" -> pure PVITMl_C4_8XLarge
        "ml.c4.large" -> pure PVITMl_C4_Large
        "ml.c4.xlarge" -> pure PVITMl_C4_XLarge
        "ml.c5.18xlarge" -> pure PVITMl_C5_18XLarge
        "ml.c5.2xlarge" -> pure PVITMl_C5_2XLarge
        "ml.c5.4xlarge" -> pure PVITMl_C5_4XLarge
        "ml.c5.9xlarge" -> pure PVITMl_C5_9XLarge
        "ml.c5.large" -> pure PVITMl_C5_Large
        "ml.c5.xlarge" -> pure PVITMl_C5_XLarge
        "ml.m4.10xlarge" -> pure PVITMl_M4_10XLarge
        "ml.m4.16xlarge" -> pure PVITMl_M4_16XLarge
        "ml.m4.2xlarge" -> pure PVITMl_M4_2XLarge
        "ml.m4.4xlarge" -> pure PVITMl_M4_4XLarge
        "ml.m4.xlarge" -> pure PVITMl_M4_XLarge
        "ml.m5.12xlarge" -> pure PVITMl_M5_12XLarge
        "ml.m5.24xlarge" -> pure PVITMl_M5_24XLarge
        "ml.m5.2xlarge" -> pure PVITMl_M5_2XLarge
        "ml.m5.4xlarge" -> pure PVITMl_M5_4XLarge
        "ml.m5.large" -> pure PVITMl_M5_Large
        "ml.m5.xlarge" -> pure PVITMl_M5_XLarge
        "ml.p2.16xlarge" -> pure PVITMl_P2_16XLarge
        "ml.p2.8xlarge" -> pure PVITMl_P2_8XLarge
        "ml.p2.xlarge" -> pure PVITMl_P2_XLarge
        "ml.p3.16xlarge" -> pure PVITMl_P3_16XLarge
        "ml.p3.2xlarge" -> pure PVITMl_P3_2XLarge
        "ml.p3.8xlarge" -> pure PVITMl_P3_8XLarge
        "ml.t2.2xlarge" -> pure PVITMl_T2_2XLarge
        "ml.t2.large" -> pure PVITMl_T2_Large
        "ml.t2.medium" -> pure PVITMl_T2_Medium
        "ml.t2.xlarge" -> pure PVITMl_T2_XLarge
        e -> fromTextError $ "Failure parsing ProductionVariantInstanceType from value: '" <> e
           <> "'. Accepted values: ml.c4.2xlarge, ml.c4.4xlarge, ml.c4.8xlarge, ml.c4.large, ml.c4.xlarge, ml.c5.18xlarge, ml.c5.2xlarge, ml.c5.4xlarge, ml.c5.9xlarge, ml.c5.large, ml.c5.xlarge, ml.m4.10xlarge, ml.m4.16xlarge, ml.m4.2xlarge, ml.m4.4xlarge, ml.m4.xlarge, ml.m5.12xlarge, ml.m5.24xlarge, ml.m5.2xlarge, ml.m5.4xlarge, ml.m5.large, ml.m5.xlarge, ml.p2.16xlarge, ml.p2.8xlarge, ml.p2.xlarge, ml.p3.16xlarge, ml.p3.2xlarge, ml.p3.8xlarge, ml.t2.2xlarge, ml.t2.large, ml.t2.medium, ml.t2.xlarge"

instance ToText ProductionVariantInstanceType where
    toText = \case
        PVITMl_C4_2XLarge -> "ml.c4.2xlarge"
        PVITMl_C4_4XLarge -> "ml.c4.4xlarge"
        PVITMl_C4_8XLarge -> "ml.c4.8xlarge"
        PVITMl_C4_Large -> "ml.c4.large"
        PVITMl_C4_XLarge -> "ml.c4.xlarge"
        PVITMl_C5_18XLarge -> "ml.c5.18xlarge"
        PVITMl_C5_2XLarge -> "ml.c5.2xlarge"
        PVITMl_C5_4XLarge -> "ml.c5.4xlarge"
        PVITMl_C5_9XLarge -> "ml.c5.9xlarge"
        PVITMl_C5_Large -> "ml.c5.large"
        PVITMl_C5_XLarge -> "ml.c5.xlarge"
        PVITMl_M4_10XLarge -> "ml.m4.10xlarge"
        PVITMl_M4_16XLarge -> "ml.m4.16xlarge"
        PVITMl_M4_2XLarge -> "ml.m4.2xlarge"
        PVITMl_M4_4XLarge -> "ml.m4.4xlarge"
        PVITMl_M4_XLarge -> "ml.m4.xlarge"
        PVITMl_M5_12XLarge -> "ml.m5.12xlarge"
        PVITMl_M5_24XLarge -> "ml.m5.24xlarge"
        PVITMl_M5_2XLarge -> "ml.m5.2xlarge"
        PVITMl_M5_4XLarge -> "ml.m5.4xlarge"
        PVITMl_M5_Large -> "ml.m5.large"
        PVITMl_M5_XLarge -> "ml.m5.xlarge"
        PVITMl_P2_16XLarge -> "ml.p2.16xlarge"
        PVITMl_P2_8XLarge -> "ml.p2.8xlarge"
        PVITMl_P2_XLarge -> "ml.p2.xlarge"
        PVITMl_P3_16XLarge -> "ml.p3.16xlarge"
        PVITMl_P3_2XLarge -> "ml.p3.2xlarge"
        PVITMl_P3_8XLarge -> "ml.p3.8xlarge"
        PVITMl_T2_2XLarge -> "ml.t2.2xlarge"
        PVITMl_T2_Large -> "ml.t2.large"
        PVITMl_T2_Medium -> "ml.t2.medium"
        PVITMl_T2_XLarge -> "ml.t2.xlarge"

instance Hashable     ProductionVariantInstanceType
instance NFData       ProductionVariantInstanceType
instance ToByteString ProductionVariantInstanceType
instance ToQuery      ProductionVariantInstanceType
instance ToHeader     ProductionVariantInstanceType

instance ToJSON ProductionVariantInstanceType where
    toJSON = toJSONText

instance FromJSON ProductionVariantInstanceType where
    parseJSON = parseJSONText "ProductionVariantInstanceType"

data RecordWrapper
  = None
  | RecordIO
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RecordWrapper where
    parser = takeLowerText >>= \case
        "none" -> pure None
        "recordio" -> pure RecordIO
        e -> fromTextError $ "Failure parsing RecordWrapper from value: '" <> e
           <> "'. Accepted values: none, recordio"

instance ToText RecordWrapper where
    toText = \case
        None -> "None"
        RecordIO -> "RecordIO"

instance Hashable     RecordWrapper
instance NFData       RecordWrapper
instance ToByteString RecordWrapper
instance ToQuery      RecordWrapper
instance ToHeader     RecordWrapper

instance ToJSON RecordWrapper where
    toJSON = toJSONText

instance FromJSON RecordWrapper where
    parseJSON = parseJSONText "RecordWrapper"

data S3DataDistribution
  = FullyReplicated
  | ShardedByS3Key
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText S3DataDistribution where
    parser = takeLowerText >>= \case
        "fullyreplicated" -> pure FullyReplicated
        "shardedbys3key" -> pure ShardedByS3Key
        e -> fromTextError $ "Failure parsing S3DataDistribution from value: '" <> e
           <> "'. Accepted values: fullyreplicated, shardedbys3key"

instance ToText S3DataDistribution where
    toText = \case
        FullyReplicated -> "FullyReplicated"
        ShardedByS3Key -> "ShardedByS3Key"

instance Hashable     S3DataDistribution
instance NFData       S3DataDistribution
instance ToByteString S3DataDistribution
instance ToQuery      S3DataDistribution
instance ToHeader     S3DataDistribution

instance ToJSON S3DataDistribution where
    toJSON = toJSONText

instance FromJSON S3DataDistribution where
    parseJSON = parseJSONText "S3DataDistribution"

data S3DataType
  = ManifestFile
  | S3Prefix
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText S3DataType where
    parser = takeLowerText >>= \case
        "manifestfile" -> pure ManifestFile
        "s3prefix" -> pure S3Prefix
        e -> fromTextError $ "Failure parsing S3DataType from value: '" <> e
           <> "'. Accepted values: manifestfile, s3prefix"

instance ToText S3DataType where
    toText = \case
        ManifestFile -> "ManifestFile"
        S3Prefix -> "S3Prefix"

instance Hashable     S3DataType
instance NFData       S3DataType
instance ToByteString S3DataType
instance ToQuery      S3DataType
instance ToHeader     S3DataType

instance ToJSON S3DataType where
    toJSON = toJSONText

instance FromJSON S3DataType where
    parseJSON = parseJSONText "S3DataType"

data SecondaryStatus
  = SSCompleted
  | SSDownloading
  | SSFailed
  | SSMaxRuntimeExceeded
  | SSStarting
  | SSStopped
  | SSStopping
  | SSTraining
  | SSUploading
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SecondaryStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure SSCompleted
        "downloading" -> pure SSDownloading
        "failed" -> pure SSFailed
        "maxruntimeexceeded" -> pure SSMaxRuntimeExceeded
        "starting" -> pure SSStarting
        "stopped" -> pure SSStopped
        "stopping" -> pure SSStopping
        "training" -> pure SSTraining
        "uploading" -> pure SSUploading
        e -> fromTextError $ "Failure parsing SecondaryStatus from value: '" <> e
           <> "'. Accepted values: completed, downloading, failed, maxruntimeexceeded, starting, stopped, stopping, training, uploading"

instance ToText SecondaryStatus where
    toText = \case
        SSCompleted -> "Completed"
        SSDownloading -> "Downloading"
        SSFailed -> "Failed"
        SSMaxRuntimeExceeded -> "MaxRuntimeExceeded"
        SSStarting -> "Starting"
        SSStopped -> "Stopped"
        SSStopping -> "Stopping"
        SSTraining -> "Training"
        SSUploading -> "Uploading"

instance Hashable     SecondaryStatus
instance NFData       SecondaryStatus
instance ToByteString SecondaryStatus
instance ToQuery      SecondaryStatus
instance ToHeader     SecondaryStatus

instance FromJSON SecondaryStatus where
    parseJSON = parseJSONText "SecondaryStatus"

data SortBy
  = SBCreationTime
  | SBName
  | SBStatus
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SortBy where
    parser = takeLowerText >>= \case
        "creationtime" -> pure SBCreationTime
        "name" -> pure SBName
        "status" -> pure SBStatus
        e -> fromTextError $ "Failure parsing SortBy from value: '" <> e
           <> "'. Accepted values: creationtime, name, status"

instance ToText SortBy where
    toText = \case
        SBCreationTime -> "CreationTime"
        SBName -> "Name"
        SBStatus -> "Status"

instance Hashable     SortBy
instance NFData       SortBy
instance ToByteString SortBy
instance ToQuery      SortBy
instance ToHeader     SortBy

instance ToJSON SortBy where
    toJSON = toJSONText

data SortOrder
  = SOAscending
  | SODescending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SortOrder where
    parser = takeLowerText >>= \case
        "ascending" -> pure SOAscending
        "descending" -> pure SODescending
        e -> fromTextError $ "Failure parsing SortOrder from value: '" <> e
           <> "'. Accepted values: ascending, descending"

instance ToText SortOrder where
    toText = \case
        SOAscending -> "Ascending"
        SODescending -> "Descending"

instance Hashable     SortOrder
instance NFData       SortOrder
instance ToByteString SortOrder
instance ToQuery      SortOrder
instance ToHeader     SortOrder

instance ToJSON SortOrder where
    toJSON = toJSONText

data TrainingInputMode
  = File
  | Pipe
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TrainingInputMode where
    parser = takeLowerText >>= \case
        "file" -> pure File
        "pipe" -> pure Pipe
        e -> fromTextError $ "Failure parsing TrainingInputMode from value: '" <> e
           <> "'. Accepted values: file, pipe"

instance ToText TrainingInputMode where
    toText = \case
        File -> "File"
        Pipe -> "Pipe"

instance Hashable     TrainingInputMode
instance NFData       TrainingInputMode
instance ToByteString TrainingInputMode
instance ToQuery      TrainingInputMode
instance ToHeader     TrainingInputMode

instance ToJSON TrainingInputMode where
    toJSON = toJSONText

instance FromJSON TrainingInputMode where
    parseJSON = parseJSONText "TrainingInputMode"

data TrainingInstanceType
  = TITMl_C4_2XLarge
  | TITMl_C4_4XLarge
  | TITMl_C4_8XLarge
  | TITMl_C4_XLarge
  | TITMl_C5_18XLarge
  | TITMl_C5_2XLarge
  | TITMl_C5_4XLarge
  | TITMl_C5_9XLarge
  | TITMl_C5_XLarge
  | TITMl_M4_10XLarge
  | TITMl_M4_16XLarge
  | TITMl_M4_2XLarge
  | TITMl_M4_4XLarge
  | TITMl_M4_XLarge
  | TITMl_M5_12XLarge
  | TITMl_M5_24XLarge
  | TITMl_M5_2XLarge
  | TITMl_M5_4XLarge
  | TITMl_M5_Large
  | TITMl_M5_XLarge
  | TITMl_P2_16XLarge
  | TITMl_P2_8XLarge
  | TITMl_P2_XLarge
  | TITMl_P3_16XLarge
  | TITMl_P3_2XLarge
  | TITMl_P3_8XLarge
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TrainingInstanceType where
    parser = takeLowerText >>= \case
        "ml.c4.2xlarge" -> pure TITMl_C4_2XLarge
        "ml.c4.4xlarge" -> pure TITMl_C4_4XLarge
        "ml.c4.8xlarge" -> pure TITMl_C4_8XLarge
        "ml.c4.xlarge" -> pure TITMl_C4_XLarge
        "ml.c5.18xlarge" -> pure TITMl_C5_18XLarge
        "ml.c5.2xlarge" -> pure TITMl_C5_2XLarge
        "ml.c5.4xlarge" -> pure TITMl_C5_4XLarge
        "ml.c5.9xlarge" -> pure TITMl_C5_9XLarge
        "ml.c5.xlarge" -> pure TITMl_C5_XLarge
        "ml.m4.10xlarge" -> pure TITMl_M4_10XLarge
        "ml.m4.16xlarge" -> pure TITMl_M4_16XLarge
        "ml.m4.2xlarge" -> pure TITMl_M4_2XLarge
        "ml.m4.4xlarge" -> pure TITMl_M4_4XLarge
        "ml.m4.xlarge" -> pure TITMl_M4_XLarge
        "ml.m5.12xlarge" -> pure TITMl_M5_12XLarge
        "ml.m5.24xlarge" -> pure TITMl_M5_24XLarge
        "ml.m5.2xlarge" -> pure TITMl_M5_2XLarge
        "ml.m5.4xlarge" -> pure TITMl_M5_4XLarge
        "ml.m5.large" -> pure TITMl_M5_Large
        "ml.m5.xlarge" -> pure TITMl_M5_XLarge
        "ml.p2.16xlarge" -> pure TITMl_P2_16XLarge
        "ml.p2.8xlarge" -> pure TITMl_P2_8XLarge
        "ml.p2.xlarge" -> pure TITMl_P2_XLarge
        "ml.p3.16xlarge" -> pure TITMl_P3_16XLarge
        "ml.p3.2xlarge" -> pure TITMl_P3_2XLarge
        "ml.p3.8xlarge" -> pure TITMl_P3_8XLarge
        e -> fromTextError $ "Failure parsing TrainingInstanceType from value: '" <> e
           <> "'. Accepted values: ml.c4.2xlarge, ml.c4.4xlarge, ml.c4.8xlarge, ml.c4.xlarge, ml.c5.18xlarge, ml.c5.2xlarge, ml.c5.4xlarge, ml.c5.9xlarge, ml.c5.xlarge, ml.m4.10xlarge, ml.m4.16xlarge, ml.m4.2xlarge, ml.m4.4xlarge, ml.m4.xlarge, ml.m5.12xlarge, ml.m5.24xlarge, ml.m5.2xlarge, ml.m5.4xlarge, ml.m5.large, ml.m5.xlarge, ml.p2.16xlarge, ml.p2.8xlarge, ml.p2.xlarge, ml.p3.16xlarge, ml.p3.2xlarge, ml.p3.8xlarge"

instance ToText TrainingInstanceType where
    toText = \case
        TITMl_C4_2XLarge -> "ml.c4.2xlarge"
        TITMl_C4_4XLarge -> "ml.c4.4xlarge"
        TITMl_C4_8XLarge -> "ml.c4.8xlarge"
        TITMl_C4_XLarge -> "ml.c4.xlarge"
        TITMl_C5_18XLarge -> "ml.c5.18xlarge"
        TITMl_C5_2XLarge -> "ml.c5.2xlarge"
        TITMl_C5_4XLarge -> "ml.c5.4xlarge"
        TITMl_C5_9XLarge -> "ml.c5.9xlarge"
        TITMl_C5_XLarge -> "ml.c5.xlarge"
        TITMl_M4_10XLarge -> "ml.m4.10xlarge"
        TITMl_M4_16XLarge -> "ml.m4.16xlarge"
        TITMl_M4_2XLarge -> "ml.m4.2xlarge"
        TITMl_M4_4XLarge -> "ml.m4.4xlarge"
        TITMl_M4_XLarge -> "ml.m4.xlarge"
        TITMl_M5_12XLarge -> "ml.m5.12xlarge"
        TITMl_M5_24XLarge -> "ml.m5.24xlarge"
        TITMl_M5_2XLarge -> "ml.m5.2xlarge"
        TITMl_M5_4XLarge -> "ml.m5.4xlarge"
        TITMl_M5_Large -> "ml.m5.large"
        TITMl_M5_XLarge -> "ml.m5.xlarge"
        TITMl_P2_16XLarge -> "ml.p2.16xlarge"
        TITMl_P2_8XLarge -> "ml.p2.8xlarge"
        TITMl_P2_XLarge -> "ml.p2.xlarge"
        TITMl_P3_16XLarge -> "ml.p3.16xlarge"
        TITMl_P3_2XLarge -> "ml.p3.2xlarge"
        TITMl_P3_8XLarge -> "ml.p3.8xlarge"

instance Hashable     TrainingInstanceType
instance NFData       TrainingInstanceType
instance ToByteString TrainingInstanceType
instance ToQuery      TrainingInstanceType
instance ToHeader     TrainingInstanceType

instance ToJSON TrainingInstanceType where
    toJSON = toJSONText

instance FromJSON TrainingInstanceType where
    parseJSON = parseJSONText "TrainingInstanceType"

data TrainingJobStatus
  = TJSCompleted
  | TJSFailed
  | TJSInProgress
  | TJSStopped
  | TJSStopping
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TrainingJobStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure TJSCompleted
        "failed" -> pure TJSFailed
        "inprogress" -> pure TJSInProgress
        "stopped" -> pure TJSStopped
        "stopping" -> pure TJSStopping
        e -> fromTextError $ "Failure parsing TrainingJobStatus from value: '" <> e
           <> "'. Accepted values: completed, failed, inprogress, stopped, stopping"

instance ToText TrainingJobStatus where
    toText = \case
        TJSCompleted -> "Completed"
        TJSFailed -> "Failed"
        TJSInProgress -> "InProgress"
        TJSStopped -> "Stopped"
        TJSStopping -> "Stopping"

instance Hashable     TrainingJobStatus
instance NFData       TrainingJobStatus
instance ToByteString TrainingJobStatus
instance ToQuery      TrainingJobStatus
instance ToHeader     TrainingJobStatus

instance ToJSON TrainingJobStatus where
    toJSON = toJSONText

instance FromJSON TrainingJobStatus where
    parseJSON = parseJSONText "TrainingJobStatus"
