{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.Sum where

import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

data ParameterApplyType
  = Dynamic
  | Static
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ParameterApplyType where
    parser = takeLowerText >>= \case
        "dynamic" -> pure Dynamic
        "static" -> pure Static
        e -> fromTextError $ "Failure parsing ParameterApplyType from value: '" <> e
           <> "'. Accepted values: dynamic, static"

instance ToText ParameterApplyType where
    toText = \case
        Dynamic -> "dynamic"
        Static -> "static"

instance Hashable     ParameterApplyType
instance NFData       ParameterApplyType
instance ToByteString ParameterApplyType
instance ToQuery      ParameterApplyType
instance ToHeader     ParameterApplyType

instance FromXML ParameterApplyType where
    parseXML = parseXMLText "ParameterApplyType"

data ReservedNodeOfferingType
  = Regular
  | Upgradable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReservedNodeOfferingType where
    parser = takeLowerText >>= \case
        "regular" -> pure Regular
        "upgradable" -> pure Upgradable
        e -> fromTextError $ "Failure parsing ReservedNodeOfferingType from value: '" <> e
           <> "'. Accepted values: regular, upgradable"

instance ToText ReservedNodeOfferingType where
    toText = \case
        Regular -> "Regular"
        Upgradable -> "Upgradable"

instance Hashable     ReservedNodeOfferingType
instance NFData       ReservedNodeOfferingType
instance ToByteString ReservedNodeOfferingType
instance ToQuery      ReservedNodeOfferingType
instance ToHeader     ReservedNodeOfferingType

instance FromXML ReservedNodeOfferingType where
    parseXML = parseXMLText "ReservedNodeOfferingType"

data ScheduleState
  = Active
  | Failed
  | Modifying
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ScheduleState where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "failed" -> pure Failed
        "modifying" -> pure Modifying
        e -> fromTextError $ "Failure parsing ScheduleState from value: '" <> e
           <> "'. Accepted values: active, failed, modifying"

instance ToText ScheduleState where
    toText = \case
        Active -> "ACTIVE"
        Failed -> "FAILED"
        Modifying -> "MODIFYING"

instance Hashable     ScheduleState
instance NFData       ScheduleState
instance ToByteString ScheduleState
instance ToQuery      ScheduleState
instance ToHeader     ScheduleState

instance FromXML ScheduleState where
    parseXML = parseXMLText "ScheduleState"

data SnapshotAttributeToSortBy
  = CreateTime
  | SourceType
  | TotalSize
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SnapshotAttributeToSortBy where
    parser = takeLowerText >>= \case
        "create_time" -> pure CreateTime
        "source_type" -> pure SourceType
        "total_size" -> pure TotalSize
        e -> fromTextError $ "Failure parsing SnapshotAttributeToSortBy from value: '" <> e
           <> "'. Accepted values: create_time, source_type, total_size"

instance ToText SnapshotAttributeToSortBy where
    toText = \case
        CreateTime -> "CREATE_TIME"
        SourceType -> "SOURCE_TYPE"
        TotalSize -> "TOTAL_SIZE"

instance Hashable     SnapshotAttributeToSortBy
instance NFData       SnapshotAttributeToSortBy
instance ToByteString SnapshotAttributeToSortBy
instance ToQuery      SnapshotAttributeToSortBy
instance ToHeader     SnapshotAttributeToSortBy

data SortByOrder
  = Asc
  | Desc
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SortByOrder where
    parser = takeLowerText >>= \case
        "asc" -> pure Asc
        "desc" -> pure Desc
        e -> fromTextError $ "Failure parsing SortByOrder from value: '" <> e
           <> "'. Accepted values: asc, desc"

instance ToText SortByOrder where
    toText = \case
        Asc -> "ASC"
        Desc -> "DESC"

instance Hashable     SortByOrder
instance NFData       SortByOrder
instance ToByteString SortByOrder
instance ToQuery      SortByOrder
instance ToHeader     SortByOrder

data SourceType
  = Cluster
  | ClusterParameterGroup
  | ClusterSecurityGroup
  | ClusterSnapshot
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SourceType where
    parser = takeLowerText >>= \case
        "cluster" -> pure Cluster
        "cluster-parameter-group" -> pure ClusterParameterGroup
        "cluster-security-group" -> pure ClusterSecurityGroup
        "cluster-snapshot" -> pure ClusterSnapshot
        e -> fromTextError $ "Failure parsing SourceType from value: '" <> e
           <> "'. Accepted values: cluster, cluster-parameter-group, cluster-security-group, cluster-snapshot"

instance ToText SourceType where
    toText = \case
        Cluster -> "cluster"
        ClusterParameterGroup -> "cluster-parameter-group"
        ClusterSecurityGroup -> "cluster-security-group"
        ClusterSnapshot -> "cluster-snapshot"

instance Hashable     SourceType
instance NFData       SourceType
instance ToByteString SourceType
instance ToQuery      SourceType
instance ToHeader     SourceType

instance FromXML SourceType where
    parseXML = parseXMLText "SourceType"

data TableRestoreStatusType
  = TRSTCanceled
  | TRSTFailed
  | TRSTInProgress
  | TRSTPending
  | TRSTSucceeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TableRestoreStatusType where
    parser = takeLowerText >>= \case
        "canceled" -> pure TRSTCanceled
        "failed" -> pure TRSTFailed
        "in_progress" -> pure TRSTInProgress
        "pending" -> pure TRSTPending
        "succeeded" -> pure TRSTSucceeded
        e -> fromTextError $ "Failure parsing TableRestoreStatusType from value: '" <> e
           <> "'. Accepted values: canceled, failed, in_progress, pending, succeeded"

instance ToText TableRestoreStatusType where
    toText = \case
        TRSTCanceled -> "CANCELED"
        TRSTFailed -> "FAILED"
        TRSTInProgress -> "IN_PROGRESS"
        TRSTPending -> "PENDING"
        TRSTSucceeded -> "SUCCEEDED"

instance Hashable     TableRestoreStatusType
instance NFData       TableRestoreStatusType
instance ToByteString TableRestoreStatusType
instance ToQuery      TableRestoreStatusType
instance ToHeader     TableRestoreStatusType

instance FromXML TableRestoreStatusType where
    parseXML = parseXMLText "TableRestoreStatusType"
