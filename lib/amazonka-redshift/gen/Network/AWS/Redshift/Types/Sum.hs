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
  = Canceled
  | Failed
  | InProgress
  | Pending
  | Succeeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TableRestoreStatusType where
    parser = takeLowerText >>= \case
        "canceled" -> pure Canceled
        "failed" -> pure Failed
        "in_progress" -> pure InProgress
        "pending" -> pure Pending
        "succeeded" -> pure Succeeded
        e -> fromTextError $ "Failure parsing TableRestoreStatusType from value: '" <> e
           <> "'. Accepted values: canceled, failed, in_progress, pending, succeeded"

instance ToText TableRestoreStatusType where
    toText = \case
        Canceled -> "CANCELED"
        Failed -> "FAILED"
        InProgress -> "IN_PROGRESS"
        Pending -> "PENDING"
        Succeeded -> "SUCCEEDED"

instance Hashable     TableRestoreStatusType
instance NFData       TableRestoreStatusType
instance ToByteString TableRestoreStatusType
instance ToQuery      TableRestoreStatusType
instance ToHeader     TableRestoreStatusType

instance FromXML TableRestoreStatusType where
    parseXML = parseXMLText "TableRestoreStatusType"
