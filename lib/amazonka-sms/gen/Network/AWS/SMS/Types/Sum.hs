{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.Sum where

import Network.AWS.Prelude

-- | Capabilities for a Connector
data ConnectorCapability =
  CCVsphere
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ConnectorCapability where
    parser = takeLowerText >>= \case
        "vsphere" -> pure CCVsphere
        e -> fromTextError $ "Failure parsing ConnectorCapability from value: '" <> e
           <> "'. Accepted values: vsphere"

instance ToText ConnectorCapability where
    toText = \case
        CCVsphere -> "VSPHERE"

instance Hashable     ConnectorCapability
instance NFData       ConnectorCapability
instance ToByteString ConnectorCapability
instance ToQuery      ConnectorCapability
instance ToHeader     ConnectorCapability

instance FromJSON ConnectorCapability where
    parseJSON = parseJSONText "ConnectorCapability"

-- | Status of on-premise Connector
data ConnectorStatus
  = Healthy
  | Unhealthy
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ConnectorStatus where
    parser = takeLowerText >>= \case
        "healthy" -> pure Healthy
        "unhealthy" -> pure Unhealthy
        e -> fromTextError $ "Failure parsing ConnectorStatus from value: '" <> e
           <> "'. Accepted values: healthy, unhealthy"

instance ToText ConnectorStatus where
    toText = \case
        Healthy -> "HEALTHY"
        Unhealthy -> "UNHEALTHY"

instance Hashable     ConnectorStatus
instance NFData       ConnectorStatus
instance ToByteString ConnectorStatus
instance ToQuery      ConnectorStatus
instance ToHeader     ConnectorStatus

instance FromJSON ConnectorStatus where
    parseJSON = parseJSONText "ConnectorStatus"

-- | The license type to be used for the Amazon Machine Image (AMI) created after a successful ReplicationRun.
data LicenseType
  = AWS
  | Byol
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LicenseType where
    parser = takeLowerText >>= \case
        "aws" -> pure AWS
        "byol" -> pure Byol
        e -> fromTextError $ "Failure parsing LicenseType from value: '" <> e
           <> "'. Accepted values: aws, byol"

instance ToText LicenseType where
    toText = \case
        AWS -> "AWS"
        Byol -> "BYOL"

instance Hashable     LicenseType
instance NFData       LicenseType
instance ToByteString LicenseType
instance ToQuery      LicenseType
instance ToHeader     LicenseType

instance ToJSON LicenseType where
    toJSON = toJSONText

instance FromJSON LicenseType where
    parseJSON = parseJSONText "LicenseType"

-- | Current state of Replication Job
data ReplicationJobState
  = RJSActive
  | RJSDeleted
  | RJSDeleting
  | RJSFailed
  | RJSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReplicationJobState where
    parser = takeLowerText >>= \case
        "active" -> pure RJSActive
        "deleted" -> pure RJSDeleted
        "deleting" -> pure RJSDeleting
        "failed" -> pure RJSFailed
        "pending" -> pure RJSPending
        e -> fromTextError $ "Failure parsing ReplicationJobState from value: '" <> e
           <> "'. Accepted values: active, deleted, deleting, failed, pending"

instance ToText ReplicationJobState where
    toText = \case
        RJSActive -> "ACTIVE"
        RJSDeleted -> "DELETED"
        RJSDeleting -> "DELETING"
        RJSFailed -> "FAILED"
        RJSPending -> "PENDING"

instance Hashable     ReplicationJobState
instance NFData       ReplicationJobState
instance ToByteString ReplicationJobState
instance ToQuery      ReplicationJobState
instance ToHeader     ReplicationJobState

instance FromJSON ReplicationJobState where
    parseJSON = parseJSONText "ReplicationJobState"

-- | Current state of Replication Run
data ReplicationRunState
  = RRSActive
  | RRSCompleted
  | RRSDeleted
  | RRSDeleting
  | RRSFailed
  | RRSMissed
  | RRSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReplicationRunState where
    parser = takeLowerText >>= \case
        "active" -> pure RRSActive
        "completed" -> pure RRSCompleted
        "deleted" -> pure RRSDeleted
        "deleting" -> pure RRSDeleting
        "failed" -> pure RRSFailed
        "missed" -> pure RRSMissed
        "pending" -> pure RRSPending
        e -> fromTextError $ "Failure parsing ReplicationRunState from value: '" <> e
           <> "'. Accepted values: active, completed, deleted, deleting, failed, missed, pending"

instance ToText ReplicationRunState where
    toText = \case
        RRSActive -> "ACTIVE"
        RRSCompleted -> "COMPLETED"
        RRSDeleted -> "DELETED"
        RRSDeleting -> "DELETING"
        RRSFailed -> "FAILED"
        RRSMissed -> "MISSED"
        RRSPending -> "PENDING"

instance Hashable     ReplicationRunState
instance NFData       ReplicationRunState
instance ToByteString ReplicationRunState
instance ToQuery      ReplicationRunState
instance ToHeader     ReplicationRunState

instance FromJSON ReplicationRunState where
    parseJSON = parseJSONText "ReplicationRunState"

-- | Type of Replication Run
data ReplicationRunType
  = Automatic
  | OnDemand
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReplicationRunType where
    parser = takeLowerText >>= \case
        "automatic" -> pure Automatic
        "on_demand" -> pure OnDemand
        e -> fromTextError $ "Failure parsing ReplicationRunType from value: '" <> e
           <> "'. Accepted values: automatic, on_demand"

instance ToText ReplicationRunType where
    toText = \case
        Automatic -> "AUTOMATIC"
        OnDemand -> "ON_DEMAND"

instance Hashable     ReplicationRunType
instance NFData       ReplicationRunType
instance ToByteString ReplicationRunType
instance ToQuery      ReplicationRunType
instance ToHeader     ReplicationRunType

instance FromJSON ReplicationRunType where
    parseJSON = parseJSONText "ReplicationRunType"

-- | Status of Server catalog
data ServerCatalogStatus
  = Available
  | Deleted
  | Expired
  | Importing
  | NotImported
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ServerCatalogStatus where
    parser = takeLowerText >>= \case
        "available" -> pure Available
        "deleted" -> pure Deleted
        "expired" -> pure Expired
        "importing" -> pure Importing
        "not_imported" -> pure NotImported
        e -> fromTextError $ "Failure parsing ServerCatalogStatus from value: '" <> e
           <> "'. Accepted values: available, deleted, expired, importing, not_imported"

instance ToText ServerCatalogStatus where
    toText = \case
        Available -> "AVAILABLE"
        Deleted -> "DELETED"
        Expired -> "EXPIRED"
        Importing -> "IMPORTING"
        NotImported -> "NOT_IMPORTED"

instance Hashable     ServerCatalogStatus
instance NFData       ServerCatalogStatus
instance ToByteString ServerCatalogStatus
instance ToQuery      ServerCatalogStatus
instance ToHeader     ServerCatalogStatus

instance FromJSON ServerCatalogStatus where
    parseJSON = parseJSONText "ServerCatalogStatus"

-- | Type of server.
data ServerType =
  VirtualMachine
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ServerType where
    parser = takeLowerText >>= \case
        "virtual_machine" -> pure VirtualMachine
        e -> fromTextError $ "Failure parsing ServerType from value: '" <> e
           <> "'. Accepted values: virtual_machine"

instance ToText ServerType where
    toText = \case
        VirtualMachine -> "VIRTUAL_MACHINE"

instance Hashable     ServerType
instance NFData       ServerType
instance ToByteString ServerType
instance ToQuery      ServerType
instance ToHeader     ServerType

instance FromJSON ServerType where
    parseJSON = parseJSONText "ServerType"

-- | VM Management Product
data VMManagerType =
  Vsphere
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VMManagerType where
    parser = takeLowerText >>= \case
        "vsphere" -> pure Vsphere
        e -> fromTextError $ "Failure parsing VMManagerType from value: '" <> e
           <> "'. Accepted values: vsphere"

instance ToText VMManagerType where
    toText = \case
        Vsphere -> "VSPHERE"

instance Hashable     VMManagerType
instance NFData       VMManagerType
instance ToByteString VMManagerType
instance ToQuery      VMManagerType
instance ToHeader     VMManagerType

instance FromJSON VMManagerType where
    parseJSON = parseJSONText "VMManagerType"
