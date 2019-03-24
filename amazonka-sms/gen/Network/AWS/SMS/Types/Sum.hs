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

data AppLaunchStatus
  = ConfigurationInProgress
  | ConfigurationInvalid
  | DeltaLaunchFailed
  | DeltaLaunchInProgress
  | LaunchFailed
  | LaunchInProgress
  | LaunchPending
  | Launched
  | ReadyForConfiguration
  | ReadyForLaunch
  | TerminateFailed
  | TerminateInProgress
  | Terminated
  | ValidationInProgress
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AppLaunchStatus where
    parser = takeLowerText >>= \case
        "configuration_in_progress" -> pure ConfigurationInProgress
        "configuration_invalid" -> pure ConfigurationInvalid
        "delta_launch_failed" -> pure DeltaLaunchFailed
        "delta_launch_in_progress" -> pure DeltaLaunchInProgress
        "launch_failed" -> pure LaunchFailed
        "launch_in_progress" -> pure LaunchInProgress
        "launch_pending" -> pure LaunchPending
        "launched" -> pure Launched
        "ready_for_configuration" -> pure ReadyForConfiguration
        "ready_for_launch" -> pure ReadyForLaunch
        "terminate_failed" -> pure TerminateFailed
        "terminate_in_progress" -> pure TerminateInProgress
        "terminated" -> pure Terminated
        "validation_in_progress" -> pure ValidationInProgress
        e -> fromTextError $ "Failure parsing AppLaunchStatus from value: '" <> e
           <> "'. Accepted values: configuration_in_progress, configuration_invalid, delta_launch_failed, delta_launch_in_progress, launch_failed, launch_in_progress, launch_pending, launched, ready_for_configuration, ready_for_launch, terminate_failed, terminate_in_progress, terminated, validation_in_progress"

instance ToText AppLaunchStatus where
    toText = \case
        ConfigurationInProgress -> "CONFIGURATION_IN_PROGRESS"
        ConfigurationInvalid -> "CONFIGURATION_INVALID"
        DeltaLaunchFailed -> "DELTA_LAUNCH_FAILED"
        DeltaLaunchInProgress -> "DELTA_LAUNCH_IN_PROGRESS"
        LaunchFailed -> "LAUNCH_FAILED"
        LaunchInProgress -> "LAUNCH_IN_PROGRESS"
        LaunchPending -> "LAUNCH_PENDING"
        Launched -> "LAUNCHED"
        ReadyForConfiguration -> "READY_FOR_CONFIGURATION"
        ReadyForLaunch -> "READY_FOR_LAUNCH"
        TerminateFailed -> "TERMINATE_FAILED"
        TerminateInProgress -> "TERMINATE_IN_PROGRESS"
        Terminated -> "TERMINATED"
        ValidationInProgress -> "VALIDATION_IN_PROGRESS"

instance Hashable     AppLaunchStatus
instance NFData       AppLaunchStatus
instance ToByteString AppLaunchStatus
instance ToQuery      AppLaunchStatus
instance ToHeader     AppLaunchStatus

instance FromJSON AppLaunchStatus where
    parseJSON = parseJSONText "AppLaunchStatus"

data AppReplicationStatus
  = ARSConfigurationInProgress
  | ARSConfigurationInvalid
  | ARSDeltaReplicated
  | ARSDeltaReplicationFailed
  | ARSDeltaReplicationInProgress
  | ARSReadyForConfiguration
  | ARSReadyForReplication
  | ARSReplicated
  | ARSReplicationFailed
  | ARSReplicationInProgress
  | ARSReplicationPending
  | ARSReplicationStopFailed
  | ARSReplicationStopped
  | ARSReplicationStopping
  | ARSValidationInProgress
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AppReplicationStatus where
    parser = takeLowerText >>= \case
        "configuration_in_progress" -> pure ARSConfigurationInProgress
        "configuration_invalid" -> pure ARSConfigurationInvalid
        "delta_replicated" -> pure ARSDeltaReplicated
        "delta_replication_failed" -> pure ARSDeltaReplicationFailed
        "delta_replication_in_progress" -> pure ARSDeltaReplicationInProgress
        "ready_for_configuration" -> pure ARSReadyForConfiguration
        "ready_for_replication" -> pure ARSReadyForReplication
        "replicated" -> pure ARSReplicated
        "replication_failed" -> pure ARSReplicationFailed
        "replication_in_progress" -> pure ARSReplicationInProgress
        "replication_pending" -> pure ARSReplicationPending
        "replication_stop_failed" -> pure ARSReplicationStopFailed
        "replication_stopped" -> pure ARSReplicationStopped
        "replication_stopping" -> pure ARSReplicationStopping
        "validation_in_progress" -> pure ARSValidationInProgress
        e -> fromTextError $ "Failure parsing AppReplicationStatus from value: '" <> e
           <> "'. Accepted values: configuration_in_progress, configuration_invalid, delta_replicated, delta_replication_failed, delta_replication_in_progress, ready_for_configuration, ready_for_replication, replicated, replication_failed, replication_in_progress, replication_pending, replication_stop_failed, replication_stopped, replication_stopping, validation_in_progress"

instance ToText AppReplicationStatus where
    toText = \case
        ARSConfigurationInProgress -> "CONFIGURATION_IN_PROGRESS"
        ARSConfigurationInvalid -> "CONFIGURATION_INVALID"
        ARSDeltaReplicated -> "DELTA_REPLICATED"
        ARSDeltaReplicationFailed -> "DELTA_REPLICATION_FAILED"
        ARSDeltaReplicationInProgress -> "DELTA_REPLICATION_IN_PROGRESS"
        ARSReadyForConfiguration -> "READY_FOR_CONFIGURATION"
        ARSReadyForReplication -> "READY_FOR_REPLICATION"
        ARSReplicated -> "REPLICATED"
        ARSReplicationFailed -> "REPLICATION_FAILED"
        ARSReplicationInProgress -> "REPLICATION_IN_PROGRESS"
        ARSReplicationPending -> "REPLICATION_PENDING"
        ARSReplicationStopFailed -> "REPLICATION_STOP_FAILED"
        ARSReplicationStopped -> "REPLICATION_STOPPED"
        ARSReplicationStopping -> "REPLICATION_STOPPING"
        ARSValidationInProgress -> "VALIDATION_IN_PROGRESS"

instance Hashable     AppReplicationStatus
instance NFData       AppReplicationStatus
instance ToByteString AppReplicationStatus
instance ToQuery      AppReplicationStatus
instance ToHeader     AppReplicationStatus

instance FromJSON AppReplicationStatus where
    parseJSON = parseJSONText "AppReplicationStatus"

data AppStatus
  = Active
  | Creating
  | DeleteFailed
  | Deleted
  | Deleting
  | Updating
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AppStatus where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "creating" -> pure Creating
        "delete_failed" -> pure DeleteFailed
        "deleted" -> pure Deleted
        "deleting" -> pure Deleting
        "updating" -> pure Updating
        e -> fromTextError $ "Failure parsing AppStatus from value: '" <> e
           <> "'. Accepted values: active, creating, delete_failed, deleted, deleting, updating"

instance ToText AppStatus where
    toText = \case
        Active -> "ACTIVE"
        Creating -> "CREATING"
        DeleteFailed -> "DELETE_FAILED"
        Deleted -> "DELETED"
        Deleting -> "DELETING"
        Updating -> "UPDATING"

instance Hashable     AppStatus
instance NFData       AppStatus
instance ToByteString AppStatus
instance ToQuery      AppStatus
instance ToHeader     AppStatus

instance FromJSON AppStatus where
    parseJSON = parseJSONText "AppStatus"

data ConnectorCapability
  = CCHypervManager
  | CCScvmm
  | CCSnapshotBatching
  | CCVsphere
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ConnectorCapability where
    parser = takeLowerText >>= \case
        "hyperv-manager" -> pure CCHypervManager
        "scvmm" -> pure CCScvmm
        "snapshot_batching" -> pure CCSnapshotBatching
        "vsphere" -> pure CCVsphere
        e -> fromTextError $ "Failure parsing ConnectorCapability from value: '" <> e
           <> "'. Accepted values: hyperv-manager, scvmm, snapshot_batching, vsphere"

instance ToText ConnectorCapability where
    toText = \case
        CCHypervManager -> "HYPERV-MANAGER"
        CCScvmm -> "SCVMM"
        CCSnapshotBatching -> "SNAPSHOT_BATCHING"
        CCVsphere -> "VSPHERE"

instance Hashable     ConnectorCapability
instance NFData       ConnectorCapability
instance ToByteString ConnectorCapability
instance ToQuery      ConnectorCapability
instance ToHeader     ConnectorCapability

instance FromJSON ConnectorCapability where
    parseJSON = parseJSONText "ConnectorCapability"

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

data OutputFormat
  = JSON
  | Yaml
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OutputFormat where
    parser = takeLowerText >>= \case
        "json" -> pure JSON
        "yaml" -> pure Yaml
        e -> fromTextError $ "Failure parsing OutputFormat from value: '" <> e
           <> "'. Accepted values: json, yaml"

instance ToText OutputFormat where
    toText = \case
        JSON -> "JSON"
        Yaml -> "YAML"

instance Hashable     OutputFormat
instance NFData       OutputFormat
instance ToByteString OutputFormat
instance ToQuery      OutputFormat
instance ToHeader     OutputFormat

instance ToJSON OutputFormat where
    toJSON = toJSONText

data ReplicationJobState
  = RJSActive
  | RJSCompleted
  | RJSDeleted
  | RJSDeleting
  | RJSFailed
  | RJSFailing
  | RJSPausedOnFailure
  | RJSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReplicationJobState where
    parser = takeLowerText >>= \case
        "active" -> pure RJSActive
        "completed" -> pure RJSCompleted
        "deleted" -> pure RJSDeleted
        "deleting" -> pure RJSDeleting
        "failed" -> pure RJSFailed
        "failing" -> pure RJSFailing
        "paused_on_failure" -> pure RJSPausedOnFailure
        "pending" -> pure RJSPending
        e -> fromTextError $ "Failure parsing ReplicationJobState from value: '" <> e
           <> "'. Accepted values: active, completed, deleted, deleting, failed, failing, paused_on_failure, pending"

instance ToText ReplicationJobState where
    toText = \case
        RJSActive -> "ACTIVE"
        RJSCompleted -> "COMPLETED"
        RJSDeleted -> "DELETED"
        RJSDeleting -> "DELETING"
        RJSFailed -> "FAILED"
        RJSFailing -> "FAILING"
        RJSPausedOnFailure -> "PAUSED_ON_FAILURE"
        RJSPending -> "PENDING"

instance Hashable     ReplicationJobState
instance NFData       ReplicationJobState
instance ToByteString ReplicationJobState
instance ToQuery      ReplicationJobState
instance ToHeader     ReplicationJobState

instance FromJSON ReplicationJobState where
    parseJSON = parseJSONText "ReplicationJobState"

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

data ServerCatalogStatus
  = SCSAvailable
  | SCSDeleted
  | SCSExpired
  | SCSImporting
  | SCSNotImported
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ServerCatalogStatus where
    parser = takeLowerText >>= \case
        "available" -> pure SCSAvailable
        "deleted" -> pure SCSDeleted
        "expired" -> pure SCSExpired
        "importing" -> pure SCSImporting
        "not_imported" -> pure SCSNotImported
        e -> fromTextError $ "Failure parsing ServerCatalogStatus from value: '" <> e
           <> "'. Accepted values: available, deleted, expired, importing, not_imported"

instance ToText ServerCatalogStatus where
    toText = \case
        SCSAvailable -> "AVAILABLE"
        SCSDeleted -> "DELETED"
        SCSExpired -> "EXPIRED"
        SCSImporting -> "IMPORTING"
        SCSNotImported -> "NOT_IMPORTED"

instance Hashable     ServerCatalogStatus
instance NFData       ServerCatalogStatus
instance ToByteString ServerCatalogStatus
instance ToQuery      ServerCatalogStatus
instance ToHeader     ServerCatalogStatus

instance FromJSON ServerCatalogStatus where
    parseJSON = parseJSONText "ServerCatalogStatus"

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

instance ToJSON ServerType where
    toJSON = toJSONText

instance FromJSON ServerType where
    parseJSON = parseJSONText "ServerType"

data VMManagerType
  = HypervManager
  | Scvmm
  | Vsphere
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VMManagerType where
    parser = takeLowerText >>= \case
        "hyperv-manager" -> pure HypervManager
        "scvmm" -> pure Scvmm
        "vsphere" -> pure Vsphere
        e -> fromTextError $ "Failure parsing VMManagerType from value: '" <> e
           <> "'. Accepted values: hyperv-manager, scvmm, vsphere"

instance ToText VMManagerType where
    toText = \case
        HypervManager -> "HYPERV-MANAGER"
        Scvmm -> "SCVMM"
        Vsphere -> "VSPHERE"

instance Hashable     VMManagerType
instance NFData       VMManagerType
instance ToByteString VMManagerType
instance ToQuery      VMManagerType
instance ToHeader     VMManagerType

instance ToJSON VMManagerType where
    toJSON = toJSONText

instance FromJSON VMManagerType where
    parseJSON = parseJSONText "VMManagerType"
