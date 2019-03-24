{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.Sum where

import Network.AWS.Prelude

data Compute
  = Graphics
  | Graphicspro
  | Performance
  | Power
  | Powerpro
  | Standard
  | Value
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Compute where
    parser = takeLowerText >>= \case
        "graphics" -> pure Graphics
        "graphicspro" -> pure Graphicspro
        "performance" -> pure Performance
        "power" -> pure Power
        "powerpro" -> pure Powerpro
        "standard" -> pure Standard
        "value" -> pure Value
        e -> fromTextError $ "Failure parsing Compute from value: '" <> e
           <> "'. Accepted values: graphics, graphicspro, performance, power, powerpro, standard, value"

instance ToText Compute where
    toText = \case
        Graphics -> "GRAPHICS"
        Graphicspro -> "GRAPHICSPRO"
        Performance -> "PERFORMANCE"
        Power -> "POWER"
        Powerpro -> "POWERPRO"
        Standard -> "STANDARD"
        Value -> "VALUE"

instance Hashable     Compute
instance NFData       Compute
instance ToByteString Compute
instance ToQuery      Compute
instance ToHeader     Compute

instance ToJSON Compute where
    toJSON = toJSONText

instance FromJSON Compute where
    parseJSON = parseJSONText "Compute"

data ConnectionState
  = Connected
  | Disconnected
  | Unknown
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ConnectionState where
    parser = takeLowerText >>= \case
        "connected" -> pure Connected
        "disconnected" -> pure Disconnected
        "unknown" -> pure Unknown
        e -> fromTextError $ "Failure parsing ConnectionState from value: '" <> e
           <> "'. Accepted values: connected, disconnected, unknown"

instance ToText ConnectionState where
    toText = \case
        Connected -> "CONNECTED"
        Disconnected -> "DISCONNECTED"
        Unknown -> "UNKNOWN"

instance Hashable     ConnectionState
instance NFData       ConnectionState
instance ToByteString ConnectionState
instance ToQuery      ConnectionState
instance ToHeader     ConnectionState

instance FromJSON ConnectionState where
    parseJSON = parseJSONText "ConnectionState"

data DedicatedTenancyModificationStateEnum
  = Completed
  | Failed
  | Pending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DedicatedTenancyModificationStateEnum where
    parser = takeLowerText >>= \case
        "completed" -> pure Completed
        "failed" -> pure Failed
        "pending" -> pure Pending
        e -> fromTextError $ "Failure parsing DedicatedTenancyModificationStateEnum from value: '" <> e
           <> "'. Accepted values: completed, failed, pending"

instance ToText DedicatedTenancyModificationStateEnum where
    toText = \case
        Completed -> "COMPLETED"
        Failed -> "FAILED"
        Pending -> "PENDING"

instance Hashable     DedicatedTenancyModificationStateEnum
instance NFData       DedicatedTenancyModificationStateEnum
instance ToByteString DedicatedTenancyModificationStateEnum
instance ToQuery      DedicatedTenancyModificationStateEnum
instance ToHeader     DedicatedTenancyModificationStateEnum

instance FromJSON DedicatedTenancyModificationStateEnum where
    parseJSON = parseJSONText "DedicatedTenancyModificationStateEnum"

data DedicatedTenancySupportEnum =
  DTSEEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DedicatedTenancySupportEnum where
    parser = takeLowerText >>= \case
        "enabled" -> pure DTSEEnabled
        e -> fromTextError $ "Failure parsing DedicatedTenancySupportEnum from value: '" <> e
           <> "'. Accepted values: enabled"

instance ToText DedicatedTenancySupportEnum where
    toText = \case
        DTSEEnabled -> "ENABLED"

instance Hashable     DedicatedTenancySupportEnum
instance NFData       DedicatedTenancySupportEnum
instance ToByteString DedicatedTenancySupportEnum
instance ToQuery      DedicatedTenancySupportEnum
instance ToHeader     DedicatedTenancySupportEnum

instance ToJSON DedicatedTenancySupportEnum where
    toJSON = toJSONText

data DedicatedTenancySupportResultEnum
  = Disabled
  | Enabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DedicatedTenancySupportResultEnum where
    parser = takeLowerText >>= \case
        "disabled" -> pure Disabled
        "enabled" -> pure Enabled
        e -> fromTextError $ "Failure parsing DedicatedTenancySupportResultEnum from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText DedicatedTenancySupportResultEnum where
    toText = \case
        Disabled -> "DISABLED"
        Enabled -> "ENABLED"

instance Hashable     DedicatedTenancySupportResultEnum
instance NFData       DedicatedTenancySupportResultEnum
instance ToByteString DedicatedTenancySupportResultEnum
instance ToQuery      DedicatedTenancySupportResultEnum
instance ToHeader     DedicatedTenancySupportResultEnum

instance FromJSON DedicatedTenancySupportResultEnum where
    parseJSON = parseJSONText "DedicatedTenancySupportResultEnum"

data ModificationResourceEnum
  = ComputeType
  | RootVolume
  | UserVolume
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ModificationResourceEnum where
    parser = takeLowerText >>= \case
        "compute_type" -> pure ComputeType
        "root_volume" -> pure RootVolume
        "user_volume" -> pure UserVolume
        e -> fromTextError $ "Failure parsing ModificationResourceEnum from value: '" <> e
           <> "'. Accepted values: compute_type, root_volume, user_volume"

instance ToText ModificationResourceEnum where
    toText = \case
        ComputeType -> "COMPUTE_TYPE"
        RootVolume -> "ROOT_VOLUME"
        UserVolume -> "USER_VOLUME"

instance Hashable     ModificationResourceEnum
instance NFData       ModificationResourceEnum
instance ToByteString ModificationResourceEnum
instance ToQuery      ModificationResourceEnum
instance ToHeader     ModificationResourceEnum

instance FromJSON ModificationResourceEnum where
    parseJSON = parseJSONText "ModificationResourceEnum"

data ModificationStateEnum
  = UpdateInProgress
  | UpdateInitiated
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ModificationStateEnum where
    parser = takeLowerText >>= \case
        "update_in_progress" -> pure UpdateInProgress
        "update_initiated" -> pure UpdateInitiated
        e -> fromTextError $ "Failure parsing ModificationStateEnum from value: '" <> e
           <> "'. Accepted values: update_in_progress, update_initiated"

instance ToText ModificationStateEnum where
    toText = \case
        UpdateInProgress -> "UPDATE_IN_PROGRESS"
        UpdateInitiated -> "UPDATE_INITIATED"

instance Hashable     ModificationStateEnum
instance NFData       ModificationStateEnum
instance ToByteString ModificationStateEnum
instance ToQuery      ModificationStateEnum
instance ToHeader     ModificationStateEnum

instance FromJSON ModificationStateEnum where
    parseJSON = parseJSONText "ModificationStateEnum"

data OperatingSystemType
  = Linux
  | Windows
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OperatingSystemType where
    parser = takeLowerText >>= \case
        "linux" -> pure Linux
        "windows" -> pure Windows
        e -> fromTextError $ "Failure parsing OperatingSystemType from value: '" <> e
           <> "'. Accepted values: linux, windows"

instance ToText OperatingSystemType where
    toText = \case
        Linux -> "LINUX"
        Windows -> "WINDOWS"

instance Hashable     OperatingSystemType
instance NFData       OperatingSystemType
instance ToByteString OperatingSystemType
instance ToQuery      OperatingSystemType
instance ToHeader     OperatingSystemType

instance FromJSON OperatingSystemType where
    parseJSON = parseJSONText "OperatingSystemType"

data ReconnectEnum
  = REDisabled
  | REEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReconnectEnum where
    parser = takeLowerText >>= \case
        "disabled" -> pure REDisabled
        "enabled" -> pure REEnabled
        e -> fromTextError $ "Failure parsing ReconnectEnum from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText ReconnectEnum where
    toText = \case
        REDisabled -> "DISABLED"
        REEnabled -> "ENABLED"

instance Hashable     ReconnectEnum
instance NFData       ReconnectEnum
instance ToByteString ReconnectEnum
instance ToQuery      ReconnectEnum
instance ToHeader     ReconnectEnum

instance ToJSON ReconnectEnum where
    toJSON = toJSONText

instance FromJSON ReconnectEnum where
    parseJSON = parseJSONText "ReconnectEnum"

data RunningMode
  = AlwaysOn
  | AutoStop
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RunningMode where
    parser = takeLowerText >>= \case
        "always_on" -> pure AlwaysOn
        "auto_stop" -> pure AutoStop
        e -> fromTextError $ "Failure parsing RunningMode from value: '" <> e
           <> "'. Accepted values: always_on, auto_stop"

instance ToText RunningMode where
    toText = \case
        AlwaysOn -> "ALWAYS_ON"
        AutoStop -> "AUTO_STOP"

instance Hashable     RunningMode
instance NFData       RunningMode
instance ToByteString RunningMode
instance ToQuery      RunningMode
instance ToHeader     RunningMode

instance ToJSON RunningMode where
    toJSON = toJSONText

instance FromJSON RunningMode where
    parseJSON = parseJSONText "RunningMode"

data TargetWorkspaceState
  = AdminMaintenance
  | Available
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TargetWorkspaceState where
    parser = takeLowerText >>= \case
        "admin_maintenance" -> pure AdminMaintenance
        "available" -> pure Available
        e -> fromTextError $ "Failure parsing TargetWorkspaceState from value: '" <> e
           <> "'. Accepted values: admin_maintenance, available"

instance ToText TargetWorkspaceState where
    toText = \case
        AdminMaintenance -> "ADMIN_MAINTENANCE"
        Available -> "AVAILABLE"

instance Hashable     TargetWorkspaceState
instance NFData       TargetWorkspaceState
instance ToByteString TargetWorkspaceState
instance ToQuery      TargetWorkspaceState
instance ToHeader     TargetWorkspaceState

instance ToJSON TargetWorkspaceState where
    toJSON = toJSONText

data WorkspaceDirectoryState
  = Deregistered
  | Deregistering
  | Error'
  | Registered
  | Registering
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText WorkspaceDirectoryState where
    parser = takeLowerText >>= \case
        "deregistered" -> pure Deregistered
        "deregistering" -> pure Deregistering
        "error" -> pure Error'
        "registered" -> pure Registered
        "registering" -> pure Registering
        e -> fromTextError $ "Failure parsing WorkspaceDirectoryState from value: '" <> e
           <> "'. Accepted values: deregistered, deregistering, error, registered, registering"

instance ToText WorkspaceDirectoryState where
    toText = \case
        Deregistered -> "DEREGISTERED"
        Deregistering -> "DEREGISTERING"
        Error' -> "ERROR"
        Registered -> "REGISTERED"
        Registering -> "REGISTERING"

instance Hashable     WorkspaceDirectoryState
instance NFData       WorkspaceDirectoryState
instance ToByteString WorkspaceDirectoryState
instance ToQuery      WorkspaceDirectoryState
instance ToHeader     WorkspaceDirectoryState

instance FromJSON WorkspaceDirectoryState where
    parseJSON = parseJSONText "WorkspaceDirectoryState"

data WorkspaceDirectoryType
  = AdConnector
  | SimpleAd
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText WorkspaceDirectoryType where
    parser = takeLowerText >>= \case
        "ad_connector" -> pure AdConnector
        "simple_ad" -> pure SimpleAd
        e -> fromTextError $ "Failure parsing WorkspaceDirectoryType from value: '" <> e
           <> "'. Accepted values: ad_connector, simple_ad"

instance ToText WorkspaceDirectoryType where
    toText = \case
        AdConnector -> "AD_CONNECTOR"
        SimpleAd -> "SIMPLE_AD"

instance Hashable     WorkspaceDirectoryType
instance NFData       WorkspaceDirectoryType
instance ToByteString WorkspaceDirectoryType
instance ToQuery      WorkspaceDirectoryType
instance ToHeader     WorkspaceDirectoryType

instance FromJSON WorkspaceDirectoryType where
    parseJSON = parseJSONText "WorkspaceDirectoryType"

data WorkspaceImageIngestionProcess
  = ByolGraphics
  | ByolGraphicspro
  | ByolRegular
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText WorkspaceImageIngestionProcess where
    parser = takeLowerText >>= \case
        "byol_graphics" -> pure ByolGraphics
        "byol_graphicspro" -> pure ByolGraphicspro
        "byol_regular" -> pure ByolRegular
        e -> fromTextError $ "Failure parsing WorkspaceImageIngestionProcess from value: '" <> e
           <> "'. Accepted values: byol_graphics, byol_graphicspro, byol_regular"

instance ToText WorkspaceImageIngestionProcess where
    toText = \case
        ByolGraphics -> "BYOL_GRAPHICS"
        ByolGraphicspro -> "BYOL_GRAPHICSPRO"
        ByolRegular -> "BYOL_REGULAR"

instance Hashable     WorkspaceImageIngestionProcess
instance NFData       WorkspaceImageIngestionProcess
instance ToByteString WorkspaceImageIngestionProcess
instance ToQuery      WorkspaceImageIngestionProcess
instance ToHeader     WorkspaceImageIngestionProcess

instance ToJSON WorkspaceImageIngestionProcess where
    toJSON = toJSONText

data WorkspaceImageRequiredTenancy
  = Dedicated
  | Default
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText WorkspaceImageRequiredTenancy where
    parser = takeLowerText >>= \case
        "dedicated" -> pure Dedicated
        "default" -> pure Default
        e -> fromTextError $ "Failure parsing WorkspaceImageRequiredTenancy from value: '" <> e
           <> "'. Accepted values: dedicated, default"

instance ToText WorkspaceImageRequiredTenancy where
    toText = \case
        Dedicated -> "DEDICATED"
        Default -> "DEFAULT"

instance Hashable     WorkspaceImageRequiredTenancy
instance NFData       WorkspaceImageRequiredTenancy
instance ToByteString WorkspaceImageRequiredTenancy
instance ToQuery      WorkspaceImageRequiredTenancy
instance ToHeader     WorkspaceImageRequiredTenancy

instance FromJSON WorkspaceImageRequiredTenancy where
    parseJSON = parseJSONText "WorkspaceImageRequiredTenancy"

data WorkspaceImageState
  = WISAvailable
  | WISError'
  | WISPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText WorkspaceImageState where
    parser = takeLowerText >>= \case
        "available" -> pure WISAvailable
        "error" -> pure WISError'
        "pending" -> pure WISPending
        e -> fromTextError $ "Failure parsing WorkspaceImageState from value: '" <> e
           <> "'. Accepted values: available, error, pending"

instance ToText WorkspaceImageState where
    toText = \case
        WISAvailable -> "AVAILABLE"
        WISError' -> "ERROR"
        WISPending -> "PENDING"

instance Hashable     WorkspaceImageState
instance NFData       WorkspaceImageState
instance ToByteString WorkspaceImageState
instance ToQuery      WorkspaceImageState
instance ToHeader     WorkspaceImageState

instance FromJSON WorkspaceImageState where
    parseJSON = parseJSONText "WorkspaceImageState"

data WorkspaceState
  = WSAdminMaintenance
  | WSAvailable
  | WSError'
  | WSImpaired
  | WSMaintenance
  | WSPending
  | WSRebooting
  | WSRebuilding
  | WSStarting
  | WSStopped
  | WSStopping
  | WSSuspended
  | WSTerminated
  | WSTerminating
  | WSUnhealthy
  | WSUpdating
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText WorkspaceState where
    parser = takeLowerText >>= \case
        "admin_maintenance" -> pure WSAdminMaintenance
        "available" -> pure WSAvailable
        "error" -> pure WSError'
        "impaired" -> pure WSImpaired
        "maintenance" -> pure WSMaintenance
        "pending" -> pure WSPending
        "rebooting" -> pure WSRebooting
        "rebuilding" -> pure WSRebuilding
        "starting" -> pure WSStarting
        "stopped" -> pure WSStopped
        "stopping" -> pure WSStopping
        "suspended" -> pure WSSuspended
        "terminated" -> pure WSTerminated
        "terminating" -> pure WSTerminating
        "unhealthy" -> pure WSUnhealthy
        "updating" -> pure WSUpdating
        e -> fromTextError $ "Failure parsing WorkspaceState from value: '" <> e
           <> "'. Accepted values: admin_maintenance, available, error, impaired, maintenance, pending, rebooting, rebuilding, starting, stopped, stopping, suspended, terminated, terminating, unhealthy, updating"

instance ToText WorkspaceState where
    toText = \case
        WSAdminMaintenance -> "ADMIN_MAINTENANCE"
        WSAvailable -> "AVAILABLE"
        WSError' -> "ERROR"
        WSImpaired -> "IMPAIRED"
        WSMaintenance -> "MAINTENANCE"
        WSPending -> "PENDING"
        WSRebooting -> "REBOOTING"
        WSRebuilding -> "REBUILDING"
        WSStarting -> "STARTING"
        WSStopped -> "STOPPED"
        WSStopping -> "STOPPING"
        WSSuspended -> "SUSPENDED"
        WSTerminated -> "TERMINATED"
        WSTerminating -> "TERMINATING"
        WSUnhealthy -> "UNHEALTHY"
        WSUpdating -> "UPDATING"

instance Hashable     WorkspaceState
instance NFData       WorkspaceState
instance ToByteString WorkspaceState
instance ToQuery      WorkspaceState
instance ToHeader     WorkspaceState

instance FromJSON WorkspaceState where
    parseJSON = parseJSONText "WorkspaceState"
