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
  | Performance
  | Power
  | Standard
  | Value
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Compute where
    parser = takeLowerText >>= \case
        "graphics" -> pure Graphics
        "performance" -> pure Performance
        "power" -> pure Power
        "standard" -> pure Standard
        "value" -> pure Value
        e -> fromTextError $ "Failure parsing Compute from value: '" <> e
           <> "'. Accepted values: graphics, performance, power, standard, value"

instance ToText Compute where
    toText = \case
        Graphics -> "GRAPHICS"
        Performance -> "PERFORMANCE"
        Power -> "POWER"
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
