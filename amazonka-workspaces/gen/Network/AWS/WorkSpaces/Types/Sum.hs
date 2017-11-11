{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.Sum
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.Sum where

import Network.AWS.Prelude

data Compute
  = Performance
  | Standard
  | Value
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Compute where
    parser = takeLowerText >>= \case
        "performance" -> pure Performance
        "standard" -> pure Standard
        "value" -> pure Value
        e -> fromTextError $ "Failure parsing Compute from value: '" <> e
           <> "'. Accepted values: performance, standard, value"

instance ToText Compute where
    toText = \case
        Performance -> "PERFORMANCE"
        Standard -> "STANDARD"
        Value -> "VALUE"

instance Hashable     Compute
instance NFData       Compute
instance ToByteString Compute
instance ToQuery      Compute
instance ToHeader     Compute

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
  = WSAvailable
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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText WorkspaceState where
    parser = takeLowerText >>= \case
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
        e -> fromTextError $ "Failure parsing WorkspaceState from value: '" <> e
           <> "'. Accepted values: available, error, impaired, maintenance, pending, rebooting, rebuilding, starting, stopped, stopping, suspended, terminated, terminating, unhealthy"

instance ToText WorkspaceState where
    toText = \case
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

instance Hashable     WorkspaceState
instance NFData       WorkspaceState
instance ToByteString WorkspaceState
instance ToQuery      WorkspaceState
instance ToHeader     WorkspaceState

instance FromJSON WorkspaceState where
    parseJSON = parseJSONText "WorkspaceState"
