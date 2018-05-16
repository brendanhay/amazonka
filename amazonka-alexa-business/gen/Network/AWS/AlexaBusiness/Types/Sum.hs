{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.Sum where

import Network.AWS.Prelude

data ConnectionStatus
  = Offline
  | Online
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ConnectionStatus where
    parser = takeLowerText >>= \case
        "offline" -> pure Offline
        "online" -> pure Online
        e -> fromTextError $ "Failure parsing ConnectionStatus from value: '" <> e
           <> "'. Accepted values: offline, online"

instance ToText ConnectionStatus where
    toText = \case
        Offline -> "OFFLINE"
        Online -> "ONLINE"

instance Hashable     ConnectionStatus
instance NFData       ConnectionStatus
instance ToByteString ConnectionStatus
instance ToQuery      ConnectionStatus
instance ToHeader     ConnectionStatus

instance FromJSON ConnectionStatus where
    parseJSON = parseJSONText "ConnectionStatus"

data DeviceEventType
  = ConnectionStatus
  | DeviceStatus
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeviceEventType where
    parser = takeLowerText >>= \case
        "connection_status" -> pure ConnectionStatus
        "device_status" -> pure DeviceStatus
        e -> fromTextError $ "Failure parsing DeviceEventType from value: '" <> e
           <> "'. Accepted values: connection_status, device_status"

instance ToText DeviceEventType where
    toText = \case
        ConnectionStatus -> "CONNECTION_STATUS"
        DeviceStatus -> "DEVICE_STATUS"

instance Hashable     DeviceEventType
instance NFData       DeviceEventType
instance ToByteString DeviceEventType
instance ToQuery      DeviceEventType
instance ToHeader     DeviceEventType

instance ToJSON DeviceEventType where
    toJSON = toJSONText

instance FromJSON DeviceEventType where
    parseJSON = parseJSONText "DeviceEventType"

data DeviceStatus
  = Deregistered
  | Pending
  | Ready
  | WasOffline
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeviceStatus where
    parser = takeLowerText >>= \case
        "deregistered" -> pure Deregistered
        "pending" -> pure Pending
        "ready" -> pure Ready
        "was_offline" -> pure WasOffline
        e -> fromTextError $ "Failure parsing DeviceStatus from value: '" <> e
           <> "'. Accepted values: deregistered, pending, ready, was_offline"

instance ToText DeviceStatus where
    toText = \case
        Deregistered -> "DEREGISTERED"
        Pending -> "PENDING"
        Ready -> "READY"
        WasOffline -> "WAS_OFFLINE"

instance Hashable     DeviceStatus
instance NFData       DeviceStatus
instance ToByteString DeviceStatus
instance ToQuery      DeviceStatus
instance ToHeader     DeviceStatus

instance FromJSON DeviceStatus where
    parseJSON = parseJSONText "DeviceStatus"

data DeviceStatusDetailCode
  = DeviceSoftwareUpdateNeeded
  | DeviceWasOffline
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeviceStatusDetailCode where
    parser = takeLowerText >>= \case
        "device_software_update_needed" -> pure DeviceSoftwareUpdateNeeded
        "device_was_offline" -> pure DeviceWasOffline
        e -> fromTextError $ "Failure parsing DeviceStatusDetailCode from value: '" <> e
           <> "'. Accepted values: device_software_update_needed, device_was_offline"

instance ToText DeviceStatusDetailCode where
    toText = \case
        DeviceSoftwareUpdateNeeded -> "DEVICE_SOFTWARE_UPDATE_NEEDED"
        DeviceWasOffline -> "DEVICE_WAS_OFFLINE"

instance Hashable     DeviceStatusDetailCode
instance NFData       DeviceStatusDetailCode
instance ToByteString DeviceStatusDetailCode
instance ToQuery      DeviceStatusDetailCode
instance ToHeader     DeviceStatusDetailCode

instance FromJSON DeviceStatusDetailCode where
    parseJSON = parseJSONText "DeviceStatusDetailCode"

data DistanceUnit
  = Imperial
  | Metric
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DistanceUnit where
    parser = takeLowerText >>= \case
        "imperial" -> pure Imperial
        "metric" -> pure Metric
        e -> fromTextError $ "Failure parsing DistanceUnit from value: '" <> e
           <> "'. Accepted values: imperial, metric"

instance ToText DistanceUnit where
    toText = \case
        Imperial -> "IMPERIAL"
        Metric -> "METRIC"

instance Hashable     DistanceUnit
instance NFData       DistanceUnit
instance ToByteString DistanceUnit
instance ToQuery      DistanceUnit
instance ToHeader     DistanceUnit

instance ToJSON DistanceUnit where
    toJSON = toJSONText

instance FromJSON DistanceUnit where
    parseJSON = parseJSONText "DistanceUnit"

data EnrollmentStatus
  = ESDeregistering
  | ESDisassociating
  | ESInitialized
  | ESPending
  | ESRegistered
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EnrollmentStatus where
    parser = takeLowerText >>= \case
        "deregistering" -> pure ESDeregistering
        "disassociating" -> pure ESDisassociating
        "initialized" -> pure ESInitialized
        "pending" -> pure ESPending
        "registered" -> pure ESRegistered
        e -> fromTextError $ "Failure parsing EnrollmentStatus from value: '" <> e
           <> "'. Accepted values: deregistering, disassociating, initialized, pending, registered"

instance ToText EnrollmentStatus where
    toText = \case
        ESDeregistering -> "DEREGISTERING"
        ESDisassociating -> "DISASSOCIATING"
        ESInitialized -> "INITIALIZED"
        ESPending -> "PENDING"
        ESRegistered -> "REGISTERED"

instance Hashable     EnrollmentStatus
instance NFData       EnrollmentStatus
instance ToByteString EnrollmentStatus
instance ToQuery      EnrollmentStatus
instance ToHeader     EnrollmentStatus

instance FromJSON EnrollmentStatus where
    parseJSON = parseJSONText "EnrollmentStatus"

data Feature
  = All
  | Bluetooth
  | Lists
  | Notifications
  | Skills
  | Volume
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Feature where
    parser = takeLowerText >>= \case
        "all" -> pure All
        "bluetooth" -> pure Bluetooth
        "lists" -> pure Lists
        "notifications" -> pure Notifications
        "skills" -> pure Skills
        "volume" -> pure Volume
        e -> fromTextError $ "Failure parsing Feature from value: '" <> e
           <> "'. Accepted values: all, bluetooth, lists, notifications, skills, volume"

instance ToText Feature where
    toText = \case
        All -> "ALL"
        Bluetooth -> "BLUETOOTH"
        Lists -> "LISTS"
        Notifications -> "NOTIFICATIONS"
        Skills -> "SKILLS"
        Volume -> "VOLUME"

instance Hashable     Feature
instance NFData       Feature
instance ToByteString Feature
instance ToQuery      Feature
instance ToHeader     Feature

instance ToJSON Feature where
    toJSON = toJSONText

data SortValue
  = Asc
  | Desc
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SortValue where
    parser = takeLowerText >>= \case
        "asc" -> pure Asc
        "desc" -> pure Desc
        e -> fromTextError $ "Failure parsing SortValue from value: '" <> e
           <> "'. Accepted values: asc, desc"

instance ToText SortValue where
    toText = \case
        Asc -> "ASC"
        Desc -> "DESC"

instance Hashable     SortValue
instance NFData       SortValue
instance ToByteString SortValue
instance ToQuery      SortValue
instance ToHeader     SortValue

instance ToJSON SortValue where
    toJSON = toJSONText

data TemperatureUnit
  = Celsius
  | Fahrenheit
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TemperatureUnit where
    parser = takeLowerText >>= \case
        "celsius" -> pure Celsius
        "fahrenheit" -> pure Fahrenheit
        e -> fromTextError $ "Failure parsing TemperatureUnit from value: '" <> e
           <> "'. Accepted values: celsius, fahrenheit"

instance ToText TemperatureUnit where
    toText = \case
        Celsius -> "CELSIUS"
        Fahrenheit -> "FAHRENHEIT"

instance Hashable     TemperatureUnit
instance NFData       TemperatureUnit
instance ToByteString TemperatureUnit
instance ToQuery      TemperatureUnit
instance ToHeader     TemperatureUnit

instance ToJSON TemperatureUnit where
    toJSON = toJSONText

instance FromJSON TemperatureUnit where
    parseJSON = parseJSONText "TemperatureUnit"

data WakeWord
  = Alexa
  | Amazon
  | Computer
  | Echo
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText WakeWord where
    parser = takeLowerText >>= \case
        "alexa" -> pure Alexa
        "amazon" -> pure Amazon
        "computer" -> pure Computer
        "echo" -> pure Echo
        e -> fromTextError $ "Failure parsing WakeWord from value: '" <> e
           <> "'. Accepted values: alexa, amazon, computer, echo"

instance ToText WakeWord where
    toText = \case
        Alexa -> "ALEXA"
        Amazon -> "AMAZON"
        Computer -> "COMPUTER"
        Echo -> "ECHO"

instance Hashable     WakeWord
instance NFData       WakeWord
instance ToByteString WakeWord
instance ToQuery      WakeWord
instance ToHeader     WakeWord

instance ToJSON WakeWord where
    toJSON = toJSONText

instance FromJSON WakeWord where
    parseJSON = parseJSONText "WakeWord"
