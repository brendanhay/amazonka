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

data BusinessReportFailureCode
  = AccessDenied
  | InternalFailure
  | NoSuchBucket
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BusinessReportFailureCode where
    parser = takeLowerText >>= \case
        "access_denied" -> pure AccessDenied
        "internal_failure" -> pure InternalFailure
        "no_such_bucket" -> pure NoSuchBucket
        e -> fromTextError $ "Failure parsing BusinessReportFailureCode from value: '" <> e
           <> "'. Accepted values: access_denied, internal_failure, no_such_bucket"

instance ToText BusinessReportFailureCode where
    toText = \case
        AccessDenied -> "ACCESS_DENIED"
        InternalFailure -> "INTERNAL_FAILURE"
        NoSuchBucket -> "NO_SUCH_BUCKET"

instance Hashable     BusinessReportFailureCode
instance NFData       BusinessReportFailureCode
instance ToByteString BusinessReportFailureCode
instance ToQuery      BusinessReportFailureCode
instance ToHeader     BusinessReportFailureCode

instance FromJSON BusinessReportFailureCode where
    parseJSON = parseJSONText "BusinessReportFailureCode"

data BusinessReportFormat
  = CSV
  | CSVZip
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BusinessReportFormat where
    parser = takeLowerText >>= \case
        "csv" -> pure CSV
        "csv_zip" -> pure CSVZip
        e -> fromTextError $ "Failure parsing BusinessReportFormat from value: '" <> e
           <> "'. Accepted values: csv, csv_zip"

instance ToText BusinessReportFormat where
    toText = \case
        CSV -> "CSV"
        CSVZip -> "CSV_ZIP"

instance Hashable     BusinessReportFormat
instance NFData       BusinessReportFormat
instance ToByteString BusinessReportFormat
instance ToQuery      BusinessReportFormat
instance ToHeader     BusinessReportFormat

instance ToJSON BusinessReportFormat where
    toJSON = toJSONText

instance FromJSON BusinessReportFormat where
    parseJSON = parseJSONText "BusinessReportFormat"

data BusinessReportInterval
  = OneDay
  | OneWeek
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BusinessReportInterval where
    parser = takeLowerText >>= \case
        "one_day" -> pure OneDay
        "one_week" -> pure OneWeek
        e -> fromTextError $ "Failure parsing BusinessReportInterval from value: '" <> e
           <> "'. Accepted values: one_day, one_week"

instance ToText BusinessReportInterval where
    toText = \case
        OneDay -> "ONE_DAY"
        OneWeek -> "ONE_WEEK"

instance Hashable     BusinessReportInterval
instance NFData       BusinessReportInterval
instance ToByteString BusinessReportInterval
instance ToQuery      BusinessReportInterval
instance ToHeader     BusinessReportInterval

instance ToJSON BusinessReportInterval where
    toJSON = toJSONText

instance FromJSON BusinessReportInterval where
    parseJSON = parseJSONText "BusinessReportInterval"

data BusinessReportStatus
  = Failed
  | Running
  | Succeeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BusinessReportStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "running" -> pure Running
        "succeeded" -> pure Succeeded
        e -> fromTextError $ "Failure parsing BusinessReportStatus from value: '" <> e
           <> "'. Accepted values: failed, running, succeeded"

instance ToText BusinessReportStatus where
    toText = \case
        Failed -> "FAILED"
        Running -> "RUNNING"
        Succeeded -> "SUCCEEDED"

instance Hashable     BusinessReportStatus
instance NFData       BusinessReportStatus
instance ToByteString BusinessReportStatus
instance ToQuery      BusinessReportStatus
instance ToHeader     BusinessReportStatus

instance FromJSON BusinessReportStatus where
    parseJSON = parseJSONText "BusinessReportStatus"

data CommsProtocol
  = H323
  | Sip
  | Sips
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CommsProtocol where
    parser = takeLowerText >>= \case
        "h323" -> pure H323
        "sip" -> pure Sip
        "sips" -> pure Sips
        e -> fromTextError $ "Failure parsing CommsProtocol from value: '" <> e
           <> "'. Accepted values: h323, sip, sips"

instance ToText CommsProtocol where
    toText = \case
        H323 -> "H323"
        Sip -> "SIP"
        Sips -> "SIPS"

instance Hashable     CommsProtocol
instance NFData       CommsProtocol
instance ToByteString CommsProtocol
instance ToQuery      CommsProtocol
instance ToHeader     CommsProtocol

instance ToJSON CommsProtocol where
    toJSON = toJSONText

instance FromJSON CommsProtocol where
    parseJSON = parseJSONText "CommsProtocol"

data ConferenceProviderType
  = Bluejeans
  | Chime
  | Custom
  | Fuze
  | GoogleHangouts
  | Polycom
  | Ringcentral
  | SkypeForBusiness
  | Webex
  | Zoom
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ConferenceProviderType where
    parser = takeLowerText >>= \case
        "bluejeans" -> pure Bluejeans
        "chime" -> pure Chime
        "custom" -> pure Custom
        "fuze" -> pure Fuze
        "google_hangouts" -> pure GoogleHangouts
        "polycom" -> pure Polycom
        "ringcentral" -> pure Ringcentral
        "skype_for_business" -> pure SkypeForBusiness
        "webex" -> pure Webex
        "zoom" -> pure Zoom
        e -> fromTextError $ "Failure parsing ConferenceProviderType from value: '" <> e
           <> "'. Accepted values: bluejeans, chime, custom, fuze, google_hangouts, polycom, ringcentral, skype_for_business, webex, zoom"

instance ToText ConferenceProviderType where
    toText = \case
        Bluejeans -> "BLUEJEANS"
        Chime -> "CHIME"
        Custom -> "CUSTOM"
        Fuze -> "FUZE"
        GoogleHangouts -> "GOOGLE_HANGOUTS"
        Polycom -> "POLYCOM"
        Ringcentral -> "RINGCENTRAL"
        SkypeForBusiness -> "SKYPE_FOR_BUSINESS"
        Webex -> "WEBEX"
        Zoom -> "ZOOM"

instance Hashable     ConferenceProviderType
instance NFData       ConferenceProviderType
instance ToByteString ConferenceProviderType
instance ToQuery      ConferenceProviderType
instance ToHeader     ConferenceProviderType

instance ToJSON ConferenceProviderType where
    toJSON = toJSONText

instance FromJSON ConferenceProviderType where
    parseJSON = parseJSONText "ConferenceProviderType"

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

data EnablementType
  = ETEnabled
  | ETPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EnablementType where
    parser = takeLowerText >>= \case
        "enabled" -> pure ETEnabled
        "pending" -> pure ETPending
        e -> fromTextError $ "Failure parsing EnablementType from value: '" <> e
           <> "'. Accepted values: enabled, pending"

instance ToText EnablementType where
    toText = \case
        ETEnabled -> "ENABLED"
        ETPending -> "PENDING"

instance Hashable     EnablementType
instance NFData       EnablementType
instance ToByteString EnablementType
instance ToQuery      EnablementType
instance ToHeader     EnablementType

instance FromJSON EnablementType where
    parseJSON = parseJSONText "EnablementType"

data EnablementTypeFilter
  = ETFEnabled
  | ETFPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EnablementTypeFilter where
    parser = takeLowerText >>= \case
        "enabled" -> pure ETFEnabled
        "pending" -> pure ETFPending
        e -> fromTextError $ "Failure parsing EnablementTypeFilter from value: '" <> e
           <> "'. Accepted values: enabled, pending"

instance ToText EnablementTypeFilter where
    toText = \case
        ETFEnabled -> "ENABLED"
        ETFPending -> "PENDING"

instance Hashable     EnablementTypeFilter
instance NFData       EnablementTypeFilter
instance ToByteString EnablementTypeFilter
instance ToQuery      EnablementTypeFilter
instance ToHeader     EnablementTypeFilter

instance ToJSON EnablementTypeFilter where
    toJSON = toJSONText

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

data RequirePin
  = NO
  | Optional
  | Yes
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RequirePin where
    parser = takeLowerText >>= \case
        "no" -> pure NO
        "optional" -> pure Optional
        "yes" -> pure Yes
        e -> fromTextError $ "Failure parsing RequirePin from value: '" <> e
           <> "'. Accepted values: no, optional, yes"

instance ToText RequirePin where
    toText = \case
        NO -> "NO"
        Optional -> "OPTIONAL"
        Yes -> "YES"

instance Hashable     RequirePin
instance NFData       RequirePin
instance ToByteString RequirePin
instance ToQuery      RequirePin
instance ToHeader     RequirePin

instance ToJSON RequirePin where
    toJSON = toJSONText

instance FromJSON RequirePin where
    parseJSON = parseJSONText "RequirePin"

data SkillType
  = Private
  | Public
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SkillType where
    parser = takeLowerText >>= \case
        "private" -> pure Private
        "public" -> pure Public
        e -> fromTextError $ "Failure parsing SkillType from value: '" <> e
           <> "'. Accepted values: private, public"

instance ToText SkillType where
    toText = \case
        Private -> "PRIVATE"
        Public -> "PUBLIC"

instance Hashable     SkillType
instance NFData       SkillType
instance ToByteString SkillType
instance ToQuery      SkillType
instance ToHeader     SkillType

instance FromJSON SkillType where
    parseJSON = parseJSONText "SkillType"

data SkillTypeFilter
  = STFAll
  | STFPrivate
  | STFPublic
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SkillTypeFilter where
    parser = takeLowerText >>= \case
        "all" -> pure STFAll
        "private" -> pure STFPrivate
        "public" -> pure STFPublic
        e -> fromTextError $ "Failure parsing SkillTypeFilter from value: '" <> e
           <> "'. Accepted values: all, private, public"

instance ToText SkillTypeFilter where
    toText = \case
        STFAll -> "ALL"
        STFPrivate -> "PRIVATE"
        STFPublic -> "PUBLIC"

instance Hashable     SkillTypeFilter
instance NFData       SkillTypeFilter
instance ToByteString SkillTypeFilter
instance ToQuery      SkillTypeFilter
instance ToHeader     SkillTypeFilter

instance ToJSON SkillTypeFilter where
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
