{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MQ.Types.Sum where

import Network.AWS.Prelude

-- | The status of the broker. Possible values: CREATION_IN_PROGRESS, CREATION_FAILED, DELETION_IN_PROGRESS, RUNNING, REBOOT_IN_PROGRESS
data BrokerState
  = CreationFailed
  | CreationInProgress
  | DeletionInProgress
  | RebootInProgress
  | Running
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BrokerState where
    parser = takeLowerText >>= \case
        "creation_failed" -> pure CreationFailed
        "creation_in_progress" -> pure CreationInProgress
        "deletion_in_progress" -> pure DeletionInProgress
        "reboot_in_progress" -> pure RebootInProgress
        "running" -> pure Running
        e -> fromTextError $ "Failure parsing BrokerState from value: '" <> e
           <> "'. Accepted values: creation_failed, creation_in_progress, deletion_in_progress, reboot_in_progress, running"

instance ToText BrokerState where
    toText = \case
        CreationFailed -> "CREATION_FAILED"
        CreationInProgress -> "CREATION_IN_PROGRESS"
        DeletionInProgress -> "DELETION_IN_PROGRESS"
        RebootInProgress -> "REBOOT_IN_PROGRESS"
        Running -> "RUNNING"

instance Hashable     BrokerState
instance NFData       BrokerState
instance ToByteString BrokerState
instance ToQuery      BrokerState
instance ToHeader     BrokerState

instance FromJSON BrokerState where
    parseJSON = parseJSONText "BrokerState"

-- | The type of change pending for the ActiveMQ user. Possible values: CREATE, UPDATE, DELETE
data ChangeType
  = Create
  | Delete
  | Update
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ChangeType where
    parser = takeLowerText >>= \case
        "create" -> pure Create
        "delete" -> pure Delete
        "update" -> pure Update
        e -> fromTextError $ "Failure parsing ChangeType from value: '" <> e
           <> "'. Accepted values: create, delete, update"

instance ToText ChangeType where
    toText = \case
        Create -> "CREATE"
        Delete -> "DELETE"
        Update -> "UPDATE"

instance Hashable     ChangeType
instance NFData       ChangeType
instance ToByteString ChangeType
instance ToQuery      ChangeType
instance ToHeader     ChangeType

instance FromJSON ChangeType where
    parseJSON = parseJSONText "ChangeType"

data DayOfWeek
  = Friday
  | Monday
  | Saturday
  | Sunday
  | Thursday
  | Tuesday
  | Wednesday
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DayOfWeek where
    parser = takeLowerText >>= \case
        "friday" -> pure Friday
        "monday" -> pure Monday
        "saturday" -> pure Saturday
        "sunday" -> pure Sunday
        "thursday" -> pure Thursday
        "tuesday" -> pure Tuesday
        "wednesday" -> pure Wednesday
        e -> fromTextError $ "Failure parsing DayOfWeek from value: '" <> e
           <> "'. Accepted values: friday, monday, saturday, sunday, thursday, tuesday, wednesday"

instance ToText DayOfWeek where
    toText = \case
        Friday -> "FRIDAY"
        Monday -> "MONDAY"
        Saturday -> "SATURDAY"
        Sunday -> "SUNDAY"
        Thursday -> "THURSDAY"
        Tuesday -> "TUESDAY"
        Wednesday -> "WEDNESDAY"

instance Hashable     DayOfWeek
instance NFData       DayOfWeek
instance ToByteString DayOfWeek
instance ToQuery      DayOfWeek
instance ToHeader     DayOfWeek

instance ToJSON DayOfWeek where
    toJSON = toJSONText

instance FromJSON DayOfWeek where
    parseJSON = parseJSONText "DayOfWeek"

-- | The deployment mode of the broker. Possible values: SINGLE_INSTANCE, ACTIVE_STANDBY_MULTI_AZ SINGLE_INSTANCE creates a single-instance broker in a single Availability Zone. ACTIVE_STANDBY_MULTI_AZ creates an active/standby broker for high availability.
data DeploymentMode
  = ActiveStandbyMultiAz
  | SingleInstance
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeploymentMode where
    parser = takeLowerText >>= \case
        "active_standby_multi_az" -> pure ActiveStandbyMultiAz
        "single_instance" -> pure SingleInstance
        e -> fromTextError $ "Failure parsing DeploymentMode from value: '" <> e
           <> "'. Accepted values: active_standby_multi_az, single_instance"

instance ToText DeploymentMode where
    toText = \case
        ActiveStandbyMultiAz -> "ACTIVE_STANDBY_MULTI_AZ"
        SingleInstance -> "SINGLE_INSTANCE"

instance Hashable     DeploymentMode
instance NFData       DeploymentMode
instance ToByteString DeploymentMode
instance ToQuery      DeploymentMode
instance ToHeader     DeploymentMode

instance ToJSON DeploymentMode where
    toJSON = toJSONText

instance FromJSON DeploymentMode where
    parseJSON = parseJSONText "DeploymentMode"

-- | The type of broker engine. Note: Currently, Amazon MQ supports only ActiveMQ.
data EngineType =
  Activemq
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EngineType where
    parser = takeLowerText >>= \case
        "activemq" -> pure Activemq
        e -> fromTextError $ "Failure parsing EngineType from value: '" <> e
           <> "'. Accepted values: activemq"

instance ToText EngineType where
    toText = \case
        Activemq -> "ACTIVEMQ"

instance Hashable     EngineType
instance NFData       EngineType
instance ToByteString EngineType
instance ToQuery      EngineType
instance ToHeader     EngineType

instance ToJSON EngineType where
    toJSON = toJSONText

instance FromJSON EngineType where
    parseJSON = parseJSONText "EngineType"

-- | The reason for which the XML elements or attributes were sanitized. Possible values: DISALLOWED_ELEMENT_REMOVED, DISALLOWED_ATTRIBUTE_REMOVED, INVALID_ATTRIBUTE_VALUE_REMOVED DISALLOWED_ELEMENT_REMOVED shows that the provided element isn't allowed and has been removed. DISALLOWED_ATTRIBUTE_REMOVED shows that the provided attribute isn't allowed and has been removed. INVALID_ATTRIBUTE_VALUE_REMOVED shows that the provided value for the attribute isn't allowed and has been removed.
data SanitizationWarningReason
  = DisallowedAttributeRemoved
  | DisallowedElementRemoved
  | InvalidAttributeValueRemoved
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SanitizationWarningReason where
    parser = takeLowerText >>= \case
        "disallowed_attribute_removed" -> pure DisallowedAttributeRemoved
        "disallowed_element_removed" -> pure DisallowedElementRemoved
        "invalid_attribute_value_removed" -> pure InvalidAttributeValueRemoved
        e -> fromTextError $ "Failure parsing SanitizationWarningReason from value: '" <> e
           <> "'. Accepted values: disallowed_attribute_removed, disallowed_element_removed, invalid_attribute_value_removed"

instance ToText SanitizationWarningReason where
    toText = \case
        DisallowedAttributeRemoved -> "DISALLOWED_ATTRIBUTE_REMOVED"
        DisallowedElementRemoved -> "DISALLOWED_ELEMENT_REMOVED"
        InvalidAttributeValueRemoved -> "INVALID_ATTRIBUTE_VALUE_REMOVED"

instance Hashable     SanitizationWarningReason
instance NFData       SanitizationWarningReason
instance ToByteString SanitizationWarningReason
instance ToQuery      SanitizationWarningReason
instance ToHeader     SanitizationWarningReason

instance FromJSON SanitizationWarningReason where
    parseJSON = parseJSONText "SanitizationWarningReason"
