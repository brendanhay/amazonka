{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.DeviceAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.DeviceAttribute where

import Network.AWS.Prelude

data DeviceAttribute
  = ARN
  | AppiumVersion
  | Availability
  | FleetType
  | FormFactor
  | InstanceARN
  | InstanceLabels
  | Manufacturer
  | Model
  | OSVersion
  | Platform
  | RemoteAccessEnabled
  | RemoteDebugEnabled
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText DeviceAttribute where
  parser =
    takeLowerText >>= \case
      "arn" -> pure ARN
      "appium_version" -> pure AppiumVersion
      "availability" -> pure Availability
      "fleet_type" -> pure FleetType
      "form_factor" -> pure FormFactor
      "instance_arn" -> pure InstanceARN
      "instance_labels" -> pure InstanceLabels
      "manufacturer" -> pure Manufacturer
      "model" -> pure Model
      "os_version" -> pure OSVersion
      "platform" -> pure Platform
      "remote_access_enabled" -> pure RemoteAccessEnabled
      "remote_debug_enabled" -> pure RemoteDebugEnabled
      e ->
        fromTextError $
          "Failure parsing DeviceAttribute from value: '" <> e
            <> "'. Accepted values: arn, appium_version, availability, fleet_type, form_factor, instance_arn, instance_labels, manufacturer, model, os_version, platform, remote_access_enabled, remote_debug_enabled"

instance ToText DeviceAttribute where
  toText = \case
    ARN -> "ARN"
    AppiumVersion -> "APPIUM_VERSION"
    Availability -> "AVAILABILITY"
    FleetType -> "FLEET_TYPE"
    FormFactor -> "FORM_FACTOR"
    InstanceARN -> "INSTANCE_ARN"
    InstanceLabels -> "INSTANCE_LABELS"
    Manufacturer -> "MANUFACTURER"
    Model -> "MODEL"
    OSVersion -> "OS_VERSION"
    Platform -> "PLATFORM"
    RemoteAccessEnabled -> "REMOTE_ACCESS_ENABLED"
    RemoteDebugEnabled -> "REMOTE_DEBUG_ENABLED"

instance Hashable DeviceAttribute

instance NFData DeviceAttribute

instance ToByteString DeviceAttribute

instance ToQuery DeviceAttribute

instance ToHeader DeviceAttribute

instance ToJSON DeviceAttribute where
  toJSON = toJSONText

instance FromJSON DeviceAttribute where
  parseJSON = parseJSONText "DeviceAttribute"
