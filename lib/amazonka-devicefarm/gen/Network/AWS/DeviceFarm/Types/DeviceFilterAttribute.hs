{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.DeviceFilterAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.DeviceFilterAttribute where

import Network.AWS.Prelude

data DeviceFilterAttribute
  = DFAARN
  | DFAAvailability
  | DFAFleetType
  | DFAFormFactor
  | DFAInstanceARN
  | DFAInstanceLabels
  | DFAManufacturer
  | DFAModel
  | DFAOSVersion
  | DFAPlatform
  | DFARemoteAccessEnabled
  | DFARemoteDebugEnabled
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

instance FromText DeviceFilterAttribute where
  parser =
    takeLowerText >>= \case
      "arn" -> pure DFAARN
      "availability" -> pure DFAAvailability
      "fleet_type" -> pure DFAFleetType
      "form_factor" -> pure DFAFormFactor
      "instance_arn" -> pure DFAInstanceARN
      "instance_labels" -> pure DFAInstanceLabels
      "manufacturer" -> pure DFAManufacturer
      "model" -> pure DFAModel
      "os_version" -> pure DFAOSVersion
      "platform" -> pure DFAPlatform
      "remote_access_enabled" -> pure DFARemoteAccessEnabled
      "remote_debug_enabled" -> pure DFARemoteDebugEnabled
      e ->
        fromTextError $
          "Failure parsing DeviceFilterAttribute from value: '" <> e
            <> "'. Accepted values: arn, availability, fleet_type, form_factor, instance_arn, instance_labels, manufacturer, model, os_version, platform, remote_access_enabled, remote_debug_enabled"

instance ToText DeviceFilterAttribute where
  toText = \case
    DFAARN -> "ARN"
    DFAAvailability -> "AVAILABILITY"
    DFAFleetType -> "FLEET_TYPE"
    DFAFormFactor -> "FORM_FACTOR"
    DFAInstanceARN -> "INSTANCE_ARN"
    DFAInstanceLabels -> "INSTANCE_LABELS"
    DFAManufacturer -> "MANUFACTURER"
    DFAModel -> "MODEL"
    DFAOSVersion -> "OS_VERSION"
    DFAPlatform -> "PLATFORM"
    DFARemoteAccessEnabled -> "REMOTE_ACCESS_ENABLED"
    DFARemoteDebugEnabled -> "REMOTE_DEBUG_ENABLED"

instance Hashable DeviceFilterAttribute

instance NFData DeviceFilterAttribute

instance ToByteString DeviceFilterAttribute

instance ToQuery DeviceFilterAttribute

instance ToHeader DeviceFilterAttribute

instance ToJSON DeviceFilterAttribute where
  toJSON = toJSONText

instance FromJSON DeviceFilterAttribute where
  parseJSON = parseJSONText "DeviceFilterAttribute"
