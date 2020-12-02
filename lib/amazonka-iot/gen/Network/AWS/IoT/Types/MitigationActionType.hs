{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.MitigationActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.MitigationActionType where

import Network.AWS.Prelude

data MitigationActionType
  = AddThingsToThingGroup
  | EnableIotLogging
  | PublishFindingToSNS
  | ReplaceDefaultPolicyVersion
  | UpdateCaCertificate
  | UpdateDeviceCertificate
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

instance FromText MitigationActionType where
  parser =
    takeLowerText >>= \case
      "add_things_to_thing_group" -> pure AddThingsToThingGroup
      "enable_iot_logging" -> pure EnableIotLogging
      "publish_finding_to_sns" -> pure PublishFindingToSNS
      "replace_default_policy_version" -> pure ReplaceDefaultPolicyVersion
      "update_ca_certificate" -> pure UpdateCaCertificate
      "update_device_certificate" -> pure UpdateDeviceCertificate
      e ->
        fromTextError $
          "Failure parsing MitigationActionType from value: '" <> e
            <> "'. Accepted values: add_things_to_thing_group, enable_iot_logging, publish_finding_to_sns, replace_default_policy_version, update_ca_certificate, update_device_certificate"

instance ToText MitigationActionType where
  toText = \case
    AddThingsToThingGroup -> "ADD_THINGS_TO_THING_GROUP"
    EnableIotLogging -> "ENABLE_IOT_LOGGING"
    PublishFindingToSNS -> "PUBLISH_FINDING_TO_SNS"
    ReplaceDefaultPolicyVersion -> "REPLACE_DEFAULT_POLICY_VERSION"
    UpdateCaCertificate -> "UPDATE_CA_CERTIFICATE"
    UpdateDeviceCertificate -> "UPDATE_DEVICE_CERTIFICATE"

instance Hashable MitigationActionType

instance NFData MitigationActionType

instance ToByteString MitigationActionType

instance ToQuery MitigationActionType

instance ToHeader MitigationActionType

instance ToJSON MitigationActionType where
  toJSON = toJSONText

instance FromJSON MitigationActionType where
  parseJSON = parseJSONText "MitigationActionType"
