{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ResourceType where

import Network.AWS.Prelude

data ResourceType
  = RTAccountSettings
  | RTCaCertificate
  | RTClientId
  | RTCognitoIdentityPool
  | RTDeviceCertificate
  | RTIAMRole
  | RTIotPolicy
  | RTRoleAlias
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

instance FromText ResourceType where
  parser =
    takeLowerText >>= \case
      "account_settings" -> pure RTAccountSettings
      "ca_certificate" -> pure RTCaCertificate
      "client_id" -> pure RTClientId
      "cognito_identity_pool" -> pure RTCognitoIdentityPool
      "device_certificate" -> pure RTDeviceCertificate
      "iam_role" -> pure RTIAMRole
      "iot_policy" -> pure RTIotPolicy
      "role_alias" -> pure RTRoleAlias
      e ->
        fromTextError $
          "Failure parsing ResourceType from value: '" <> e
            <> "'. Accepted values: account_settings, ca_certificate, client_id, cognito_identity_pool, device_certificate, iam_role, iot_policy, role_alias"

instance ToText ResourceType where
  toText = \case
    RTAccountSettings -> "ACCOUNT_SETTINGS"
    RTCaCertificate -> "CA_CERTIFICATE"
    RTClientId -> "CLIENT_ID"
    RTCognitoIdentityPool -> "COGNITO_IDENTITY_POOL"
    RTDeviceCertificate -> "DEVICE_CERTIFICATE"
    RTIAMRole -> "IAM_ROLE"
    RTIotPolicy -> "IOT_POLICY"
    RTRoleAlias -> "ROLE_ALIAS"

instance Hashable ResourceType

instance NFData ResourceType

instance ToByteString ResourceType

instance ToQuery ResourceType

instance ToHeader ResourceType

instance FromJSON ResourceType where
  parseJSON = parseJSONText "ResourceType"
