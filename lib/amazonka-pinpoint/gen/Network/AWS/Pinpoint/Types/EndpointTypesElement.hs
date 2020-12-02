{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointTypesElement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointTypesElement where

import Network.AWS.Prelude

data EndpointTypesElement
  = ADM
  | APNS
  | APNSSandbox
  | APNSVoip
  | APNSVoipSandbox
  | Baidu
  | Custom
  | Email
  | GCM
  | Push
  | Sms
  | Voice
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

instance FromText EndpointTypesElement where
  parser =
    takeLowerText >>= \case
      "adm" -> pure ADM
      "apns" -> pure APNS
      "apns_sandbox" -> pure APNSSandbox
      "apns_voip" -> pure APNSVoip
      "apns_voip_sandbox" -> pure APNSVoipSandbox
      "baidu" -> pure Baidu
      "custom" -> pure Custom
      "email" -> pure Email
      "gcm" -> pure GCM
      "push" -> pure Push
      "sms" -> pure Sms
      "voice" -> pure Voice
      e ->
        fromTextError $
          "Failure parsing EndpointTypesElement from value: '" <> e
            <> "'. Accepted values: adm, apns, apns_sandbox, apns_voip, apns_voip_sandbox, baidu, custom, email, gcm, push, sms, voice"

instance ToText EndpointTypesElement where
  toText = \case
    ADM -> "ADM"
    APNS -> "APNS"
    APNSSandbox -> "APNS_SANDBOX"
    APNSVoip -> "APNS_VOIP"
    APNSVoipSandbox -> "APNS_VOIP_SANDBOX"
    Baidu -> "BAIDU"
    Custom -> "CUSTOM"
    Email -> "EMAIL"
    GCM -> "GCM"
    Push -> "PUSH"
    Sms -> "SMS"
    Voice -> "VOICE"

instance Hashable EndpointTypesElement

instance NFData EndpointTypesElement

instance ToByteString EndpointTypesElement

instance ToQuery EndpointTypesElement

instance ToHeader EndpointTypesElement

instance ToJSON EndpointTypesElement where
  toJSON = toJSONText

instance FromJSON EndpointTypesElement where
  parseJSON = parseJSONText "EndpointTypesElement"
