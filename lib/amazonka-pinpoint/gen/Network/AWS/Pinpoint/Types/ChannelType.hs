{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ChannelType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ChannelType where

import Network.AWS.Prelude

data ChannelType
  = CTADM
  | CTAPNS
  | CTAPNSSandbox
  | CTAPNSVoip
  | CTAPNSVoipSandbox
  | CTBaidu
  | CTCustom
  | CTEmail
  | CTGCM
  | CTPush
  | CTSms
  | CTVoice
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

instance FromText ChannelType where
  parser =
    takeLowerText >>= \case
      "adm" -> pure CTADM
      "apns" -> pure CTAPNS
      "apns_sandbox" -> pure CTAPNSSandbox
      "apns_voip" -> pure CTAPNSVoip
      "apns_voip_sandbox" -> pure CTAPNSVoipSandbox
      "baidu" -> pure CTBaidu
      "custom" -> pure CTCustom
      "email" -> pure CTEmail
      "gcm" -> pure CTGCM
      "push" -> pure CTPush
      "sms" -> pure CTSms
      "voice" -> pure CTVoice
      e ->
        fromTextError $
          "Failure parsing ChannelType from value: '" <> e
            <> "'. Accepted values: adm, apns, apns_sandbox, apns_voip, apns_voip_sandbox, baidu, custom, email, gcm, push, sms, voice"

instance ToText ChannelType where
  toText = \case
    CTADM -> "ADM"
    CTAPNS -> "APNS"
    CTAPNSSandbox -> "APNS_SANDBOX"
    CTAPNSVoip -> "APNS_VOIP"
    CTAPNSVoipSandbox -> "APNS_VOIP_SANDBOX"
    CTBaidu -> "BAIDU"
    CTCustom -> "CUSTOM"
    CTEmail -> "EMAIL"
    CTGCM -> "GCM"
    CTPush -> "PUSH"
    CTSms -> "SMS"
    CTVoice -> "VOICE"

instance Hashable ChannelType

instance NFData ChannelType

instance ToByteString ChannelType

instance ToQuery ChannelType

instance ToHeader ChannelType

instance ToJSON ChannelType where
  toJSON = toJSONText

instance FromJSON ChannelType where
  parseJSON = parseJSONText "ChannelType"
