{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.InstanceAttributeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.InstanceAttributeType where

import Network.AWS.Prelude

data InstanceAttributeType
  = AutoResolveBestVoices
  | ContactLens
  | ContactflowLogs
  | EarlyMedia
  | InboundCalls
  | OutboundCalls
  | UseCustomTtsVoices
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

instance FromText InstanceAttributeType where
  parser =
    takeLowerText >>= \case
      "auto_resolve_best_voices" -> pure AutoResolveBestVoices
      "contact_lens" -> pure ContactLens
      "contactflow_logs" -> pure ContactflowLogs
      "early_media" -> pure EarlyMedia
      "inbound_calls" -> pure InboundCalls
      "outbound_calls" -> pure OutboundCalls
      "use_custom_tts_voices" -> pure UseCustomTtsVoices
      e ->
        fromTextError $
          "Failure parsing InstanceAttributeType from value: '" <> e
            <> "'. Accepted values: auto_resolve_best_voices, contact_lens, contactflow_logs, early_media, inbound_calls, outbound_calls, use_custom_tts_voices"

instance ToText InstanceAttributeType where
  toText = \case
    AutoResolveBestVoices -> "AUTO_RESOLVE_BEST_VOICES"
    ContactLens -> "CONTACT_LENS"
    ContactflowLogs -> "CONTACTFLOW_LOGS"
    EarlyMedia -> "EARLY_MEDIA"
    InboundCalls -> "INBOUND_CALLS"
    OutboundCalls -> "OUTBOUND_CALLS"
    UseCustomTtsVoices -> "USE_CUSTOM_TTS_VOICES"

instance Hashable InstanceAttributeType

instance NFData InstanceAttributeType

instance ToByteString InstanceAttributeType

instance ToQuery InstanceAttributeType

instance ToHeader InstanceAttributeType

instance ToJSON InstanceAttributeType where
  toJSON = toJSONText

instance FromJSON InstanceAttributeType where
  parseJSON = parseJSONText "InstanceAttributeType"
