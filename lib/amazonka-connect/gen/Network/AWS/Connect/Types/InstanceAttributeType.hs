{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.InstanceAttributeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.InstanceAttributeType
  ( InstanceAttributeType
    ( InstanceAttributeType'
    , InstanceAttributeTypeInboundCalls
    , InstanceAttributeTypeOutboundCalls
    , InstanceAttributeTypeContactflowLogs
    , InstanceAttributeTypeContactLens
    , InstanceAttributeTypeAutoResolveBestVoices
    , InstanceAttributeTypeUseCustomTtsVoices
    , InstanceAttributeTypeEarlyMedia
    , fromInstanceAttributeType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype InstanceAttributeType = InstanceAttributeType'{fromInstanceAttributeType
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern InstanceAttributeTypeInboundCalls :: InstanceAttributeType
pattern InstanceAttributeTypeInboundCalls = InstanceAttributeType' "INBOUND_CALLS"

pattern InstanceAttributeTypeOutboundCalls :: InstanceAttributeType
pattern InstanceAttributeTypeOutboundCalls = InstanceAttributeType' "OUTBOUND_CALLS"

pattern InstanceAttributeTypeContactflowLogs :: InstanceAttributeType
pattern InstanceAttributeTypeContactflowLogs = InstanceAttributeType' "CONTACTFLOW_LOGS"

pattern InstanceAttributeTypeContactLens :: InstanceAttributeType
pattern InstanceAttributeTypeContactLens = InstanceAttributeType' "CONTACT_LENS"

pattern InstanceAttributeTypeAutoResolveBestVoices :: InstanceAttributeType
pattern InstanceAttributeTypeAutoResolveBestVoices = InstanceAttributeType' "AUTO_RESOLVE_BEST_VOICES"

pattern InstanceAttributeTypeUseCustomTtsVoices :: InstanceAttributeType
pattern InstanceAttributeTypeUseCustomTtsVoices = InstanceAttributeType' "USE_CUSTOM_TTS_VOICES"

pattern InstanceAttributeTypeEarlyMedia :: InstanceAttributeType
pattern InstanceAttributeTypeEarlyMedia = InstanceAttributeType' "EARLY_MEDIA"

{-# COMPLETE 
  InstanceAttributeTypeInboundCalls,

  InstanceAttributeTypeOutboundCalls,

  InstanceAttributeTypeContactflowLogs,

  InstanceAttributeTypeContactLens,

  InstanceAttributeTypeAutoResolveBestVoices,

  InstanceAttributeTypeUseCustomTtsVoices,

  InstanceAttributeTypeEarlyMedia,
  InstanceAttributeType'
  #-}
