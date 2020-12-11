-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.InstanceAttributeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.InstanceAttributeType
  ( InstanceAttributeType
      ( InstanceAttributeType',
        AutoResolveBestVoices,
        ContactLens,
        ContactflowLogs,
        EarlyMedia,
        InboundCalls,
        OutboundCalls,
        UseCustomTtsVoices
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype InstanceAttributeType = InstanceAttributeType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern AutoResolveBestVoices :: InstanceAttributeType
pattern AutoResolveBestVoices = InstanceAttributeType' "AUTO_RESOLVE_BEST_VOICES"

pattern ContactLens :: InstanceAttributeType
pattern ContactLens = InstanceAttributeType' "CONTACT_LENS"

pattern ContactflowLogs :: InstanceAttributeType
pattern ContactflowLogs = InstanceAttributeType' "CONTACTFLOW_LOGS"

pattern EarlyMedia :: InstanceAttributeType
pattern EarlyMedia = InstanceAttributeType' "EARLY_MEDIA"

pattern InboundCalls :: InstanceAttributeType
pattern InboundCalls = InstanceAttributeType' "INBOUND_CALLS"

pattern OutboundCalls :: InstanceAttributeType
pattern OutboundCalls = InstanceAttributeType' "OUTBOUND_CALLS"

pattern UseCustomTtsVoices :: InstanceAttributeType
pattern UseCustomTtsVoices = InstanceAttributeType' "USE_CUSTOM_TTS_VOICES"

{-# COMPLETE
  AutoResolveBestVoices,
  ContactLens,
  ContactflowLogs,
  EarlyMedia,
  InboundCalls,
  OutboundCalls,
  UseCustomTtsVoices,
  InstanceAttributeType'
  #-}
