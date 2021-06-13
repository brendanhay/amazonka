{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.InstanceAttributeType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.InstanceAttributeType
  ( InstanceAttributeType
      ( ..,
        InstanceAttributeType_AUTO_RESOLVE_BEST_VOICES,
        InstanceAttributeType_CONTACTFLOW_LOGS,
        InstanceAttributeType_CONTACT_LENS,
        InstanceAttributeType_EARLY_MEDIA,
        InstanceAttributeType_INBOUND_CALLS,
        InstanceAttributeType_OUTBOUND_CALLS,
        InstanceAttributeType_USE_CUSTOM_TTS_VOICES
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype InstanceAttributeType = InstanceAttributeType'
  { fromInstanceAttributeType ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern InstanceAttributeType_AUTO_RESOLVE_BEST_VOICES :: InstanceAttributeType
pattern InstanceAttributeType_AUTO_RESOLVE_BEST_VOICES = InstanceAttributeType' "AUTO_RESOLVE_BEST_VOICES"

pattern InstanceAttributeType_CONTACTFLOW_LOGS :: InstanceAttributeType
pattern InstanceAttributeType_CONTACTFLOW_LOGS = InstanceAttributeType' "CONTACTFLOW_LOGS"

pattern InstanceAttributeType_CONTACT_LENS :: InstanceAttributeType
pattern InstanceAttributeType_CONTACT_LENS = InstanceAttributeType' "CONTACT_LENS"

pattern InstanceAttributeType_EARLY_MEDIA :: InstanceAttributeType
pattern InstanceAttributeType_EARLY_MEDIA = InstanceAttributeType' "EARLY_MEDIA"

pattern InstanceAttributeType_INBOUND_CALLS :: InstanceAttributeType
pattern InstanceAttributeType_INBOUND_CALLS = InstanceAttributeType' "INBOUND_CALLS"

pattern InstanceAttributeType_OUTBOUND_CALLS :: InstanceAttributeType
pattern InstanceAttributeType_OUTBOUND_CALLS = InstanceAttributeType' "OUTBOUND_CALLS"

pattern InstanceAttributeType_USE_CUSTOM_TTS_VOICES :: InstanceAttributeType
pattern InstanceAttributeType_USE_CUSTOM_TTS_VOICES = InstanceAttributeType' "USE_CUSTOM_TTS_VOICES"

{-# COMPLETE
  InstanceAttributeType_AUTO_RESOLVE_BEST_VOICES,
  InstanceAttributeType_CONTACTFLOW_LOGS,
  InstanceAttributeType_CONTACT_LENS,
  InstanceAttributeType_EARLY_MEDIA,
  InstanceAttributeType_INBOUND_CALLS,
  InstanceAttributeType_OUTBOUND_CALLS,
  InstanceAttributeType_USE_CUSTOM_TTS_VOICES,
  InstanceAttributeType'
  #-}
