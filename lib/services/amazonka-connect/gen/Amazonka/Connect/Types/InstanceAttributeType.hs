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
-- Module      : Amazonka.Connect.Types.InstanceAttributeType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.InstanceAttributeType
  ( InstanceAttributeType
      ( ..,
        InstanceAttributeType_AUTO_RESOLVE_BEST_VOICES,
        InstanceAttributeType_CONTACTFLOW_LOGS,
        InstanceAttributeType_CONTACT_LENS,
        InstanceAttributeType_EARLY_MEDIA,
        InstanceAttributeType_ENHANCED_CONTACT_MONITORING,
        InstanceAttributeType_HIGH_VOLUME_OUTBOUND,
        InstanceAttributeType_INBOUND_CALLS,
        InstanceAttributeType_MULTI_PARTY_CONFERENCE,
        InstanceAttributeType_OUTBOUND_CALLS,
        InstanceAttributeType_USE_CUSTOM_TTS_VOICES
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InstanceAttributeType = InstanceAttributeType'
  { fromInstanceAttributeType ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern InstanceAttributeType_AUTO_RESOLVE_BEST_VOICES :: InstanceAttributeType
pattern InstanceAttributeType_AUTO_RESOLVE_BEST_VOICES = InstanceAttributeType' "AUTO_RESOLVE_BEST_VOICES"

pattern InstanceAttributeType_CONTACTFLOW_LOGS :: InstanceAttributeType
pattern InstanceAttributeType_CONTACTFLOW_LOGS = InstanceAttributeType' "CONTACTFLOW_LOGS"

pattern InstanceAttributeType_CONTACT_LENS :: InstanceAttributeType
pattern InstanceAttributeType_CONTACT_LENS = InstanceAttributeType' "CONTACT_LENS"

pattern InstanceAttributeType_EARLY_MEDIA :: InstanceAttributeType
pattern InstanceAttributeType_EARLY_MEDIA = InstanceAttributeType' "EARLY_MEDIA"

pattern InstanceAttributeType_ENHANCED_CONTACT_MONITORING :: InstanceAttributeType
pattern InstanceAttributeType_ENHANCED_CONTACT_MONITORING = InstanceAttributeType' "ENHANCED_CONTACT_MONITORING"

pattern InstanceAttributeType_HIGH_VOLUME_OUTBOUND :: InstanceAttributeType
pattern InstanceAttributeType_HIGH_VOLUME_OUTBOUND = InstanceAttributeType' "HIGH_VOLUME_OUTBOUND"

pattern InstanceAttributeType_INBOUND_CALLS :: InstanceAttributeType
pattern InstanceAttributeType_INBOUND_CALLS = InstanceAttributeType' "INBOUND_CALLS"

pattern InstanceAttributeType_MULTI_PARTY_CONFERENCE :: InstanceAttributeType
pattern InstanceAttributeType_MULTI_PARTY_CONFERENCE = InstanceAttributeType' "MULTI_PARTY_CONFERENCE"

pattern InstanceAttributeType_OUTBOUND_CALLS :: InstanceAttributeType
pattern InstanceAttributeType_OUTBOUND_CALLS = InstanceAttributeType' "OUTBOUND_CALLS"

pattern InstanceAttributeType_USE_CUSTOM_TTS_VOICES :: InstanceAttributeType
pattern InstanceAttributeType_USE_CUSTOM_TTS_VOICES = InstanceAttributeType' "USE_CUSTOM_TTS_VOICES"

{-# COMPLETE
  InstanceAttributeType_AUTO_RESOLVE_BEST_VOICES,
  InstanceAttributeType_CONTACTFLOW_LOGS,
  InstanceAttributeType_CONTACT_LENS,
  InstanceAttributeType_EARLY_MEDIA,
  InstanceAttributeType_ENHANCED_CONTACT_MONITORING,
  InstanceAttributeType_HIGH_VOLUME_OUTBOUND,
  InstanceAttributeType_INBOUND_CALLS,
  InstanceAttributeType_MULTI_PARTY_CONFERENCE,
  InstanceAttributeType_OUTBOUND_CALLS,
  InstanceAttributeType_USE_CUSTOM_TTS_VOICES,
  InstanceAttributeType'
  #-}
