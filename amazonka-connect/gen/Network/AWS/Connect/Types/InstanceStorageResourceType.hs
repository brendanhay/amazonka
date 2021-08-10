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
-- Module      : Network.AWS.Connect.Types.InstanceStorageResourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.InstanceStorageResourceType
  ( InstanceStorageResourceType
      ( ..,
        InstanceStorageResourceType_AGENT_EVENTS,
        InstanceStorageResourceType_CALL_RECORDINGS,
        InstanceStorageResourceType_CHAT_TRANSCRIPTS,
        InstanceStorageResourceType_CONTACT_TRACE_RECORDS,
        InstanceStorageResourceType_MEDIA_STREAMS,
        InstanceStorageResourceType_SCHEDULED_REPORTS
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype InstanceStorageResourceType = InstanceStorageResourceType'
  { fromInstanceStorageResourceType ::
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

pattern InstanceStorageResourceType_AGENT_EVENTS :: InstanceStorageResourceType
pattern InstanceStorageResourceType_AGENT_EVENTS = InstanceStorageResourceType' "AGENT_EVENTS"

pattern InstanceStorageResourceType_CALL_RECORDINGS :: InstanceStorageResourceType
pattern InstanceStorageResourceType_CALL_RECORDINGS = InstanceStorageResourceType' "CALL_RECORDINGS"

pattern InstanceStorageResourceType_CHAT_TRANSCRIPTS :: InstanceStorageResourceType
pattern InstanceStorageResourceType_CHAT_TRANSCRIPTS = InstanceStorageResourceType' "CHAT_TRANSCRIPTS"

pattern InstanceStorageResourceType_CONTACT_TRACE_RECORDS :: InstanceStorageResourceType
pattern InstanceStorageResourceType_CONTACT_TRACE_RECORDS = InstanceStorageResourceType' "CONTACT_TRACE_RECORDS"

pattern InstanceStorageResourceType_MEDIA_STREAMS :: InstanceStorageResourceType
pattern InstanceStorageResourceType_MEDIA_STREAMS = InstanceStorageResourceType' "MEDIA_STREAMS"

pattern InstanceStorageResourceType_SCHEDULED_REPORTS :: InstanceStorageResourceType
pattern InstanceStorageResourceType_SCHEDULED_REPORTS = InstanceStorageResourceType' "SCHEDULED_REPORTS"

{-# COMPLETE
  InstanceStorageResourceType_AGENT_EVENTS,
  InstanceStorageResourceType_CALL_RECORDINGS,
  InstanceStorageResourceType_CHAT_TRANSCRIPTS,
  InstanceStorageResourceType_CONTACT_TRACE_RECORDS,
  InstanceStorageResourceType_MEDIA_STREAMS,
  InstanceStorageResourceType_SCHEDULED_REPORTS,
  InstanceStorageResourceType'
  #-}
