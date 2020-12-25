{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.InstanceStorageResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.InstanceStorageResourceType
  ( InstanceStorageResourceType
      ( InstanceStorageResourceType',
        InstanceStorageResourceTypeChatTranscripts,
        InstanceStorageResourceTypeCallRecordings,
        InstanceStorageResourceTypeScheduledReports,
        InstanceStorageResourceTypeMediaStreams,
        InstanceStorageResourceTypeContactTraceRecords,
        InstanceStorageResourceTypeAgentEvents,
        fromInstanceStorageResourceType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype InstanceStorageResourceType = InstanceStorageResourceType'
  { fromInstanceStorageResourceType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern InstanceStorageResourceTypeChatTranscripts :: InstanceStorageResourceType
pattern InstanceStorageResourceTypeChatTranscripts = InstanceStorageResourceType' "CHAT_TRANSCRIPTS"

pattern InstanceStorageResourceTypeCallRecordings :: InstanceStorageResourceType
pattern InstanceStorageResourceTypeCallRecordings = InstanceStorageResourceType' "CALL_RECORDINGS"

pattern InstanceStorageResourceTypeScheduledReports :: InstanceStorageResourceType
pattern InstanceStorageResourceTypeScheduledReports = InstanceStorageResourceType' "SCHEDULED_REPORTS"

pattern InstanceStorageResourceTypeMediaStreams :: InstanceStorageResourceType
pattern InstanceStorageResourceTypeMediaStreams = InstanceStorageResourceType' "MEDIA_STREAMS"

pattern InstanceStorageResourceTypeContactTraceRecords :: InstanceStorageResourceType
pattern InstanceStorageResourceTypeContactTraceRecords = InstanceStorageResourceType' "CONTACT_TRACE_RECORDS"

pattern InstanceStorageResourceTypeAgentEvents :: InstanceStorageResourceType
pattern InstanceStorageResourceTypeAgentEvents = InstanceStorageResourceType' "AGENT_EVENTS"

{-# COMPLETE
  InstanceStorageResourceTypeChatTranscripts,
  InstanceStorageResourceTypeCallRecordings,
  InstanceStorageResourceTypeScheduledReports,
  InstanceStorageResourceTypeMediaStreams,
  InstanceStorageResourceTypeContactTraceRecords,
  InstanceStorageResourceTypeAgentEvents,
  InstanceStorageResourceType'
  #-}
