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
        AgentEvents,
        CallRecordings,
        ChatTranscripts,
        ContactTraceRecords,
        MediaStreams,
        ScheduledReports
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype InstanceStorageResourceType = InstanceStorageResourceType' Lude.Text
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

pattern AgentEvents :: InstanceStorageResourceType
pattern AgentEvents = InstanceStorageResourceType' "AGENT_EVENTS"

pattern CallRecordings :: InstanceStorageResourceType
pattern CallRecordings = InstanceStorageResourceType' "CALL_RECORDINGS"

pattern ChatTranscripts :: InstanceStorageResourceType
pattern ChatTranscripts = InstanceStorageResourceType' "CHAT_TRANSCRIPTS"

pattern ContactTraceRecords :: InstanceStorageResourceType
pattern ContactTraceRecords = InstanceStorageResourceType' "CONTACT_TRACE_RECORDS"

pattern MediaStreams :: InstanceStorageResourceType
pattern MediaStreams = InstanceStorageResourceType' "MEDIA_STREAMS"

pattern ScheduledReports :: InstanceStorageResourceType
pattern ScheduledReports = InstanceStorageResourceType' "SCHEDULED_REPORTS"

{-# COMPLETE
  AgentEvents,
  CallRecordings,
  ChatTranscripts,
  ContactTraceRecords,
  MediaStreams,
  ScheduledReports,
  InstanceStorageResourceType'
  #-}
