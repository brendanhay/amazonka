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
        ChatTranscripts,
        CallRecordings,
        ScheduledReports,
        MediaStreams,
        ContactTraceRecords,
        AgentEvents
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

pattern ChatTranscripts :: InstanceStorageResourceType
pattern ChatTranscripts = InstanceStorageResourceType' "CHAT_TRANSCRIPTS"

pattern CallRecordings :: InstanceStorageResourceType
pattern CallRecordings = InstanceStorageResourceType' "CALL_RECORDINGS"

pattern ScheduledReports :: InstanceStorageResourceType
pattern ScheduledReports = InstanceStorageResourceType' "SCHEDULED_REPORTS"

pattern MediaStreams :: InstanceStorageResourceType
pattern MediaStreams = InstanceStorageResourceType' "MEDIA_STREAMS"

pattern ContactTraceRecords :: InstanceStorageResourceType
pattern ContactTraceRecords = InstanceStorageResourceType' "CONTACT_TRACE_RECORDS"

pattern AgentEvents :: InstanceStorageResourceType
pattern AgentEvents = InstanceStorageResourceType' "AGENT_EVENTS"

{-# COMPLETE
  ChatTranscripts,
  CallRecordings,
  ScheduledReports,
  MediaStreams,
  ContactTraceRecords,
  AgentEvents,
  InstanceStorageResourceType'
  #-}
