{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.InstanceStorageResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.InstanceStorageResourceType where

import Network.AWS.Prelude

data InstanceStorageResourceType
  = AgentEvents
  | CallRecordings
  | ChatTranscripts
  | ContactTraceRecords
  | MediaStreams
  | ScheduledReports
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

instance FromText InstanceStorageResourceType where
  parser =
    takeLowerText >>= \case
      "agent_events" -> pure AgentEvents
      "call_recordings" -> pure CallRecordings
      "chat_transcripts" -> pure ChatTranscripts
      "contact_trace_records" -> pure ContactTraceRecords
      "media_streams" -> pure MediaStreams
      "scheduled_reports" -> pure ScheduledReports
      e ->
        fromTextError $
          "Failure parsing InstanceStorageResourceType from value: '" <> e
            <> "'. Accepted values: agent_events, call_recordings, chat_transcripts, contact_trace_records, media_streams, scheduled_reports"

instance ToText InstanceStorageResourceType where
  toText = \case
    AgentEvents -> "AGENT_EVENTS"
    CallRecordings -> "CALL_RECORDINGS"
    ChatTranscripts -> "CHAT_TRANSCRIPTS"
    ContactTraceRecords -> "CONTACT_TRACE_RECORDS"
    MediaStreams -> "MEDIA_STREAMS"
    ScheduledReports -> "SCHEDULED_REPORTS"

instance Hashable InstanceStorageResourceType

instance NFData InstanceStorageResourceType

instance ToByteString InstanceStorageResourceType

instance ToQuery InstanceStorageResourceType

instance ToHeader InstanceStorageResourceType

instance ToJSON InstanceStorageResourceType where
  toJSON = toJSONText
