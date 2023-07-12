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
-- Module      : Amazonka.Connect.Types.InstanceStorageResourceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.InstanceStorageResourceType
  ( InstanceStorageResourceType
      ( ..,
        InstanceStorageResourceType_AGENT_EVENTS,
        InstanceStorageResourceType_CALL_RECORDINGS,
        InstanceStorageResourceType_CHAT_TRANSCRIPTS,
        InstanceStorageResourceType_CONTACT_TRACE_RECORDS,
        InstanceStorageResourceType_MEDIA_STREAMS,
        InstanceStorageResourceType_REAL_TIME_CONTACT_ANALYSIS_SEGMENTS,
        InstanceStorageResourceType_SCHEDULED_REPORTS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InstanceStorageResourceType = InstanceStorageResourceType'
  { fromInstanceStorageResourceType ::
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

pattern InstanceStorageResourceType_REAL_TIME_CONTACT_ANALYSIS_SEGMENTS :: InstanceStorageResourceType
pattern InstanceStorageResourceType_REAL_TIME_CONTACT_ANALYSIS_SEGMENTS = InstanceStorageResourceType' "REAL_TIME_CONTACT_ANALYSIS_SEGMENTS"

pattern InstanceStorageResourceType_SCHEDULED_REPORTS :: InstanceStorageResourceType
pattern InstanceStorageResourceType_SCHEDULED_REPORTS = InstanceStorageResourceType' "SCHEDULED_REPORTS"

{-# COMPLETE
  InstanceStorageResourceType_AGENT_EVENTS,
  InstanceStorageResourceType_CALL_RECORDINGS,
  InstanceStorageResourceType_CHAT_TRANSCRIPTS,
  InstanceStorageResourceType_CONTACT_TRACE_RECORDS,
  InstanceStorageResourceType_MEDIA_STREAMS,
  InstanceStorageResourceType_REAL_TIME_CONTACT_ANALYSIS_SEGMENTS,
  InstanceStorageResourceType_SCHEDULED_REPORTS,
  InstanceStorageResourceType'
  #-}
