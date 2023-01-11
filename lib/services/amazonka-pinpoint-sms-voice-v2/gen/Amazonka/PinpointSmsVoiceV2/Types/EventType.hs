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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.EventType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.EventType
  ( EventType
      ( ..,
        EventType_ALL,
        EventType_TEXT_ALL,
        EventType_TEXT_BLOCKED,
        EventType_TEXT_CARRIER_BLOCKED,
        EventType_TEXT_CARRIER_UNREACHABLE,
        EventType_TEXT_DELIVERED,
        EventType_TEXT_INVALID,
        EventType_TEXT_INVALID_MESSAGE,
        EventType_TEXT_PENDING,
        EventType_TEXT_QUEUED,
        EventType_TEXT_SENT,
        EventType_TEXT_SPAM,
        EventType_TEXT_SUCCESSFUL,
        EventType_TEXT_TTL_EXPIRED,
        EventType_TEXT_UNKNOWN,
        EventType_TEXT_UNREACHABLE,
        EventType_VOICE_ALL,
        EventType_VOICE_ANSWERED,
        EventType_VOICE_BUSY,
        EventType_VOICE_COMPLETED,
        EventType_VOICE_FAILED,
        EventType_VOICE_INITIATED,
        EventType_VOICE_NO_ANSWER,
        EventType_VOICE_RINGING,
        EventType_VOICE_TTL_EXPIRED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EventType = EventType'
  { fromEventType ::
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

pattern EventType_ALL :: EventType
pattern EventType_ALL = EventType' "ALL"

pattern EventType_TEXT_ALL :: EventType
pattern EventType_TEXT_ALL = EventType' "TEXT_ALL"

pattern EventType_TEXT_BLOCKED :: EventType
pattern EventType_TEXT_BLOCKED = EventType' "TEXT_BLOCKED"

pattern EventType_TEXT_CARRIER_BLOCKED :: EventType
pattern EventType_TEXT_CARRIER_BLOCKED = EventType' "TEXT_CARRIER_BLOCKED"

pattern EventType_TEXT_CARRIER_UNREACHABLE :: EventType
pattern EventType_TEXT_CARRIER_UNREACHABLE = EventType' "TEXT_CARRIER_UNREACHABLE"

pattern EventType_TEXT_DELIVERED :: EventType
pattern EventType_TEXT_DELIVERED = EventType' "TEXT_DELIVERED"

pattern EventType_TEXT_INVALID :: EventType
pattern EventType_TEXT_INVALID = EventType' "TEXT_INVALID"

pattern EventType_TEXT_INVALID_MESSAGE :: EventType
pattern EventType_TEXT_INVALID_MESSAGE = EventType' "TEXT_INVALID_MESSAGE"

pattern EventType_TEXT_PENDING :: EventType
pattern EventType_TEXT_PENDING = EventType' "TEXT_PENDING"

pattern EventType_TEXT_QUEUED :: EventType
pattern EventType_TEXT_QUEUED = EventType' "TEXT_QUEUED"

pattern EventType_TEXT_SENT :: EventType
pattern EventType_TEXT_SENT = EventType' "TEXT_SENT"

pattern EventType_TEXT_SPAM :: EventType
pattern EventType_TEXT_SPAM = EventType' "TEXT_SPAM"

pattern EventType_TEXT_SUCCESSFUL :: EventType
pattern EventType_TEXT_SUCCESSFUL = EventType' "TEXT_SUCCESSFUL"

pattern EventType_TEXT_TTL_EXPIRED :: EventType
pattern EventType_TEXT_TTL_EXPIRED = EventType' "TEXT_TTL_EXPIRED"

pattern EventType_TEXT_UNKNOWN :: EventType
pattern EventType_TEXT_UNKNOWN = EventType' "TEXT_UNKNOWN"

pattern EventType_TEXT_UNREACHABLE :: EventType
pattern EventType_TEXT_UNREACHABLE = EventType' "TEXT_UNREACHABLE"

pattern EventType_VOICE_ALL :: EventType
pattern EventType_VOICE_ALL = EventType' "VOICE_ALL"

pattern EventType_VOICE_ANSWERED :: EventType
pattern EventType_VOICE_ANSWERED = EventType' "VOICE_ANSWERED"

pattern EventType_VOICE_BUSY :: EventType
pattern EventType_VOICE_BUSY = EventType' "VOICE_BUSY"

pattern EventType_VOICE_COMPLETED :: EventType
pattern EventType_VOICE_COMPLETED = EventType' "VOICE_COMPLETED"

pattern EventType_VOICE_FAILED :: EventType
pattern EventType_VOICE_FAILED = EventType' "VOICE_FAILED"

pattern EventType_VOICE_INITIATED :: EventType
pattern EventType_VOICE_INITIATED = EventType' "VOICE_INITIATED"

pattern EventType_VOICE_NO_ANSWER :: EventType
pattern EventType_VOICE_NO_ANSWER = EventType' "VOICE_NO_ANSWER"

pattern EventType_VOICE_RINGING :: EventType
pattern EventType_VOICE_RINGING = EventType' "VOICE_RINGING"

pattern EventType_VOICE_TTL_EXPIRED :: EventType
pattern EventType_VOICE_TTL_EXPIRED = EventType' "VOICE_TTL_EXPIRED"

{-# COMPLETE
  EventType_ALL,
  EventType_TEXT_ALL,
  EventType_TEXT_BLOCKED,
  EventType_TEXT_CARRIER_BLOCKED,
  EventType_TEXT_CARRIER_UNREACHABLE,
  EventType_TEXT_DELIVERED,
  EventType_TEXT_INVALID,
  EventType_TEXT_INVALID_MESSAGE,
  EventType_TEXT_PENDING,
  EventType_TEXT_QUEUED,
  EventType_TEXT_SENT,
  EventType_TEXT_SPAM,
  EventType_TEXT_SUCCESSFUL,
  EventType_TEXT_TTL_EXPIRED,
  EventType_TEXT_UNKNOWN,
  EventType_TEXT_UNREACHABLE,
  EventType_VOICE_ALL,
  EventType_VOICE_ANSWERED,
  EventType_VOICE_BUSY,
  EventType_VOICE_COMPLETED,
  EventType_VOICE_FAILED,
  EventType_VOICE_INITIATED,
  EventType_VOICE_NO_ANSWER,
  EventType_VOICE_RINGING,
  EventType_VOICE_TTL_EXPIRED,
  EventType'
  #-}
