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
-- Module      : Amazonka.SmsVoice.Types.EventType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SmsVoice.Types.EventType
  ( EventType
      ( ..,
        EventType_ANSWERED,
        EventType_BUSY,
        EventType_COMPLETED_CALL,
        EventType_FAILED,
        EventType_INITIATED_CALL,
        EventType_NO_ANSWER,
        EventType_RINGING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The types of events that are sent to the event destination.
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

pattern EventType_ANSWERED :: EventType
pattern EventType_ANSWERED = EventType' "ANSWERED"

pattern EventType_BUSY :: EventType
pattern EventType_BUSY = EventType' "BUSY"

pattern EventType_COMPLETED_CALL :: EventType
pattern EventType_COMPLETED_CALL = EventType' "COMPLETED_CALL"

pattern EventType_FAILED :: EventType
pattern EventType_FAILED = EventType' "FAILED"

pattern EventType_INITIATED_CALL :: EventType
pattern EventType_INITIATED_CALL = EventType' "INITIATED_CALL"

pattern EventType_NO_ANSWER :: EventType
pattern EventType_NO_ANSWER = EventType' "NO_ANSWER"

pattern EventType_RINGING :: EventType
pattern EventType_RINGING = EventType' "RINGING"

{-# COMPLETE
  EventType_ANSWERED,
  EventType_BUSY,
  EventType_COMPLETED_CALL,
  EventType_FAILED,
  EventType_INITIATED_CALL,
  EventType_NO_ANSWER,
  EventType_RINGING,
  EventType'
  #-}
