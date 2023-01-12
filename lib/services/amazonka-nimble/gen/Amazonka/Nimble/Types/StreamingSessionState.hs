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
-- Module      : Amazonka.Nimble.Types.StreamingSessionState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StreamingSessionState
  ( StreamingSessionState
      ( ..,
        StreamingSessionState_CREATE_FAILED,
        StreamingSessionState_CREATE_IN_PROGRESS,
        StreamingSessionState_DELETED,
        StreamingSessionState_DELETE_FAILED,
        StreamingSessionState_DELETE_IN_PROGRESS,
        StreamingSessionState_READY,
        StreamingSessionState_START_FAILED,
        StreamingSessionState_START_IN_PROGRESS,
        StreamingSessionState_STOPPED,
        StreamingSessionState_STOP_FAILED,
        StreamingSessionState_STOP_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The streaming session state.
newtype StreamingSessionState = StreamingSessionState'
  { fromStreamingSessionState ::
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

pattern StreamingSessionState_CREATE_FAILED :: StreamingSessionState
pattern StreamingSessionState_CREATE_FAILED = StreamingSessionState' "CREATE_FAILED"

pattern StreamingSessionState_CREATE_IN_PROGRESS :: StreamingSessionState
pattern StreamingSessionState_CREATE_IN_PROGRESS = StreamingSessionState' "CREATE_IN_PROGRESS"

pattern StreamingSessionState_DELETED :: StreamingSessionState
pattern StreamingSessionState_DELETED = StreamingSessionState' "DELETED"

pattern StreamingSessionState_DELETE_FAILED :: StreamingSessionState
pattern StreamingSessionState_DELETE_FAILED = StreamingSessionState' "DELETE_FAILED"

pattern StreamingSessionState_DELETE_IN_PROGRESS :: StreamingSessionState
pattern StreamingSessionState_DELETE_IN_PROGRESS = StreamingSessionState' "DELETE_IN_PROGRESS"

pattern StreamingSessionState_READY :: StreamingSessionState
pattern StreamingSessionState_READY = StreamingSessionState' "READY"

pattern StreamingSessionState_START_FAILED :: StreamingSessionState
pattern StreamingSessionState_START_FAILED = StreamingSessionState' "START_FAILED"

pattern StreamingSessionState_START_IN_PROGRESS :: StreamingSessionState
pattern StreamingSessionState_START_IN_PROGRESS = StreamingSessionState' "START_IN_PROGRESS"

pattern StreamingSessionState_STOPPED :: StreamingSessionState
pattern StreamingSessionState_STOPPED = StreamingSessionState' "STOPPED"

pattern StreamingSessionState_STOP_FAILED :: StreamingSessionState
pattern StreamingSessionState_STOP_FAILED = StreamingSessionState' "STOP_FAILED"

pattern StreamingSessionState_STOP_IN_PROGRESS :: StreamingSessionState
pattern StreamingSessionState_STOP_IN_PROGRESS = StreamingSessionState' "STOP_IN_PROGRESS"

{-# COMPLETE
  StreamingSessionState_CREATE_FAILED,
  StreamingSessionState_CREATE_IN_PROGRESS,
  StreamingSessionState_DELETED,
  StreamingSessionState_DELETE_FAILED,
  StreamingSessionState_DELETE_IN_PROGRESS,
  StreamingSessionState_READY,
  StreamingSessionState_START_FAILED,
  StreamingSessionState_START_IN_PROGRESS,
  StreamingSessionState_STOPPED,
  StreamingSessionState_STOP_FAILED,
  StreamingSessionState_STOP_IN_PROGRESS,
  StreamingSessionState'
  #-}
