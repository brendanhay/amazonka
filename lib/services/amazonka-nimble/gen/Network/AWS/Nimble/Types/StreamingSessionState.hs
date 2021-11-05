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
-- Module      : Network.AWS.Nimble.Types.StreamingSessionState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Nimble.Types.StreamingSessionState
  ( StreamingSessionState
      ( ..,
        StreamingSessionState_CREATE_FAILED,
        StreamingSessionState_CREATE_IN_PROGRESS,
        StreamingSessionState_DELETED,
        StreamingSessionState_DELETE_FAILED,
        StreamingSessionState_DELETE_IN_PROGRESS,
        StreamingSessionState_READY
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | The streaming session state.
newtype StreamingSessionState = StreamingSessionState'
  { fromStreamingSessionState ::
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

{-# COMPLETE
  StreamingSessionState_CREATE_FAILED,
  StreamingSessionState_CREATE_IN_PROGRESS,
  StreamingSessionState_DELETED,
  StreamingSessionState_DELETE_FAILED,
  StreamingSessionState_DELETE_IN_PROGRESS,
  StreamingSessionState_READY,
  StreamingSessionState'
  #-}
