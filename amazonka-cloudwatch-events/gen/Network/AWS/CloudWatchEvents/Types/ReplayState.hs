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
-- Module      : Network.AWS.CloudWatchEvents.Types.ReplayState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.ReplayState
  ( ReplayState
      ( ..,
        ReplayState_CANCELLED,
        ReplayState_CANCELLING,
        ReplayState_COMPLETED,
        ReplayState_FAILED,
        ReplayState_RUNNING,
        ReplayState_STARTING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ReplayState = ReplayState'
  { fromReplayState ::
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

pattern ReplayState_CANCELLED :: ReplayState
pattern ReplayState_CANCELLED = ReplayState' "CANCELLED"

pattern ReplayState_CANCELLING :: ReplayState
pattern ReplayState_CANCELLING = ReplayState' "CANCELLING"

pattern ReplayState_COMPLETED :: ReplayState
pattern ReplayState_COMPLETED = ReplayState' "COMPLETED"

pattern ReplayState_FAILED :: ReplayState
pattern ReplayState_FAILED = ReplayState' "FAILED"

pattern ReplayState_RUNNING :: ReplayState
pattern ReplayState_RUNNING = ReplayState' "RUNNING"

pattern ReplayState_STARTING :: ReplayState
pattern ReplayState_STARTING = ReplayState' "STARTING"

{-# COMPLETE
  ReplayState_CANCELLED,
  ReplayState_CANCELLING,
  ReplayState_COMPLETED,
  ReplayState_FAILED,
  ReplayState_RUNNING,
  ReplayState_STARTING,
  ReplayState'
  #-}
