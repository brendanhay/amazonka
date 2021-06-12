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
-- Module      : Network.AWS.MediaLive.Types.MultiplexState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexState
  ( MultiplexState
      ( ..,
        MultiplexState_CREATE_FAILED,
        MultiplexState_CREATING,
        MultiplexState_DELETED,
        MultiplexState_DELETING,
        MultiplexState_IDLE,
        MultiplexState_RECOVERING,
        MultiplexState_RUNNING,
        MultiplexState_STARTING,
        MultiplexState_STOPPING
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | The current state of the multiplex.
newtype MultiplexState = MultiplexState'
  { fromMultiplexState ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern MultiplexState_CREATE_FAILED :: MultiplexState
pattern MultiplexState_CREATE_FAILED = MultiplexState' "CREATE_FAILED"

pattern MultiplexState_CREATING :: MultiplexState
pattern MultiplexState_CREATING = MultiplexState' "CREATING"

pattern MultiplexState_DELETED :: MultiplexState
pattern MultiplexState_DELETED = MultiplexState' "DELETED"

pattern MultiplexState_DELETING :: MultiplexState
pattern MultiplexState_DELETING = MultiplexState' "DELETING"

pattern MultiplexState_IDLE :: MultiplexState
pattern MultiplexState_IDLE = MultiplexState' "IDLE"

pattern MultiplexState_RECOVERING :: MultiplexState
pattern MultiplexState_RECOVERING = MultiplexState' "RECOVERING"

pattern MultiplexState_RUNNING :: MultiplexState
pattern MultiplexState_RUNNING = MultiplexState' "RUNNING"

pattern MultiplexState_STARTING :: MultiplexState
pattern MultiplexState_STARTING = MultiplexState' "STARTING"

pattern MultiplexState_STOPPING :: MultiplexState
pattern MultiplexState_STOPPING = MultiplexState' "STOPPING"

{-# COMPLETE
  MultiplexState_CREATE_FAILED,
  MultiplexState_CREATING,
  MultiplexState_DELETED,
  MultiplexState_DELETING,
  MultiplexState_IDLE,
  MultiplexState_RECOVERING,
  MultiplexState_RUNNING,
  MultiplexState_STARTING,
  MultiplexState_STOPPING,
  MultiplexState'
  #-}
