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
-- Module      : Network.AWS.MediaLive.Types.ChannelState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ChannelState
  ( ChannelState
      ( ..,
        ChannelState_CREATE_FAILED,
        ChannelState_CREATING,
        ChannelState_DELETED,
        ChannelState_DELETING,
        ChannelState_IDLE,
        ChannelState_RECOVERING,
        ChannelState_RUNNING,
        ChannelState_STARTING,
        ChannelState_STOPPING,
        ChannelState_UPDATE_FAILED,
        ChannelState_UPDATING
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Placeholder documentation for ChannelState
newtype ChannelState = ChannelState'
  { fromChannelState ::
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

pattern ChannelState_CREATE_FAILED :: ChannelState
pattern ChannelState_CREATE_FAILED = ChannelState' "CREATE_FAILED"

pattern ChannelState_CREATING :: ChannelState
pattern ChannelState_CREATING = ChannelState' "CREATING"

pattern ChannelState_DELETED :: ChannelState
pattern ChannelState_DELETED = ChannelState' "DELETED"

pattern ChannelState_DELETING :: ChannelState
pattern ChannelState_DELETING = ChannelState' "DELETING"

pattern ChannelState_IDLE :: ChannelState
pattern ChannelState_IDLE = ChannelState' "IDLE"

pattern ChannelState_RECOVERING :: ChannelState
pattern ChannelState_RECOVERING = ChannelState' "RECOVERING"

pattern ChannelState_RUNNING :: ChannelState
pattern ChannelState_RUNNING = ChannelState' "RUNNING"

pattern ChannelState_STARTING :: ChannelState
pattern ChannelState_STARTING = ChannelState' "STARTING"

pattern ChannelState_STOPPING :: ChannelState
pattern ChannelState_STOPPING = ChannelState' "STOPPING"

pattern ChannelState_UPDATE_FAILED :: ChannelState
pattern ChannelState_UPDATE_FAILED = ChannelState' "UPDATE_FAILED"

pattern ChannelState_UPDATING :: ChannelState
pattern ChannelState_UPDATING = ChannelState' "UPDATING"

{-# COMPLETE
  ChannelState_CREATE_FAILED,
  ChannelState_CREATING,
  ChannelState_DELETED,
  ChannelState_DELETING,
  ChannelState_IDLE,
  ChannelState_RECOVERING,
  ChannelState_RUNNING,
  ChannelState_STARTING,
  ChannelState_STOPPING,
  ChannelState_UPDATE_FAILED,
  ChannelState_UPDATING,
  ChannelState'
  #-}
