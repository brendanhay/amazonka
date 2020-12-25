{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AgentHealthCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AgentHealthCode
  ( AgentHealthCode
      ( AgentHealthCode',
        AgentHealthCodeIdle,
        AgentHealthCodeRunning,
        AgentHealthCodeShutdown,
        AgentHealthCodeUnhealthy,
        AgentHealthCodeThrottled,
        AgentHealthCodeUnknown,
        fromAgentHealthCode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype AgentHealthCode = AgentHealthCode'
  { fromAgentHealthCode ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern AgentHealthCodeIdle :: AgentHealthCode
pattern AgentHealthCodeIdle = AgentHealthCode' "IDLE"

pattern AgentHealthCodeRunning :: AgentHealthCode
pattern AgentHealthCodeRunning = AgentHealthCode' "RUNNING"

pattern AgentHealthCodeShutdown :: AgentHealthCode
pattern AgentHealthCodeShutdown = AgentHealthCode' "SHUTDOWN"

pattern AgentHealthCodeUnhealthy :: AgentHealthCode
pattern AgentHealthCodeUnhealthy = AgentHealthCode' "UNHEALTHY"

pattern AgentHealthCodeThrottled :: AgentHealthCode
pattern AgentHealthCodeThrottled = AgentHealthCode' "THROTTLED"

pattern AgentHealthCodeUnknown :: AgentHealthCode
pattern AgentHealthCodeUnknown = AgentHealthCode' "UNKNOWN"

{-# COMPLETE
  AgentHealthCodeIdle,
  AgentHealthCodeRunning,
  AgentHealthCodeShutdown,
  AgentHealthCodeUnhealthy,
  AgentHealthCodeThrottled,
  AgentHealthCodeUnknown,
  AgentHealthCode'
  #-}
