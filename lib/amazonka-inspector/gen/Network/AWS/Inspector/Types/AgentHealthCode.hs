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
        Idle,
        Running,
        Shutdown,
        Throttled,
        Unhealthy,
        Unknown
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AgentHealthCode = AgentHealthCode' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Idle :: AgentHealthCode
pattern Idle = AgentHealthCode' "IDLE"

pattern Running :: AgentHealthCode
pattern Running = AgentHealthCode' "RUNNING"

pattern Shutdown :: AgentHealthCode
pattern Shutdown = AgentHealthCode' "SHUTDOWN"

pattern Throttled :: AgentHealthCode
pattern Throttled = AgentHealthCode' "THROTTLED"

pattern Unhealthy :: AgentHealthCode
pattern Unhealthy = AgentHealthCode' "UNHEALTHY"

pattern Unknown :: AgentHealthCode
pattern Unknown = AgentHealthCode' "UNKNOWN"

{-# COMPLETE
  Idle,
  Running,
  Shutdown,
  Throttled,
  Unhealthy,
  Unknown,
  AgentHealthCode'
  #-}
