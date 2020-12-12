{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.AgentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.AgentStatus
  ( AgentStatus
      ( AgentStatus',
        Blacklisted,
        Healthy,
        Running,
        Shutdown,
        Unhealthy,
        Unknown
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AgentStatus = AgentStatus' Lude.Text
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

pattern Blacklisted :: AgentStatus
pattern Blacklisted = AgentStatus' "BLACKLISTED"

pattern Healthy :: AgentStatus
pattern Healthy = AgentStatus' "HEALTHY"

pattern Running :: AgentStatus
pattern Running = AgentStatus' "RUNNING"

pattern Shutdown :: AgentStatus
pattern Shutdown = AgentStatus' "SHUTDOWN"

pattern Unhealthy :: AgentStatus
pattern Unhealthy = AgentStatus' "UNHEALTHY"

pattern Unknown :: AgentStatus
pattern Unknown = AgentStatus' "UNKNOWN"

{-# COMPLETE
  Blacklisted,
  Healthy,
  Running,
  Shutdown,
  Unhealthy,
  Unknown,
  AgentStatus'
  #-}
