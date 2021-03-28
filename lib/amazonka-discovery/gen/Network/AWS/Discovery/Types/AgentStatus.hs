{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.AgentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Discovery.Types.AgentStatus
  ( AgentStatus
    ( AgentStatus'
    , AgentStatusHealthy
    , AgentStatusUnhealthy
    , AgentStatusRunning
    , AgentStatusUnknown
    , AgentStatusBlacklisted
    , AgentStatusShutdown
    , fromAgentStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype AgentStatus = AgentStatus'{fromAgentStatus :: Core.Text}
                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                        Core.Generic)
                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                          Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                          Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                          Core.FromText, Core.ToByteString, Core.ToQuery,
                                          Core.ToHeader)

pattern AgentStatusHealthy :: AgentStatus
pattern AgentStatusHealthy = AgentStatus' "HEALTHY"

pattern AgentStatusUnhealthy :: AgentStatus
pattern AgentStatusUnhealthy = AgentStatus' "UNHEALTHY"

pattern AgentStatusRunning :: AgentStatus
pattern AgentStatusRunning = AgentStatus' "RUNNING"

pattern AgentStatusUnknown :: AgentStatus
pattern AgentStatusUnknown = AgentStatus' "UNKNOWN"

pattern AgentStatusBlacklisted :: AgentStatus
pattern AgentStatusBlacklisted = AgentStatus' "BLACKLISTED"

pattern AgentStatusShutdown :: AgentStatus
pattern AgentStatusShutdown = AgentStatus' "SHUTDOWN"

{-# COMPLETE 
  AgentStatusHealthy,

  AgentStatusUnhealthy,

  AgentStatusRunning,

  AgentStatusUnknown,

  AgentStatusBlacklisted,

  AgentStatusShutdown,
  AgentStatus'
  #-}
