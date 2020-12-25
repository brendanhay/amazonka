{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AgentFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AgentFilter
  ( AgentFilter (..),

    -- * Smart constructor
    mkAgentFilter,

    -- * Lenses
    afAgentHealths,
    afAgentHealthCodes,
  )
where

import qualified Network.AWS.Inspector.Types.AgentHealth as Types
import qualified Network.AWS.Inspector.Types.AgentHealthCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an Amazon Inspector agent. This data type is used as a request parameter in the 'ListAssessmentRunAgents' action.
--
-- /See:/ 'mkAgentFilter' smart constructor.
data AgentFilter = AgentFilter'
  { -- | The current health state of the agent. Values can be set to __HEALTHY__ or __UNHEALTHY__ .
    agentHealths :: [Types.AgentHealth],
    -- | The detailed health state of the agent. Values can be set to __IDLE__ , __RUNNING__ , __SHUTDOWN__ , __UNHEALTHY__ , __THROTTLED__ , and __UNKNOWN__ .
    agentHealthCodes :: [Types.AgentHealthCode]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AgentFilter' value with any optional fields omitted.
mkAgentFilter ::
  AgentFilter
mkAgentFilter =
  AgentFilter'
    { agentHealths = Core.mempty,
      agentHealthCodes = Core.mempty
    }

-- | The current health state of the agent. Values can be set to __HEALTHY__ or __UNHEALTHY__ .
--
-- /Note:/ Consider using 'agentHealths' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afAgentHealths :: Lens.Lens' AgentFilter [Types.AgentHealth]
afAgentHealths = Lens.field @"agentHealths"
{-# DEPRECATED afAgentHealths "Use generic-lens or generic-optics with 'agentHealths' instead." #-}

-- | The detailed health state of the agent. Values can be set to __IDLE__ , __RUNNING__ , __SHUTDOWN__ , __UNHEALTHY__ , __THROTTLED__ , and __UNKNOWN__ .
--
-- /Note:/ Consider using 'agentHealthCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afAgentHealthCodes :: Lens.Lens' AgentFilter [Types.AgentHealthCode]
afAgentHealthCodes = Lens.field @"agentHealthCodes"
{-# DEPRECATED afAgentHealthCodes "Use generic-lens or generic-optics with 'agentHealthCodes' instead." #-}

instance Core.FromJSON AgentFilter where
  toJSON AgentFilter {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("agentHealths" Core..= agentHealths),
            Core.Just ("agentHealthCodes" Core..= agentHealthCodes)
          ]
      )
