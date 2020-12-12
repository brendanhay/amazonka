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

import Network.AWS.Inspector.Types.AgentHealth
import Network.AWS.Inspector.Types.AgentHealthCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about an Amazon Inspector agent. This data type is used as a request parameter in the 'ListAssessmentRunAgents' action.
--
-- /See:/ 'mkAgentFilter' smart constructor.
data AgentFilter = AgentFilter'
  { agentHealths :: [AgentHealth],
    agentHealthCodes :: [AgentHealthCode]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AgentFilter' with the minimum fields required to make a request.
--
-- * 'agentHealthCodes' - The detailed health state of the agent. Values can be set to __IDLE__ , __RUNNING__ , __SHUTDOWN__ , __UNHEALTHY__ , __THROTTLED__ , and __UNKNOWN__ .
-- * 'agentHealths' - The current health state of the agent. Values can be set to __HEALTHY__ or __UNHEALTHY__ .
mkAgentFilter ::
  AgentFilter
mkAgentFilter =
  AgentFilter'
    { agentHealths = Lude.mempty,
      agentHealthCodes = Lude.mempty
    }

-- | The current health state of the agent. Values can be set to __HEALTHY__ or __UNHEALTHY__ .
--
-- /Note:/ Consider using 'agentHealths' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afAgentHealths :: Lens.Lens' AgentFilter [AgentHealth]
afAgentHealths = Lens.lens (agentHealths :: AgentFilter -> [AgentHealth]) (\s a -> s {agentHealths = a} :: AgentFilter)
{-# DEPRECATED afAgentHealths "Use generic-lens or generic-optics with 'agentHealths' instead." #-}

-- | The detailed health state of the agent. Values can be set to __IDLE__ , __RUNNING__ , __SHUTDOWN__ , __UNHEALTHY__ , __THROTTLED__ , and __UNKNOWN__ .
--
-- /Note:/ Consider using 'agentHealthCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afAgentHealthCodes :: Lens.Lens' AgentFilter [AgentHealthCode]
afAgentHealthCodes = Lens.lens (agentHealthCodes :: AgentFilter -> [AgentHealthCode]) (\s a -> s {agentHealthCodes = a} :: AgentFilter)
{-# DEPRECATED afAgentHealthCodes "Use generic-lens or generic-optics with 'agentHealthCodes' instead." #-}

instance Lude.ToJSON AgentFilter where
  toJSON AgentFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("agentHealths" Lude..= agentHealths),
            Lude.Just ("agentHealthCodes" Lude..= agentHealthCodes)
          ]
      )
