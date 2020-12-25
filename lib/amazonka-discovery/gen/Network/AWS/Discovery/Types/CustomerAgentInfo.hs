{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.CustomerAgentInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.CustomerAgentInfo
  ( CustomerAgentInfo (..),

    -- * Smart constructor
    mkCustomerAgentInfo,

    -- * Lenses
    caiActiveAgents,
    caiHealthyAgents,
    caiBlackListedAgents,
    caiShutdownAgents,
    caiUnhealthyAgents,
    caiTotalAgents,
    caiUnknownAgents,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Inventory data for installed discovery agents.
--
-- /See:/ 'mkCustomerAgentInfo' smart constructor.
data CustomerAgentInfo = CustomerAgentInfo'
  { -- | Number of active discovery agents.
    activeAgents :: Core.Int,
    -- | Number of healthy discovery agents
    healthyAgents :: Core.Int,
    -- | Number of blacklisted discovery agents.
    blackListedAgents :: Core.Int,
    -- | Number of discovery agents with status SHUTDOWN.
    shutdownAgents :: Core.Int,
    -- | Number of unhealthy discovery agents.
    unhealthyAgents :: Core.Int,
    -- | Total number of discovery agents.
    totalAgents :: Core.Int,
    -- | Number of unknown discovery agents.
    unknownAgents :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CustomerAgentInfo' value with any optional fields omitted.
mkCustomerAgentInfo ::
  -- | 'activeAgents'
  Core.Int ->
  -- | 'healthyAgents'
  Core.Int ->
  -- | 'blackListedAgents'
  Core.Int ->
  -- | 'shutdownAgents'
  Core.Int ->
  -- | 'unhealthyAgents'
  Core.Int ->
  -- | 'totalAgents'
  Core.Int ->
  -- | 'unknownAgents'
  Core.Int ->
  CustomerAgentInfo
mkCustomerAgentInfo
  activeAgents
  healthyAgents
  blackListedAgents
  shutdownAgents
  unhealthyAgents
  totalAgents
  unknownAgents =
    CustomerAgentInfo'
      { activeAgents,
        healthyAgents,
        blackListedAgents,
        shutdownAgents,
        unhealthyAgents,
        totalAgents,
        unknownAgents
      }

-- | Number of active discovery agents.
--
-- /Note:/ Consider using 'activeAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caiActiveAgents :: Lens.Lens' CustomerAgentInfo Core.Int
caiActiveAgents = Lens.field @"activeAgents"
{-# DEPRECATED caiActiveAgents "Use generic-lens or generic-optics with 'activeAgents' instead." #-}

-- | Number of healthy discovery agents
--
-- /Note:/ Consider using 'healthyAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caiHealthyAgents :: Lens.Lens' CustomerAgentInfo Core.Int
caiHealthyAgents = Lens.field @"healthyAgents"
{-# DEPRECATED caiHealthyAgents "Use generic-lens or generic-optics with 'healthyAgents' instead." #-}

-- | Number of blacklisted discovery agents.
--
-- /Note:/ Consider using 'blackListedAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caiBlackListedAgents :: Lens.Lens' CustomerAgentInfo Core.Int
caiBlackListedAgents = Lens.field @"blackListedAgents"
{-# DEPRECATED caiBlackListedAgents "Use generic-lens or generic-optics with 'blackListedAgents' instead." #-}

-- | Number of discovery agents with status SHUTDOWN.
--
-- /Note:/ Consider using 'shutdownAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caiShutdownAgents :: Lens.Lens' CustomerAgentInfo Core.Int
caiShutdownAgents = Lens.field @"shutdownAgents"
{-# DEPRECATED caiShutdownAgents "Use generic-lens or generic-optics with 'shutdownAgents' instead." #-}

-- | Number of unhealthy discovery agents.
--
-- /Note:/ Consider using 'unhealthyAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caiUnhealthyAgents :: Lens.Lens' CustomerAgentInfo Core.Int
caiUnhealthyAgents = Lens.field @"unhealthyAgents"
{-# DEPRECATED caiUnhealthyAgents "Use generic-lens or generic-optics with 'unhealthyAgents' instead." #-}

-- | Total number of discovery agents.
--
-- /Note:/ Consider using 'totalAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caiTotalAgents :: Lens.Lens' CustomerAgentInfo Core.Int
caiTotalAgents = Lens.field @"totalAgents"
{-# DEPRECATED caiTotalAgents "Use generic-lens or generic-optics with 'totalAgents' instead." #-}

-- | Number of unknown discovery agents.
--
-- /Note:/ Consider using 'unknownAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caiUnknownAgents :: Lens.Lens' CustomerAgentInfo Core.Int
caiUnknownAgents = Lens.field @"unknownAgents"
{-# DEPRECATED caiUnknownAgents "Use generic-lens or generic-optics with 'unknownAgents' instead." #-}

instance Core.FromJSON CustomerAgentInfo where
  parseJSON =
    Core.withObject "CustomerAgentInfo" Core.$
      \x ->
        CustomerAgentInfo'
          Core.<$> (x Core..: "activeAgents")
          Core.<*> (x Core..: "healthyAgents")
          Core.<*> (x Core..: "blackListedAgents")
          Core.<*> (x Core..: "shutdownAgents")
          Core.<*> (x Core..: "unhealthyAgents")
          Core.<*> (x Core..: "totalAgents")
          Core.<*> (x Core..: "unknownAgents")
