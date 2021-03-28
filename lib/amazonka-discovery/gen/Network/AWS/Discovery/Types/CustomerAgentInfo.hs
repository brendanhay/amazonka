{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.CustomerAgentInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Discovery.Types.CustomerAgentInfo
  ( CustomerAgentInfo (..)
  -- * Smart constructor
  , mkCustomerAgentInfo
  -- * Lenses
  , caiActiveAgents
  , caiHealthyAgents
  , caiBlackListedAgents
  , caiShutdownAgents
  , caiUnhealthyAgents
  , caiTotalAgents
  , caiUnknownAgents
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Inventory data for installed discovery agents.
--
-- /See:/ 'mkCustomerAgentInfo' smart constructor.
data CustomerAgentInfo = CustomerAgentInfo'
  { activeAgents :: Core.Int
    -- ^ Number of active discovery agents.
  , healthyAgents :: Core.Int
    -- ^ Number of healthy discovery agents
  , blackListedAgents :: Core.Int
    -- ^ Number of blacklisted discovery agents.
  , shutdownAgents :: Core.Int
    -- ^ Number of discovery agents with status SHUTDOWN.
  , unhealthyAgents :: Core.Int
    -- ^ Number of unhealthy discovery agents.
  , totalAgents :: Core.Int
    -- ^ Total number of discovery agents.
  , unknownAgents :: Core.Int
    -- ^ Number of unknown discovery agents.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CustomerAgentInfo' value with any optional fields omitted.
mkCustomerAgentInfo
    :: Core.Int -- ^ 'activeAgents'
    -> Core.Int -- ^ 'healthyAgents'
    -> Core.Int -- ^ 'blackListedAgents'
    -> Core.Int -- ^ 'shutdownAgents'
    -> Core.Int -- ^ 'unhealthyAgents'
    -> Core.Int -- ^ 'totalAgents'
    -> Core.Int -- ^ 'unknownAgents'
    -> CustomerAgentInfo
mkCustomerAgentInfo activeAgents healthyAgents blackListedAgents
  shutdownAgents unhealthyAgents totalAgents unknownAgents
  = CustomerAgentInfo'{activeAgents, healthyAgents,
                       blackListedAgents, shutdownAgents, unhealthyAgents, totalAgents,
                       unknownAgents}

-- | Number of active discovery agents.
--
-- /Note:/ Consider using 'activeAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caiActiveAgents :: Lens.Lens' CustomerAgentInfo Core.Int
caiActiveAgents = Lens.field @"activeAgents"
{-# INLINEABLE caiActiveAgents #-}
{-# DEPRECATED activeAgents "Use generic-lens or generic-optics with 'activeAgents' instead"  #-}

-- | Number of healthy discovery agents
--
-- /Note:/ Consider using 'healthyAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caiHealthyAgents :: Lens.Lens' CustomerAgentInfo Core.Int
caiHealthyAgents = Lens.field @"healthyAgents"
{-# INLINEABLE caiHealthyAgents #-}
{-# DEPRECATED healthyAgents "Use generic-lens or generic-optics with 'healthyAgents' instead"  #-}

-- | Number of blacklisted discovery agents.
--
-- /Note:/ Consider using 'blackListedAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caiBlackListedAgents :: Lens.Lens' CustomerAgentInfo Core.Int
caiBlackListedAgents = Lens.field @"blackListedAgents"
{-# INLINEABLE caiBlackListedAgents #-}
{-# DEPRECATED blackListedAgents "Use generic-lens or generic-optics with 'blackListedAgents' instead"  #-}

-- | Number of discovery agents with status SHUTDOWN.
--
-- /Note:/ Consider using 'shutdownAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caiShutdownAgents :: Lens.Lens' CustomerAgentInfo Core.Int
caiShutdownAgents = Lens.field @"shutdownAgents"
{-# INLINEABLE caiShutdownAgents #-}
{-# DEPRECATED shutdownAgents "Use generic-lens or generic-optics with 'shutdownAgents' instead"  #-}

-- | Number of unhealthy discovery agents.
--
-- /Note:/ Consider using 'unhealthyAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caiUnhealthyAgents :: Lens.Lens' CustomerAgentInfo Core.Int
caiUnhealthyAgents = Lens.field @"unhealthyAgents"
{-# INLINEABLE caiUnhealthyAgents #-}
{-# DEPRECATED unhealthyAgents "Use generic-lens or generic-optics with 'unhealthyAgents' instead"  #-}

-- | Total number of discovery agents.
--
-- /Note:/ Consider using 'totalAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caiTotalAgents :: Lens.Lens' CustomerAgentInfo Core.Int
caiTotalAgents = Lens.field @"totalAgents"
{-# INLINEABLE caiTotalAgents #-}
{-# DEPRECATED totalAgents "Use generic-lens or generic-optics with 'totalAgents' instead"  #-}

-- | Number of unknown discovery agents.
--
-- /Note:/ Consider using 'unknownAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caiUnknownAgents :: Lens.Lens' CustomerAgentInfo Core.Int
caiUnknownAgents = Lens.field @"unknownAgents"
{-# INLINEABLE caiUnknownAgents #-}
{-# DEPRECATED unknownAgents "Use generic-lens or generic-optics with 'unknownAgents' instead"  #-}

instance Core.FromJSON CustomerAgentInfo where
        parseJSON
          = Core.withObject "CustomerAgentInfo" Core.$
              \ x ->
                CustomerAgentInfo' Core.<$>
                  (x Core..: "activeAgents") Core.<*> x Core..: "healthyAgents"
                    Core.<*> x Core..: "blackListedAgents"
                    Core.<*> x Core..: "shutdownAgents"
                    Core.<*> x Core..: "unhealthyAgents"
                    Core.<*> x Core..: "totalAgents"
                    Core.<*> x Core..: "unknownAgents"
