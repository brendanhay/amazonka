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
    caiUnhealthyAgents,
    caiHealthyAgents,
    caiUnknownAgents,
    caiShutdownAgents,
    caiActiveAgents,
    caiTotalAgents,
    caiBlackListedAgents,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Inventory data for installed discovery agents.
--
-- /See:/ 'mkCustomerAgentInfo' smart constructor.
data CustomerAgentInfo = CustomerAgentInfo'
  { -- | Number of unhealthy discovery agents.
    unhealthyAgents :: Lude.Int,
    -- | Number of healthy discovery agents
    healthyAgents :: Lude.Int,
    -- | Number of unknown discovery agents.
    unknownAgents :: Lude.Int,
    -- | Number of discovery agents with status SHUTDOWN.
    shutdownAgents :: Lude.Int,
    -- | Number of active discovery agents.
    activeAgents :: Lude.Int,
    -- | Total number of discovery agents.
    totalAgents :: Lude.Int,
    -- | Number of blacklisted discovery agents.
    blackListedAgents :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CustomerAgentInfo' with the minimum fields required to make a request.
--
-- * 'unhealthyAgents' - Number of unhealthy discovery agents.
-- * 'healthyAgents' - Number of healthy discovery agents
-- * 'unknownAgents' - Number of unknown discovery agents.
-- * 'shutdownAgents' - Number of discovery agents with status SHUTDOWN.
-- * 'activeAgents' - Number of active discovery agents.
-- * 'totalAgents' - Total number of discovery agents.
-- * 'blackListedAgents' - Number of blacklisted discovery agents.
mkCustomerAgentInfo ::
  -- | 'unhealthyAgents'
  Lude.Int ->
  -- | 'healthyAgents'
  Lude.Int ->
  -- | 'unknownAgents'
  Lude.Int ->
  -- | 'shutdownAgents'
  Lude.Int ->
  -- | 'activeAgents'
  Lude.Int ->
  -- | 'totalAgents'
  Lude.Int ->
  -- | 'blackListedAgents'
  Lude.Int ->
  CustomerAgentInfo
mkCustomerAgentInfo
  pUnhealthyAgents_
  pHealthyAgents_
  pUnknownAgents_
  pShutdownAgents_
  pActiveAgents_
  pTotalAgents_
  pBlackListedAgents_ =
    CustomerAgentInfo'
      { unhealthyAgents = pUnhealthyAgents_,
        healthyAgents = pHealthyAgents_,
        unknownAgents = pUnknownAgents_,
        shutdownAgents = pShutdownAgents_,
        activeAgents = pActiveAgents_,
        totalAgents = pTotalAgents_,
        blackListedAgents = pBlackListedAgents_
      }

-- | Number of unhealthy discovery agents.
--
-- /Note:/ Consider using 'unhealthyAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caiUnhealthyAgents :: Lens.Lens' CustomerAgentInfo Lude.Int
caiUnhealthyAgents = Lens.lens (unhealthyAgents :: CustomerAgentInfo -> Lude.Int) (\s a -> s {unhealthyAgents = a} :: CustomerAgentInfo)
{-# DEPRECATED caiUnhealthyAgents "Use generic-lens or generic-optics with 'unhealthyAgents' instead." #-}

-- | Number of healthy discovery agents
--
-- /Note:/ Consider using 'healthyAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caiHealthyAgents :: Lens.Lens' CustomerAgentInfo Lude.Int
caiHealthyAgents = Lens.lens (healthyAgents :: CustomerAgentInfo -> Lude.Int) (\s a -> s {healthyAgents = a} :: CustomerAgentInfo)
{-# DEPRECATED caiHealthyAgents "Use generic-lens or generic-optics with 'healthyAgents' instead." #-}

-- | Number of unknown discovery agents.
--
-- /Note:/ Consider using 'unknownAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caiUnknownAgents :: Lens.Lens' CustomerAgentInfo Lude.Int
caiUnknownAgents = Lens.lens (unknownAgents :: CustomerAgentInfo -> Lude.Int) (\s a -> s {unknownAgents = a} :: CustomerAgentInfo)
{-# DEPRECATED caiUnknownAgents "Use generic-lens or generic-optics with 'unknownAgents' instead." #-}

-- | Number of discovery agents with status SHUTDOWN.
--
-- /Note:/ Consider using 'shutdownAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caiShutdownAgents :: Lens.Lens' CustomerAgentInfo Lude.Int
caiShutdownAgents = Lens.lens (shutdownAgents :: CustomerAgentInfo -> Lude.Int) (\s a -> s {shutdownAgents = a} :: CustomerAgentInfo)
{-# DEPRECATED caiShutdownAgents "Use generic-lens or generic-optics with 'shutdownAgents' instead." #-}

-- | Number of active discovery agents.
--
-- /Note:/ Consider using 'activeAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caiActiveAgents :: Lens.Lens' CustomerAgentInfo Lude.Int
caiActiveAgents = Lens.lens (activeAgents :: CustomerAgentInfo -> Lude.Int) (\s a -> s {activeAgents = a} :: CustomerAgentInfo)
{-# DEPRECATED caiActiveAgents "Use generic-lens or generic-optics with 'activeAgents' instead." #-}

-- | Total number of discovery agents.
--
-- /Note:/ Consider using 'totalAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caiTotalAgents :: Lens.Lens' CustomerAgentInfo Lude.Int
caiTotalAgents = Lens.lens (totalAgents :: CustomerAgentInfo -> Lude.Int) (\s a -> s {totalAgents = a} :: CustomerAgentInfo)
{-# DEPRECATED caiTotalAgents "Use generic-lens or generic-optics with 'totalAgents' instead." #-}

-- | Number of blacklisted discovery agents.
--
-- /Note:/ Consider using 'blackListedAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caiBlackListedAgents :: Lens.Lens' CustomerAgentInfo Lude.Int
caiBlackListedAgents = Lens.lens (blackListedAgents :: CustomerAgentInfo -> Lude.Int) (\s a -> s {blackListedAgents = a} :: CustomerAgentInfo)
{-# DEPRECATED caiBlackListedAgents "Use generic-lens or generic-optics with 'blackListedAgents' instead." #-}

instance Lude.FromJSON CustomerAgentInfo where
  parseJSON =
    Lude.withObject
      "CustomerAgentInfo"
      ( \x ->
          CustomerAgentInfo'
            Lude.<$> (x Lude..: "unhealthyAgents")
            Lude.<*> (x Lude..: "healthyAgents")
            Lude.<*> (x Lude..: "unknownAgents")
            Lude.<*> (x Lude..: "shutdownAgents")
            Lude.<*> (x Lude..: "activeAgents")
            Lude.<*> (x Lude..: "totalAgents")
            Lude.<*> (x Lude..: "blackListedAgents")
      )
