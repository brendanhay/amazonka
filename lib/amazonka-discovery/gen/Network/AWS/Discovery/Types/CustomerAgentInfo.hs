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
import qualified Network.AWS.Prelude as Lude

-- | Inventory data for installed discovery agents.
--
-- /See:/ 'mkCustomerAgentInfo' smart constructor.
data CustomerAgentInfo = CustomerAgentInfo'
  { activeAgents ::
      Lude.Int,
    healthyAgents :: Lude.Int,
    blackListedAgents :: Lude.Int,
    shutdownAgents :: Lude.Int,
    unhealthyAgents :: Lude.Int,
    totalAgents :: Lude.Int,
    unknownAgents :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CustomerAgentInfo' with the minimum fields required to make a request.
--
-- * 'activeAgents' - Number of active discovery agents.
-- * 'blackListedAgents' - Number of blacklisted discovery agents.
-- * 'healthyAgents' - Number of healthy discovery agents
-- * 'shutdownAgents' - Number of discovery agents with status SHUTDOWN.
-- * 'totalAgents' - Total number of discovery agents.
-- * 'unhealthyAgents' - Number of unhealthy discovery agents.
-- * 'unknownAgents' - Number of unknown discovery agents.
mkCustomerAgentInfo ::
  -- | 'activeAgents'
  Lude.Int ->
  -- | 'healthyAgents'
  Lude.Int ->
  -- | 'blackListedAgents'
  Lude.Int ->
  -- | 'shutdownAgents'
  Lude.Int ->
  -- | 'unhealthyAgents'
  Lude.Int ->
  -- | 'totalAgents'
  Lude.Int ->
  -- | 'unknownAgents'
  Lude.Int ->
  CustomerAgentInfo
mkCustomerAgentInfo
  pActiveAgents_
  pHealthyAgents_
  pBlackListedAgents_
  pShutdownAgents_
  pUnhealthyAgents_
  pTotalAgents_
  pUnknownAgents_ =
    CustomerAgentInfo'
      { activeAgents = pActiveAgents_,
        healthyAgents = pHealthyAgents_,
        blackListedAgents = pBlackListedAgents_,
        shutdownAgents = pShutdownAgents_,
        unhealthyAgents = pUnhealthyAgents_,
        totalAgents = pTotalAgents_,
        unknownAgents = pUnknownAgents_
      }

-- | Number of active discovery agents.
--
-- /Note:/ Consider using 'activeAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caiActiveAgents :: Lens.Lens' CustomerAgentInfo Lude.Int
caiActiveAgents = Lens.lens (activeAgents :: CustomerAgentInfo -> Lude.Int) (\s a -> s {activeAgents = a} :: CustomerAgentInfo)
{-# DEPRECATED caiActiveAgents "Use generic-lens or generic-optics with 'activeAgents' instead." #-}

-- | Number of healthy discovery agents
--
-- /Note:/ Consider using 'healthyAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caiHealthyAgents :: Lens.Lens' CustomerAgentInfo Lude.Int
caiHealthyAgents = Lens.lens (healthyAgents :: CustomerAgentInfo -> Lude.Int) (\s a -> s {healthyAgents = a} :: CustomerAgentInfo)
{-# DEPRECATED caiHealthyAgents "Use generic-lens or generic-optics with 'healthyAgents' instead." #-}

-- | Number of blacklisted discovery agents.
--
-- /Note:/ Consider using 'blackListedAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caiBlackListedAgents :: Lens.Lens' CustomerAgentInfo Lude.Int
caiBlackListedAgents = Lens.lens (blackListedAgents :: CustomerAgentInfo -> Lude.Int) (\s a -> s {blackListedAgents = a} :: CustomerAgentInfo)
{-# DEPRECATED caiBlackListedAgents "Use generic-lens or generic-optics with 'blackListedAgents' instead." #-}

-- | Number of discovery agents with status SHUTDOWN.
--
-- /Note:/ Consider using 'shutdownAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caiShutdownAgents :: Lens.Lens' CustomerAgentInfo Lude.Int
caiShutdownAgents = Lens.lens (shutdownAgents :: CustomerAgentInfo -> Lude.Int) (\s a -> s {shutdownAgents = a} :: CustomerAgentInfo)
{-# DEPRECATED caiShutdownAgents "Use generic-lens or generic-optics with 'shutdownAgents' instead." #-}

-- | Number of unhealthy discovery agents.
--
-- /Note:/ Consider using 'unhealthyAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caiUnhealthyAgents :: Lens.Lens' CustomerAgentInfo Lude.Int
caiUnhealthyAgents = Lens.lens (unhealthyAgents :: CustomerAgentInfo -> Lude.Int) (\s a -> s {unhealthyAgents = a} :: CustomerAgentInfo)
{-# DEPRECATED caiUnhealthyAgents "Use generic-lens or generic-optics with 'unhealthyAgents' instead." #-}

-- | Total number of discovery agents.
--
-- /Note:/ Consider using 'totalAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caiTotalAgents :: Lens.Lens' CustomerAgentInfo Lude.Int
caiTotalAgents = Lens.lens (totalAgents :: CustomerAgentInfo -> Lude.Int) (\s a -> s {totalAgents = a} :: CustomerAgentInfo)
{-# DEPRECATED caiTotalAgents "Use generic-lens or generic-optics with 'totalAgents' instead." #-}

-- | Number of unknown discovery agents.
--
-- /Note:/ Consider using 'unknownAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caiUnknownAgents :: Lens.Lens' CustomerAgentInfo Lude.Int
caiUnknownAgents = Lens.lens (unknownAgents :: CustomerAgentInfo -> Lude.Int) (\s a -> s {unknownAgents = a} :: CustomerAgentInfo)
{-# DEPRECATED caiUnknownAgents "Use generic-lens or generic-optics with 'unknownAgents' instead." #-}

instance Lude.FromJSON CustomerAgentInfo where
  parseJSON =
    Lude.withObject
      "CustomerAgentInfo"
      ( \x ->
          CustomerAgentInfo'
            Lude.<$> (x Lude..: "activeAgents")
            Lude.<*> (x Lude..: "healthyAgents")
            Lude.<*> (x Lude..: "blackListedAgents")
            Lude.<*> (x Lude..: "shutdownAgents")
            Lude.<*> (x Lude..: "unhealthyAgents")
            Lude.<*> (x Lude..: "totalAgents")
            Lude.<*> (x Lude..: "unknownAgents")
      )
