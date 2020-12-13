{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.CustomerConnectorInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.CustomerConnectorInfo
  ( CustomerConnectorInfo (..),

    -- * Smart constructor
    mkCustomerConnectorInfo,

    -- * Lenses
    cciBlackListedConnectors,
    cciHealthyConnectors,
    cciUnknownConnectors,
    cciShutdownConnectors,
    cciActiveConnectors,
    cciTotalConnectors,
    cciUnhealthyConnectors,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Inventory data for installed discovery connectors.
--
-- /See:/ 'mkCustomerConnectorInfo' smart constructor.
data CustomerConnectorInfo = CustomerConnectorInfo'
  { -- | Number of blacklisted discovery connectors.
    blackListedConnectors :: Lude.Int,
    -- | Number of healthy discovery connectors.
    healthyConnectors :: Lude.Int,
    -- | Number of unknown discovery connectors.
    unknownConnectors :: Lude.Int,
    -- | Number of discovery connectors with status SHUTDOWN,
    shutdownConnectors :: Lude.Int,
    -- | Number of active discovery connectors.
    activeConnectors :: Lude.Int,
    -- | Total number of discovery connectors.
    totalConnectors :: Lude.Int,
    -- | Number of unhealthy discovery connectors.
    unhealthyConnectors :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CustomerConnectorInfo' with the minimum fields required to make a request.
--
-- * 'blackListedConnectors' - Number of blacklisted discovery connectors.
-- * 'healthyConnectors' - Number of healthy discovery connectors.
-- * 'unknownConnectors' - Number of unknown discovery connectors.
-- * 'shutdownConnectors' - Number of discovery connectors with status SHUTDOWN,
-- * 'activeConnectors' - Number of active discovery connectors.
-- * 'totalConnectors' - Total number of discovery connectors.
-- * 'unhealthyConnectors' - Number of unhealthy discovery connectors.
mkCustomerConnectorInfo ::
  -- | 'blackListedConnectors'
  Lude.Int ->
  -- | 'healthyConnectors'
  Lude.Int ->
  -- | 'unknownConnectors'
  Lude.Int ->
  -- | 'shutdownConnectors'
  Lude.Int ->
  -- | 'activeConnectors'
  Lude.Int ->
  -- | 'totalConnectors'
  Lude.Int ->
  -- | 'unhealthyConnectors'
  Lude.Int ->
  CustomerConnectorInfo
mkCustomerConnectorInfo
  pBlackListedConnectors_
  pHealthyConnectors_
  pUnknownConnectors_
  pShutdownConnectors_
  pActiveConnectors_
  pTotalConnectors_
  pUnhealthyConnectors_ =
    CustomerConnectorInfo'
      { blackListedConnectors =
          pBlackListedConnectors_,
        healthyConnectors = pHealthyConnectors_,
        unknownConnectors = pUnknownConnectors_,
        shutdownConnectors = pShutdownConnectors_,
        activeConnectors = pActiveConnectors_,
        totalConnectors = pTotalConnectors_,
        unhealthyConnectors = pUnhealthyConnectors_
      }

-- | Number of blacklisted discovery connectors.
--
-- /Note:/ Consider using 'blackListedConnectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cciBlackListedConnectors :: Lens.Lens' CustomerConnectorInfo Lude.Int
cciBlackListedConnectors = Lens.lens (blackListedConnectors :: CustomerConnectorInfo -> Lude.Int) (\s a -> s {blackListedConnectors = a} :: CustomerConnectorInfo)
{-# DEPRECATED cciBlackListedConnectors "Use generic-lens or generic-optics with 'blackListedConnectors' instead." #-}

-- | Number of healthy discovery connectors.
--
-- /Note:/ Consider using 'healthyConnectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cciHealthyConnectors :: Lens.Lens' CustomerConnectorInfo Lude.Int
cciHealthyConnectors = Lens.lens (healthyConnectors :: CustomerConnectorInfo -> Lude.Int) (\s a -> s {healthyConnectors = a} :: CustomerConnectorInfo)
{-# DEPRECATED cciHealthyConnectors "Use generic-lens or generic-optics with 'healthyConnectors' instead." #-}

-- | Number of unknown discovery connectors.
--
-- /Note:/ Consider using 'unknownConnectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cciUnknownConnectors :: Lens.Lens' CustomerConnectorInfo Lude.Int
cciUnknownConnectors = Lens.lens (unknownConnectors :: CustomerConnectorInfo -> Lude.Int) (\s a -> s {unknownConnectors = a} :: CustomerConnectorInfo)
{-# DEPRECATED cciUnknownConnectors "Use generic-lens or generic-optics with 'unknownConnectors' instead." #-}

-- | Number of discovery connectors with status SHUTDOWN,
--
-- /Note:/ Consider using 'shutdownConnectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cciShutdownConnectors :: Lens.Lens' CustomerConnectorInfo Lude.Int
cciShutdownConnectors = Lens.lens (shutdownConnectors :: CustomerConnectorInfo -> Lude.Int) (\s a -> s {shutdownConnectors = a} :: CustomerConnectorInfo)
{-# DEPRECATED cciShutdownConnectors "Use generic-lens or generic-optics with 'shutdownConnectors' instead." #-}

-- | Number of active discovery connectors.
--
-- /Note:/ Consider using 'activeConnectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cciActiveConnectors :: Lens.Lens' CustomerConnectorInfo Lude.Int
cciActiveConnectors = Lens.lens (activeConnectors :: CustomerConnectorInfo -> Lude.Int) (\s a -> s {activeConnectors = a} :: CustomerConnectorInfo)
{-# DEPRECATED cciActiveConnectors "Use generic-lens or generic-optics with 'activeConnectors' instead." #-}

-- | Total number of discovery connectors.
--
-- /Note:/ Consider using 'totalConnectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cciTotalConnectors :: Lens.Lens' CustomerConnectorInfo Lude.Int
cciTotalConnectors = Lens.lens (totalConnectors :: CustomerConnectorInfo -> Lude.Int) (\s a -> s {totalConnectors = a} :: CustomerConnectorInfo)
{-# DEPRECATED cciTotalConnectors "Use generic-lens or generic-optics with 'totalConnectors' instead." #-}

-- | Number of unhealthy discovery connectors.
--
-- /Note:/ Consider using 'unhealthyConnectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cciUnhealthyConnectors :: Lens.Lens' CustomerConnectorInfo Lude.Int
cciUnhealthyConnectors = Lens.lens (unhealthyConnectors :: CustomerConnectorInfo -> Lude.Int) (\s a -> s {unhealthyConnectors = a} :: CustomerConnectorInfo)
{-# DEPRECATED cciUnhealthyConnectors "Use generic-lens or generic-optics with 'unhealthyConnectors' instead." #-}

instance Lude.FromJSON CustomerConnectorInfo where
  parseJSON =
    Lude.withObject
      "CustomerConnectorInfo"
      ( \x ->
          CustomerConnectorInfo'
            Lude.<$> (x Lude..: "blackListedConnectors")
            Lude.<*> (x Lude..: "healthyConnectors")
            Lude.<*> (x Lude..: "unknownConnectors")
            Lude.<*> (x Lude..: "shutdownConnectors")
            Lude.<*> (x Lude..: "activeConnectors")
            Lude.<*> (x Lude..: "totalConnectors")
            Lude.<*> (x Lude..: "unhealthyConnectors")
      )
