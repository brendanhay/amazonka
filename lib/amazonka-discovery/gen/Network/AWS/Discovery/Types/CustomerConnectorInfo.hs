{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.CustomerConnectorInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Discovery.Types.CustomerConnectorInfo
  ( CustomerConnectorInfo (..)
  -- * Smart constructor
  , mkCustomerConnectorInfo
  -- * Lenses
  , cciActiveConnectors
  , cciHealthyConnectors
  , cciBlackListedConnectors
  , cciShutdownConnectors
  , cciUnhealthyConnectors
  , cciTotalConnectors
  , cciUnknownConnectors
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Inventory data for installed discovery connectors.
--
-- /See:/ 'mkCustomerConnectorInfo' smart constructor.
data CustomerConnectorInfo = CustomerConnectorInfo'
  { activeConnectors :: Core.Int
    -- ^ Number of active discovery connectors.
  , healthyConnectors :: Core.Int
    -- ^ Number of healthy discovery connectors.
  , blackListedConnectors :: Core.Int
    -- ^ Number of blacklisted discovery connectors.
  , shutdownConnectors :: Core.Int
    -- ^ Number of discovery connectors with status SHUTDOWN,
  , unhealthyConnectors :: Core.Int
    -- ^ Number of unhealthy discovery connectors.
  , totalConnectors :: Core.Int
    -- ^ Total number of discovery connectors.
  , unknownConnectors :: Core.Int
    -- ^ Number of unknown discovery connectors.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CustomerConnectorInfo' value with any optional fields omitted.
mkCustomerConnectorInfo
    :: Core.Int -- ^ 'activeConnectors'
    -> Core.Int -- ^ 'healthyConnectors'
    -> Core.Int -- ^ 'blackListedConnectors'
    -> Core.Int -- ^ 'shutdownConnectors'
    -> Core.Int -- ^ 'unhealthyConnectors'
    -> Core.Int -- ^ 'totalConnectors'
    -> Core.Int -- ^ 'unknownConnectors'
    -> CustomerConnectorInfo
mkCustomerConnectorInfo activeConnectors healthyConnectors
  blackListedConnectors shutdownConnectors unhealthyConnectors
  totalConnectors unknownConnectors
  = CustomerConnectorInfo'{activeConnectors, healthyConnectors,
                           blackListedConnectors, shutdownConnectors, unhealthyConnectors,
                           totalConnectors, unknownConnectors}

-- | Number of active discovery connectors.
--
-- /Note:/ Consider using 'activeConnectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cciActiveConnectors :: Lens.Lens' CustomerConnectorInfo Core.Int
cciActiveConnectors = Lens.field @"activeConnectors"
{-# INLINEABLE cciActiveConnectors #-}
{-# DEPRECATED activeConnectors "Use generic-lens or generic-optics with 'activeConnectors' instead"  #-}

-- | Number of healthy discovery connectors.
--
-- /Note:/ Consider using 'healthyConnectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cciHealthyConnectors :: Lens.Lens' CustomerConnectorInfo Core.Int
cciHealthyConnectors = Lens.field @"healthyConnectors"
{-# INLINEABLE cciHealthyConnectors #-}
{-# DEPRECATED healthyConnectors "Use generic-lens or generic-optics with 'healthyConnectors' instead"  #-}

-- | Number of blacklisted discovery connectors.
--
-- /Note:/ Consider using 'blackListedConnectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cciBlackListedConnectors :: Lens.Lens' CustomerConnectorInfo Core.Int
cciBlackListedConnectors = Lens.field @"blackListedConnectors"
{-# INLINEABLE cciBlackListedConnectors #-}
{-# DEPRECATED blackListedConnectors "Use generic-lens or generic-optics with 'blackListedConnectors' instead"  #-}

-- | Number of discovery connectors with status SHUTDOWN,
--
-- /Note:/ Consider using 'shutdownConnectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cciShutdownConnectors :: Lens.Lens' CustomerConnectorInfo Core.Int
cciShutdownConnectors = Lens.field @"shutdownConnectors"
{-# INLINEABLE cciShutdownConnectors #-}
{-# DEPRECATED shutdownConnectors "Use generic-lens or generic-optics with 'shutdownConnectors' instead"  #-}

-- | Number of unhealthy discovery connectors.
--
-- /Note:/ Consider using 'unhealthyConnectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cciUnhealthyConnectors :: Lens.Lens' CustomerConnectorInfo Core.Int
cciUnhealthyConnectors = Lens.field @"unhealthyConnectors"
{-# INLINEABLE cciUnhealthyConnectors #-}
{-# DEPRECATED unhealthyConnectors "Use generic-lens or generic-optics with 'unhealthyConnectors' instead"  #-}

-- | Total number of discovery connectors.
--
-- /Note:/ Consider using 'totalConnectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cciTotalConnectors :: Lens.Lens' CustomerConnectorInfo Core.Int
cciTotalConnectors = Lens.field @"totalConnectors"
{-# INLINEABLE cciTotalConnectors #-}
{-# DEPRECATED totalConnectors "Use generic-lens or generic-optics with 'totalConnectors' instead"  #-}

-- | Number of unknown discovery connectors.
--
-- /Note:/ Consider using 'unknownConnectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cciUnknownConnectors :: Lens.Lens' CustomerConnectorInfo Core.Int
cciUnknownConnectors = Lens.field @"unknownConnectors"
{-# INLINEABLE cciUnknownConnectors #-}
{-# DEPRECATED unknownConnectors "Use generic-lens or generic-optics with 'unknownConnectors' instead"  #-}

instance Core.FromJSON CustomerConnectorInfo where
        parseJSON
          = Core.withObject "CustomerConnectorInfo" Core.$
              \ x ->
                CustomerConnectorInfo' Core.<$>
                  (x Core..: "activeConnectors") Core.<*>
                    x Core..: "healthyConnectors"
                    Core.<*> x Core..: "blackListedConnectors"
                    Core.<*> x Core..: "shutdownConnectors"
                    Core.<*> x Core..: "unhealthyConnectors"
                    Core.<*> x Core..: "totalConnectors"
                    Core.<*> x Core..: "unknownConnectors"
