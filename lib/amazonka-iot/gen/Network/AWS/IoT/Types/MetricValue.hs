{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.MetricValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.MetricValue
  ( MetricValue (..)
  -- * Smart constructor
  , mkMetricValue
  -- * Lenses
  , mvCidrs
  , mvCount
  , mvPorts
  ) where

import qualified Network.AWS.IoT.Types.Cidr as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The value to be compared with the @metric@ .
--
-- /See:/ 'mkMetricValue' smart constructor.
data MetricValue = MetricValue'
  { cidrs :: Core.Maybe [Types.Cidr]
    -- ^ If the @comparisonOperator@ calls for a set of CIDRs, use this to specify that set to be compared with the @metric@ .
  , count :: Core.Maybe Core.Natural
    -- ^ If the @comparisonOperator@ calls for a numeric value, use this to specify that numeric value to be compared with the @metric@ .
  , ports :: Core.Maybe [Core.Natural]
    -- ^ If the @comparisonOperator@ calls for a set of ports, use this to specify that set to be compared with the @metric@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MetricValue' value with any optional fields omitted.
mkMetricValue
    :: MetricValue
mkMetricValue
  = MetricValue'{cidrs = Core.Nothing, count = Core.Nothing,
                 ports = Core.Nothing}

-- | If the @comparisonOperator@ calls for a set of CIDRs, use this to specify that set to be compared with the @metric@ .
--
-- /Note:/ Consider using 'cidrs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvCidrs :: Lens.Lens' MetricValue (Core.Maybe [Types.Cidr])
mvCidrs = Lens.field @"cidrs"
{-# INLINEABLE mvCidrs #-}
{-# DEPRECATED cidrs "Use generic-lens or generic-optics with 'cidrs' instead"  #-}

-- | If the @comparisonOperator@ calls for a numeric value, use this to specify that numeric value to be compared with the @metric@ .
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvCount :: Lens.Lens' MetricValue (Core.Maybe Core.Natural)
mvCount = Lens.field @"count"
{-# INLINEABLE mvCount #-}
{-# DEPRECATED count "Use generic-lens or generic-optics with 'count' instead"  #-}

-- | If the @comparisonOperator@ calls for a set of ports, use this to specify that set to be compared with the @metric@ .
--
-- /Note:/ Consider using 'ports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvPorts :: Lens.Lens' MetricValue (Core.Maybe [Core.Natural])
mvPorts = Lens.field @"ports"
{-# INLINEABLE mvPorts #-}
{-# DEPRECATED ports "Use generic-lens or generic-optics with 'ports' instead"  #-}

instance Core.FromJSON MetricValue where
        toJSON MetricValue{..}
          = Core.object
              (Core.catMaybes
                 [("cidrs" Core..=) Core.<$> cidrs,
                  ("count" Core..=) Core.<$> count,
                  ("ports" Core..=) Core.<$> ports])

instance Core.FromJSON MetricValue where
        parseJSON
          = Core.withObject "MetricValue" Core.$
              \ x ->
                MetricValue' Core.<$>
                  (x Core..:? "cidrs") Core.<*> x Core..:? "count" Core.<*>
                    x Core..:? "ports"
