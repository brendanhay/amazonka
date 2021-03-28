{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.EnabledMetric
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.EnabledMetric
  ( EnabledMetric (..)
  -- * Smart constructor
  , mkEnabledMetric
  -- * Lenses
  , emGranularity
  , emMetric
  ) where

import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen255 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an enabled metric.
--
-- /See:/ 'mkEnabledMetric' smart constructor.
data EnabledMetric = EnabledMetric'
  { granularity :: Core.Maybe Types.XmlStringMaxLen255
    -- ^ The granularity of the metric. The only valid value is @1Minute@ .
  , metric :: Core.Maybe Types.XmlStringMaxLen255
    -- ^ One of the following metrics:
--
--
--     * @GroupMinSize@ 
--
--
--     * @GroupMaxSize@ 
--
--
--     * @GroupDesiredCapacity@ 
--
--
--     * @GroupInServiceInstances@ 
--
--
--     * @GroupPendingInstances@ 
--
--
--     * @GroupStandbyInstances@ 
--
--
--     * @GroupTerminatingInstances@ 
--
--
--     * @GroupTotalInstances@ 
--
--
--     * @GroupInServiceCapacity@ 
--
--
--     * @GroupPendingCapacity@ 
--
--
--     * @GroupStandbyCapacity@ 
--
--
--     * @GroupTerminatingCapacity@ 
--
--
--     * @GroupTotalCapacity@ 
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnabledMetric' value with any optional fields omitted.
mkEnabledMetric
    :: EnabledMetric
mkEnabledMetric
  = EnabledMetric'{granularity = Core.Nothing, metric = Core.Nothing}

-- | The granularity of the metric. The only valid value is @1Minute@ .
--
-- /Note:/ Consider using 'granularity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emGranularity :: Lens.Lens' EnabledMetric (Core.Maybe Types.XmlStringMaxLen255)
emGranularity = Lens.field @"granularity"
{-# INLINEABLE emGranularity #-}
{-# DEPRECATED granularity "Use generic-lens or generic-optics with 'granularity' instead"  #-}

-- | One of the following metrics:
--
--
--     * @GroupMinSize@ 
--
--
--     * @GroupMaxSize@ 
--
--
--     * @GroupDesiredCapacity@ 
--
--
--     * @GroupInServiceInstances@ 
--
--
--     * @GroupPendingInstances@ 
--
--
--     * @GroupStandbyInstances@ 
--
--
--     * @GroupTerminatingInstances@ 
--
--
--     * @GroupTotalInstances@ 
--
--
--     * @GroupInServiceCapacity@ 
--
--
--     * @GroupPendingCapacity@ 
--
--
--     * @GroupStandbyCapacity@ 
--
--
--     * @GroupTerminatingCapacity@ 
--
--
--     * @GroupTotalCapacity@ 
--
--
--
-- /Note:/ Consider using 'metric' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emMetric :: Lens.Lens' EnabledMetric (Core.Maybe Types.XmlStringMaxLen255)
emMetric = Lens.field @"metric"
{-# INLINEABLE emMetric #-}
{-# DEPRECATED metric "Use generic-lens or generic-optics with 'metric' instead"  #-}

instance Core.FromXML EnabledMetric where
        parseXML x
          = EnabledMetric' Core.<$>
              (x Core..@? "Granularity") Core.<*> x Core..@? "Metric"
