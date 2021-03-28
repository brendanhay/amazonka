{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.MetricCollectionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.MetricCollectionType
  ( MetricCollectionType (..)
  -- * Smart constructor
  , mkMetricCollectionType
  -- * Lenses
  , mctMetric
  ) where

import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen255 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a metric.
--
-- /See:/ 'mkMetricCollectionType' smart constructor.
newtype MetricCollectionType = MetricCollectionType'
  { metric :: Core.Maybe Types.XmlStringMaxLen255
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
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'MetricCollectionType' value with any optional fields omitted.
mkMetricCollectionType
    :: MetricCollectionType
mkMetricCollectionType
  = MetricCollectionType'{metric = Core.Nothing}

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
mctMetric :: Lens.Lens' MetricCollectionType (Core.Maybe Types.XmlStringMaxLen255)
mctMetric = Lens.field @"metric"
{-# INLINEABLE mctMetric #-}
{-# DEPRECATED metric "Use generic-lens or generic-optics with 'metric' instead"  #-}

instance Core.FromXML MetricCollectionType where
        parseXML x = MetricCollectionType' Core.<$> (x Core..@? "Metric")
