{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.Metric
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.Metric
  ( Metric (..),

    -- * Smart constructor
    mkMetric,

    -- * Lenses
    mDimensions,
    mMetricName,
    mNamespace,
  )
where

import qualified Network.AWS.CloudWatch.Types.Dimension as Types
import qualified Network.AWS.CloudWatch.Types.MetricName as Types
import qualified Network.AWS.CloudWatch.Types.Namespace as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a specific metric.
--
-- /See:/ 'mkMetric' smart constructor.
data Metric = Metric'
  { -- | The dimensions for the metric.
    dimensions :: Core.Maybe [Types.Dimension],
    -- | The name of the metric. This is a required field.
    metricName :: Core.Maybe Types.MetricName,
    -- | The namespace of the metric.
    namespace :: Core.Maybe Types.Namespace
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Metric' value with any optional fields omitted.
mkMetric ::
  Metric
mkMetric =
  Metric'
    { dimensions = Core.Nothing,
      metricName = Core.Nothing,
      namespace = Core.Nothing
    }

-- | The dimensions for the metric.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mDimensions :: Lens.Lens' Metric (Core.Maybe [Types.Dimension])
mDimensions = Lens.field @"dimensions"
{-# DEPRECATED mDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

-- | The name of the metric. This is a required field.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMetricName :: Lens.Lens' Metric (Core.Maybe Types.MetricName)
mMetricName = Lens.field @"metricName"
{-# DEPRECATED mMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The namespace of the metric.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mNamespace :: Lens.Lens' Metric (Core.Maybe Types.Namespace)
mNamespace = Lens.field @"namespace"
{-# DEPRECATED mNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

instance Core.FromXML Metric where
  parseXML x =
    Metric'
      Core.<$> (x Core..@? "Dimensions" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "MetricName")
      Core.<*> (x Core..@? "Namespace")
