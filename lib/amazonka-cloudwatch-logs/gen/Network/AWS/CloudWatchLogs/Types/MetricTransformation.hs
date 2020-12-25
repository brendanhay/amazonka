{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.MetricTransformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.MetricTransformation
  ( MetricTransformation (..),

    -- * Smart constructor
    mkMetricTransformation,

    -- * Lenses
    mtMetricName,
    mtMetricNamespace,
    mtMetricValue,
    mtDefaultValue,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types.MetricName as Types
import qualified Network.AWS.CloudWatchLogs.Types.MetricNamespace as Types
import qualified Network.AWS.CloudWatchLogs.Types.MetricValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Indicates how to transform ingested log events to metric data in a CloudWatch metric.
--
-- /See:/ 'mkMetricTransformation' smart constructor.
data MetricTransformation = MetricTransformation'
  { -- | The name of the CloudWatch metric.
    metricName :: Types.MetricName,
    -- | A custom namespace to contain your metric in CloudWatch. Use namespaces to group together metrics that are similar. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html#Namespace Namespaces> .
    metricNamespace :: Types.MetricNamespace,
    -- | The value to publish to the CloudWatch metric when a filter pattern matches a log event.
    metricValue :: Types.MetricValue,
    -- | (Optional) The value to emit when a filter pattern does not match a log event. This value can be null.
    defaultValue :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MetricTransformation' value with any optional fields omitted.
mkMetricTransformation ::
  -- | 'metricName'
  Types.MetricName ->
  -- | 'metricNamespace'
  Types.MetricNamespace ->
  -- | 'metricValue'
  Types.MetricValue ->
  MetricTransformation
mkMetricTransformation metricName metricNamespace metricValue =
  MetricTransformation'
    { metricName,
      metricNamespace,
      metricValue,
      defaultValue = Core.Nothing
    }

-- | The name of the CloudWatch metric.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtMetricName :: Lens.Lens' MetricTransformation Types.MetricName
mtMetricName = Lens.field @"metricName"
{-# DEPRECATED mtMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | A custom namespace to contain your metric in CloudWatch. Use namespaces to group together metrics that are similar. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html#Namespace Namespaces> .
--
-- /Note:/ Consider using 'metricNamespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtMetricNamespace :: Lens.Lens' MetricTransformation Types.MetricNamespace
mtMetricNamespace = Lens.field @"metricNamespace"
{-# DEPRECATED mtMetricNamespace "Use generic-lens or generic-optics with 'metricNamespace' instead." #-}

-- | The value to publish to the CloudWatch metric when a filter pattern matches a log event.
--
-- /Note:/ Consider using 'metricValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtMetricValue :: Lens.Lens' MetricTransformation Types.MetricValue
mtMetricValue = Lens.field @"metricValue"
{-# DEPRECATED mtMetricValue "Use generic-lens or generic-optics with 'metricValue' instead." #-}

-- | (Optional) The value to emit when a filter pattern does not match a log event. This value can be null.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtDefaultValue :: Lens.Lens' MetricTransformation (Core.Maybe Core.Double)
mtDefaultValue = Lens.field @"defaultValue"
{-# DEPRECATED mtDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

instance Core.FromJSON MetricTransformation where
  toJSON MetricTransformation {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("metricName" Core..= metricName),
            Core.Just ("metricNamespace" Core..= metricNamespace),
            Core.Just ("metricValue" Core..= metricValue),
            ("defaultValue" Core..=) Core.<$> defaultValue
          ]
      )

instance Core.FromJSON MetricTransformation where
  parseJSON =
    Core.withObject "MetricTransformation" Core.$
      \x ->
        MetricTransformation'
          Core.<$> (x Core..: "metricName")
          Core.<*> (x Core..: "metricNamespace")
          Core.<*> (x Core..: "metricValue")
          Core.<*> (x Core..:? "defaultValue")
