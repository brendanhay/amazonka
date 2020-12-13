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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Indicates how to transform ingested log events to metric data in a CloudWatch metric.
--
-- /See:/ 'mkMetricTransformation' smart constructor.
data MetricTransformation = MetricTransformation'
  { -- | The name of the CloudWatch metric.
    metricName :: Lude.Text,
    -- | A custom namespace to contain your metric in CloudWatch. Use namespaces to group together metrics that are similar. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html#Namespace Namespaces> .
    metricNamespace :: Lude.Text,
    -- | The value to publish to the CloudWatch metric when a filter pattern matches a log event.
    metricValue :: Lude.Text,
    -- | (Optional) The value to emit when a filter pattern does not match a log event. This value can be null.
    defaultValue :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MetricTransformation' with the minimum fields required to make a request.
--
-- * 'metricName' - The name of the CloudWatch metric.
-- * 'metricNamespace' - A custom namespace to contain your metric in CloudWatch. Use namespaces to group together metrics that are similar. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html#Namespace Namespaces> .
-- * 'metricValue' - The value to publish to the CloudWatch metric when a filter pattern matches a log event.
-- * 'defaultValue' - (Optional) The value to emit when a filter pattern does not match a log event. This value can be null.
mkMetricTransformation ::
  -- | 'metricName'
  Lude.Text ->
  -- | 'metricNamespace'
  Lude.Text ->
  -- | 'metricValue'
  Lude.Text ->
  MetricTransformation
mkMetricTransformation pMetricName_ pMetricNamespace_ pMetricValue_ =
  MetricTransformation'
    { metricName = pMetricName_,
      metricNamespace = pMetricNamespace_,
      metricValue = pMetricValue_,
      defaultValue = Lude.Nothing
    }

-- | The name of the CloudWatch metric.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtMetricName :: Lens.Lens' MetricTransformation Lude.Text
mtMetricName = Lens.lens (metricName :: MetricTransformation -> Lude.Text) (\s a -> s {metricName = a} :: MetricTransformation)
{-# DEPRECATED mtMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | A custom namespace to contain your metric in CloudWatch. Use namespaces to group together metrics that are similar. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html#Namespace Namespaces> .
--
-- /Note:/ Consider using 'metricNamespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtMetricNamespace :: Lens.Lens' MetricTransformation Lude.Text
mtMetricNamespace = Lens.lens (metricNamespace :: MetricTransformation -> Lude.Text) (\s a -> s {metricNamespace = a} :: MetricTransformation)
{-# DEPRECATED mtMetricNamespace "Use generic-lens or generic-optics with 'metricNamespace' instead." #-}

-- | The value to publish to the CloudWatch metric when a filter pattern matches a log event.
--
-- /Note:/ Consider using 'metricValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtMetricValue :: Lens.Lens' MetricTransformation Lude.Text
mtMetricValue = Lens.lens (metricValue :: MetricTransformation -> Lude.Text) (\s a -> s {metricValue = a} :: MetricTransformation)
{-# DEPRECATED mtMetricValue "Use generic-lens or generic-optics with 'metricValue' instead." #-}

-- | (Optional) The value to emit when a filter pattern does not match a log event. This value can be null.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtDefaultValue :: Lens.Lens' MetricTransformation (Lude.Maybe Lude.Double)
mtDefaultValue = Lens.lens (defaultValue :: MetricTransformation -> Lude.Maybe Lude.Double) (\s a -> s {defaultValue = a} :: MetricTransformation)
{-# DEPRECATED mtDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

instance Lude.FromJSON MetricTransformation where
  parseJSON =
    Lude.withObject
      "MetricTransformation"
      ( \x ->
          MetricTransformation'
            Lude.<$> (x Lude..: "metricName")
            Lude.<*> (x Lude..: "metricNamespace")
            Lude.<*> (x Lude..: "metricValue")
            Lude.<*> (x Lude..:? "defaultValue")
      )

instance Lude.ToJSON MetricTransformation where
  toJSON MetricTransformation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("metricName" Lude..= metricName),
            Lude.Just ("metricNamespace" Lude..= metricNamespace),
            Lude.Just ("metricValue" Lude..= metricValue),
            ("defaultValue" Lude..=) Lude.<$> defaultValue
          ]
      )
