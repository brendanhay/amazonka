{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.CustomizedMetricSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.CustomizedMetricSpecification where

import Network.AWS.ApplicationAutoScaling.Types.MetricDimension
import Network.AWS.ApplicationAutoScaling.Types.MetricStatistic
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents a CloudWatch metric of your choosing for a target tracking
-- scaling policy to use with Application Auto Scaling.
--
-- For information about the available metrics for a service, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/aws-services-cloudwatch-metrics.html AWS Services That Publish CloudWatch Metrics>
-- in the /Amazon CloudWatch User Guide/.
--
-- To create your customized metric specification:
--
-- -   Add values for each required parameter from CloudWatch. You can use
--     an existing metric, or a new metric that you create. To use your own
--     metric, you must first publish the metric to CloudWatch. For more
--     information, see
--     <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html Publish Custom Metrics>
--     in the /Amazon CloudWatch User Guide/.
--
-- -   Choose a metric that changes proportionally with capacity. The value
--     of the metric should increase or decrease in inverse proportion to
--     the number of capacity units. That is, the value of the metric
--     should decrease when capacity increases, and increase when capacity
--     decreases.
--
-- For more information about CloudWatch, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html Amazon CloudWatch Concepts>.
--
-- /See:/ 'newCustomizedMetricSpecification' smart constructor.
data CustomizedMetricSpecification = CustomizedMetricSpecification'
  { -- | The unit of the metric.
    unit :: Core.Maybe Core.Text,
    -- | The dimensions of the metric.
    --
    -- Conditional: If you published your metric with dimensions, you must
    -- specify the same dimensions in your scaling policy.
    dimensions :: Core.Maybe [MetricDimension],
    -- | The name of the metric.
    metricName :: Core.Text,
    -- | The namespace of the metric.
    namespace :: Core.Text,
    -- | The statistic of the metric.
    statistic :: MetricStatistic
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CustomizedMetricSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unit', 'customizedMetricSpecification_unit' - The unit of the metric.
--
-- 'dimensions', 'customizedMetricSpecification_dimensions' - The dimensions of the metric.
--
-- Conditional: If you published your metric with dimensions, you must
-- specify the same dimensions in your scaling policy.
--
-- 'metricName', 'customizedMetricSpecification_metricName' - The name of the metric.
--
-- 'namespace', 'customizedMetricSpecification_namespace' - The namespace of the metric.
--
-- 'statistic', 'customizedMetricSpecification_statistic' - The statistic of the metric.
newCustomizedMetricSpecification ::
  -- | 'metricName'
  Core.Text ->
  -- | 'namespace'
  Core.Text ->
  -- | 'statistic'
  MetricStatistic ->
  CustomizedMetricSpecification
newCustomizedMetricSpecification
  pMetricName_
  pNamespace_
  pStatistic_ =
    CustomizedMetricSpecification'
      { unit = Core.Nothing,
        dimensions = Core.Nothing,
        metricName = pMetricName_,
        namespace = pNamespace_,
        statistic = pStatistic_
      }

-- | The unit of the metric.
customizedMetricSpecification_unit :: Lens.Lens' CustomizedMetricSpecification (Core.Maybe Core.Text)
customizedMetricSpecification_unit = Lens.lens (\CustomizedMetricSpecification' {unit} -> unit) (\s@CustomizedMetricSpecification' {} a -> s {unit = a} :: CustomizedMetricSpecification)

-- | The dimensions of the metric.
--
-- Conditional: If you published your metric with dimensions, you must
-- specify the same dimensions in your scaling policy.
customizedMetricSpecification_dimensions :: Lens.Lens' CustomizedMetricSpecification (Core.Maybe [MetricDimension])
customizedMetricSpecification_dimensions = Lens.lens (\CustomizedMetricSpecification' {dimensions} -> dimensions) (\s@CustomizedMetricSpecification' {} a -> s {dimensions = a} :: CustomizedMetricSpecification) Core.. Lens.mapping Lens._Coerce

-- | The name of the metric.
customizedMetricSpecification_metricName :: Lens.Lens' CustomizedMetricSpecification Core.Text
customizedMetricSpecification_metricName = Lens.lens (\CustomizedMetricSpecification' {metricName} -> metricName) (\s@CustomizedMetricSpecification' {} a -> s {metricName = a} :: CustomizedMetricSpecification)

-- | The namespace of the metric.
customizedMetricSpecification_namespace :: Lens.Lens' CustomizedMetricSpecification Core.Text
customizedMetricSpecification_namespace = Lens.lens (\CustomizedMetricSpecification' {namespace} -> namespace) (\s@CustomizedMetricSpecification' {} a -> s {namespace = a} :: CustomizedMetricSpecification)

-- | The statistic of the metric.
customizedMetricSpecification_statistic :: Lens.Lens' CustomizedMetricSpecification MetricStatistic
customizedMetricSpecification_statistic = Lens.lens (\CustomizedMetricSpecification' {statistic} -> statistic) (\s@CustomizedMetricSpecification' {} a -> s {statistic = a} :: CustomizedMetricSpecification)

instance Core.FromJSON CustomizedMetricSpecification where
  parseJSON =
    Core.withObject
      "CustomizedMetricSpecification"
      ( \x ->
          CustomizedMetricSpecification'
            Core.<$> (x Core..:? "Unit")
            Core.<*> (x Core..:? "Dimensions" Core..!= Core.mempty)
            Core.<*> (x Core..: "MetricName")
            Core.<*> (x Core..: "Namespace")
            Core.<*> (x Core..: "Statistic")
      )

instance Core.Hashable CustomizedMetricSpecification

instance Core.NFData CustomizedMetricSpecification

instance Core.ToJSON CustomizedMetricSpecification where
  toJSON CustomizedMetricSpecification' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Unit" Core..=) Core.<$> unit,
            ("Dimensions" Core..=) Core.<$> dimensions,
            Core.Just ("MetricName" Core..= metricName),
            Core.Just ("Namespace" Core..= namespace),
            Core.Just ("Statistic" Core..= statistic)
          ]
      )
