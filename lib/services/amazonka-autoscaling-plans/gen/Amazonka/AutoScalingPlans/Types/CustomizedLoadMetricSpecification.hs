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
-- Module      : Amazonka.AutoScalingPlans.Types.CustomizedLoadMetricSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScalingPlans.Types.CustomizedLoadMetricSpecification where

import Amazonka.AutoScalingPlans.Types.MetricDimension
import Amazonka.AutoScalingPlans.Types.MetricStatistic
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a CloudWatch metric of your choosing that can be used for
-- predictive scaling.
--
-- For predictive scaling to work with a customized load metric
-- specification, AWS Auto Scaling needs access to the @Sum@ and @Average@
-- statistics that CloudWatch computes from metric data.
--
-- When you choose a load metric, make sure that the required @Sum@ and
-- @Average@ statistics for your metric are available in CloudWatch and
-- that they provide relevant data for predictive scaling. The @Sum@
-- statistic must represent the total load on the resource, and the
-- @Average@ statistic must represent the average load per capacity unit of
-- the resource. For example, there is a metric that counts the number of
-- requests processed by your Auto Scaling group. If the @Sum@ statistic
-- represents the total request count processed by the group, then the
-- @Average@ statistic for the specified metric must represent the average
-- request count processed by each instance of the group.
--
-- If you publish your own metrics, you can aggregate the data points at a
-- given interval and then publish the aggregated data points to
-- CloudWatch. Before AWS Auto Scaling generates the forecast, it sums up
-- all the metric data points that occurred within each hour to match the
-- granularity period that is used in the forecast (60 minutes).
--
-- For information about terminology, available metrics, or how to publish
-- new metrics, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html Amazon CloudWatch Concepts>
-- in the /Amazon CloudWatch User Guide/.
--
-- After creating your scaling plan, you can use the AWS Auto Scaling
-- console to visualize forecasts for the specified metric. For more
-- information, see
-- <https://docs.aws.amazon.com/autoscaling/plans/userguide/gs-create-scaling-plan.html#gs-view-resource View Scaling Information for a Resource>
-- in the /AWS Auto Scaling User Guide/.
--
-- /See:/ 'newCustomizedLoadMetricSpecification' smart constructor.
data CustomizedLoadMetricSpecification = CustomizedLoadMetricSpecification'
  { -- | The dimensions of the metric.
    --
    -- Conditional: If you published your metric with dimensions, you must
    -- specify the same dimensions in your customized load metric
    -- specification.
    dimensions :: Prelude.Maybe [MetricDimension],
    -- | The unit of the metric.
    unit :: Prelude.Maybe Prelude.Text,
    -- | The name of the metric.
    metricName :: Prelude.Text,
    -- | The namespace of the metric.
    namespace :: Prelude.Text,
    -- | The statistic of the metric. The only valid value is @Sum@.
    statistic :: MetricStatistic
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomizedLoadMetricSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensions', 'customizedLoadMetricSpecification_dimensions' - The dimensions of the metric.
--
-- Conditional: If you published your metric with dimensions, you must
-- specify the same dimensions in your customized load metric
-- specification.
--
-- 'unit', 'customizedLoadMetricSpecification_unit' - The unit of the metric.
--
-- 'metricName', 'customizedLoadMetricSpecification_metricName' - The name of the metric.
--
-- 'namespace', 'customizedLoadMetricSpecification_namespace' - The namespace of the metric.
--
-- 'statistic', 'customizedLoadMetricSpecification_statistic' - The statistic of the metric. The only valid value is @Sum@.
newCustomizedLoadMetricSpecification ::
  -- | 'metricName'
  Prelude.Text ->
  -- | 'namespace'
  Prelude.Text ->
  -- | 'statistic'
  MetricStatistic ->
  CustomizedLoadMetricSpecification
newCustomizedLoadMetricSpecification
  pMetricName_
  pNamespace_
  pStatistic_ =
    CustomizedLoadMetricSpecification'
      { dimensions =
          Prelude.Nothing,
        unit = Prelude.Nothing,
        metricName = pMetricName_,
        namespace = pNamespace_,
        statistic = pStatistic_
      }

-- | The dimensions of the metric.
--
-- Conditional: If you published your metric with dimensions, you must
-- specify the same dimensions in your customized load metric
-- specification.
customizedLoadMetricSpecification_dimensions :: Lens.Lens' CustomizedLoadMetricSpecification (Prelude.Maybe [MetricDimension])
customizedLoadMetricSpecification_dimensions = Lens.lens (\CustomizedLoadMetricSpecification' {dimensions} -> dimensions) (\s@CustomizedLoadMetricSpecification' {} a -> s {dimensions = a} :: CustomizedLoadMetricSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The unit of the metric.
customizedLoadMetricSpecification_unit :: Lens.Lens' CustomizedLoadMetricSpecification (Prelude.Maybe Prelude.Text)
customizedLoadMetricSpecification_unit = Lens.lens (\CustomizedLoadMetricSpecification' {unit} -> unit) (\s@CustomizedLoadMetricSpecification' {} a -> s {unit = a} :: CustomizedLoadMetricSpecification)

-- | The name of the metric.
customizedLoadMetricSpecification_metricName :: Lens.Lens' CustomizedLoadMetricSpecification Prelude.Text
customizedLoadMetricSpecification_metricName = Lens.lens (\CustomizedLoadMetricSpecification' {metricName} -> metricName) (\s@CustomizedLoadMetricSpecification' {} a -> s {metricName = a} :: CustomizedLoadMetricSpecification)

-- | The namespace of the metric.
customizedLoadMetricSpecification_namespace :: Lens.Lens' CustomizedLoadMetricSpecification Prelude.Text
customizedLoadMetricSpecification_namespace = Lens.lens (\CustomizedLoadMetricSpecification' {namespace} -> namespace) (\s@CustomizedLoadMetricSpecification' {} a -> s {namespace = a} :: CustomizedLoadMetricSpecification)

-- | The statistic of the metric. The only valid value is @Sum@.
customizedLoadMetricSpecification_statistic :: Lens.Lens' CustomizedLoadMetricSpecification MetricStatistic
customizedLoadMetricSpecification_statistic = Lens.lens (\CustomizedLoadMetricSpecification' {statistic} -> statistic) (\s@CustomizedLoadMetricSpecification' {} a -> s {statistic = a} :: CustomizedLoadMetricSpecification)

instance
  Core.FromJSON
    CustomizedLoadMetricSpecification
  where
  parseJSON =
    Core.withObject
      "CustomizedLoadMetricSpecification"
      ( \x ->
          CustomizedLoadMetricSpecification'
            Prelude.<$> (x Core..:? "Dimensions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Unit")
            Prelude.<*> (x Core..: "MetricName")
            Prelude.<*> (x Core..: "Namespace")
            Prelude.<*> (x Core..: "Statistic")
      )

instance
  Prelude.Hashable
    CustomizedLoadMetricSpecification
  where
  hashWithSalt
    _salt
    CustomizedLoadMetricSpecification' {..} =
      _salt `Prelude.hashWithSalt` dimensions
        `Prelude.hashWithSalt` unit
        `Prelude.hashWithSalt` metricName
        `Prelude.hashWithSalt` namespace
        `Prelude.hashWithSalt` statistic

instance
  Prelude.NFData
    CustomizedLoadMetricSpecification
  where
  rnf CustomizedLoadMetricSpecification' {..} =
    Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf statistic

instance
  Core.ToJSON
    CustomizedLoadMetricSpecification
  where
  toJSON CustomizedLoadMetricSpecification' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Dimensions" Core..=) Prelude.<$> dimensions,
            ("Unit" Core..=) Prelude.<$> unit,
            Prelude.Just ("MetricName" Core..= metricName),
            Prelude.Just ("Namespace" Core..= namespace),
            Prelude.Just ("Statistic" Core..= statistic)
          ]
      )
