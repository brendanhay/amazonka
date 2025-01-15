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
-- Module      : Amazonka.AutoScalingPlans.Types.CustomizedScalingMetricSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScalingPlans.Types.CustomizedScalingMetricSpecification where

import Amazonka.AutoScalingPlans.Types.MetricDimension
import Amazonka.AutoScalingPlans.Types.MetricStatistic
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a CloudWatch metric of your choosing that can be used for
-- dynamic scaling as part of a target tracking scaling policy.
--
-- To create your customized scaling metric specification:
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
--     should decrease when capacity increases.
--
-- For information about terminology, available metrics, or how to publish
-- new metrics, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html Amazon CloudWatch Concepts>
-- in the /Amazon CloudWatch User Guide/.
--
-- /See:/ 'newCustomizedScalingMetricSpecification' smart constructor.
data CustomizedScalingMetricSpecification = CustomizedScalingMetricSpecification'
  { -- | The dimensions of the metric.
    --
    -- Conditional: If you published your metric with dimensions, you must
    -- specify the same dimensions in your customized scaling metric
    -- specification.
    dimensions :: Prelude.Maybe [MetricDimension],
    -- | The unit of the metric.
    unit :: Prelude.Maybe Prelude.Text,
    -- | The name of the metric.
    metricName :: Prelude.Text,
    -- | The namespace of the metric.
    namespace :: Prelude.Text,
    -- | The statistic of the metric.
    statistic :: MetricStatistic
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomizedScalingMetricSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensions', 'customizedScalingMetricSpecification_dimensions' - The dimensions of the metric.
--
-- Conditional: If you published your metric with dimensions, you must
-- specify the same dimensions in your customized scaling metric
-- specification.
--
-- 'unit', 'customizedScalingMetricSpecification_unit' - The unit of the metric.
--
-- 'metricName', 'customizedScalingMetricSpecification_metricName' - The name of the metric.
--
-- 'namespace', 'customizedScalingMetricSpecification_namespace' - The namespace of the metric.
--
-- 'statistic', 'customizedScalingMetricSpecification_statistic' - The statistic of the metric.
newCustomizedScalingMetricSpecification ::
  -- | 'metricName'
  Prelude.Text ->
  -- | 'namespace'
  Prelude.Text ->
  -- | 'statistic'
  MetricStatistic ->
  CustomizedScalingMetricSpecification
newCustomizedScalingMetricSpecification
  pMetricName_
  pNamespace_
  pStatistic_ =
    CustomizedScalingMetricSpecification'
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
-- specify the same dimensions in your customized scaling metric
-- specification.
customizedScalingMetricSpecification_dimensions :: Lens.Lens' CustomizedScalingMetricSpecification (Prelude.Maybe [MetricDimension])
customizedScalingMetricSpecification_dimensions = Lens.lens (\CustomizedScalingMetricSpecification' {dimensions} -> dimensions) (\s@CustomizedScalingMetricSpecification' {} a -> s {dimensions = a} :: CustomizedScalingMetricSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The unit of the metric.
customizedScalingMetricSpecification_unit :: Lens.Lens' CustomizedScalingMetricSpecification (Prelude.Maybe Prelude.Text)
customizedScalingMetricSpecification_unit = Lens.lens (\CustomizedScalingMetricSpecification' {unit} -> unit) (\s@CustomizedScalingMetricSpecification' {} a -> s {unit = a} :: CustomizedScalingMetricSpecification)

-- | The name of the metric.
customizedScalingMetricSpecification_metricName :: Lens.Lens' CustomizedScalingMetricSpecification Prelude.Text
customizedScalingMetricSpecification_metricName = Lens.lens (\CustomizedScalingMetricSpecification' {metricName} -> metricName) (\s@CustomizedScalingMetricSpecification' {} a -> s {metricName = a} :: CustomizedScalingMetricSpecification)

-- | The namespace of the metric.
customizedScalingMetricSpecification_namespace :: Lens.Lens' CustomizedScalingMetricSpecification Prelude.Text
customizedScalingMetricSpecification_namespace = Lens.lens (\CustomizedScalingMetricSpecification' {namespace} -> namespace) (\s@CustomizedScalingMetricSpecification' {} a -> s {namespace = a} :: CustomizedScalingMetricSpecification)

-- | The statistic of the metric.
customizedScalingMetricSpecification_statistic :: Lens.Lens' CustomizedScalingMetricSpecification MetricStatistic
customizedScalingMetricSpecification_statistic = Lens.lens (\CustomizedScalingMetricSpecification' {statistic} -> statistic) (\s@CustomizedScalingMetricSpecification' {} a -> s {statistic = a} :: CustomizedScalingMetricSpecification)

instance
  Data.FromJSON
    CustomizedScalingMetricSpecification
  where
  parseJSON =
    Data.withObject
      "CustomizedScalingMetricSpecification"
      ( \x ->
          CustomizedScalingMetricSpecification'
            Prelude.<$> (x Data..:? "Dimensions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Unit")
            Prelude.<*> (x Data..: "MetricName")
            Prelude.<*> (x Data..: "Namespace")
            Prelude.<*> (x Data..: "Statistic")
      )

instance
  Prelude.Hashable
    CustomizedScalingMetricSpecification
  where
  hashWithSalt
    _salt
    CustomizedScalingMetricSpecification' {..} =
      _salt
        `Prelude.hashWithSalt` dimensions
        `Prelude.hashWithSalt` unit
        `Prelude.hashWithSalt` metricName
        `Prelude.hashWithSalt` namespace
        `Prelude.hashWithSalt` statistic

instance
  Prelude.NFData
    CustomizedScalingMetricSpecification
  where
  rnf CustomizedScalingMetricSpecification' {..} =
    Prelude.rnf dimensions `Prelude.seq`
      Prelude.rnf unit `Prelude.seq`
        Prelude.rnf metricName `Prelude.seq`
          Prelude.rnf namespace `Prelude.seq`
            Prelude.rnf statistic

instance
  Data.ToJSON
    CustomizedScalingMetricSpecification
  where
  toJSON CustomizedScalingMetricSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Dimensions" Data..=) Prelude.<$> dimensions,
            ("Unit" Data..=) Prelude.<$> unit,
            Prelude.Just ("MetricName" Data..= metricName),
            Prelude.Just ("Namespace" Data..= namespace),
            Prelude.Just ("Statistic" Data..= statistic)
          ]
      )
