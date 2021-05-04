{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AutoScaling.Types.CustomizedMetricSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.CustomizedMetricSpecification where

import Network.AWS.AutoScaling.Types.MetricDimension
import Network.AWS.AutoScaling.Types.MetricStatistic
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a CloudWatch metric of your choosing for a target tracking
-- scaling policy to use with Amazon EC2 Auto Scaling.
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
--     should decrease when capacity increases.
--
-- For more information about CloudWatch, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html Amazon CloudWatch Concepts>.
--
-- /See:/ 'newCustomizedMetricSpecification' smart constructor.
data CustomizedMetricSpecification = CustomizedMetricSpecification'
  { -- | The unit of the metric.
    unit :: Prelude.Maybe Prelude.Text,
    -- | The dimensions of the metric.
    --
    -- Conditional: If you published your metric with dimensions, you must
    -- specify the same dimensions in your scaling policy.
    dimensions :: Prelude.Maybe [MetricDimension],
    -- | The name of the metric.
    metricName :: Prelude.Text,
    -- | The namespace of the metric.
    namespace :: Prelude.Text,
    -- | The statistic of the metric.
    statistic :: MetricStatistic
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'namespace'
  Prelude.Text ->
  -- | 'statistic'
  MetricStatistic ->
  CustomizedMetricSpecification
newCustomizedMetricSpecification
  pMetricName_
  pNamespace_
  pStatistic_ =
    CustomizedMetricSpecification'
      { unit =
          Prelude.Nothing,
        dimensions = Prelude.Nothing,
        metricName = pMetricName_,
        namespace = pNamespace_,
        statistic = pStatistic_
      }

-- | The unit of the metric.
customizedMetricSpecification_unit :: Lens.Lens' CustomizedMetricSpecification (Prelude.Maybe Prelude.Text)
customizedMetricSpecification_unit = Lens.lens (\CustomizedMetricSpecification' {unit} -> unit) (\s@CustomizedMetricSpecification' {} a -> s {unit = a} :: CustomizedMetricSpecification)

-- | The dimensions of the metric.
--
-- Conditional: If you published your metric with dimensions, you must
-- specify the same dimensions in your scaling policy.
customizedMetricSpecification_dimensions :: Lens.Lens' CustomizedMetricSpecification (Prelude.Maybe [MetricDimension])
customizedMetricSpecification_dimensions = Lens.lens (\CustomizedMetricSpecification' {dimensions} -> dimensions) (\s@CustomizedMetricSpecification' {} a -> s {dimensions = a} :: CustomizedMetricSpecification) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the metric.
customizedMetricSpecification_metricName :: Lens.Lens' CustomizedMetricSpecification Prelude.Text
customizedMetricSpecification_metricName = Lens.lens (\CustomizedMetricSpecification' {metricName} -> metricName) (\s@CustomizedMetricSpecification' {} a -> s {metricName = a} :: CustomizedMetricSpecification)

-- | The namespace of the metric.
customizedMetricSpecification_namespace :: Lens.Lens' CustomizedMetricSpecification Prelude.Text
customizedMetricSpecification_namespace = Lens.lens (\CustomizedMetricSpecification' {namespace} -> namespace) (\s@CustomizedMetricSpecification' {} a -> s {namespace = a} :: CustomizedMetricSpecification)

-- | The statistic of the metric.
customizedMetricSpecification_statistic :: Lens.Lens' CustomizedMetricSpecification MetricStatistic
customizedMetricSpecification_statistic = Lens.lens (\CustomizedMetricSpecification' {statistic} -> statistic) (\s@CustomizedMetricSpecification' {} a -> s {statistic = a} :: CustomizedMetricSpecification)

instance
  Prelude.FromXML
    CustomizedMetricSpecification
  where
  parseXML x =
    CustomizedMetricSpecification'
      Prelude.<$> (x Prelude..@? "Unit")
      Prelude.<*> ( x Prelude..@? "Dimensions"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@ "MetricName")
      Prelude.<*> (x Prelude..@ "Namespace")
      Prelude.<*> (x Prelude..@ "Statistic")

instance
  Prelude.Hashable
    CustomizedMetricSpecification

instance Prelude.NFData CustomizedMetricSpecification

instance
  Prelude.ToQuery
    CustomizedMetricSpecification
  where
  toQuery CustomizedMetricSpecification' {..} =
    Prelude.mconcat
      [ "Unit" Prelude.=: unit,
        "Dimensions"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> dimensions
            ),
        "MetricName" Prelude.=: metricName,
        "Namespace" Prelude.=: namespace,
        "Statistic" Prelude.=: statistic
      ]
