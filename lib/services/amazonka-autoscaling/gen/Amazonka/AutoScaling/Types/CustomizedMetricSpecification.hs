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
-- Module      : Amazonka.AutoScaling.Types.CustomizedMetricSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.CustomizedMetricSpecification where

import Amazonka.AutoScaling.Types.MetricDimension
import Amazonka.AutoScaling.Types.MetricStatistic
import Amazonka.AutoScaling.Types.TargetTrackingMetricDataQuery
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a CloudWatch metric of your choosing for a target tracking
-- scaling policy to use with Amazon EC2 Auto Scaling.
--
-- To create your customized metric specification:
--
-- -   Add values for each required property from CloudWatch. You can use
--     an existing metric, or a new metric that you create. To use your own
--     metric, you must first publish the metric to CloudWatch. For more
--     information, see
--     <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html Publish custom metrics>
--     in the /Amazon CloudWatch User Guide/.
--
-- -   Choose a metric that changes proportionally with capacity. The value
--     of the metric should increase or decrease in inverse proportion to
--     the number of capacity units. That is, the value of the metric
--     should decrease when capacity increases.
--
-- For more information about the CloudWatch terminology below, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html Amazon CloudWatch concepts>.
--
-- Each individual service provides information about the metrics,
-- namespace, and dimensions they use. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/aws-services-cloudwatch-metrics.html Amazon Web Services services that publish CloudWatch metrics>
-- in the /Amazon CloudWatch User Guide/.
--
-- /See:/ 'newCustomizedMetricSpecification' smart constructor.
data CustomizedMetricSpecification = CustomizedMetricSpecification'
  { -- | The dimensions of the metric.
    --
    -- Conditional: If you published your metric with dimensions, you must
    -- specify the same dimensions in your scaling policy.
    dimensions :: Prelude.Maybe [MetricDimension],
    -- | The name of the metric. To get the exact metric name, namespace, and
    -- dimensions, inspect the
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_Metric.html Metric>
    -- object that is returned by a call to
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_ListMetrics.html ListMetrics>.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The metrics to include in the target tracking scaling policy, as a
    -- metric data query. This can include both raw metric and metric math
    -- expressions.
    metrics :: Prelude.Maybe [TargetTrackingMetricDataQuery],
    -- | The namespace of the metric.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The statistic of the metric.
    statistic :: Prelude.Maybe MetricStatistic,
    -- | The unit of the metric. For a complete list of the units that CloudWatch
    -- supports, see the
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_MetricDatum.html MetricDatum>
    -- data type in the /Amazon CloudWatch API Reference/.
    unit :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomizedMetricSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensions', 'customizedMetricSpecification_dimensions' - The dimensions of the metric.
--
-- Conditional: If you published your metric with dimensions, you must
-- specify the same dimensions in your scaling policy.
--
-- 'metricName', 'customizedMetricSpecification_metricName' - The name of the metric. To get the exact metric name, namespace, and
-- dimensions, inspect the
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_Metric.html Metric>
-- object that is returned by a call to
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_ListMetrics.html ListMetrics>.
--
-- 'metrics', 'customizedMetricSpecification_metrics' - The metrics to include in the target tracking scaling policy, as a
-- metric data query. This can include both raw metric and metric math
-- expressions.
--
-- 'namespace', 'customizedMetricSpecification_namespace' - The namespace of the metric.
--
-- 'statistic', 'customizedMetricSpecification_statistic' - The statistic of the metric.
--
-- 'unit', 'customizedMetricSpecification_unit' - The unit of the metric. For a complete list of the units that CloudWatch
-- supports, see the
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_MetricDatum.html MetricDatum>
-- data type in the /Amazon CloudWatch API Reference/.
newCustomizedMetricSpecification ::
  CustomizedMetricSpecification
newCustomizedMetricSpecification =
  CustomizedMetricSpecification'
    { dimensions =
        Prelude.Nothing,
      metricName = Prelude.Nothing,
      metrics = Prelude.Nothing,
      namespace = Prelude.Nothing,
      statistic = Prelude.Nothing,
      unit = Prelude.Nothing
    }

-- | The dimensions of the metric.
--
-- Conditional: If you published your metric with dimensions, you must
-- specify the same dimensions in your scaling policy.
customizedMetricSpecification_dimensions :: Lens.Lens' CustomizedMetricSpecification (Prelude.Maybe [MetricDimension])
customizedMetricSpecification_dimensions = Lens.lens (\CustomizedMetricSpecification' {dimensions} -> dimensions) (\s@CustomizedMetricSpecification' {} a -> s {dimensions = a} :: CustomizedMetricSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The name of the metric. To get the exact metric name, namespace, and
-- dimensions, inspect the
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_Metric.html Metric>
-- object that is returned by a call to
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_ListMetrics.html ListMetrics>.
customizedMetricSpecification_metricName :: Lens.Lens' CustomizedMetricSpecification (Prelude.Maybe Prelude.Text)
customizedMetricSpecification_metricName = Lens.lens (\CustomizedMetricSpecification' {metricName} -> metricName) (\s@CustomizedMetricSpecification' {} a -> s {metricName = a} :: CustomizedMetricSpecification)

-- | The metrics to include in the target tracking scaling policy, as a
-- metric data query. This can include both raw metric and metric math
-- expressions.
customizedMetricSpecification_metrics :: Lens.Lens' CustomizedMetricSpecification (Prelude.Maybe [TargetTrackingMetricDataQuery])
customizedMetricSpecification_metrics = Lens.lens (\CustomizedMetricSpecification' {metrics} -> metrics) (\s@CustomizedMetricSpecification' {} a -> s {metrics = a} :: CustomizedMetricSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The namespace of the metric.
customizedMetricSpecification_namespace :: Lens.Lens' CustomizedMetricSpecification (Prelude.Maybe Prelude.Text)
customizedMetricSpecification_namespace = Lens.lens (\CustomizedMetricSpecification' {namespace} -> namespace) (\s@CustomizedMetricSpecification' {} a -> s {namespace = a} :: CustomizedMetricSpecification)

-- | The statistic of the metric.
customizedMetricSpecification_statistic :: Lens.Lens' CustomizedMetricSpecification (Prelude.Maybe MetricStatistic)
customizedMetricSpecification_statistic = Lens.lens (\CustomizedMetricSpecification' {statistic} -> statistic) (\s@CustomizedMetricSpecification' {} a -> s {statistic = a} :: CustomizedMetricSpecification)

-- | The unit of the metric. For a complete list of the units that CloudWatch
-- supports, see the
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_MetricDatum.html MetricDatum>
-- data type in the /Amazon CloudWatch API Reference/.
customizedMetricSpecification_unit :: Lens.Lens' CustomizedMetricSpecification (Prelude.Maybe Prelude.Text)
customizedMetricSpecification_unit = Lens.lens (\CustomizedMetricSpecification' {unit} -> unit) (\s@CustomizedMetricSpecification' {} a -> s {unit = a} :: CustomizedMetricSpecification)

instance Data.FromXML CustomizedMetricSpecification where
  parseXML x =
    CustomizedMetricSpecification'
      Prelude.<$> ( x Data..@? "Dimensions" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "MetricName")
      Prelude.<*> ( x Data..@? "Metrics" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "Namespace")
      Prelude.<*> (x Data..@? "Statistic")
      Prelude.<*> (x Data..@? "Unit")

instance
  Prelude.Hashable
    CustomizedMetricSpecification
  where
  hashWithSalt _salt CustomizedMetricSpecification' {..} =
    _salt `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` metrics
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` statistic
      `Prelude.hashWithSalt` unit

instance Prelude.NFData CustomizedMetricSpecification where
  rnf CustomizedMetricSpecification' {..} =
    Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf statistic
      `Prelude.seq` Prelude.rnf unit

instance Data.ToQuery CustomizedMetricSpecification where
  toQuery CustomizedMetricSpecification' {..} =
    Prelude.mconcat
      [ "Dimensions"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> dimensions),
        "MetricName" Data.=: metricName,
        "Metrics"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> metrics),
        "Namespace" Data.=: namespace,
        "Statistic" Data.=: statistic,
        "Unit" Data.=: unit
      ]
