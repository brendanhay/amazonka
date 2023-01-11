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
-- Module      : Amazonka.AutoScaling.Types.Metric
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.Metric where

import Amazonka.AutoScaling.Types.MetricDimension
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a specific metric.
--
-- /See:/ 'newMetric' smart constructor.
data Metric = Metric'
  { -- | The dimensions for the metric. For the list of available dimensions, see
    -- the Amazon Web Services documentation available from the table in
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/aws-services-cloudwatch-metrics.html Amazon Web Services services that publish CloudWatch metrics>
    -- in the /Amazon CloudWatch User Guide/.
    --
    -- Conditional: If you published your metric with dimensions, you must
    -- specify the same dimensions in your scaling policy.
    dimensions :: Prelude.Maybe [MetricDimension],
    -- | The namespace of the metric. For more information, see the table in
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/aws-services-cloudwatch-metrics.html Amazon Web Services services that publish CloudWatch metrics>
    -- in the /Amazon CloudWatch User Guide/.
    namespace :: Prelude.Text,
    -- | The name of the metric.
    metricName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Metric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensions', 'metric_dimensions' - The dimensions for the metric. For the list of available dimensions, see
-- the Amazon Web Services documentation available from the table in
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/aws-services-cloudwatch-metrics.html Amazon Web Services services that publish CloudWatch metrics>
-- in the /Amazon CloudWatch User Guide/.
--
-- Conditional: If you published your metric with dimensions, you must
-- specify the same dimensions in your scaling policy.
--
-- 'namespace', 'metric_namespace' - The namespace of the metric. For more information, see the table in
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/aws-services-cloudwatch-metrics.html Amazon Web Services services that publish CloudWatch metrics>
-- in the /Amazon CloudWatch User Guide/.
--
-- 'metricName', 'metric_metricName' - The name of the metric.
newMetric ::
  -- | 'namespace'
  Prelude.Text ->
  -- | 'metricName'
  Prelude.Text ->
  Metric
newMetric pNamespace_ pMetricName_ =
  Metric'
    { dimensions = Prelude.Nothing,
      namespace = pNamespace_,
      metricName = pMetricName_
    }

-- | The dimensions for the metric. For the list of available dimensions, see
-- the Amazon Web Services documentation available from the table in
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/aws-services-cloudwatch-metrics.html Amazon Web Services services that publish CloudWatch metrics>
-- in the /Amazon CloudWatch User Guide/.
--
-- Conditional: If you published your metric with dimensions, you must
-- specify the same dimensions in your scaling policy.
metric_dimensions :: Lens.Lens' Metric (Prelude.Maybe [MetricDimension])
metric_dimensions = Lens.lens (\Metric' {dimensions} -> dimensions) (\s@Metric' {} a -> s {dimensions = a} :: Metric) Prelude.. Lens.mapping Lens.coerced

-- | The namespace of the metric. For more information, see the table in
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/aws-services-cloudwatch-metrics.html Amazon Web Services services that publish CloudWatch metrics>
-- in the /Amazon CloudWatch User Guide/.
metric_namespace :: Lens.Lens' Metric Prelude.Text
metric_namespace = Lens.lens (\Metric' {namespace} -> namespace) (\s@Metric' {} a -> s {namespace = a} :: Metric)

-- | The name of the metric.
metric_metricName :: Lens.Lens' Metric Prelude.Text
metric_metricName = Lens.lens (\Metric' {metricName} -> metricName) (\s@Metric' {} a -> s {metricName = a} :: Metric)

instance Data.FromXML Metric where
  parseXML x =
    Metric'
      Prelude.<$> ( x Data..@? "Dimensions" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@ "Namespace")
      Prelude.<*> (x Data..@ "MetricName")

instance Prelude.Hashable Metric where
  hashWithSalt _salt Metric' {..} =
    _salt `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` metricName

instance Prelude.NFData Metric where
  rnf Metric' {..} =
    Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf metricName

instance Data.ToQuery Metric where
  toQuery Metric' {..} =
    Prelude.mconcat
      [ "Dimensions"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> dimensions),
        "Namespace" Data.=: namespace,
        "MetricName" Data.=: metricName
      ]
