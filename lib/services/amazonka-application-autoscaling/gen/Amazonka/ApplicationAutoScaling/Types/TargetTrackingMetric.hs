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
-- Module      : Amazonka.ApplicationAutoScaling.Types.TargetTrackingMetric
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationAutoScaling.Types.TargetTrackingMetric where

import Amazonka.ApplicationAutoScaling.Types.TargetTrackingMetricDimension
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a specific metric.
--
-- Metric is a property of the TargetTrackingMetricStat object.
--
-- /See:/ 'newTargetTrackingMetric' smart constructor.
data TargetTrackingMetric = TargetTrackingMetric'
  { -- | The dimensions for the metric. For the list of available dimensions, see
    -- the Amazon Web Services documentation available from the table in
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/aws-services-cloudwatch-metrics.html Amazon Web Services services that publish CloudWatch metrics>
    -- in the /Amazon CloudWatch User Guide/.
    --
    -- Conditional: If you published your metric with dimensions, you must
    -- specify the same dimensions in your scaling policy.
    dimensions :: Prelude.Maybe [TargetTrackingMetricDimension],
    -- | The name of the metric.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The namespace of the metric. For more information, see the table in
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/aws-services-cloudwatch-metrics.html Amazon Web Services services that publish CloudWatch metrics>
    -- in the /Amazon CloudWatch User Guide/.
    namespace :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetTrackingMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensions', 'targetTrackingMetric_dimensions' - The dimensions for the metric. For the list of available dimensions, see
-- the Amazon Web Services documentation available from the table in
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/aws-services-cloudwatch-metrics.html Amazon Web Services services that publish CloudWatch metrics>
-- in the /Amazon CloudWatch User Guide/.
--
-- Conditional: If you published your metric with dimensions, you must
-- specify the same dimensions in your scaling policy.
--
-- 'metricName', 'targetTrackingMetric_metricName' - The name of the metric.
--
-- 'namespace', 'targetTrackingMetric_namespace' - The namespace of the metric. For more information, see the table in
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/aws-services-cloudwatch-metrics.html Amazon Web Services services that publish CloudWatch metrics>
-- in the /Amazon CloudWatch User Guide/.
newTargetTrackingMetric ::
  TargetTrackingMetric
newTargetTrackingMetric =
  TargetTrackingMetric'
    { dimensions = Prelude.Nothing,
      metricName = Prelude.Nothing,
      namespace = Prelude.Nothing
    }

-- | The dimensions for the metric. For the list of available dimensions, see
-- the Amazon Web Services documentation available from the table in
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/aws-services-cloudwatch-metrics.html Amazon Web Services services that publish CloudWatch metrics>
-- in the /Amazon CloudWatch User Guide/.
--
-- Conditional: If you published your metric with dimensions, you must
-- specify the same dimensions in your scaling policy.
targetTrackingMetric_dimensions :: Lens.Lens' TargetTrackingMetric (Prelude.Maybe [TargetTrackingMetricDimension])
targetTrackingMetric_dimensions = Lens.lens (\TargetTrackingMetric' {dimensions} -> dimensions) (\s@TargetTrackingMetric' {} a -> s {dimensions = a} :: TargetTrackingMetric) Prelude.. Lens.mapping Lens.coerced

-- | The name of the metric.
targetTrackingMetric_metricName :: Lens.Lens' TargetTrackingMetric (Prelude.Maybe Prelude.Text)
targetTrackingMetric_metricName = Lens.lens (\TargetTrackingMetric' {metricName} -> metricName) (\s@TargetTrackingMetric' {} a -> s {metricName = a} :: TargetTrackingMetric)

-- | The namespace of the metric. For more information, see the table in
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/aws-services-cloudwatch-metrics.html Amazon Web Services services that publish CloudWatch metrics>
-- in the /Amazon CloudWatch User Guide/.
targetTrackingMetric_namespace :: Lens.Lens' TargetTrackingMetric (Prelude.Maybe Prelude.Text)
targetTrackingMetric_namespace = Lens.lens (\TargetTrackingMetric' {namespace} -> namespace) (\s@TargetTrackingMetric' {} a -> s {namespace = a} :: TargetTrackingMetric)

instance Data.FromJSON TargetTrackingMetric where
  parseJSON =
    Data.withObject
      "TargetTrackingMetric"
      ( \x ->
          TargetTrackingMetric'
            Prelude.<$> (x Data..:? "Dimensions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "MetricName")
            Prelude.<*> (x Data..:? "Namespace")
      )

instance Prelude.Hashable TargetTrackingMetric where
  hashWithSalt _salt TargetTrackingMetric' {..} =
    _salt
      `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData TargetTrackingMetric where
  rnf TargetTrackingMetric' {..} =
    Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf namespace

instance Data.ToJSON TargetTrackingMetric where
  toJSON TargetTrackingMetric' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Dimensions" Data..=) Prelude.<$> dimensions,
            ("MetricName" Data..=) Prelude.<$> metricName,
            ("Namespace" Data..=) Prelude.<$> namespace
          ]
      )
