{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudWatch.PutAnomalyDetector
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an anomaly detection model for a CloudWatch metric. You can use
-- the model to display a band of expected normal values when the metric is
-- graphed.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Anomaly_Detection.html CloudWatch Anomaly Detection>.
module Amazonka.CloudWatch.PutAnomalyDetector
  ( -- * Creating a Request
    PutAnomalyDetector (..),
    newPutAnomalyDetector,

    -- * Request Lenses
    putAnomalyDetector_singleMetricAnomalyDetector,
    putAnomalyDetector_configuration,
    putAnomalyDetector_dimensions,
    putAnomalyDetector_metricMathAnomalyDetector,
    putAnomalyDetector_stat,
    putAnomalyDetector_metricName,
    putAnomalyDetector_namespace,

    -- * Destructuring the Response
    PutAnomalyDetectorResponse (..),
    newPutAnomalyDetectorResponse,

    -- * Response Lenses
    putAnomalyDetectorResponse_httpStatus,
  )
where

import Amazonka.CloudWatch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutAnomalyDetector' smart constructor.
data PutAnomalyDetector = PutAnomalyDetector'
  { -- | A single metric anomaly detector to be created.
    --
    -- When using @SingleMetricAnomalyDetector@, you cannot include the
    -- following parameters in the same operation:
    --
    -- -   @Dimensions@
    --
    -- -   @MetricName@
    --
    -- -   @Namespace@
    --
    -- -   @Stat@
    --
    -- -   the @MetricMatchAnomalyDetector@ parameters of
    --     @PutAnomalyDetectorInput@
    --
    -- Instead, specify the single metric anomaly detector attributes as part
    -- of the property @SingleMetricAnomalyDetector@.
    singleMetricAnomalyDetector :: Prelude.Maybe SingleMetricAnomalyDetector,
    -- | The configuration specifies details about how the anomaly detection
    -- model is to be trained, including time ranges to exclude when training
    -- and updating the model. You can specify as many as 10 time ranges.
    --
    -- The configuration can also include the time zone to use for the metric.
    configuration :: Prelude.Maybe AnomalyDetectorConfiguration,
    -- | The metric dimensions to create the anomaly detection model for.
    dimensions :: Prelude.Maybe [Dimension],
    -- | The metric math anomaly detector to be created.
    --
    -- When using @MetricMathAnomalyDetector@, you cannot include the following
    -- parameters in the same operation:
    --
    -- -   @Dimensions@
    --
    -- -   @MetricName@
    --
    -- -   @Namespace@
    --
    -- -   @Stat@
    --
    -- -   the @SingleMetricAnomalyDetector@ parameters of
    --     @PutAnomalyDetectorInput@
    --
    -- Instead, specify the metric math anomaly detector attributes as part of
    -- the property @MetricMathAnomalyDetector@.
    metricMathAnomalyDetector :: Prelude.Maybe MetricMathAnomalyDetector,
    -- | The statistic to use for the metric and the anomaly detection model.
    stat :: Prelude.Maybe Prelude.Text,
    -- | The name of the metric to create the anomaly detection model for.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The namespace of the metric to create the anomaly detection model for.
    namespace :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAnomalyDetector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'singleMetricAnomalyDetector', 'putAnomalyDetector_singleMetricAnomalyDetector' - A single metric anomaly detector to be created.
--
-- When using @SingleMetricAnomalyDetector@, you cannot include the
-- following parameters in the same operation:
--
-- -   @Dimensions@
--
-- -   @MetricName@
--
-- -   @Namespace@
--
-- -   @Stat@
--
-- -   the @MetricMatchAnomalyDetector@ parameters of
--     @PutAnomalyDetectorInput@
--
-- Instead, specify the single metric anomaly detector attributes as part
-- of the property @SingleMetricAnomalyDetector@.
--
-- 'configuration', 'putAnomalyDetector_configuration' - The configuration specifies details about how the anomaly detection
-- model is to be trained, including time ranges to exclude when training
-- and updating the model. You can specify as many as 10 time ranges.
--
-- The configuration can also include the time zone to use for the metric.
--
-- 'dimensions', 'putAnomalyDetector_dimensions' - The metric dimensions to create the anomaly detection model for.
--
-- 'metricMathAnomalyDetector', 'putAnomalyDetector_metricMathAnomalyDetector' - The metric math anomaly detector to be created.
--
-- When using @MetricMathAnomalyDetector@, you cannot include the following
-- parameters in the same operation:
--
-- -   @Dimensions@
--
-- -   @MetricName@
--
-- -   @Namespace@
--
-- -   @Stat@
--
-- -   the @SingleMetricAnomalyDetector@ parameters of
--     @PutAnomalyDetectorInput@
--
-- Instead, specify the metric math anomaly detector attributes as part of
-- the property @MetricMathAnomalyDetector@.
--
-- 'stat', 'putAnomalyDetector_stat' - The statistic to use for the metric and the anomaly detection model.
--
-- 'metricName', 'putAnomalyDetector_metricName' - The name of the metric to create the anomaly detection model for.
--
-- 'namespace', 'putAnomalyDetector_namespace' - The namespace of the metric to create the anomaly detection model for.
newPutAnomalyDetector ::
  PutAnomalyDetector
newPutAnomalyDetector =
  PutAnomalyDetector'
    { singleMetricAnomalyDetector =
        Prelude.Nothing,
      configuration = Prelude.Nothing,
      dimensions = Prelude.Nothing,
      metricMathAnomalyDetector = Prelude.Nothing,
      stat = Prelude.Nothing,
      metricName = Prelude.Nothing,
      namespace = Prelude.Nothing
    }

-- | A single metric anomaly detector to be created.
--
-- When using @SingleMetricAnomalyDetector@, you cannot include the
-- following parameters in the same operation:
--
-- -   @Dimensions@
--
-- -   @MetricName@
--
-- -   @Namespace@
--
-- -   @Stat@
--
-- -   the @MetricMatchAnomalyDetector@ parameters of
--     @PutAnomalyDetectorInput@
--
-- Instead, specify the single metric anomaly detector attributes as part
-- of the property @SingleMetricAnomalyDetector@.
putAnomalyDetector_singleMetricAnomalyDetector :: Lens.Lens' PutAnomalyDetector (Prelude.Maybe SingleMetricAnomalyDetector)
putAnomalyDetector_singleMetricAnomalyDetector = Lens.lens (\PutAnomalyDetector' {singleMetricAnomalyDetector} -> singleMetricAnomalyDetector) (\s@PutAnomalyDetector' {} a -> s {singleMetricAnomalyDetector = a} :: PutAnomalyDetector)

-- | The configuration specifies details about how the anomaly detection
-- model is to be trained, including time ranges to exclude when training
-- and updating the model. You can specify as many as 10 time ranges.
--
-- The configuration can also include the time zone to use for the metric.
putAnomalyDetector_configuration :: Lens.Lens' PutAnomalyDetector (Prelude.Maybe AnomalyDetectorConfiguration)
putAnomalyDetector_configuration = Lens.lens (\PutAnomalyDetector' {configuration} -> configuration) (\s@PutAnomalyDetector' {} a -> s {configuration = a} :: PutAnomalyDetector)

-- | The metric dimensions to create the anomaly detection model for.
putAnomalyDetector_dimensions :: Lens.Lens' PutAnomalyDetector (Prelude.Maybe [Dimension])
putAnomalyDetector_dimensions = Lens.lens (\PutAnomalyDetector' {dimensions} -> dimensions) (\s@PutAnomalyDetector' {} a -> s {dimensions = a} :: PutAnomalyDetector) Prelude.. Lens.mapping Lens.coerced

-- | The metric math anomaly detector to be created.
--
-- When using @MetricMathAnomalyDetector@, you cannot include the following
-- parameters in the same operation:
--
-- -   @Dimensions@
--
-- -   @MetricName@
--
-- -   @Namespace@
--
-- -   @Stat@
--
-- -   the @SingleMetricAnomalyDetector@ parameters of
--     @PutAnomalyDetectorInput@
--
-- Instead, specify the metric math anomaly detector attributes as part of
-- the property @MetricMathAnomalyDetector@.
putAnomalyDetector_metricMathAnomalyDetector :: Lens.Lens' PutAnomalyDetector (Prelude.Maybe MetricMathAnomalyDetector)
putAnomalyDetector_metricMathAnomalyDetector = Lens.lens (\PutAnomalyDetector' {metricMathAnomalyDetector} -> metricMathAnomalyDetector) (\s@PutAnomalyDetector' {} a -> s {metricMathAnomalyDetector = a} :: PutAnomalyDetector)

-- | The statistic to use for the metric and the anomaly detection model.
putAnomalyDetector_stat :: Lens.Lens' PutAnomalyDetector (Prelude.Maybe Prelude.Text)
putAnomalyDetector_stat = Lens.lens (\PutAnomalyDetector' {stat} -> stat) (\s@PutAnomalyDetector' {} a -> s {stat = a} :: PutAnomalyDetector)

-- | The name of the metric to create the anomaly detection model for.
putAnomalyDetector_metricName :: Lens.Lens' PutAnomalyDetector (Prelude.Maybe Prelude.Text)
putAnomalyDetector_metricName = Lens.lens (\PutAnomalyDetector' {metricName} -> metricName) (\s@PutAnomalyDetector' {} a -> s {metricName = a} :: PutAnomalyDetector)

-- | The namespace of the metric to create the anomaly detection model for.
putAnomalyDetector_namespace :: Lens.Lens' PutAnomalyDetector (Prelude.Maybe Prelude.Text)
putAnomalyDetector_namespace = Lens.lens (\PutAnomalyDetector' {namespace} -> namespace) (\s@PutAnomalyDetector' {} a -> s {namespace = a} :: PutAnomalyDetector)

instance Core.AWSRequest PutAnomalyDetector where
  type
    AWSResponse PutAnomalyDetector =
      PutAnomalyDetectorResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "PutAnomalyDetectorResult"
      ( \s h x ->
          PutAnomalyDetectorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutAnomalyDetector where
  hashWithSalt _salt PutAnomalyDetector' {..} =
    _salt
      `Prelude.hashWithSalt` singleMetricAnomalyDetector
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` metricMathAnomalyDetector
      `Prelude.hashWithSalt` stat
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData PutAnomalyDetector where
  rnf PutAnomalyDetector' {..} =
    Prelude.rnf singleMetricAnomalyDetector
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf metricMathAnomalyDetector
      `Prelude.seq` Prelude.rnf stat
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf namespace

instance Data.ToHeaders PutAnomalyDetector where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath PutAnomalyDetector where
  toPath = Prelude.const "/"

instance Data.ToQuery PutAnomalyDetector where
  toQuery PutAnomalyDetector' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("PutAnomalyDetector" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-08-01" :: Prelude.ByteString),
        "SingleMetricAnomalyDetector"
          Data.=: singleMetricAnomalyDetector,
        "Configuration" Data.=: configuration,
        "Dimensions"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> dimensions),
        "MetricMathAnomalyDetector"
          Data.=: metricMathAnomalyDetector,
        "Stat" Data.=: stat,
        "MetricName" Data.=: metricName,
        "Namespace" Data.=: namespace
      ]

-- | /See:/ 'newPutAnomalyDetectorResponse' smart constructor.
data PutAnomalyDetectorResponse = PutAnomalyDetectorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAnomalyDetectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putAnomalyDetectorResponse_httpStatus' - The response's http status code.
newPutAnomalyDetectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutAnomalyDetectorResponse
newPutAnomalyDetectorResponse pHttpStatus_ =
  PutAnomalyDetectorResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putAnomalyDetectorResponse_httpStatus :: Lens.Lens' PutAnomalyDetectorResponse Prelude.Int
putAnomalyDetectorResponse_httpStatus = Lens.lens (\PutAnomalyDetectorResponse' {httpStatus} -> httpStatus) (\s@PutAnomalyDetectorResponse' {} a -> s {httpStatus = a} :: PutAnomalyDetectorResponse)

instance Prelude.NFData PutAnomalyDetectorResponse where
  rnf PutAnomalyDetectorResponse' {..} =
    Prelude.rnf httpStatus
