{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudWatch.PutAnomalyDetector
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.CloudWatch.PutAnomalyDetector
  ( -- * Creating a Request
    PutAnomalyDetector (..),
    newPutAnomalyDetector,

    -- * Request Lenses
    putAnomalyDetector_configuration,
    putAnomalyDetector_dimensions,
    putAnomalyDetector_namespace,
    putAnomalyDetector_metricName,
    putAnomalyDetector_stat,

    -- * Destructuring the Response
    PutAnomalyDetectorResponse (..),
    newPutAnomalyDetectorResponse,

    -- * Response Lenses
    putAnomalyDetectorResponse_httpStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutAnomalyDetector' smart constructor.
data PutAnomalyDetector = PutAnomalyDetector'
  { -- | The configuration specifies details about how the anomaly detection
    -- model is to be trained, including time ranges to exclude when training
    -- and updating the model. You can specify as many as 10 time ranges.
    --
    -- The configuration can also include the time zone to use for the metric.
    configuration :: Prelude.Maybe AnomalyDetectorConfiguration,
    -- | The metric dimensions to create the anomaly detection model for.
    dimensions :: Prelude.Maybe [Dimension],
    -- | The namespace of the metric to create the anomaly detection model for.
    namespace :: Prelude.Text,
    -- | The name of the metric to create the anomaly detection model for.
    metricName :: Prelude.Text,
    -- | The statistic to use for the metric and the anomaly detection model.
    stat :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutAnomalyDetector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'putAnomalyDetector_configuration' - The configuration specifies details about how the anomaly detection
-- model is to be trained, including time ranges to exclude when training
-- and updating the model. You can specify as many as 10 time ranges.
--
-- The configuration can also include the time zone to use for the metric.
--
-- 'dimensions', 'putAnomalyDetector_dimensions' - The metric dimensions to create the anomaly detection model for.
--
-- 'namespace', 'putAnomalyDetector_namespace' - The namespace of the metric to create the anomaly detection model for.
--
-- 'metricName', 'putAnomalyDetector_metricName' - The name of the metric to create the anomaly detection model for.
--
-- 'stat', 'putAnomalyDetector_stat' - The statistic to use for the metric and the anomaly detection model.
newPutAnomalyDetector ::
  -- | 'namespace'
  Prelude.Text ->
  -- | 'metricName'
  Prelude.Text ->
  -- | 'stat'
  Prelude.Text ->
  PutAnomalyDetector
newPutAnomalyDetector pNamespace_ pMetricName_ pStat_ =
  PutAnomalyDetector'
    { configuration =
        Prelude.Nothing,
      dimensions = Prelude.Nothing,
      namespace = pNamespace_,
      metricName = pMetricName_,
      stat = pStat_
    }

-- | The configuration specifies details about how the anomaly detection
-- model is to be trained, including time ranges to exclude when training
-- and updating the model. You can specify as many as 10 time ranges.
--
-- The configuration can also include the time zone to use for the metric.
putAnomalyDetector_configuration :: Lens.Lens' PutAnomalyDetector (Prelude.Maybe AnomalyDetectorConfiguration)
putAnomalyDetector_configuration = Lens.lens (\PutAnomalyDetector' {configuration} -> configuration) (\s@PutAnomalyDetector' {} a -> s {configuration = a} :: PutAnomalyDetector)

-- | The metric dimensions to create the anomaly detection model for.
putAnomalyDetector_dimensions :: Lens.Lens' PutAnomalyDetector (Prelude.Maybe [Dimension])
putAnomalyDetector_dimensions = Lens.lens (\PutAnomalyDetector' {dimensions} -> dimensions) (\s@PutAnomalyDetector' {} a -> s {dimensions = a} :: PutAnomalyDetector) Prelude.. Lens.mapping Prelude._Coerce

-- | The namespace of the metric to create the anomaly detection model for.
putAnomalyDetector_namespace :: Lens.Lens' PutAnomalyDetector Prelude.Text
putAnomalyDetector_namespace = Lens.lens (\PutAnomalyDetector' {namespace} -> namespace) (\s@PutAnomalyDetector' {} a -> s {namespace = a} :: PutAnomalyDetector)

-- | The name of the metric to create the anomaly detection model for.
putAnomalyDetector_metricName :: Lens.Lens' PutAnomalyDetector Prelude.Text
putAnomalyDetector_metricName = Lens.lens (\PutAnomalyDetector' {metricName} -> metricName) (\s@PutAnomalyDetector' {} a -> s {metricName = a} :: PutAnomalyDetector)

-- | The statistic to use for the metric and the anomaly detection model.
putAnomalyDetector_stat :: Lens.Lens' PutAnomalyDetector Prelude.Text
putAnomalyDetector_stat = Lens.lens (\PutAnomalyDetector' {stat} -> stat) (\s@PutAnomalyDetector' {} a -> s {stat = a} :: PutAnomalyDetector)

instance Prelude.AWSRequest PutAnomalyDetector where
  type
    Rs PutAnomalyDetector =
      PutAnomalyDetectorResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "PutAnomalyDetectorResult"
      ( \s h x ->
          PutAnomalyDetectorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutAnomalyDetector

instance Prelude.NFData PutAnomalyDetector

instance Prelude.ToHeaders PutAnomalyDetector where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath PutAnomalyDetector where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutAnomalyDetector where
  toQuery PutAnomalyDetector' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("PutAnomalyDetector" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-08-01" :: Prelude.ByteString),
        "Configuration" Prelude.=: configuration,
        "Dimensions"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> dimensions
            ),
        "Namespace" Prelude.=: namespace,
        "MetricName" Prelude.=: metricName,
        "Stat" Prelude.=: stat
      ]

-- | /See:/ 'newPutAnomalyDetectorResponse' smart constructor.
data PutAnomalyDetectorResponse = PutAnomalyDetectorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData PutAnomalyDetectorResponse
