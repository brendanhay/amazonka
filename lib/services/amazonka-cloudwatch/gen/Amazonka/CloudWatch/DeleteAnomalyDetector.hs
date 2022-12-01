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
-- Module      : Amazonka.CloudWatch.DeleteAnomalyDetector
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified anomaly detection model from your account. For
-- more information about how to delete an anomaly detection model, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/Create_Anomaly_Detection_Alarm.html#Delete_Anomaly_Detection_Model Deleting an anomaly detection model>
-- in the /CloudWatch User Guide/.
module Amazonka.CloudWatch.DeleteAnomalyDetector
  ( -- * Creating a Request
    DeleteAnomalyDetector (..),
    newDeleteAnomalyDetector,

    -- * Request Lenses
    deleteAnomalyDetector_singleMetricAnomalyDetector,
    deleteAnomalyDetector_dimensions,
    deleteAnomalyDetector_metricMathAnomalyDetector,
    deleteAnomalyDetector_stat,
    deleteAnomalyDetector_metricName,
    deleteAnomalyDetector_namespace,

    -- * Destructuring the Response
    DeleteAnomalyDetectorResponse (..),
    newDeleteAnomalyDetectorResponse,

    -- * Response Lenses
    deleteAnomalyDetectorResponse_httpStatus,
  )
where

import Amazonka.CloudWatch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAnomalyDetector' smart constructor.
data DeleteAnomalyDetector = DeleteAnomalyDetector'
  { -- | A single metric anomaly detector to be deleted.
    --
    -- When using @SingleMetricAnomalyDetector@, you cannot include the
    -- following parameters in the same operation:
    --
    -- -   @Dimensions@,
    --
    -- -   @MetricName@
    --
    -- -   @Namespace@
    --
    -- -   @Stat@
    --
    -- -   the @MetricMathAnomalyDetector@ parameters of
    --     @DeleteAnomalyDetectorInput@
    --
    -- Instead, specify the single metric anomaly detector attributes as part
    -- of the @SingleMetricAnomalyDetector@ property.
    singleMetricAnomalyDetector :: Prelude.Maybe SingleMetricAnomalyDetector,
    -- | The metric dimensions associated with the anomaly detection model to
    -- delete.
    dimensions :: Prelude.Maybe [Dimension],
    -- | The metric math anomaly detector to be deleted.
    --
    -- When using @MetricMathAnomalyDetector@, you cannot include following
    -- parameters in the same operation:
    --
    -- -   @Dimensions@,
    --
    -- -   @MetricName@
    --
    -- -   @Namespace@
    --
    -- -   @Stat@
    --
    -- -   the @SingleMetricAnomalyDetector@ parameters of
    --     @DeleteAnomalyDetectorInput@
    --
    -- Instead, specify the metric math anomaly detector attributes as part of
    -- the @MetricMathAnomalyDetector@ property.
    metricMathAnomalyDetector :: Prelude.Maybe MetricMathAnomalyDetector,
    -- | The statistic associated with the anomaly detection model to delete.
    stat :: Prelude.Maybe Prelude.Text,
    -- | The metric name associated with the anomaly detection model to delete.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The namespace associated with the anomaly detection model to delete.
    namespace :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAnomalyDetector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'singleMetricAnomalyDetector', 'deleteAnomalyDetector_singleMetricAnomalyDetector' - A single metric anomaly detector to be deleted.
--
-- When using @SingleMetricAnomalyDetector@, you cannot include the
-- following parameters in the same operation:
--
-- -   @Dimensions@,
--
-- -   @MetricName@
--
-- -   @Namespace@
--
-- -   @Stat@
--
-- -   the @MetricMathAnomalyDetector@ parameters of
--     @DeleteAnomalyDetectorInput@
--
-- Instead, specify the single metric anomaly detector attributes as part
-- of the @SingleMetricAnomalyDetector@ property.
--
-- 'dimensions', 'deleteAnomalyDetector_dimensions' - The metric dimensions associated with the anomaly detection model to
-- delete.
--
-- 'metricMathAnomalyDetector', 'deleteAnomalyDetector_metricMathAnomalyDetector' - The metric math anomaly detector to be deleted.
--
-- When using @MetricMathAnomalyDetector@, you cannot include following
-- parameters in the same operation:
--
-- -   @Dimensions@,
--
-- -   @MetricName@
--
-- -   @Namespace@
--
-- -   @Stat@
--
-- -   the @SingleMetricAnomalyDetector@ parameters of
--     @DeleteAnomalyDetectorInput@
--
-- Instead, specify the metric math anomaly detector attributes as part of
-- the @MetricMathAnomalyDetector@ property.
--
-- 'stat', 'deleteAnomalyDetector_stat' - The statistic associated with the anomaly detection model to delete.
--
-- 'metricName', 'deleteAnomalyDetector_metricName' - The metric name associated with the anomaly detection model to delete.
--
-- 'namespace', 'deleteAnomalyDetector_namespace' - The namespace associated with the anomaly detection model to delete.
newDeleteAnomalyDetector ::
  DeleteAnomalyDetector
newDeleteAnomalyDetector =
  DeleteAnomalyDetector'
    { singleMetricAnomalyDetector =
        Prelude.Nothing,
      dimensions = Prelude.Nothing,
      metricMathAnomalyDetector = Prelude.Nothing,
      stat = Prelude.Nothing,
      metricName = Prelude.Nothing,
      namespace = Prelude.Nothing
    }

-- | A single metric anomaly detector to be deleted.
--
-- When using @SingleMetricAnomalyDetector@, you cannot include the
-- following parameters in the same operation:
--
-- -   @Dimensions@,
--
-- -   @MetricName@
--
-- -   @Namespace@
--
-- -   @Stat@
--
-- -   the @MetricMathAnomalyDetector@ parameters of
--     @DeleteAnomalyDetectorInput@
--
-- Instead, specify the single metric anomaly detector attributes as part
-- of the @SingleMetricAnomalyDetector@ property.
deleteAnomalyDetector_singleMetricAnomalyDetector :: Lens.Lens' DeleteAnomalyDetector (Prelude.Maybe SingleMetricAnomalyDetector)
deleteAnomalyDetector_singleMetricAnomalyDetector = Lens.lens (\DeleteAnomalyDetector' {singleMetricAnomalyDetector} -> singleMetricAnomalyDetector) (\s@DeleteAnomalyDetector' {} a -> s {singleMetricAnomalyDetector = a} :: DeleteAnomalyDetector)

-- | The metric dimensions associated with the anomaly detection model to
-- delete.
deleteAnomalyDetector_dimensions :: Lens.Lens' DeleteAnomalyDetector (Prelude.Maybe [Dimension])
deleteAnomalyDetector_dimensions = Lens.lens (\DeleteAnomalyDetector' {dimensions} -> dimensions) (\s@DeleteAnomalyDetector' {} a -> s {dimensions = a} :: DeleteAnomalyDetector) Prelude.. Lens.mapping Lens.coerced

-- | The metric math anomaly detector to be deleted.
--
-- When using @MetricMathAnomalyDetector@, you cannot include following
-- parameters in the same operation:
--
-- -   @Dimensions@,
--
-- -   @MetricName@
--
-- -   @Namespace@
--
-- -   @Stat@
--
-- -   the @SingleMetricAnomalyDetector@ parameters of
--     @DeleteAnomalyDetectorInput@
--
-- Instead, specify the metric math anomaly detector attributes as part of
-- the @MetricMathAnomalyDetector@ property.
deleteAnomalyDetector_metricMathAnomalyDetector :: Lens.Lens' DeleteAnomalyDetector (Prelude.Maybe MetricMathAnomalyDetector)
deleteAnomalyDetector_metricMathAnomalyDetector = Lens.lens (\DeleteAnomalyDetector' {metricMathAnomalyDetector} -> metricMathAnomalyDetector) (\s@DeleteAnomalyDetector' {} a -> s {metricMathAnomalyDetector = a} :: DeleteAnomalyDetector)

-- | The statistic associated with the anomaly detection model to delete.
deleteAnomalyDetector_stat :: Lens.Lens' DeleteAnomalyDetector (Prelude.Maybe Prelude.Text)
deleteAnomalyDetector_stat = Lens.lens (\DeleteAnomalyDetector' {stat} -> stat) (\s@DeleteAnomalyDetector' {} a -> s {stat = a} :: DeleteAnomalyDetector)

-- | The metric name associated with the anomaly detection model to delete.
deleteAnomalyDetector_metricName :: Lens.Lens' DeleteAnomalyDetector (Prelude.Maybe Prelude.Text)
deleteAnomalyDetector_metricName = Lens.lens (\DeleteAnomalyDetector' {metricName} -> metricName) (\s@DeleteAnomalyDetector' {} a -> s {metricName = a} :: DeleteAnomalyDetector)

-- | The namespace associated with the anomaly detection model to delete.
deleteAnomalyDetector_namespace :: Lens.Lens' DeleteAnomalyDetector (Prelude.Maybe Prelude.Text)
deleteAnomalyDetector_namespace = Lens.lens (\DeleteAnomalyDetector' {namespace} -> namespace) (\s@DeleteAnomalyDetector' {} a -> s {namespace = a} :: DeleteAnomalyDetector)

instance Core.AWSRequest DeleteAnomalyDetector where
  type
    AWSResponse DeleteAnomalyDetector =
      DeleteAnomalyDetectorResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteAnomalyDetectorResult"
      ( \s h x ->
          DeleteAnomalyDetectorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAnomalyDetector where
  hashWithSalt _salt DeleteAnomalyDetector' {..} =
    _salt
      `Prelude.hashWithSalt` singleMetricAnomalyDetector
      `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` metricMathAnomalyDetector
      `Prelude.hashWithSalt` stat
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData DeleteAnomalyDetector where
  rnf DeleteAnomalyDetector' {..} =
    Prelude.rnf singleMetricAnomalyDetector
      `Prelude.seq` Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf metricMathAnomalyDetector
      `Prelude.seq` Prelude.rnf stat
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf namespace

instance Core.ToHeaders DeleteAnomalyDetector where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteAnomalyDetector where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteAnomalyDetector where
  toQuery DeleteAnomalyDetector' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeleteAnomalyDetector" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-08-01" :: Prelude.ByteString),
        "SingleMetricAnomalyDetector"
          Core.=: singleMetricAnomalyDetector,
        "Dimensions"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> dimensions),
        "MetricMathAnomalyDetector"
          Core.=: metricMathAnomalyDetector,
        "Stat" Core.=: stat,
        "MetricName" Core.=: metricName,
        "Namespace" Core.=: namespace
      ]

-- | /See:/ 'newDeleteAnomalyDetectorResponse' smart constructor.
data DeleteAnomalyDetectorResponse = DeleteAnomalyDetectorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAnomalyDetectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAnomalyDetectorResponse_httpStatus' - The response's http status code.
newDeleteAnomalyDetectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAnomalyDetectorResponse
newDeleteAnomalyDetectorResponse pHttpStatus_ =
  DeleteAnomalyDetectorResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteAnomalyDetectorResponse_httpStatus :: Lens.Lens' DeleteAnomalyDetectorResponse Prelude.Int
deleteAnomalyDetectorResponse_httpStatus = Lens.lens (\DeleteAnomalyDetectorResponse' {httpStatus} -> httpStatus) (\s@DeleteAnomalyDetectorResponse' {} a -> s {httpStatus = a} :: DeleteAnomalyDetectorResponse)

instance Prelude.NFData DeleteAnomalyDetectorResponse where
  rnf DeleteAnomalyDetectorResponse' {..} =
    Prelude.rnf httpStatus
