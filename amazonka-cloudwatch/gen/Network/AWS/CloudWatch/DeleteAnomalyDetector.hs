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
-- Module      : Network.AWS.CloudWatch.DeleteAnomalyDetector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified anomaly detection model from your account.
module Network.AWS.CloudWatch.DeleteAnomalyDetector
  ( -- * Creating a Request
    DeleteAnomalyDetector (..),
    newDeleteAnomalyDetector,

    -- * Request Lenses
    deleteAnomalyDetector_dimensions,
    deleteAnomalyDetector_namespace,
    deleteAnomalyDetector_metricName,
    deleteAnomalyDetector_stat,

    -- * Destructuring the Response
    DeleteAnomalyDetectorResponse (..),
    newDeleteAnomalyDetectorResponse,

    -- * Response Lenses
    deleteAnomalyDetectorResponse_httpStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteAnomalyDetector' smart constructor.
data DeleteAnomalyDetector = DeleteAnomalyDetector'
  { -- | The metric dimensions associated with the anomaly detection model to
    -- delete.
    dimensions :: Prelude.Maybe [Dimension],
    -- | The namespace associated with the anomaly detection model to delete.
    namespace :: Prelude.Text,
    -- | The metric name associated with the anomaly detection model to delete.
    metricName :: Prelude.Text,
    -- | The statistic associated with the anomaly detection model to delete.
    stat :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteAnomalyDetector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensions', 'deleteAnomalyDetector_dimensions' - The metric dimensions associated with the anomaly detection model to
-- delete.
--
-- 'namespace', 'deleteAnomalyDetector_namespace' - The namespace associated with the anomaly detection model to delete.
--
-- 'metricName', 'deleteAnomalyDetector_metricName' - The metric name associated with the anomaly detection model to delete.
--
-- 'stat', 'deleteAnomalyDetector_stat' - The statistic associated with the anomaly detection model to delete.
newDeleteAnomalyDetector ::
  -- | 'namespace'
  Prelude.Text ->
  -- | 'metricName'
  Prelude.Text ->
  -- | 'stat'
  Prelude.Text ->
  DeleteAnomalyDetector
newDeleteAnomalyDetector
  pNamespace_
  pMetricName_
  pStat_ =
    DeleteAnomalyDetector'
      { dimensions =
          Prelude.Nothing,
        namespace = pNamespace_,
        metricName = pMetricName_,
        stat = pStat_
      }

-- | The metric dimensions associated with the anomaly detection model to
-- delete.
deleteAnomalyDetector_dimensions :: Lens.Lens' DeleteAnomalyDetector (Prelude.Maybe [Dimension])
deleteAnomalyDetector_dimensions = Lens.lens (\DeleteAnomalyDetector' {dimensions} -> dimensions) (\s@DeleteAnomalyDetector' {} a -> s {dimensions = a} :: DeleteAnomalyDetector) Prelude.. Lens.mapping Prelude._Coerce

-- | The namespace associated with the anomaly detection model to delete.
deleteAnomalyDetector_namespace :: Lens.Lens' DeleteAnomalyDetector Prelude.Text
deleteAnomalyDetector_namespace = Lens.lens (\DeleteAnomalyDetector' {namespace} -> namespace) (\s@DeleteAnomalyDetector' {} a -> s {namespace = a} :: DeleteAnomalyDetector)

-- | The metric name associated with the anomaly detection model to delete.
deleteAnomalyDetector_metricName :: Lens.Lens' DeleteAnomalyDetector Prelude.Text
deleteAnomalyDetector_metricName = Lens.lens (\DeleteAnomalyDetector' {metricName} -> metricName) (\s@DeleteAnomalyDetector' {} a -> s {metricName = a} :: DeleteAnomalyDetector)

-- | The statistic associated with the anomaly detection model to delete.
deleteAnomalyDetector_stat :: Lens.Lens' DeleteAnomalyDetector Prelude.Text
deleteAnomalyDetector_stat = Lens.lens (\DeleteAnomalyDetector' {stat} -> stat) (\s@DeleteAnomalyDetector' {} a -> s {stat = a} :: DeleteAnomalyDetector)

instance Prelude.AWSRequest DeleteAnomalyDetector where
  type
    Rs DeleteAnomalyDetector =
      DeleteAnomalyDetectorResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteAnomalyDetectorResult"
      ( \s h x ->
          DeleteAnomalyDetectorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAnomalyDetector

instance Prelude.NFData DeleteAnomalyDetector

instance Prelude.ToHeaders DeleteAnomalyDetector where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteAnomalyDetector where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteAnomalyDetector where
  toQuery DeleteAnomalyDetector' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteAnomalyDetector" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-08-01" :: Prelude.ByteString),
        "Dimensions"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> dimensions
            ),
        "Namespace" Prelude.=: namespace,
        "MetricName" Prelude.=: metricName,
        "Stat" Prelude.=: stat
      ]

-- | /See:/ 'newDeleteAnomalyDetectorResponse' smart constructor.
data DeleteAnomalyDetectorResponse = DeleteAnomalyDetectorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteAnomalyDetectorResponse
