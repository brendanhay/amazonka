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
-- Module      : Amazonka.Kafka.UpdateMonitoring
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the monitoring settings for the cluster. You can use this
-- operation to specify which Apache Kafka metrics you want Amazon MSK to
-- send to Amazon CloudWatch. You can also specify settings for open
-- monitoring with Prometheus.
module Amazonka.Kafka.UpdateMonitoring
  ( -- * Creating a Request
    UpdateMonitoring (..),
    newUpdateMonitoring,

    -- * Request Lenses
    updateMonitoring_enhancedMonitoring,
    updateMonitoring_loggingInfo,
    updateMonitoring_openMonitoring,
    updateMonitoring_clusterArn,
    updateMonitoring_currentVersion,

    -- * Destructuring the Response
    UpdateMonitoringResponse (..),
    newUpdateMonitoringResponse,

    -- * Response Lenses
    updateMonitoringResponse_clusterArn,
    updateMonitoringResponse_clusterOperationArn,
    updateMonitoringResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request body for UpdateMonitoring.
--
-- /See:/ 'newUpdateMonitoring' smart constructor.
data UpdateMonitoring = UpdateMonitoring'
  { -- | Specifies which Apache Kafka metrics Amazon MSK gathers and sends to
    -- Amazon CloudWatch for this cluster.
    enhancedMonitoring :: Prelude.Maybe EnhancedMonitoring,
    loggingInfo :: Prelude.Maybe LoggingInfo,
    -- | The settings for open monitoring.
    openMonitoring :: Prelude.Maybe OpenMonitoringInfo,
    -- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
    clusterArn :: Prelude.Text,
    -- | The version of the MSK cluster to update. Cluster versions aren\'t
    -- simple numbers. You can describe an MSK cluster to find its version.
    -- When this update operation is successful, it generates a new cluster
    -- version.
    currentVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMonitoring' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enhancedMonitoring', 'updateMonitoring_enhancedMonitoring' - Specifies which Apache Kafka metrics Amazon MSK gathers and sends to
-- Amazon CloudWatch for this cluster.
--
-- 'loggingInfo', 'updateMonitoring_loggingInfo' - Undocumented member.
--
-- 'openMonitoring', 'updateMonitoring_openMonitoring' - The settings for open monitoring.
--
-- 'clusterArn', 'updateMonitoring_clusterArn' - The Amazon Resource Name (ARN) that uniquely identifies the cluster.
--
-- 'currentVersion', 'updateMonitoring_currentVersion' - The version of the MSK cluster to update. Cluster versions aren\'t
-- simple numbers. You can describe an MSK cluster to find its version.
-- When this update operation is successful, it generates a new cluster
-- version.
newUpdateMonitoring ::
  -- | 'clusterArn'
  Prelude.Text ->
  -- | 'currentVersion'
  Prelude.Text ->
  UpdateMonitoring
newUpdateMonitoring pClusterArn_ pCurrentVersion_ =
  UpdateMonitoring'
    { enhancedMonitoring =
        Prelude.Nothing,
      loggingInfo = Prelude.Nothing,
      openMonitoring = Prelude.Nothing,
      clusterArn = pClusterArn_,
      currentVersion = pCurrentVersion_
    }

-- | Specifies which Apache Kafka metrics Amazon MSK gathers and sends to
-- Amazon CloudWatch for this cluster.
updateMonitoring_enhancedMonitoring :: Lens.Lens' UpdateMonitoring (Prelude.Maybe EnhancedMonitoring)
updateMonitoring_enhancedMonitoring = Lens.lens (\UpdateMonitoring' {enhancedMonitoring} -> enhancedMonitoring) (\s@UpdateMonitoring' {} a -> s {enhancedMonitoring = a} :: UpdateMonitoring)

-- | Undocumented member.
updateMonitoring_loggingInfo :: Lens.Lens' UpdateMonitoring (Prelude.Maybe LoggingInfo)
updateMonitoring_loggingInfo = Lens.lens (\UpdateMonitoring' {loggingInfo} -> loggingInfo) (\s@UpdateMonitoring' {} a -> s {loggingInfo = a} :: UpdateMonitoring)

-- | The settings for open monitoring.
updateMonitoring_openMonitoring :: Lens.Lens' UpdateMonitoring (Prelude.Maybe OpenMonitoringInfo)
updateMonitoring_openMonitoring = Lens.lens (\UpdateMonitoring' {openMonitoring} -> openMonitoring) (\s@UpdateMonitoring' {} a -> s {openMonitoring = a} :: UpdateMonitoring)

-- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
updateMonitoring_clusterArn :: Lens.Lens' UpdateMonitoring Prelude.Text
updateMonitoring_clusterArn = Lens.lens (\UpdateMonitoring' {clusterArn} -> clusterArn) (\s@UpdateMonitoring' {} a -> s {clusterArn = a} :: UpdateMonitoring)

-- | The version of the MSK cluster to update. Cluster versions aren\'t
-- simple numbers. You can describe an MSK cluster to find its version.
-- When this update operation is successful, it generates a new cluster
-- version.
updateMonitoring_currentVersion :: Lens.Lens' UpdateMonitoring Prelude.Text
updateMonitoring_currentVersion = Lens.lens (\UpdateMonitoring' {currentVersion} -> currentVersion) (\s@UpdateMonitoring' {} a -> s {currentVersion = a} :: UpdateMonitoring)

instance Core.AWSRequest UpdateMonitoring where
  type
    AWSResponse UpdateMonitoring =
      UpdateMonitoringResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMonitoringResponse'
            Prelude.<$> (x Data..?> "clusterArn")
            Prelude.<*> (x Data..?> "clusterOperationArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateMonitoring where
  hashWithSalt _salt UpdateMonitoring' {..} =
    _salt
      `Prelude.hashWithSalt` enhancedMonitoring
      `Prelude.hashWithSalt` loggingInfo
      `Prelude.hashWithSalt` openMonitoring
      `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` currentVersion

instance Prelude.NFData UpdateMonitoring where
  rnf UpdateMonitoring' {..} =
    Prelude.rnf enhancedMonitoring
      `Prelude.seq` Prelude.rnf loggingInfo
      `Prelude.seq` Prelude.rnf openMonitoring
      `Prelude.seq` Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf currentVersion

instance Data.ToHeaders UpdateMonitoring where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateMonitoring where
  toJSON UpdateMonitoring' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("enhancedMonitoring" Data..=)
              Prelude.<$> enhancedMonitoring,
            ("loggingInfo" Data..=) Prelude.<$> loggingInfo,
            ("openMonitoring" Data..=)
              Prelude.<$> openMonitoring,
            Prelude.Just
              ("currentVersion" Data..= currentVersion)
          ]
      )

instance Data.ToPath UpdateMonitoring where
  toPath UpdateMonitoring' {..} =
    Prelude.mconcat
      [ "/v1/clusters/",
        Data.toBS clusterArn,
        "/monitoring"
      ]

instance Data.ToQuery UpdateMonitoring where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMonitoringResponse' smart constructor.
data UpdateMonitoringResponse = UpdateMonitoringResponse'
  { -- | The Amazon Resource Name (ARN) of the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the cluster operation.
    clusterOperationArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMonitoringResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'updateMonitoringResponse_clusterArn' - The Amazon Resource Name (ARN) of the cluster.
--
-- 'clusterOperationArn', 'updateMonitoringResponse_clusterOperationArn' - The Amazon Resource Name (ARN) of the cluster operation.
--
-- 'httpStatus', 'updateMonitoringResponse_httpStatus' - The response's http status code.
newUpdateMonitoringResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateMonitoringResponse
newUpdateMonitoringResponse pHttpStatus_ =
  UpdateMonitoringResponse'
    { clusterArn =
        Prelude.Nothing,
      clusterOperationArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the cluster.
updateMonitoringResponse_clusterArn :: Lens.Lens' UpdateMonitoringResponse (Prelude.Maybe Prelude.Text)
updateMonitoringResponse_clusterArn = Lens.lens (\UpdateMonitoringResponse' {clusterArn} -> clusterArn) (\s@UpdateMonitoringResponse' {} a -> s {clusterArn = a} :: UpdateMonitoringResponse)

-- | The Amazon Resource Name (ARN) of the cluster operation.
updateMonitoringResponse_clusterOperationArn :: Lens.Lens' UpdateMonitoringResponse (Prelude.Maybe Prelude.Text)
updateMonitoringResponse_clusterOperationArn = Lens.lens (\UpdateMonitoringResponse' {clusterOperationArn} -> clusterOperationArn) (\s@UpdateMonitoringResponse' {} a -> s {clusterOperationArn = a} :: UpdateMonitoringResponse)

-- | The response's http status code.
updateMonitoringResponse_httpStatus :: Lens.Lens' UpdateMonitoringResponse Prelude.Int
updateMonitoringResponse_httpStatus = Lens.lens (\UpdateMonitoringResponse' {httpStatus} -> httpStatus) (\s@UpdateMonitoringResponse' {} a -> s {httpStatus = a} :: UpdateMonitoringResponse)

instance Prelude.NFData UpdateMonitoringResponse where
  rnf UpdateMonitoringResponse' {..} =
    Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf clusterOperationArn
      `Prelude.seq` Prelude.rnf httpStatus
