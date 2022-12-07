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
-- Module      : Amazonka.Kafka.UpdateConnectivity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the cluster\'s connectivity configuration.
module Amazonka.Kafka.UpdateConnectivity
  ( -- * Creating a Request
    UpdateConnectivity (..),
    newUpdateConnectivity,

    -- * Request Lenses
    updateConnectivity_clusterArn,
    updateConnectivity_connectivityInfo,
    updateConnectivity_currentVersion,

    -- * Destructuring the Response
    UpdateConnectivityResponse (..),
    newUpdateConnectivityResponse,

    -- * Response Lenses
    updateConnectivityResponse_clusterArn,
    updateConnectivityResponse_clusterOperationArn,
    updateConnectivityResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request body for UpdateConnectivity.
--
-- /See:/ 'newUpdateConnectivity' smart constructor.
data UpdateConnectivity = UpdateConnectivity'
  { -- | The Amazon Resource Name (ARN) of the configuration.
    clusterArn :: Prelude.Text,
    -- | Information about the broker access configuration.
    connectivityInfo :: ConnectivityInfo,
    -- | The version of the MSK cluster to update. Cluster versions aren\'t
    -- simple numbers. You can describe an MSK cluster to find its version.
    -- When this update operation is successful, it generates a new cluster
    -- version.
    currentVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConnectivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'updateConnectivity_clusterArn' - The Amazon Resource Name (ARN) of the configuration.
--
-- 'connectivityInfo', 'updateConnectivity_connectivityInfo' - Information about the broker access configuration.
--
-- 'currentVersion', 'updateConnectivity_currentVersion' - The version of the MSK cluster to update. Cluster versions aren\'t
-- simple numbers. You can describe an MSK cluster to find its version.
-- When this update operation is successful, it generates a new cluster
-- version.
newUpdateConnectivity ::
  -- | 'clusterArn'
  Prelude.Text ->
  -- | 'connectivityInfo'
  ConnectivityInfo ->
  -- | 'currentVersion'
  Prelude.Text ->
  UpdateConnectivity
newUpdateConnectivity
  pClusterArn_
  pConnectivityInfo_
  pCurrentVersion_ =
    UpdateConnectivity'
      { clusterArn = pClusterArn_,
        connectivityInfo = pConnectivityInfo_,
        currentVersion = pCurrentVersion_
      }

-- | The Amazon Resource Name (ARN) of the configuration.
updateConnectivity_clusterArn :: Lens.Lens' UpdateConnectivity Prelude.Text
updateConnectivity_clusterArn = Lens.lens (\UpdateConnectivity' {clusterArn} -> clusterArn) (\s@UpdateConnectivity' {} a -> s {clusterArn = a} :: UpdateConnectivity)

-- | Information about the broker access configuration.
updateConnectivity_connectivityInfo :: Lens.Lens' UpdateConnectivity ConnectivityInfo
updateConnectivity_connectivityInfo = Lens.lens (\UpdateConnectivity' {connectivityInfo} -> connectivityInfo) (\s@UpdateConnectivity' {} a -> s {connectivityInfo = a} :: UpdateConnectivity)

-- | The version of the MSK cluster to update. Cluster versions aren\'t
-- simple numbers. You can describe an MSK cluster to find its version.
-- When this update operation is successful, it generates a new cluster
-- version.
updateConnectivity_currentVersion :: Lens.Lens' UpdateConnectivity Prelude.Text
updateConnectivity_currentVersion = Lens.lens (\UpdateConnectivity' {currentVersion} -> currentVersion) (\s@UpdateConnectivity' {} a -> s {currentVersion = a} :: UpdateConnectivity)

instance Core.AWSRequest UpdateConnectivity where
  type
    AWSResponse UpdateConnectivity =
      UpdateConnectivityResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateConnectivityResponse'
            Prelude.<$> (x Data..?> "clusterArn")
            Prelude.<*> (x Data..?> "clusterOperationArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateConnectivity where
  hashWithSalt _salt UpdateConnectivity' {..} =
    _salt `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` connectivityInfo
      `Prelude.hashWithSalt` currentVersion

instance Prelude.NFData UpdateConnectivity where
  rnf UpdateConnectivity' {..} =
    Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf connectivityInfo
      `Prelude.seq` Prelude.rnf currentVersion

instance Data.ToHeaders UpdateConnectivity where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateConnectivity where
  toJSON UpdateConnectivity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("connectivityInfo" Data..= connectivityInfo),
            Prelude.Just
              ("currentVersion" Data..= currentVersion)
          ]
      )

instance Data.ToPath UpdateConnectivity where
  toPath UpdateConnectivity' {..} =
    Prelude.mconcat
      [ "/v1/clusters/",
        Data.toBS clusterArn,
        "/connectivity"
      ]

instance Data.ToQuery UpdateConnectivity where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateConnectivityResponse' smart constructor.
data UpdateConnectivityResponse = UpdateConnectivityResponse'
  { -- | The Amazon Resource Name (ARN) of the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the cluster operation.
    clusterOperationArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConnectivityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'updateConnectivityResponse_clusterArn' - The Amazon Resource Name (ARN) of the cluster.
--
-- 'clusterOperationArn', 'updateConnectivityResponse_clusterOperationArn' - The Amazon Resource Name (ARN) of the cluster operation.
--
-- 'httpStatus', 'updateConnectivityResponse_httpStatus' - The response's http status code.
newUpdateConnectivityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateConnectivityResponse
newUpdateConnectivityResponse pHttpStatus_ =
  UpdateConnectivityResponse'
    { clusterArn =
        Prelude.Nothing,
      clusterOperationArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the cluster.
updateConnectivityResponse_clusterArn :: Lens.Lens' UpdateConnectivityResponse (Prelude.Maybe Prelude.Text)
updateConnectivityResponse_clusterArn = Lens.lens (\UpdateConnectivityResponse' {clusterArn} -> clusterArn) (\s@UpdateConnectivityResponse' {} a -> s {clusterArn = a} :: UpdateConnectivityResponse)

-- | The Amazon Resource Name (ARN) of the cluster operation.
updateConnectivityResponse_clusterOperationArn :: Lens.Lens' UpdateConnectivityResponse (Prelude.Maybe Prelude.Text)
updateConnectivityResponse_clusterOperationArn = Lens.lens (\UpdateConnectivityResponse' {clusterOperationArn} -> clusterOperationArn) (\s@UpdateConnectivityResponse' {} a -> s {clusterOperationArn = a} :: UpdateConnectivityResponse)

-- | The response's http status code.
updateConnectivityResponse_httpStatus :: Lens.Lens' UpdateConnectivityResponse Prelude.Int
updateConnectivityResponse_httpStatus = Lens.lens (\UpdateConnectivityResponse' {httpStatus} -> httpStatus) (\s@UpdateConnectivityResponse' {} a -> s {httpStatus = a} :: UpdateConnectivityResponse)

instance Prelude.NFData UpdateConnectivityResponse where
  rnf UpdateConnectivityResponse' {..} =
    Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf clusterOperationArn
      `Prelude.seq` Prelude.rnf httpStatus
