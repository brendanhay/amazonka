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
-- Module      : Amazonka.Kafka.UpdateStorage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates cluster broker volume size (or) sets cluster storage mode to
-- TIERED.
module Amazonka.Kafka.UpdateStorage
  ( -- * Creating a Request
    UpdateStorage (..),
    newUpdateStorage,

    -- * Request Lenses
    updateStorage_provisionedThroughput,
    updateStorage_storageMode,
    updateStorage_volumeSizeGB,
    updateStorage_clusterArn,
    updateStorage_currentVersion,

    -- * Destructuring the Response
    UpdateStorageResponse (..),
    newUpdateStorageResponse,

    -- * Response Lenses
    updateStorageResponse_clusterArn,
    updateStorageResponse_clusterOperationArn,
    updateStorageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request object for UpdateStorage api. Its used to update the storage
-- attributes for the cluster.
--
-- /See:/ 'newUpdateStorage' smart constructor.
data UpdateStorage = UpdateStorage'
  { -- | EBS volume provisioned throughput information.
    provisionedThroughput :: Prelude.Maybe ProvisionedThroughput,
    -- | Controls storage mode for supported storage tiers.
    storageMode :: Prelude.Maybe StorageMode,
    -- | size of the EBS volume to update.
    volumeSizeGB :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the cluster to be updated.
    clusterArn :: Prelude.Text,
    -- | The version of cluster to update from. A successful operation will then
    -- generate a new version.
    currentVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStorage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisionedThroughput', 'updateStorage_provisionedThroughput' - EBS volume provisioned throughput information.
--
-- 'storageMode', 'updateStorage_storageMode' - Controls storage mode for supported storage tiers.
--
-- 'volumeSizeGB', 'updateStorage_volumeSizeGB' - size of the EBS volume to update.
--
-- 'clusterArn', 'updateStorage_clusterArn' - The Amazon Resource Name (ARN) of the cluster to be updated.
--
-- 'currentVersion', 'updateStorage_currentVersion' - The version of cluster to update from. A successful operation will then
-- generate a new version.
newUpdateStorage ::
  -- | 'clusterArn'
  Prelude.Text ->
  -- | 'currentVersion'
  Prelude.Text ->
  UpdateStorage
newUpdateStorage pClusterArn_ pCurrentVersion_ =
  UpdateStorage'
    { provisionedThroughput =
        Prelude.Nothing,
      storageMode = Prelude.Nothing,
      volumeSizeGB = Prelude.Nothing,
      clusterArn = pClusterArn_,
      currentVersion = pCurrentVersion_
    }

-- | EBS volume provisioned throughput information.
updateStorage_provisionedThroughput :: Lens.Lens' UpdateStorage (Prelude.Maybe ProvisionedThroughput)
updateStorage_provisionedThroughput = Lens.lens (\UpdateStorage' {provisionedThroughput} -> provisionedThroughput) (\s@UpdateStorage' {} a -> s {provisionedThroughput = a} :: UpdateStorage)

-- | Controls storage mode for supported storage tiers.
updateStorage_storageMode :: Lens.Lens' UpdateStorage (Prelude.Maybe StorageMode)
updateStorage_storageMode = Lens.lens (\UpdateStorage' {storageMode} -> storageMode) (\s@UpdateStorage' {} a -> s {storageMode = a} :: UpdateStorage)

-- | size of the EBS volume to update.
updateStorage_volumeSizeGB :: Lens.Lens' UpdateStorage (Prelude.Maybe Prelude.Int)
updateStorage_volumeSizeGB = Lens.lens (\UpdateStorage' {volumeSizeGB} -> volumeSizeGB) (\s@UpdateStorage' {} a -> s {volumeSizeGB = a} :: UpdateStorage)

-- | The Amazon Resource Name (ARN) of the cluster to be updated.
updateStorage_clusterArn :: Lens.Lens' UpdateStorage Prelude.Text
updateStorage_clusterArn = Lens.lens (\UpdateStorage' {clusterArn} -> clusterArn) (\s@UpdateStorage' {} a -> s {clusterArn = a} :: UpdateStorage)

-- | The version of cluster to update from. A successful operation will then
-- generate a new version.
updateStorage_currentVersion :: Lens.Lens' UpdateStorage Prelude.Text
updateStorage_currentVersion = Lens.lens (\UpdateStorage' {currentVersion} -> currentVersion) (\s@UpdateStorage' {} a -> s {currentVersion = a} :: UpdateStorage)

instance Core.AWSRequest UpdateStorage where
  type
    AWSResponse UpdateStorage =
      UpdateStorageResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateStorageResponse'
            Prelude.<$> (x Data..?> "clusterArn")
            Prelude.<*> (x Data..?> "clusterOperationArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateStorage where
  hashWithSalt _salt UpdateStorage' {..} =
    _salt `Prelude.hashWithSalt` provisionedThroughput
      `Prelude.hashWithSalt` storageMode
      `Prelude.hashWithSalt` volumeSizeGB
      `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` currentVersion

instance Prelude.NFData UpdateStorage where
  rnf UpdateStorage' {..} =
    Prelude.rnf provisionedThroughput
      `Prelude.seq` Prelude.rnf storageMode
      `Prelude.seq` Prelude.rnf volumeSizeGB
      `Prelude.seq` Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf currentVersion

instance Data.ToHeaders UpdateStorage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateStorage where
  toJSON UpdateStorage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("provisionedThroughput" Data..=)
              Prelude.<$> provisionedThroughput,
            ("storageMode" Data..=) Prelude.<$> storageMode,
            ("volumeSizeGB" Data..=) Prelude.<$> volumeSizeGB,
            Prelude.Just
              ("currentVersion" Data..= currentVersion)
          ]
      )

instance Data.ToPath UpdateStorage where
  toPath UpdateStorage' {..} =
    Prelude.mconcat
      ["/v1/clusters/", Data.toBS clusterArn, "/storage"]

instance Data.ToQuery UpdateStorage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateStorageResponse' smart constructor.
data UpdateStorageResponse = UpdateStorageResponse'
  { -- | The Amazon Resource Name (ARN) of the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the cluster operation.
    clusterOperationArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStorageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'updateStorageResponse_clusterArn' - The Amazon Resource Name (ARN) of the cluster.
--
-- 'clusterOperationArn', 'updateStorageResponse_clusterOperationArn' - The Amazon Resource Name (ARN) of the cluster operation.
--
-- 'httpStatus', 'updateStorageResponse_httpStatus' - The response's http status code.
newUpdateStorageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateStorageResponse
newUpdateStorageResponse pHttpStatus_ =
  UpdateStorageResponse'
    { clusterArn =
        Prelude.Nothing,
      clusterOperationArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the cluster.
updateStorageResponse_clusterArn :: Lens.Lens' UpdateStorageResponse (Prelude.Maybe Prelude.Text)
updateStorageResponse_clusterArn = Lens.lens (\UpdateStorageResponse' {clusterArn} -> clusterArn) (\s@UpdateStorageResponse' {} a -> s {clusterArn = a} :: UpdateStorageResponse)

-- | The Amazon Resource Name (ARN) of the cluster operation.
updateStorageResponse_clusterOperationArn :: Lens.Lens' UpdateStorageResponse (Prelude.Maybe Prelude.Text)
updateStorageResponse_clusterOperationArn = Lens.lens (\UpdateStorageResponse' {clusterOperationArn} -> clusterOperationArn) (\s@UpdateStorageResponse' {} a -> s {clusterOperationArn = a} :: UpdateStorageResponse)

-- | The response's http status code.
updateStorageResponse_httpStatus :: Lens.Lens' UpdateStorageResponse Prelude.Int
updateStorageResponse_httpStatus = Lens.lens (\UpdateStorageResponse' {httpStatus} -> httpStatus) (\s@UpdateStorageResponse' {} a -> s {httpStatus = a} :: UpdateStorageResponse)

instance Prelude.NFData UpdateStorageResponse where
  rnf UpdateStorageResponse' {..} =
    Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf clusterOperationArn
      `Prelude.seq` Prelude.rnf httpStatus
