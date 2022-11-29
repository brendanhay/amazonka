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
-- Module      : Amazonka.Kafka.UpdateBrokerStorage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the EBS storage associated with MSK brokers.
module Amazonka.Kafka.UpdateBrokerStorage
  ( -- * Creating a Request
    UpdateBrokerStorage (..),
    newUpdateBrokerStorage,

    -- * Request Lenses
    updateBrokerStorage_clusterArn,
    updateBrokerStorage_targetBrokerEBSVolumeInfo,
    updateBrokerStorage_currentVersion,

    -- * Destructuring the Response
    UpdateBrokerStorageResponse (..),
    newUpdateBrokerStorageResponse,

    -- * Response Lenses
    updateBrokerStorageResponse_clusterArn,
    updateBrokerStorageResponse_clusterOperationArn,
    updateBrokerStorageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateBrokerStorage' smart constructor.
data UpdateBrokerStorage = UpdateBrokerStorage'
  { -- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
    clusterArn :: Prelude.Text,
    -- | Describes the target volume size and the ID of the broker to apply the
    -- update to.
    targetBrokerEBSVolumeInfo :: [BrokerEBSVolumeInfo],
    -- | The version of cluster to update from. A successful operation will then
    -- generate a new version.
    currentVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBrokerStorage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'updateBrokerStorage_clusterArn' - The Amazon Resource Name (ARN) that uniquely identifies the cluster.
--
-- 'targetBrokerEBSVolumeInfo', 'updateBrokerStorage_targetBrokerEBSVolumeInfo' - Describes the target volume size and the ID of the broker to apply the
-- update to.
--
-- 'currentVersion', 'updateBrokerStorage_currentVersion' - The version of cluster to update from. A successful operation will then
-- generate a new version.
newUpdateBrokerStorage ::
  -- | 'clusterArn'
  Prelude.Text ->
  -- | 'currentVersion'
  Prelude.Text ->
  UpdateBrokerStorage
newUpdateBrokerStorage pClusterArn_ pCurrentVersion_ =
  UpdateBrokerStorage'
    { clusterArn = pClusterArn_,
      targetBrokerEBSVolumeInfo = Prelude.mempty,
      currentVersion = pCurrentVersion_
    }

-- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
updateBrokerStorage_clusterArn :: Lens.Lens' UpdateBrokerStorage Prelude.Text
updateBrokerStorage_clusterArn = Lens.lens (\UpdateBrokerStorage' {clusterArn} -> clusterArn) (\s@UpdateBrokerStorage' {} a -> s {clusterArn = a} :: UpdateBrokerStorage)

-- | Describes the target volume size and the ID of the broker to apply the
-- update to.
updateBrokerStorage_targetBrokerEBSVolumeInfo :: Lens.Lens' UpdateBrokerStorage [BrokerEBSVolumeInfo]
updateBrokerStorage_targetBrokerEBSVolumeInfo = Lens.lens (\UpdateBrokerStorage' {targetBrokerEBSVolumeInfo} -> targetBrokerEBSVolumeInfo) (\s@UpdateBrokerStorage' {} a -> s {targetBrokerEBSVolumeInfo = a} :: UpdateBrokerStorage) Prelude.. Lens.coerced

-- | The version of cluster to update from. A successful operation will then
-- generate a new version.
updateBrokerStorage_currentVersion :: Lens.Lens' UpdateBrokerStorage Prelude.Text
updateBrokerStorage_currentVersion = Lens.lens (\UpdateBrokerStorage' {currentVersion} -> currentVersion) (\s@UpdateBrokerStorage' {} a -> s {currentVersion = a} :: UpdateBrokerStorage)

instance Core.AWSRequest UpdateBrokerStorage where
  type
    AWSResponse UpdateBrokerStorage =
      UpdateBrokerStorageResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBrokerStorageResponse'
            Prelude.<$> (x Core..?> "clusterArn")
            Prelude.<*> (x Core..?> "clusterOperationArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateBrokerStorage where
  hashWithSalt _salt UpdateBrokerStorage' {..} =
    _salt `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` targetBrokerEBSVolumeInfo
      `Prelude.hashWithSalt` currentVersion

instance Prelude.NFData UpdateBrokerStorage where
  rnf UpdateBrokerStorage' {..} =
    Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf targetBrokerEBSVolumeInfo
      `Prelude.seq` Prelude.rnf currentVersion

instance Core.ToHeaders UpdateBrokerStorage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateBrokerStorage where
  toJSON UpdateBrokerStorage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "targetBrokerEBSVolumeInfo"
                  Core..= targetBrokerEBSVolumeInfo
              ),
            Prelude.Just
              ("currentVersion" Core..= currentVersion)
          ]
      )

instance Core.ToPath UpdateBrokerStorage where
  toPath UpdateBrokerStorage' {..} =
    Prelude.mconcat
      [ "/v1/clusters/",
        Core.toBS clusterArn,
        "/nodes/storage"
      ]

instance Core.ToQuery UpdateBrokerStorage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateBrokerStorageResponse' smart constructor.
data UpdateBrokerStorageResponse = UpdateBrokerStorageResponse'
  { -- | The Amazon Resource Name (ARN) of the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the cluster operation.
    clusterOperationArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBrokerStorageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'updateBrokerStorageResponse_clusterArn' - The Amazon Resource Name (ARN) of the cluster.
--
-- 'clusterOperationArn', 'updateBrokerStorageResponse_clusterOperationArn' - The Amazon Resource Name (ARN) of the cluster operation.
--
-- 'httpStatus', 'updateBrokerStorageResponse_httpStatus' - The response's http status code.
newUpdateBrokerStorageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateBrokerStorageResponse
newUpdateBrokerStorageResponse pHttpStatus_ =
  UpdateBrokerStorageResponse'
    { clusterArn =
        Prelude.Nothing,
      clusterOperationArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the cluster.
updateBrokerStorageResponse_clusterArn :: Lens.Lens' UpdateBrokerStorageResponse (Prelude.Maybe Prelude.Text)
updateBrokerStorageResponse_clusterArn = Lens.lens (\UpdateBrokerStorageResponse' {clusterArn} -> clusterArn) (\s@UpdateBrokerStorageResponse' {} a -> s {clusterArn = a} :: UpdateBrokerStorageResponse)

-- | The Amazon Resource Name (ARN) of the cluster operation.
updateBrokerStorageResponse_clusterOperationArn :: Lens.Lens' UpdateBrokerStorageResponse (Prelude.Maybe Prelude.Text)
updateBrokerStorageResponse_clusterOperationArn = Lens.lens (\UpdateBrokerStorageResponse' {clusterOperationArn} -> clusterOperationArn) (\s@UpdateBrokerStorageResponse' {} a -> s {clusterOperationArn = a} :: UpdateBrokerStorageResponse)

-- | The response's http status code.
updateBrokerStorageResponse_httpStatus :: Lens.Lens' UpdateBrokerStorageResponse Prelude.Int
updateBrokerStorageResponse_httpStatus = Lens.lens (\UpdateBrokerStorageResponse' {httpStatus} -> httpStatus) (\s@UpdateBrokerStorageResponse' {} a -> s {httpStatus = a} :: UpdateBrokerStorageResponse)

instance Prelude.NFData UpdateBrokerStorageResponse where
  rnf UpdateBrokerStorageResponse' {..} =
    Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf clusterOperationArn
      `Prelude.seq` Prelude.rnf httpStatus
