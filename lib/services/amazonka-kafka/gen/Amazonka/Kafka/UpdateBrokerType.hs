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
-- Module      : Amazonka.Kafka.UpdateBrokerType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates EC2 instance type.
module Amazonka.Kafka.UpdateBrokerType
  ( -- * Creating a Request
    UpdateBrokerType (..),
    newUpdateBrokerType,

    -- * Request Lenses
    updateBrokerType_clusterArn,
    updateBrokerType_currentVersion,
    updateBrokerType_targetInstanceType,

    -- * Destructuring the Response
    UpdateBrokerTypeResponse (..),
    newUpdateBrokerTypeResponse,

    -- * Response Lenses
    updateBrokerTypeResponse_clusterArn,
    updateBrokerTypeResponse_clusterOperationArn,
    updateBrokerTypeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateBrokerType' smart constructor.
data UpdateBrokerType = UpdateBrokerType'
  { -- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
    clusterArn :: Prelude.Text,
    -- | The cluster version that you want to change. After this operation
    -- completes successfully, the cluster will have a new version.
    currentVersion :: Prelude.Text,
    -- | The Amazon MSK broker type that you want all of the brokers in this
    -- cluster to be.
    targetInstanceType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBrokerType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'updateBrokerType_clusterArn' - The Amazon Resource Name (ARN) that uniquely identifies the cluster.
--
-- 'currentVersion', 'updateBrokerType_currentVersion' - The cluster version that you want to change. After this operation
-- completes successfully, the cluster will have a new version.
--
-- 'targetInstanceType', 'updateBrokerType_targetInstanceType' - The Amazon MSK broker type that you want all of the brokers in this
-- cluster to be.
newUpdateBrokerType ::
  -- | 'clusterArn'
  Prelude.Text ->
  -- | 'currentVersion'
  Prelude.Text ->
  -- | 'targetInstanceType'
  Prelude.Text ->
  UpdateBrokerType
newUpdateBrokerType
  pClusterArn_
  pCurrentVersion_
  pTargetInstanceType_ =
    UpdateBrokerType'
      { clusterArn = pClusterArn_,
        currentVersion = pCurrentVersion_,
        targetInstanceType = pTargetInstanceType_
      }

-- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
updateBrokerType_clusterArn :: Lens.Lens' UpdateBrokerType Prelude.Text
updateBrokerType_clusterArn = Lens.lens (\UpdateBrokerType' {clusterArn} -> clusterArn) (\s@UpdateBrokerType' {} a -> s {clusterArn = a} :: UpdateBrokerType)

-- | The cluster version that you want to change. After this operation
-- completes successfully, the cluster will have a new version.
updateBrokerType_currentVersion :: Lens.Lens' UpdateBrokerType Prelude.Text
updateBrokerType_currentVersion = Lens.lens (\UpdateBrokerType' {currentVersion} -> currentVersion) (\s@UpdateBrokerType' {} a -> s {currentVersion = a} :: UpdateBrokerType)

-- | The Amazon MSK broker type that you want all of the brokers in this
-- cluster to be.
updateBrokerType_targetInstanceType :: Lens.Lens' UpdateBrokerType Prelude.Text
updateBrokerType_targetInstanceType = Lens.lens (\UpdateBrokerType' {targetInstanceType} -> targetInstanceType) (\s@UpdateBrokerType' {} a -> s {targetInstanceType = a} :: UpdateBrokerType)

instance Core.AWSRequest UpdateBrokerType where
  type
    AWSResponse UpdateBrokerType =
      UpdateBrokerTypeResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBrokerTypeResponse'
            Prelude.<$> (x Core..?> "clusterArn")
            Prelude.<*> (x Core..?> "clusterOperationArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateBrokerType where
  hashWithSalt _salt UpdateBrokerType' {..} =
    _salt `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` currentVersion
      `Prelude.hashWithSalt` targetInstanceType

instance Prelude.NFData UpdateBrokerType where
  rnf UpdateBrokerType' {..} =
    Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf currentVersion
      `Prelude.seq` Prelude.rnf targetInstanceType

instance Core.ToHeaders UpdateBrokerType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateBrokerType where
  toJSON UpdateBrokerType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("currentVersion" Core..= currentVersion),
            Prelude.Just
              ("targetInstanceType" Core..= targetInstanceType)
          ]
      )

instance Core.ToPath UpdateBrokerType where
  toPath UpdateBrokerType' {..} =
    Prelude.mconcat
      [ "/v1/clusters/",
        Core.toBS clusterArn,
        "/nodes/type"
      ]

instance Core.ToQuery UpdateBrokerType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateBrokerTypeResponse' smart constructor.
data UpdateBrokerTypeResponse = UpdateBrokerTypeResponse'
  { -- | The Amazon Resource Name (ARN) of the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the cluster operation.
    clusterOperationArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBrokerTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'updateBrokerTypeResponse_clusterArn' - The Amazon Resource Name (ARN) of the cluster.
--
-- 'clusterOperationArn', 'updateBrokerTypeResponse_clusterOperationArn' - The Amazon Resource Name (ARN) of the cluster operation.
--
-- 'httpStatus', 'updateBrokerTypeResponse_httpStatus' - The response's http status code.
newUpdateBrokerTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateBrokerTypeResponse
newUpdateBrokerTypeResponse pHttpStatus_ =
  UpdateBrokerTypeResponse'
    { clusterArn =
        Prelude.Nothing,
      clusterOperationArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the cluster.
updateBrokerTypeResponse_clusterArn :: Lens.Lens' UpdateBrokerTypeResponse (Prelude.Maybe Prelude.Text)
updateBrokerTypeResponse_clusterArn = Lens.lens (\UpdateBrokerTypeResponse' {clusterArn} -> clusterArn) (\s@UpdateBrokerTypeResponse' {} a -> s {clusterArn = a} :: UpdateBrokerTypeResponse)

-- | The Amazon Resource Name (ARN) of the cluster operation.
updateBrokerTypeResponse_clusterOperationArn :: Lens.Lens' UpdateBrokerTypeResponse (Prelude.Maybe Prelude.Text)
updateBrokerTypeResponse_clusterOperationArn = Lens.lens (\UpdateBrokerTypeResponse' {clusterOperationArn} -> clusterOperationArn) (\s@UpdateBrokerTypeResponse' {} a -> s {clusterOperationArn = a} :: UpdateBrokerTypeResponse)

-- | The response's http status code.
updateBrokerTypeResponse_httpStatus :: Lens.Lens' UpdateBrokerTypeResponse Prelude.Int
updateBrokerTypeResponse_httpStatus = Lens.lens (\UpdateBrokerTypeResponse' {httpStatus} -> httpStatus) (\s@UpdateBrokerTypeResponse' {} a -> s {httpStatus = a} :: UpdateBrokerTypeResponse)

instance Prelude.NFData UpdateBrokerTypeResponse where
  rnf UpdateBrokerTypeResponse' {..} =
    Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf clusterOperationArn
      `Prelude.seq` Prelude.rnf httpStatus
