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
-- Module      : Amazonka.Kafka.UpdateClusterKafkaVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Apache Kafka version for the cluster.
module Amazonka.Kafka.UpdateClusterKafkaVersion
  ( -- * Creating a Request
    UpdateClusterKafkaVersion (..),
    newUpdateClusterKafkaVersion,

    -- * Request Lenses
    updateClusterKafkaVersion_configurationInfo,
    updateClusterKafkaVersion_clusterArn,
    updateClusterKafkaVersion_targetKafkaVersion,
    updateClusterKafkaVersion_currentVersion,

    -- * Destructuring the Response
    UpdateClusterKafkaVersionResponse (..),
    newUpdateClusterKafkaVersionResponse,

    -- * Response Lenses
    updateClusterKafkaVersionResponse_clusterArn,
    updateClusterKafkaVersionResponse_clusterOperationArn,
    updateClusterKafkaVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateClusterKafkaVersion' smart constructor.
data UpdateClusterKafkaVersion = UpdateClusterKafkaVersion'
  { -- | The custom configuration that should be applied on the new version of
    -- cluster.
    configurationInfo :: Prelude.Maybe ConfigurationInfo,
    -- | The Amazon Resource Name (ARN) of the cluster to be updated.
    clusterArn :: Prelude.Text,
    -- | Target Kafka version.
    targetKafkaVersion :: Prelude.Text,
    -- | Current cluster version.
    currentVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateClusterKafkaVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationInfo', 'updateClusterKafkaVersion_configurationInfo' - The custom configuration that should be applied on the new version of
-- cluster.
--
-- 'clusterArn', 'updateClusterKafkaVersion_clusterArn' - The Amazon Resource Name (ARN) of the cluster to be updated.
--
-- 'targetKafkaVersion', 'updateClusterKafkaVersion_targetKafkaVersion' - Target Kafka version.
--
-- 'currentVersion', 'updateClusterKafkaVersion_currentVersion' - Current cluster version.
newUpdateClusterKafkaVersion ::
  -- | 'clusterArn'
  Prelude.Text ->
  -- | 'targetKafkaVersion'
  Prelude.Text ->
  -- | 'currentVersion'
  Prelude.Text ->
  UpdateClusterKafkaVersion
newUpdateClusterKafkaVersion
  pClusterArn_
  pTargetKafkaVersion_
  pCurrentVersion_ =
    UpdateClusterKafkaVersion'
      { configurationInfo =
          Prelude.Nothing,
        clusterArn = pClusterArn_,
        targetKafkaVersion = pTargetKafkaVersion_,
        currentVersion = pCurrentVersion_
      }

-- | The custom configuration that should be applied on the new version of
-- cluster.
updateClusterKafkaVersion_configurationInfo :: Lens.Lens' UpdateClusterKafkaVersion (Prelude.Maybe ConfigurationInfo)
updateClusterKafkaVersion_configurationInfo = Lens.lens (\UpdateClusterKafkaVersion' {configurationInfo} -> configurationInfo) (\s@UpdateClusterKafkaVersion' {} a -> s {configurationInfo = a} :: UpdateClusterKafkaVersion)

-- | The Amazon Resource Name (ARN) of the cluster to be updated.
updateClusterKafkaVersion_clusterArn :: Lens.Lens' UpdateClusterKafkaVersion Prelude.Text
updateClusterKafkaVersion_clusterArn = Lens.lens (\UpdateClusterKafkaVersion' {clusterArn} -> clusterArn) (\s@UpdateClusterKafkaVersion' {} a -> s {clusterArn = a} :: UpdateClusterKafkaVersion)

-- | Target Kafka version.
updateClusterKafkaVersion_targetKafkaVersion :: Lens.Lens' UpdateClusterKafkaVersion Prelude.Text
updateClusterKafkaVersion_targetKafkaVersion = Lens.lens (\UpdateClusterKafkaVersion' {targetKafkaVersion} -> targetKafkaVersion) (\s@UpdateClusterKafkaVersion' {} a -> s {targetKafkaVersion = a} :: UpdateClusterKafkaVersion)

-- | Current cluster version.
updateClusterKafkaVersion_currentVersion :: Lens.Lens' UpdateClusterKafkaVersion Prelude.Text
updateClusterKafkaVersion_currentVersion = Lens.lens (\UpdateClusterKafkaVersion' {currentVersion} -> currentVersion) (\s@UpdateClusterKafkaVersion' {} a -> s {currentVersion = a} :: UpdateClusterKafkaVersion)

instance Core.AWSRequest UpdateClusterKafkaVersion where
  type
    AWSResponse UpdateClusterKafkaVersion =
      UpdateClusterKafkaVersionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateClusterKafkaVersionResponse'
            Prelude.<$> (x Data..?> "clusterArn")
            Prelude.<*> (x Data..?> "clusterOperationArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateClusterKafkaVersion where
  hashWithSalt _salt UpdateClusterKafkaVersion' {..} =
    _salt
      `Prelude.hashWithSalt` configurationInfo
      `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` targetKafkaVersion
      `Prelude.hashWithSalt` currentVersion

instance Prelude.NFData UpdateClusterKafkaVersion where
  rnf UpdateClusterKafkaVersion' {..} =
    Prelude.rnf configurationInfo
      `Prelude.seq` Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf targetKafkaVersion
      `Prelude.seq` Prelude.rnf currentVersion

instance Data.ToHeaders UpdateClusterKafkaVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateClusterKafkaVersion where
  toJSON UpdateClusterKafkaVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("configurationInfo" Data..=)
              Prelude.<$> configurationInfo,
            Prelude.Just
              ("targetKafkaVersion" Data..= targetKafkaVersion),
            Prelude.Just
              ("currentVersion" Data..= currentVersion)
          ]
      )

instance Data.ToPath UpdateClusterKafkaVersion where
  toPath UpdateClusterKafkaVersion' {..} =
    Prelude.mconcat
      ["/v1/clusters/", Data.toBS clusterArn, "/version"]

instance Data.ToQuery UpdateClusterKafkaVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateClusterKafkaVersionResponse' smart constructor.
data UpdateClusterKafkaVersionResponse = UpdateClusterKafkaVersionResponse'
  { -- | The Amazon Resource Name (ARN) of the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the cluster operation.
    clusterOperationArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateClusterKafkaVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'updateClusterKafkaVersionResponse_clusterArn' - The Amazon Resource Name (ARN) of the cluster.
--
-- 'clusterOperationArn', 'updateClusterKafkaVersionResponse_clusterOperationArn' - The Amazon Resource Name (ARN) of the cluster operation.
--
-- 'httpStatus', 'updateClusterKafkaVersionResponse_httpStatus' - The response's http status code.
newUpdateClusterKafkaVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateClusterKafkaVersionResponse
newUpdateClusterKafkaVersionResponse pHttpStatus_ =
  UpdateClusterKafkaVersionResponse'
    { clusterArn =
        Prelude.Nothing,
      clusterOperationArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the cluster.
updateClusterKafkaVersionResponse_clusterArn :: Lens.Lens' UpdateClusterKafkaVersionResponse (Prelude.Maybe Prelude.Text)
updateClusterKafkaVersionResponse_clusterArn = Lens.lens (\UpdateClusterKafkaVersionResponse' {clusterArn} -> clusterArn) (\s@UpdateClusterKafkaVersionResponse' {} a -> s {clusterArn = a} :: UpdateClusterKafkaVersionResponse)

-- | The Amazon Resource Name (ARN) of the cluster operation.
updateClusterKafkaVersionResponse_clusterOperationArn :: Lens.Lens' UpdateClusterKafkaVersionResponse (Prelude.Maybe Prelude.Text)
updateClusterKafkaVersionResponse_clusterOperationArn = Lens.lens (\UpdateClusterKafkaVersionResponse' {clusterOperationArn} -> clusterOperationArn) (\s@UpdateClusterKafkaVersionResponse' {} a -> s {clusterOperationArn = a} :: UpdateClusterKafkaVersionResponse)

-- | The response's http status code.
updateClusterKafkaVersionResponse_httpStatus :: Lens.Lens' UpdateClusterKafkaVersionResponse Prelude.Int
updateClusterKafkaVersionResponse_httpStatus = Lens.lens (\UpdateClusterKafkaVersionResponse' {httpStatus} -> httpStatus) (\s@UpdateClusterKafkaVersionResponse' {} a -> s {httpStatus = a} :: UpdateClusterKafkaVersionResponse)

instance
  Prelude.NFData
    UpdateClusterKafkaVersionResponse
  where
  rnf UpdateClusterKafkaVersionResponse' {..} =
    Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf clusterOperationArn
      `Prelude.seq` Prelude.rnf httpStatus
