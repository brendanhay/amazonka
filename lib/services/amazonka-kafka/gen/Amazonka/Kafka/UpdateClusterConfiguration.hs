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
-- Module      : Amazonka.Kafka.UpdateClusterConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the cluster with the configuration that is specified in the
-- request body.
module Amazonka.Kafka.UpdateClusterConfiguration
  ( -- * Creating a Request
    UpdateClusterConfiguration (..),
    newUpdateClusterConfiguration,

    -- * Request Lenses
    updateClusterConfiguration_clusterArn,
    updateClusterConfiguration_currentVersion,
    updateClusterConfiguration_configurationInfo,

    -- * Destructuring the Response
    UpdateClusterConfigurationResponse (..),
    newUpdateClusterConfigurationResponse,

    -- * Response Lenses
    updateClusterConfigurationResponse_clusterArn,
    updateClusterConfigurationResponse_clusterOperationArn,
    updateClusterConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateClusterConfiguration' smart constructor.
data UpdateClusterConfiguration = UpdateClusterConfiguration'
  { -- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
    clusterArn :: Prelude.Text,
    -- | The version of the cluster that needs to be updated.
    currentVersion :: Prelude.Text,
    -- | Represents the configuration that you want MSK to use for the brokers in
    -- a cluster.
    configurationInfo :: ConfigurationInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateClusterConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'updateClusterConfiguration_clusterArn' - The Amazon Resource Name (ARN) that uniquely identifies the cluster.
--
-- 'currentVersion', 'updateClusterConfiguration_currentVersion' - The version of the cluster that needs to be updated.
--
-- 'configurationInfo', 'updateClusterConfiguration_configurationInfo' - Represents the configuration that you want MSK to use for the brokers in
-- a cluster.
newUpdateClusterConfiguration ::
  -- | 'clusterArn'
  Prelude.Text ->
  -- | 'currentVersion'
  Prelude.Text ->
  -- | 'configurationInfo'
  ConfigurationInfo ->
  UpdateClusterConfiguration
newUpdateClusterConfiguration
  pClusterArn_
  pCurrentVersion_
  pConfigurationInfo_ =
    UpdateClusterConfiguration'
      { clusterArn =
          pClusterArn_,
        currentVersion = pCurrentVersion_,
        configurationInfo = pConfigurationInfo_
      }

-- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
updateClusterConfiguration_clusterArn :: Lens.Lens' UpdateClusterConfiguration Prelude.Text
updateClusterConfiguration_clusterArn = Lens.lens (\UpdateClusterConfiguration' {clusterArn} -> clusterArn) (\s@UpdateClusterConfiguration' {} a -> s {clusterArn = a} :: UpdateClusterConfiguration)

-- | The version of the cluster that needs to be updated.
updateClusterConfiguration_currentVersion :: Lens.Lens' UpdateClusterConfiguration Prelude.Text
updateClusterConfiguration_currentVersion = Lens.lens (\UpdateClusterConfiguration' {currentVersion} -> currentVersion) (\s@UpdateClusterConfiguration' {} a -> s {currentVersion = a} :: UpdateClusterConfiguration)

-- | Represents the configuration that you want MSK to use for the brokers in
-- a cluster.
updateClusterConfiguration_configurationInfo :: Lens.Lens' UpdateClusterConfiguration ConfigurationInfo
updateClusterConfiguration_configurationInfo = Lens.lens (\UpdateClusterConfiguration' {configurationInfo} -> configurationInfo) (\s@UpdateClusterConfiguration' {} a -> s {configurationInfo = a} :: UpdateClusterConfiguration)

instance Core.AWSRequest UpdateClusterConfiguration where
  type
    AWSResponse UpdateClusterConfiguration =
      UpdateClusterConfigurationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateClusterConfigurationResponse'
            Prelude.<$> (x Core..?> "clusterArn")
            Prelude.<*> (x Core..?> "clusterOperationArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateClusterConfiguration where
  hashWithSalt _salt UpdateClusterConfiguration' {..} =
    _salt `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` currentVersion
      `Prelude.hashWithSalt` configurationInfo

instance Prelude.NFData UpdateClusterConfiguration where
  rnf UpdateClusterConfiguration' {..} =
    Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf currentVersion
      `Prelude.seq` Prelude.rnf configurationInfo

instance Core.ToHeaders UpdateClusterConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateClusterConfiguration where
  toJSON UpdateClusterConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("currentVersion" Core..= currentVersion),
            Prelude.Just
              ("configurationInfo" Core..= configurationInfo)
          ]
      )

instance Core.ToPath UpdateClusterConfiguration where
  toPath UpdateClusterConfiguration' {..} =
    Prelude.mconcat
      [ "/v1/clusters/",
        Core.toBS clusterArn,
        "/configuration"
      ]

instance Core.ToQuery UpdateClusterConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateClusterConfigurationResponse' smart constructor.
data UpdateClusterConfigurationResponse = UpdateClusterConfigurationResponse'
  { -- | The Amazon Resource Name (ARN) of the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the cluster operation.
    clusterOperationArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateClusterConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'updateClusterConfigurationResponse_clusterArn' - The Amazon Resource Name (ARN) of the cluster.
--
-- 'clusterOperationArn', 'updateClusterConfigurationResponse_clusterOperationArn' - The Amazon Resource Name (ARN) of the cluster operation.
--
-- 'httpStatus', 'updateClusterConfigurationResponse_httpStatus' - The response's http status code.
newUpdateClusterConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateClusterConfigurationResponse
newUpdateClusterConfigurationResponse pHttpStatus_ =
  UpdateClusterConfigurationResponse'
    { clusterArn =
        Prelude.Nothing,
      clusterOperationArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the cluster.
updateClusterConfigurationResponse_clusterArn :: Lens.Lens' UpdateClusterConfigurationResponse (Prelude.Maybe Prelude.Text)
updateClusterConfigurationResponse_clusterArn = Lens.lens (\UpdateClusterConfigurationResponse' {clusterArn} -> clusterArn) (\s@UpdateClusterConfigurationResponse' {} a -> s {clusterArn = a} :: UpdateClusterConfigurationResponse)

-- | The Amazon Resource Name (ARN) of the cluster operation.
updateClusterConfigurationResponse_clusterOperationArn :: Lens.Lens' UpdateClusterConfigurationResponse (Prelude.Maybe Prelude.Text)
updateClusterConfigurationResponse_clusterOperationArn = Lens.lens (\UpdateClusterConfigurationResponse' {clusterOperationArn} -> clusterOperationArn) (\s@UpdateClusterConfigurationResponse' {} a -> s {clusterOperationArn = a} :: UpdateClusterConfigurationResponse)

-- | The response's http status code.
updateClusterConfigurationResponse_httpStatus :: Lens.Lens' UpdateClusterConfigurationResponse Prelude.Int
updateClusterConfigurationResponse_httpStatus = Lens.lens (\UpdateClusterConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateClusterConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateClusterConfigurationResponse)

instance
  Prelude.NFData
    UpdateClusterConfigurationResponse
  where
  rnf UpdateClusterConfigurationResponse' {..} =
    Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf clusterOperationArn
      `Prelude.seq` Prelude.rnf httpStatus
