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
-- Module      : Amazonka.IoT.UpdatePackageConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the package configuration.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions UpdatePackageConfiguration>
-- and
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use_passrole.html iam:PassRole>
-- actions.
module Amazonka.IoT.UpdatePackageConfiguration
  ( -- * Creating a Request
    UpdatePackageConfiguration (..),
    newUpdatePackageConfiguration,

    -- * Request Lenses
    updatePackageConfiguration_clientToken,
    updatePackageConfiguration_versionUpdateByJobsConfig,

    -- * Destructuring the Response
    UpdatePackageConfigurationResponse (..),
    newUpdatePackageConfigurationResponse,

    -- * Response Lenses
    updatePackageConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePackageConfiguration' smart constructor.
data UpdatePackageConfiguration = UpdatePackageConfiguration'
  { -- | A unique case-sensitive identifier that you can provide to ensure the
    -- idempotency of the request. Don\'t reuse this client token if a new
    -- idempotent request is required.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Configuration to manage job\'s package version reporting. This updates
    -- the thing\'s reserved named shadow that the job targets.
    versionUpdateByJobsConfig :: Prelude.Maybe VersionUpdateByJobsConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePackageConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updatePackageConfiguration_clientToken' - A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
--
-- 'versionUpdateByJobsConfig', 'updatePackageConfiguration_versionUpdateByJobsConfig' - Configuration to manage job\'s package version reporting. This updates
-- the thing\'s reserved named shadow that the job targets.
newUpdatePackageConfiguration ::
  UpdatePackageConfiguration
newUpdatePackageConfiguration =
  UpdatePackageConfiguration'
    { clientToken =
        Prelude.Nothing,
      versionUpdateByJobsConfig = Prelude.Nothing
    }

-- | A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
updatePackageConfiguration_clientToken :: Lens.Lens' UpdatePackageConfiguration (Prelude.Maybe Prelude.Text)
updatePackageConfiguration_clientToken = Lens.lens (\UpdatePackageConfiguration' {clientToken} -> clientToken) (\s@UpdatePackageConfiguration' {} a -> s {clientToken = a} :: UpdatePackageConfiguration)

-- | Configuration to manage job\'s package version reporting. This updates
-- the thing\'s reserved named shadow that the job targets.
updatePackageConfiguration_versionUpdateByJobsConfig :: Lens.Lens' UpdatePackageConfiguration (Prelude.Maybe VersionUpdateByJobsConfig)
updatePackageConfiguration_versionUpdateByJobsConfig = Lens.lens (\UpdatePackageConfiguration' {versionUpdateByJobsConfig} -> versionUpdateByJobsConfig) (\s@UpdatePackageConfiguration' {} a -> s {versionUpdateByJobsConfig = a} :: UpdatePackageConfiguration)

instance Core.AWSRequest UpdatePackageConfiguration where
  type
    AWSResponse UpdatePackageConfiguration =
      UpdatePackageConfigurationResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdatePackageConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePackageConfiguration where
  hashWithSalt _salt UpdatePackageConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` versionUpdateByJobsConfig

instance Prelude.NFData UpdatePackageConfiguration where
  rnf UpdatePackageConfiguration' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf versionUpdateByJobsConfig

instance Data.ToHeaders UpdatePackageConfiguration where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdatePackageConfiguration where
  toJSON UpdatePackageConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("versionUpdateByJobsConfig" Data..=)
              Prelude.<$> versionUpdateByJobsConfig
          ]
      )

instance Data.ToPath UpdatePackageConfiguration where
  toPath = Prelude.const "/package-configuration"

instance Data.ToQuery UpdatePackageConfiguration where
  toQuery UpdatePackageConfiguration' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | /See:/ 'newUpdatePackageConfigurationResponse' smart constructor.
data UpdatePackageConfigurationResponse = UpdatePackageConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePackageConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updatePackageConfigurationResponse_httpStatus' - The response's http status code.
newUpdatePackageConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdatePackageConfigurationResponse
newUpdatePackageConfigurationResponse pHttpStatus_ =
  UpdatePackageConfigurationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updatePackageConfigurationResponse_httpStatus :: Lens.Lens' UpdatePackageConfigurationResponse Prelude.Int
updatePackageConfigurationResponse_httpStatus = Lens.lens (\UpdatePackageConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdatePackageConfigurationResponse' {} a -> s {httpStatus = a} :: UpdatePackageConfigurationResponse)

instance
  Prelude.NFData
    UpdatePackageConfigurationResponse
  where
  rnf UpdatePackageConfigurationResponse' {..} =
    Prelude.rnf httpStatus
