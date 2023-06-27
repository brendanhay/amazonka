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
-- Module      : Amazonka.Grafana.UpdateWorkspaceConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration string for the given workspace
module Amazonka.Grafana.UpdateWorkspaceConfiguration
  ( -- * Creating a Request
    UpdateWorkspaceConfiguration (..),
    newUpdateWorkspaceConfiguration,

    -- * Request Lenses
    updateWorkspaceConfiguration_configuration,
    updateWorkspaceConfiguration_workspaceId,

    -- * Destructuring the Response
    UpdateWorkspaceConfigurationResponse (..),
    newUpdateWorkspaceConfigurationResponse,

    -- * Response Lenses
    updateWorkspaceConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Grafana.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateWorkspaceConfiguration' smart constructor.
data UpdateWorkspaceConfiguration = UpdateWorkspaceConfiguration'
  { -- | The new configuration string for the workspace. For more information
    -- about the format and configuration options available, see
    -- <https://docs.aws.amazon.com/grafana/latest/userguide/AMG-configure-workspace.html Working in your Grafana workspace>.
    configuration :: Prelude.Text,
    -- | The ID of the workspace to update.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkspaceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'updateWorkspaceConfiguration_configuration' - The new configuration string for the workspace. For more information
-- about the format and configuration options available, see
-- <https://docs.aws.amazon.com/grafana/latest/userguide/AMG-configure-workspace.html Working in your Grafana workspace>.
--
-- 'workspaceId', 'updateWorkspaceConfiguration_workspaceId' - The ID of the workspace to update.
newUpdateWorkspaceConfiguration ::
  -- | 'configuration'
  Prelude.Text ->
  -- | 'workspaceId'
  Prelude.Text ->
  UpdateWorkspaceConfiguration
newUpdateWorkspaceConfiguration
  pConfiguration_
  pWorkspaceId_ =
    UpdateWorkspaceConfiguration'
      { configuration =
          pConfiguration_,
        workspaceId = pWorkspaceId_
      }

-- | The new configuration string for the workspace. For more information
-- about the format and configuration options available, see
-- <https://docs.aws.amazon.com/grafana/latest/userguide/AMG-configure-workspace.html Working in your Grafana workspace>.
updateWorkspaceConfiguration_configuration :: Lens.Lens' UpdateWorkspaceConfiguration Prelude.Text
updateWorkspaceConfiguration_configuration = Lens.lens (\UpdateWorkspaceConfiguration' {configuration} -> configuration) (\s@UpdateWorkspaceConfiguration' {} a -> s {configuration = a} :: UpdateWorkspaceConfiguration)

-- | The ID of the workspace to update.
updateWorkspaceConfiguration_workspaceId :: Lens.Lens' UpdateWorkspaceConfiguration Prelude.Text
updateWorkspaceConfiguration_workspaceId = Lens.lens (\UpdateWorkspaceConfiguration' {workspaceId} -> workspaceId) (\s@UpdateWorkspaceConfiguration' {} a -> s {workspaceId = a} :: UpdateWorkspaceConfiguration)

instance Core.AWSRequest UpdateWorkspaceConfiguration where
  type
    AWSResponse UpdateWorkspaceConfiguration =
      UpdateWorkspaceConfigurationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateWorkspaceConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateWorkspaceConfiguration
  where
  hashWithSalt _salt UpdateWorkspaceConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData UpdateWorkspaceConfiguration where
  rnf UpdateWorkspaceConfiguration' {..} =
    Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf workspaceId

instance Data.ToHeaders UpdateWorkspaceConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateWorkspaceConfiguration where
  toJSON UpdateWorkspaceConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("configuration" Data..= configuration)
          ]
      )

instance Data.ToPath UpdateWorkspaceConfiguration where
  toPath UpdateWorkspaceConfiguration' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Data.toBS workspaceId,
        "/configuration"
      ]

instance Data.ToQuery UpdateWorkspaceConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateWorkspaceConfigurationResponse' smart constructor.
data UpdateWorkspaceConfigurationResponse = UpdateWorkspaceConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkspaceConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateWorkspaceConfigurationResponse_httpStatus' - The response's http status code.
newUpdateWorkspaceConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateWorkspaceConfigurationResponse
newUpdateWorkspaceConfigurationResponse pHttpStatus_ =
  UpdateWorkspaceConfigurationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateWorkspaceConfigurationResponse_httpStatus :: Lens.Lens' UpdateWorkspaceConfigurationResponse Prelude.Int
updateWorkspaceConfigurationResponse_httpStatus = Lens.lens (\UpdateWorkspaceConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateWorkspaceConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateWorkspaceConfigurationResponse)

instance
  Prelude.NFData
    UpdateWorkspaceConfigurationResponse
  where
  rnf UpdateWorkspaceConfigurationResponse' {..} =
    Prelude.rnf httpStatus
