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
-- Module      : Amazonka.AMP.UpdateLoggingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update logging configuration.
module Amazonka.AMP.UpdateLoggingConfiguration
  ( -- * Creating a Request
    UpdateLoggingConfiguration (..),
    newUpdateLoggingConfiguration,

    -- * Request Lenses
    updateLoggingConfiguration_clientToken,
    updateLoggingConfiguration_logGroupArn,
    updateLoggingConfiguration_workspaceId,

    -- * Destructuring the Response
    UpdateLoggingConfigurationResponse (..),
    newUpdateLoggingConfigurationResponse,

    -- * Response Lenses
    updateLoggingConfigurationResponse_httpStatus,
    updateLoggingConfigurationResponse_status,
  )
where

import Amazonka.AMP.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of an UpdateLoggingConfiguration operation.
--
-- /See:/ 'newUpdateLoggingConfiguration' smart constructor.
data UpdateLoggingConfiguration = UpdateLoggingConfiguration'
  { -- | Optional, unique, case-sensitive, user-provided identifier to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the CW log group to which the vended log data will be
    -- published.
    logGroupArn :: Prelude.Text,
    -- | The ID of the workspace to vend logs to.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLoggingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateLoggingConfiguration_clientToken' - Optional, unique, case-sensitive, user-provided identifier to ensure the
-- idempotency of the request.
--
-- 'logGroupArn', 'updateLoggingConfiguration_logGroupArn' - The ARN of the CW log group to which the vended log data will be
-- published.
--
-- 'workspaceId', 'updateLoggingConfiguration_workspaceId' - The ID of the workspace to vend logs to.
newUpdateLoggingConfiguration ::
  -- | 'logGroupArn'
  Prelude.Text ->
  -- | 'workspaceId'
  Prelude.Text ->
  UpdateLoggingConfiguration
newUpdateLoggingConfiguration
  pLogGroupArn_
  pWorkspaceId_ =
    UpdateLoggingConfiguration'
      { clientToken =
          Prelude.Nothing,
        logGroupArn = pLogGroupArn_,
        workspaceId = pWorkspaceId_
      }

-- | Optional, unique, case-sensitive, user-provided identifier to ensure the
-- idempotency of the request.
updateLoggingConfiguration_clientToken :: Lens.Lens' UpdateLoggingConfiguration (Prelude.Maybe Prelude.Text)
updateLoggingConfiguration_clientToken = Lens.lens (\UpdateLoggingConfiguration' {clientToken} -> clientToken) (\s@UpdateLoggingConfiguration' {} a -> s {clientToken = a} :: UpdateLoggingConfiguration)

-- | The ARN of the CW log group to which the vended log data will be
-- published.
updateLoggingConfiguration_logGroupArn :: Lens.Lens' UpdateLoggingConfiguration Prelude.Text
updateLoggingConfiguration_logGroupArn = Lens.lens (\UpdateLoggingConfiguration' {logGroupArn} -> logGroupArn) (\s@UpdateLoggingConfiguration' {} a -> s {logGroupArn = a} :: UpdateLoggingConfiguration)

-- | The ID of the workspace to vend logs to.
updateLoggingConfiguration_workspaceId :: Lens.Lens' UpdateLoggingConfiguration Prelude.Text
updateLoggingConfiguration_workspaceId = Lens.lens (\UpdateLoggingConfiguration' {workspaceId} -> workspaceId) (\s@UpdateLoggingConfiguration' {} a -> s {workspaceId = a} :: UpdateLoggingConfiguration)

instance Core.AWSRequest UpdateLoggingConfiguration where
  type
    AWSResponse UpdateLoggingConfiguration =
      UpdateLoggingConfigurationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateLoggingConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "status")
      )

instance Prelude.Hashable UpdateLoggingConfiguration where
  hashWithSalt _salt UpdateLoggingConfiguration' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` logGroupArn
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData UpdateLoggingConfiguration where
  rnf UpdateLoggingConfiguration' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf logGroupArn
      `Prelude.seq` Prelude.rnf workspaceId

instance Core.ToHeaders UpdateLoggingConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateLoggingConfiguration where
  toJSON UpdateLoggingConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientToken" Core..=) Prelude.<$> clientToken,
            Prelude.Just ("logGroupArn" Core..= logGroupArn)
          ]
      )

instance Core.ToPath UpdateLoggingConfiguration where
  toPath UpdateLoggingConfiguration' {..} =
    Prelude.mconcat
      ["/workspaces/", Core.toBS workspaceId, "/logging"]

instance Core.ToQuery UpdateLoggingConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of an UpdateLoggingConfiguration operation.
--
-- /See:/ 'newUpdateLoggingConfigurationResponse' smart constructor.
data UpdateLoggingConfigurationResponse = UpdateLoggingConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of the logging configuration.
    status :: LoggingConfigurationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLoggingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateLoggingConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'status', 'updateLoggingConfigurationResponse_status' - The status of the logging configuration.
newUpdateLoggingConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'status'
  LoggingConfigurationStatus ->
  UpdateLoggingConfigurationResponse
newUpdateLoggingConfigurationResponse
  pHttpStatus_
  pStatus_ =
    UpdateLoggingConfigurationResponse'
      { httpStatus =
          pHttpStatus_,
        status = pStatus_
      }

-- | The response's http status code.
updateLoggingConfigurationResponse_httpStatus :: Lens.Lens' UpdateLoggingConfigurationResponse Prelude.Int
updateLoggingConfigurationResponse_httpStatus = Lens.lens (\UpdateLoggingConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateLoggingConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateLoggingConfigurationResponse)

-- | The status of the logging configuration.
updateLoggingConfigurationResponse_status :: Lens.Lens' UpdateLoggingConfigurationResponse LoggingConfigurationStatus
updateLoggingConfigurationResponse_status = Lens.lens (\UpdateLoggingConfigurationResponse' {status} -> status) (\s@UpdateLoggingConfigurationResponse' {} a -> s {status = a} :: UpdateLoggingConfigurationResponse)

instance
  Prelude.NFData
    UpdateLoggingConfigurationResponse
  where
  rnf UpdateLoggingConfigurationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf status
