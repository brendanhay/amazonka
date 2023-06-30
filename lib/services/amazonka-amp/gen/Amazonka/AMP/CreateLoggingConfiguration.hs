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
-- Module      : Amazonka.AMP.CreateLoggingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create logging configuration.
module Amazonka.AMP.CreateLoggingConfiguration
  ( -- * Creating a Request
    CreateLoggingConfiguration (..),
    newCreateLoggingConfiguration,

    -- * Request Lenses
    createLoggingConfiguration_clientToken,
    createLoggingConfiguration_logGroupArn,
    createLoggingConfiguration_workspaceId,

    -- * Destructuring the Response
    CreateLoggingConfigurationResponse (..),
    newCreateLoggingConfigurationResponse,

    -- * Response Lenses
    createLoggingConfigurationResponse_httpStatus,
    createLoggingConfigurationResponse_status,
  )
where

import Amazonka.AMP.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a CreateLoggingConfiguration operation.
--
-- /See:/ 'newCreateLoggingConfiguration' smart constructor.
data CreateLoggingConfiguration = CreateLoggingConfiguration'
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
-- Create a value of 'CreateLoggingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createLoggingConfiguration_clientToken' - Optional, unique, case-sensitive, user-provided identifier to ensure the
-- idempotency of the request.
--
-- 'logGroupArn', 'createLoggingConfiguration_logGroupArn' - The ARN of the CW log group to which the vended log data will be
-- published.
--
-- 'workspaceId', 'createLoggingConfiguration_workspaceId' - The ID of the workspace to vend logs to.
newCreateLoggingConfiguration ::
  -- | 'logGroupArn'
  Prelude.Text ->
  -- | 'workspaceId'
  Prelude.Text ->
  CreateLoggingConfiguration
newCreateLoggingConfiguration
  pLogGroupArn_
  pWorkspaceId_ =
    CreateLoggingConfiguration'
      { clientToken =
          Prelude.Nothing,
        logGroupArn = pLogGroupArn_,
        workspaceId = pWorkspaceId_
      }

-- | Optional, unique, case-sensitive, user-provided identifier to ensure the
-- idempotency of the request.
createLoggingConfiguration_clientToken :: Lens.Lens' CreateLoggingConfiguration (Prelude.Maybe Prelude.Text)
createLoggingConfiguration_clientToken = Lens.lens (\CreateLoggingConfiguration' {clientToken} -> clientToken) (\s@CreateLoggingConfiguration' {} a -> s {clientToken = a} :: CreateLoggingConfiguration)

-- | The ARN of the CW log group to which the vended log data will be
-- published.
createLoggingConfiguration_logGroupArn :: Lens.Lens' CreateLoggingConfiguration Prelude.Text
createLoggingConfiguration_logGroupArn = Lens.lens (\CreateLoggingConfiguration' {logGroupArn} -> logGroupArn) (\s@CreateLoggingConfiguration' {} a -> s {logGroupArn = a} :: CreateLoggingConfiguration)

-- | The ID of the workspace to vend logs to.
createLoggingConfiguration_workspaceId :: Lens.Lens' CreateLoggingConfiguration Prelude.Text
createLoggingConfiguration_workspaceId = Lens.lens (\CreateLoggingConfiguration' {workspaceId} -> workspaceId) (\s@CreateLoggingConfiguration' {} a -> s {workspaceId = a} :: CreateLoggingConfiguration)

instance Core.AWSRequest CreateLoggingConfiguration where
  type
    AWSResponse CreateLoggingConfiguration =
      CreateLoggingConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLoggingConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "status")
      )

instance Prelude.Hashable CreateLoggingConfiguration where
  hashWithSalt _salt CreateLoggingConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` logGroupArn
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData CreateLoggingConfiguration where
  rnf CreateLoggingConfiguration' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf logGroupArn
      `Prelude.seq` Prelude.rnf workspaceId

instance Data.ToHeaders CreateLoggingConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLoggingConfiguration where
  toJSON CreateLoggingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("logGroupArn" Data..= logGroupArn)
          ]
      )

instance Data.ToPath CreateLoggingConfiguration where
  toPath CreateLoggingConfiguration' {..} =
    Prelude.mconcat
      ["/workspaces/", Data.toBS workspaceId, "/logging"]

instance Data.ToQuery CreateLoggingConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a CreateLoggingConfiguration operation.
--
-- /See:/ 'newCreateLoggingConfigurationResponse' smart constructor.
data CreateLoggingConfigurationResponse = CreateLoggingConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of the logging configuration.
    status :: LoggingConfigurationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLoggingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createLoggingConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'status', 'createLoggingConfigurationResponse_status' - The status of the logging configuration.
newCreateLoggingConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'status'
  LoggingConfigurationStatus ->
  CreateLoggingConfigurationResponse
newCreateLoggingConfigurationResponse
  pHttpStatus_
  pStatus_ =
    CreateLoggingConfigurationResponse'
      { httpStatus =
          pHttpStatus_,
        status = pStatus_
      }

-- | The response's http status code.
createLoggingConfigurationResponse_httpStatus :: Lens.Lens' CreateLoggingConfigurationResponse Prelude.Int
createLoggingConfigurationResponse_httpStatus = Lens.lens (\CreateLoggingConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateLoggingConfigurationResponse' {} a -> s {httpStatus = a} :: CreateLoggingConfigurationResponse)

-- | The status of the logging configuration.
createLoggingConfigurationResponse_status :: Lens.Lens' CreateLoggingConfigurationResponse LoggingConfigurationStatus
createLoggingConfigurationResponse_status = Lens.lens (\CreateLoggingConfigurationResponse' {status} -> status) (\s@CreateLoggingConfigurationResponse' {} a -> s {status = a} :: CreateLoggingConfigurationResponse)

instance
  Prelude.NFData
    CreateLoggingConfigurationResponse
  where
  rnf CreateLoggingConfigurationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf status
