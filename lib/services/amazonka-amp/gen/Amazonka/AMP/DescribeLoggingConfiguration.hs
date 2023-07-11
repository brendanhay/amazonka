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
-- Module      : Amazonka.AMP.DescribeLoggingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes logging configuration.
module Amazonka.AMP.DescribeLoggingConfiguration
  ( -- * Creating a Request
    DescribeLoggingConfiguration (..),
    newDescribeLoggingConfiguration,

    -- * Request Lenses
    describeLoggingConfiguration_workspaceId,

    -- * Destructuring the Response
    DescribeLoggingConfigurationResponse (..),
    newDescribeLoggingConfigurationResponse,

    -- * Response Lenses
    describeLoggingConfigurationResponse_httpStatus,
    describeLoggingConfigurationResponse_loggingConfiguration,
  )
where

import Amazonka.AMP.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a DescribeLoggingConfiguration operation.
--
-- /See:/ 'newDescribeLoggingConfiguration' smart constructor.
data DescribeLoggingConfiguration = DescribeLoggingConfiguration'
  { -- | The ID of the workspace to vend logs to.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLoggingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaceId', 'describeLoggingConfiguration_workspaceId' - The ID of the workspace to vend logs to.
newDescribeLoggingConfiguration ::
  -- | 'workspaceId'
  Prelude.Text ->
  DescribeLoggingConfiguration
newDescribeLoggingConfiguration pWorkspaceId_ =
  DescribeLoggingConfiguration'
    { workspaceId =
        pWorkspaceId_
    }

-- | The ID of the workspace to vend logs to.
describeLoggingConfiguration_workspaceId :: Lens.Lens' DescribeLoggingConfiguration Prelude.Text
describeLoggingConfiguration_workspaceId = Lens.lens (\DescribeLoggingConfiguration' {workspaceId} -> workspaceId) (\s@DescribeLoggingConfiguration' {} a -> s {workspaceId = a} :: DescribeLoggingConfiguration)

instance Core.AWSRequest DescribeLoggingConfiguration where
  type
    AWSResponse DescribeLoggingConfiguration =
      DescribeLoggingConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLoggingConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "loggingConfiguration")
      )

instance
  Prelude.Hashable
    DescribeLoggingConfiguration
  where
  hashWithSalt _salt DescribeLoggingConfiguration' {..} =
    _salt `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData DescribeLoggingConfiguration where
  rnf DescribeLoggingConfiguration' {..} =
    Prelude.rnf workspaceId

instance Data.ToHeaders DescribeLoggingConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeLoggingConfiguration where
  toPath DescribeLoggingConfiguration' {..} =
    Prelude.mconcat
      ["/workspaces/", Data.toBS workspaceId, "/logging"]

instance Data.ToQuery DescribeLoggingConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a DescribeLoggingConfiguration operation.
--
-- /See:/ 'newDescribeLoggingConfigurationResponse' smart constructor.
data DescribeLoggingConfigurationResponse = DescribeLoggingConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Metadata object containing information about the logging configuration
    -- of a workspace.
    loggingConfiguration :: LoggingConfigurationMetadata
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLoggingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeLoggingConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'loggingConfiguration', 'describeLoggingConfigurationResponse_loggingConfiguration' - Metadata object containing information about the logging configuration
-- of a workspace.
newDescribeLoggingConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'loggingConfiguration'
  LoggingConfigurationMetadata ->
  DescribeLoggingConfigurationResponse
newDescribeLoggingConfigurationResponse
  pHttpStatus_
  pLoggingConfiguration_ =
    DescribeLoggingConfigurationResponse'
      { httpStatus =
          pHttpStatus_,
        loggingConfiguration =
          pLoggingConfiguration_
      }

-- | The response's http status code.
describeLoggingConfigurationResponse_httpStatus :: Lens.Lens' DescribeLoggingConfigurationResponse Prelude.Int
describeLoggingConfigurationResponse_httpStatus = Lens.lens (\DescribeLoggingConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeLoggingConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeLoggingConfigurationResponse)

-- | Metadata object containing information about the logging configuration
-- of a workspace.
describeLoggingConfigurationResponse_loggingConfiguration :: Lens.Lens' DescribeLoggingConfigurationResponse LoggingConfigurationMetadata
describeLoggingConfigurationResponse_loggingConfiguration = Lens.lens (\DescribeLoggingConfigurationResponse' {loggingConfiguration} -> loggingConfiguration) (\s@DescribeLoggingConfigurationResponse' {} a -> s {loggingConfiguration = a} :: DescribeLoggingConfigurationResponse)

instance
  Prelude.NFData
    DescribeLoggingConfigurationResponse
  where
  rnf DescribeLoggingConfigurationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf loggingConfiguration
