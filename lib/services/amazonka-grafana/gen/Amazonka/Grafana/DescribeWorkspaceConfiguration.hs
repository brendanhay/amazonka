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
-- Module      : Amazonka.Grafana.DescribeWorkspaceConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the current configuration string for the given workspace.
module Amazonka.Grafana.DescribeWorkspaceConfiguration
  ( -- * Creating a Request
    DescribeWorkspaceConfiguration (..),
    newDescribeWorkspaceConfiguration,

    -- * Request Lenses
    describeWorkspaceConfiguration_workspaceId,

    -- * Destructuring the Response
    DescribeWorkspaceConfigurationResponse (..),
    newDescribeWorkspaceConfigurationResponse,

    -- * Response Lenses
    describeWorkspaceConfigurationResponse_httpStatus,
    describeWorkspaceConfigurationResponse_configuration,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Grafana.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeWorkspaceConfiguration' smart constructor.
data DescribeWorkspaceConfiguration = DescribeWorkspaceConfiguration'
  { -- | The ID of the workspace to get configuration information for.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorkspaceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaceId', 'describeWorkspaceConfiguration_workspaceId' - The ID of the workspace to get configuration information for.
newDescribeWorkspaceConfiguration ::
  -- | 'workspaceId'
  Prelude.Text ->
  DescribeWorkspaceConfiguration
newDescribeWorkspaceConfiguration pWorkspaceId_ =
  DescribeWorkspaceConfiguration'
    { workspaceId =
        pWorkspaceId_
    }

-- | The ID of the workspace to get configuration information for.
describeWorkspaceConfiguration_workspaceId :: Lens.Lens' DescribeWorkspaceConfiguration Prelude.Text
describeWorkspaceConfiguration_workspaceId = Lens.lens (\DescribeWorkspaceConfiguration' {workspaceId} -> workspaceId) (\s@DescribeWorkspaceConfiguration' {} a -> s {workspaceId = a} :: DescribeWorkspaceConfiguration)

instance
  Core.AWSRequest
    DescribeWorkspaceConfiguration
  where
  type
    AWSResponse DescribeWorkspaceConfiguration =
      DescribeWorkspaceConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkspaceConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "configuration")
      )

instance
  Prelude.Hashable
    DescribeWorkspaceConfiguration
  where
  hashWithSalt
    _salt
    DescribeWorkspaceConfiguration' {..} =
      _salt `Prelude.hashWithSalt` workspaceId

instance
  Prelude.NFData
    DescribeWorkspaceConfiguration
  where
  rnf DescribeWorkspaceConfiguration' {..} =
    Prelude.rnf workspaceId

instance
  Data.ToHeaders
    DescribeWorkspaceConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeWorkspaceConfiguration where
  toPath DescribeWorkspaceConfiguration' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Data.toBS workspaceId,
        "/configuration"
      ]

instance Data.ToQuery DescribeWorkspaceConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeWorkspaceConfigurationResponse' smart constructor.
data DescribeWorkspaceConfigurationResponse = DescribeWorkspaceConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The configuration string for the workspace that you requested. For more
    -- information about the format and configuration options available, see
    -- <https://docs.aws.amazon.com/grafana/latest/userguide/AMG-configure-workspace.html Working in your Grafana workspace>.
    configuration :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorkspaceConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeWorkspaceConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'configuration', 'describeWorkspaceConfigurationResponse_configuration' - The configuration string for the workspace that you requested. For more
-- information about the format and configuration options available, see
-- <https://docs.aws.amazon.com/grafana/latest/userguide/AMG-configure-workspace.html Working in your Grafana workspace>.
newDescribeWorkspaceConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'configuration'
  Prelude.Text ->
  DescribeWorkspaceConfigurationResponse
newDescribeWorkspaceConfigurationResponse
  pHttpStatus_
  pConfiguration_ =
    DescribeWorkspaceConfigurationResponse'
      { httpStatus =
          pHttpStatus_,
        configuration = pConfiguration_
      }

-- | The response's http status code.
describeWorkspaceConfigurationResponse_httpStatus :: Lens.Lens' DescribeWorkspaceConfigurationResponse Prelude.Int
describeWorkspaceConfigurationResponse_httpStatus = Lens.lens (\DescribeWorkspaceConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeWorkspaceConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeWorkspaceConfigurationResponse)

-- | The configuration string for the workspace that you requested. For more
-- information about the format and configuration options available, see
-- <https://docs.aws.amazon.com/grafana/latest/userguide/AMG-configure-workspace.html Working in your Grafana workspace>.
describeWorkspaceConfigurationResponse_configuration :: Lens.Lens' DescribeWorkspaceConfigurationResponse Prelude.Text
describeWorkspaceConfigurationResponse_configuration = Lens.lens (\DescribeWorkspaceConfigurationResponse' {configuration} -> configuration) (\s@DescribeWorkspaceConfigurationResponse' {} a -> s {configuration = a} :: DescribeWorkspaceConfigurationResponse)

instance
  Prelude.NFData
    DescribeWorkspaceConfigurationResponse
  where
  rnf DescribeWorkspaceConfigurationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf configuration
