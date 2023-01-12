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
-- Module      : Amazonka.OpsWorks.DescribeStackProvisioningParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a description of a stack\'s provisioning parameters.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack or an attached
-- policy that explicitly grants permissions. For more information about
-- user permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Amazonka.OpsWorks.DescribeStackProvisioningParameters
  ( -- * Creating a Request
    DescribeStackProvisioningParameters (..),
    newDescribeStackProvisioningParameters,

    -- * Request Lenses
    describeStackProvisioningParameters_stackId,

    -- * Destructuring the Response
    DescribeStackProvisioningParametersResponse (..),
    newDescribeStackProvisioningParametersResponse,

    -- * Response Lenses
    describeStackProvisioningParametersResponse_agentInstallerUrl,
    describeStackProvisioningParametersResponse_parameters,
    describeStackProvisioningParametersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeStackProvisioningParameters' smart constructor.
data DescribeStackProvisioningParameters = DescribeStackProvisioningParameters'
  { -- | The stack ID.
    stackId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStackProvisioningParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackId', 'describeStackProvisioningParameters_stackId' - The stack ID.
newDescribeStackProvisioningParameters ::
  -- | 'stackId'
  Prelude.Text ->
  DescribeStackProvisioningParameters
newDescribeStackProvisioningParameters pStackId_ =
  DescribeStackProvisioningParameters'
    { stackId =
        pStackId_
    }

-- | The stack ID.
describeStackProvisioningParameters_stackId :: Lens.Lens' DescribeStackProvisioningParameters Prelude.Text
describeStackProvisioningParameters_stackId = Lens.lens (\DescribeStackProvisioningParameters' {stackId} -> stackId) (\s@DescribeStackProvisioningParameters' {} a -> s {stackId = a} :: DescribeStackProvisioningParameters)

instance
  Core.AWSRequest
    DescribeStackProvisioningParameters
  where
  type
    AWSResponse DescribeStackProvisioningParameters =
      DescribeStackProvisioningParametersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStackProvisioningParametersResponse'
            Prelude.<$> (x Data..?> "AgentInstallerUrl")
              Prelude.<*> (x Data..?> "Parameters" Core..!@ Prelude.mempty)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeStackProvisioningParameters
  where
  hashWithSalt
    _salt
    DescribeStackProvisioningParameters' {..} =
      _salt `Prelude.hashWithSalt` stackId

instance
  Prelude.NFData
    DescribeStackProvisioningParameters
  where
  rnf DescribeStackProvisioningParameters' {..} =
    Prelude.rnf stackId

instance
  Data.ToHeaders
    DescribeStackProvisioningParameters
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.DescribeStackProvisioningParameters" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeStackProvisioningParameters
  where
  toJSON DescribeStackProvisioningParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("StackId" Data..= stackId)]
      )

instance
  Data.ToPath
    DescribeStackProvisioningParameters
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeStackProvisioningParameters
  where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @DescribeStackProvisioningParameters@
-- request.
--
-- /See:/ 'newDescribeStackProvisioningParametersResponse' smart constructor.
data DescribeStackProvisioningParametersResponse = DescribeStackProvisioningParametersResponse'
  { -- | The AWS OpsWorks Stacks agent installer\'s URL.
    agentInstallerUrl :: Prelude.Maybe Prelude.Text,
    -- | An embedded object that contains the provisioning parameters.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStackProvisioningParametersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentInstallerUrl', 'describeStackProvisioningParametersResponse_agentInstallerUrl' - The AWS OpsWorks Stacks agent installer\'s URL.
--
-- 'parameters', 'describeStackProvisioningParametersResponse_parameters' - An embedded object that contains the provisioning parameters.
--
-- 'httpStatus', 'describeStackProvisioningParametersResponse_httpStatus' - The response's http status code.
newDescribeStackProvisioningParametersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeStackProvisioningParametersResponse
newDescribeStackProvisioningParametersResponse
  pHttpStatus_ =
    DescribeStackProvisioningParametersResponse'
      { agentInstallerUrl =
          Prelude.Nothing,
        parameters = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The AWS OpsWorks Stacks agent installer\'s URL.
describeStackProvisioningParametersResponse_agentInstallerUrl :: Lens.Lens' DescribeStackProvisioningParametersResponse (Prelude.Maybe Prelude.Text)
describeStackProvisioningParametersResponse_agentInstallerUrl = Lens.lens (\DescribeStackProvisioningParametersResponse' {agentInstallerUrl} -> agentInstallerUrl) (\s@DescribeStackProvisioningParametersResponse' {} a -> s {agentInstallerUrl = a} :: DescribeStackProvisioningParametersResponse)

-- | An embedded object that contains the provisioning parameters.
describeStackProvisioningParametersResponse_parameters :: Lens.Lens' DescribeStackProvisioningParametersResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeStackProvisioningParametersResponse_parameters = Lens.lens (\DescribeStackProvisioningParametersResponse' {parameters} -> parameters) (\s@DescribeStackProvisioningParametersResponse' {} a -> s {parameters = a} :: DescribeStackProvisioningParametersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeStackProvisioningParametersResponse_httpStatus :: Lens.Lens' DescribeStackProvisioningParametersResponse Prelude.Int
describeStackProvisioningParametersResponse_httpStatus = Lens.lens (\DescribeStackProvisioningParametersResponse' {httpStatus} -> httpStatus) (\s@DescribeStackProvisioningParametersResponse' {} a -> s {httpStatus = a} :: DescribeStackProvisioningParametersResponse)

instance
  Prelude.NFData
    DescribeStackProvisioningParametersResponse
  where
  rnf DescribeStackProvisioningParametersResponse' {..} =
    Prelude.rnf agentInstallerUrl
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf httpStatus
