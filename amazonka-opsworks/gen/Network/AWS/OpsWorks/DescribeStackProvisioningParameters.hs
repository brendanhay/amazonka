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
-- Module      : Network.AWS.OpsWorks.DescribeStackProvisioningParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.OpsWorks.DescribeStackProvisioningParameters
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeStackProvisioningParameters' smart constructor.
data DescribeStackProvisioningParameters = DescribeStackProvisioningParameters'
  { -- | The stack ID.
    stackId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeStackProvisioningParameters
newDescribeStackProvisioningParameters pStackId_ =
  DescribeStackProvisioningParameters'
    { stackId =
        pStackId_
    }

-- | The stack ID.
describeStackProvisioningParameters_stackId :: Lens.Lens' DescribeStackProvisioningParameters Core.Text
describeStackProvisioningParameters_stackId = Lens.lens (\DescribeStackProvisioningParameters' {stackId} -> stackId) (\s@DescribeStackProvisioningParameters' {} a -> s {stackId = a} :: DescribeStackProvisioningParameters)

instance
  Core.AWSRequest
    DescribeStackProvisioningParameters
  where
  type
    AWSResponse DescribeStackProvisioningParameters =
      DescribeStackProvisioningParametersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStackProvisioningParametersResponse'
            Core.<$> (x Core..?> "AgentInstallerUrl")
            Core.<*> (x Core..?> "Parameters" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeStackProvisioningParameters

instance
  Core.NFData
    DescribeStackProvisioningParameters

instance
  Core.ToHeaders
    DescribeStackProvisioningParameters
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.DescribeStackProvisioningParameters" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeStackProvisioningParameters
  where
  toJSON DescribeStackProvisioningParameters' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("StackId" Core..= stackId)]
      )

instance
  Core.ToPath
    DescribeStackProvisioningParameters
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeStackProvisioningParameters
  where
  toQuery = Core.const Core.mempty

-- | Contains the response to a @DescribeStackProvisioningParameters@
-- request.
--
-- /See:/ 'newDescribeStackProvisioningParametersResponse' smart constructor.
data DescribeStackProvisioningParametersResponse = DescribeStackProvisioningParametersResponse'
  { -- | The AWS OpsWorks Stacks agent installer\'s URL.
    agentInstallerUrl :: Core.Maybe Core.Text,
    -- | An embedded object that contains the provisioning parameters.
    parameters :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeStackProvisioningParametersResponse
newDescribeStackProvisioningParametersResponse
  pHttpStatus_ =
    DescribeStackProvisioningParametersResponse'
      { agentInstallerUrl =
          Core.Nothing,
        parameters = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The AWS OpsWorks Stacks agent installer\'s URL.
describeStackProvisioningParametersResponse_agentInstallerUrl :: Lens.Lens' DescribeStackProvisioningParametersResponse (Core.Maybe Core.Text)
describeStackProvisioningParametersResponse_agentInstallerUrl = Lens.lens (\DescribeStackProvisioningParametersResponse' {agentInstallerUrl} -> agentInstallerUrl) (\s@DescribeStackProvisioningParametersResponse' {} a -> s {agentInstallerUrl = a} :: DescribeStackProvisioningParametersResponse)

-- | An embedded object that contains the provisioning parameters.
describeStackProvisioningParametersResponse_parameters :: Lens.Lens' DescribeStackProvisioningParametersResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
describeStackProvisioningParametersResponse_parameters = Lens.lens (\DescribeStackProvisioningParametersResponse' {parameters} -> parameters) (\s@DescribeStackProvisioningParametersResponse' {} a -> s {parameters = a} :: DescribeStackProvisioningParametersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeStackProvisioningParametersResponse_httpStatus :: Lens.Lens' DescribeStackProvisioningParametersResponse Core.Int
describeStackProvisioningParametersResponse_httpStatus = Lens.lens (\DescribeStackProvisioningParametersResponse' {httpStatus} -> httpStatus) (\s@DescribeStackProvisioningParametersResponse' {} a -> s {httpStatus = a} :: DescribeStackProvisioningParametersResponse)

instance
  Core.NFData
    DescribeStackProvisioningParametersResponse
