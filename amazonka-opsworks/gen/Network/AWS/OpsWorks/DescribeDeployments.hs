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
-- Module      : Network.AWS.OpsWorks.DescribeDeployments
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a description of a specified set of deployments.
--
-- This call accepts only one resource-identifying parameter.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information about
-- user permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.DescribeDeployments
  ( -- * Creating a Request
    DescribeDeployments (..),
    newDescribeDeployments,

    -- * Request Lenses
    describeDeployments_deploymentIds,
    describeDeployments_appId,
    describeDeployments_stackId,

    -- * Destructuring the Response
    DescribeDeploymentsResponse (..),
    newDescribeDeploymentsResponse,

    -- * Response Lenses
    describeDeploymentsResponse_deployments,
    describeDeploymentsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeDeployments' smart constructor.
data DescribeDeployments = DescribeDeployments'
  { -- | An array of deployment IDs to be described. If you include this
    -- parameter, the command returns a description of the specified
    -- deployments. Otherwise, it returns a description of every deployment.
    deploymentIds :: Core.Maybe [Core.Text],
    -- | The app ID. If you include this parameter, the command returns a
    -- description of the commands associated with the specified app.
    appId :: Core.Maybe Core.Text,
    -- | The stack ID. If you include this parameter, the command returns a
    -- description of the commands associated with the specified stack.
    stackId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDeployments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentIds', 'describeDeployments_deploymentIds' - An array of deployment IDs to be described. If you include this
-- parameter, the command returns a description of the specified
-- deployments. Otherwise, it returns a description of every deployment.
--
-- 'appId', 'describeDeployments_appId' - The app ID. If you include this parameter, the command returns a
-- description of the commands associated with the specified app.
--
-- 'stackId', 'describeDeployments_stackId' - The stack ID. If you include this parameter, the command returns a
-- description of the commands associated with the specified stack.
newDescribeDeployments ::
  DescribeDeployments
newDescribeDeployments =
  DescribeDeployments'
    { deploymentIds = Core.Nothing,
      appId = Core.Nothing,
      stackId = Core.Nothing
    }

-- | An array of deployment IDs to be described. If you include this
-- parameter, the command returns a description of the specified
-- deployments. Otherwise, it returns a description of every deployment.
describeDeployments_deploymentIds :: Lens.Lens' DescribeDeployments (Core.Maybe [Core.Text])
describeDeployments_deploymentIds = Lens.lens (\DescribeDeployments' {deploymentIds} -> deploymentIds) (\s@DescribeDeployments' {} a -> s {deploymentIds = a} :: DescribeDeployments) Core.. Lens.mapping Lens._Coerce

-- | The app ID. If you include this parameter, the command returns a
-- description of the commands associated with the specified app.
describeDeployments_appId :: Lens.Lens' DescribeDeployments (Core.Maybe Core.Text)
describeDeployments_appId = Lens.lens (\DescribeDeployments' {appId} -> appId) (\s@DescribeDeployments' {} a -> s {appId = a} :: DescribeDeployments)

-- | The stack ID. If you include this parameter, the command returns a
-- description of the commands associated with the specified stack.
describeDeployments_stackId :: Lens.Lens' DescribeDeployments (Core.Maybe Core.Text)
describeDeployments_stackId = Lens.lens (\DescribeDeployments' {stackId} -> stackId) (\s@DescribeDeployments' {} a -> s {stackId = a} :: DescribeDeployments)

instance Core.AWSRequest DescribeDeployments where
  type
    AWSResponse DescribeDeployments =
      DescribeDeploymentsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDeploymentsResponse'
            Core.<$> (x Core..?> "Deployments" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeDeployments

instance Core.NFData DescribeDeployments

instance Core.ToHeaders DescribeDeployments where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.DescribeDeployments" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeDeployments where
  toJSON DescribeDeployments' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DeploymentIds" Core..=) Core.<$> deploymentIds,
            ("AppId" Core..=) Core.<$> appId,
            ("StackId" Core..=) Core.<$> stackId
          ]
      )

instance Core.ToPath DescribeDeployments where
  toPath = Core.const "/"

instance Core.ToQuery DescribeDeployments where
  toQuery = Core.const Core.mempty

-- | Contains the response to a @DescribeDeployments@ request.
--
-- /See:/ 'newDescribeDeploymentsResponse' smart constructor.
data DescribeDeploymentsResponse = DescribeDeploymentsResponse'
  { -- | An array of @Deployment@ objects that describe the deployments.
    deployments :: Core.Maybe [Deployment],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDeploymentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deployments', 'describeDeploymentsResponse_deployments' - An array of @Deployment@ objects that describe the deployments.
--
-- 'httpStatus', 'describeDeploymentsResponse_httpStatus' - The response's http status code.
newDescribeDeploymentsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeDeploymentsResponse
newDescribeDeploymentsResponse pHttpStatus_ =
  DescribeDeploymentsResponse'
    { deployments =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @Deployment@ objects that describe the deployments.
describeDeploymentsResponse_deployments :: Lens.Lens' DescribeDeploymentsResponse (Core.Maybe [Deployment])
describeDeploymentsResponse_deployments = Lens.lens (\DescribeDeploymentsResponse' {deployments} -> deployments) (\s@DescribeDeploymentsResponse' {} a -> s {deployments = a} :: DescribeDeploymentsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeDeploymentsResponse_httpStatus :: Lens.Lens' DescribeDeploymentsResponse Core.Int
describeDeploymentsResponse_httpStatus = Lens.lens (\DescribeDeploymentsResponse' {httpStatus} -> httpStatus) (\s@DescribeDeploymentsResponse' {} a -> s {httpStatus = a} :: DescribeDeploymentsResponse)

instance Core.NFData DescribeDeploymentsResponse
