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
-- Module      : Amazonka.OpsWorks.DescribeDeployments
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.OpsWorks.DescribeDeployments
  ( -- * Creating a Request
    DescribeDeployments (..),
    newDescribeDeployments,

    -- * Request Lenses
    describeDeployments_stackId,
    describeDeployments_deploymentIds,
    describeDeployments_appId,

    -- * Destructuring the Response
    DescribeDeploymentsResponse (..),
    newDescribeDeploymentsResponse,

    -- * Response Lenses
    describeDeploymentsResponse_deployments,
    describeDeploymentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDeployments' smart constructor.
data DescribeDeployments = DescribeDeployments'
  { -- | The stack ID. If you include this parameter, the command returns a
    -- description of the commands associated with the specified stack.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | An array of deployment IDs to be described. If you include this
    -- parameter, the command returns a description of the specified
    -- deployments. Otherwise, it returns a description of every deployment.
    deploymentIds :: Prelude.Maybe [Prelude.Text],
    -- | The app ID. If you include this parameter, the command returns a
    -- description of the commands associated with the specified app.
    appId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDeployments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackId', 'describeDeployments_stackId' - The stack ID. If you include this parameter, the command returns a
-- description of the commands associated with the specified stack.
--
-- 'deploymentIds', 'describeDeployments_deploymentIds' - An array of deployment IDs to be described. If you include this
-- parameter, the command returns a description of the specified
-- deployments. Otherwise, it returns a description of every deployment.
--
-- 'appId', 'describeDeployments_appId' - The app ID. If you include this parameter, the command returns a
-- description of the commands associated with the specified app.
newDescribeDeployments ::
  DescribeDeployments
newDescribeDeployments =
  DescribeDeployments'
    { stackId = Prelude.Nothing,
      deploymentIds = Prelude.Nothing,
      appId = Prelude.Nothing
    }

-- | The stack ID. If you include this parameter, the command returns a
-- description of the commands associated with the specified stack.
describeDeployments_stackId :: Lens.Lens' DescribeDeployments (Prelude.Maybe Prelude.Text)
describeDeployments_stackId = Lens.lens (\DescribeDeployments' {stackId} -> stackId) (\s@DescribeDeployments' {} a -> s {stackId = a} :: DescribeDeployments)

-- | An array of deployment IDs to be described. If you include this
-- parameter, the command returns a description of the specified
-- deployments. Otherwise, it returns a description of every deployment.
describeDeployments_deploymentIds :: Lens.Lens' DescribeDeployments (Prelude.Maybe [Prelude.Text])
describeDeployments_deploymentIds = Lens.lens (\DescribeDeployments' {deploymentIds} -> deploymentIds) (\s@DescribeDeployments' {} a -> s {deploymentIds = a} :: DescribeDeployments) Prelude.. Lens.mapping Lens.coerced

-- | The app ID. If you include this parameter, the command returns a
-- description of the commands associated with the specified app.
describeDeployments_appId :: Lens.Lens' DescribeDeployments (Prelude.Maybe Prelude.Text)
describeDeployments_appId = Lens.lens (\DescribeDeployments' {appId} -> appId) (\s@DescribeDeployments' {} a -> s {appId = a} :: DescribeDeployments)

instance Core.AWSRequest DescribeDeployments where
  type
    AWSResponse DescribeDeployments =
      DescribeDeploymentsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDeploymentsResponse'
            Prelude.<$> (x Core..?> "Deployments" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDeployments where
  hashWithSalt _salt DescribeDeployments' {..} =
    _salt `Prelude.hashWithSalt` stackId
      `Prelude.hashWithSalt` deploymentIds
      `Prelude.hashWithSalt` appId

instance Prelude.NFData DescribeDeployments where
  rnf DescribeDeployments' {..} =
    Prelude.rnf stackId
      `Prelude.seq` Prelude.rnf deploymentIds
      `Prelude.seq` Prelude.rnf appId

instance Core.ToHeaders DescribeDeployments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.DescribeDeployments" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeDeployments where
  toJSON DescribeDeployments' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StackId" Core..=) Prelude.<$> stackId,
            ("DeploymentIds" Core..=) Prelude.<$> deploymentIds,
            ("AppId" Core..=) Prelude.<$> appId
          ]
      )

instance Core.ToPath DescribeDeployments where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeDeployments where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @DescribeDeployments@ request.
--
-- /See:/ 'newDescribeDeploymentsResponse' smart constructor.
data DescribeDeploymentsResponse = DescribeDeploymentsResponse'
  { -- | An array of @Deployment@ objects that describe the deployments.
    deployments :: Prelude.Maybe [Deployment],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeDeploymentsResponse
newDescribeDeploymentsResponse pHttpStatus_ =
  DescribeDeploymentsResponse'
    { deployments =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @Deployment@ objects that describe the deployments.
describeDeploymentsResponse_deployments :: Lens.Lens' DescribeDeploymentsResponse (Prelude.Maybe [Deployment])
describeDeploymentsResponse_deployments = Lens.lens (\DescribeDeploymentsResponse' {deployments} -> deployments) (\s@DescribeDeploymentsResponse' {} a -> s {deployments = a} :: DescribeDeploymentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeDeploymentsResponse_httpStatus :: Lens.Lens' DescribeDeploymentsResponse Prelude.Int
describeDeploymentsResponse_httpStatus = Lens.lens (\DescribeDeploymentsResponse' {httpStatus} -> httpStatus) (\s@DescribeDeploymentsResponse' {} a -> s {httpStatus = a} :: DescribeDeploymentsResponse)

instance Prelude.NFData DescribeDeploymentsResponse where
  rnf DescribeDeploymentsResponse' {..} =
    Prelude.rnf deployments
      `Prelude.seq` Prelude.rnf httpStatus
