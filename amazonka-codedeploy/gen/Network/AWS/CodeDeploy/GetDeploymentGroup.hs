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
-- Module      : Network.AWS.CodeDeploy.GetDeploymentGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a deployment group.
module Network.AWS.CodeDeploy.GetDeploymentGroup
  ( -- * Creating a Request
    GetDeploymentGroup (..),
    newGetDeploymentGroup,

    -- * Request Lenses
    getDeploymentGroup_applicationName,
    getDeploymentGroup_deploymentGroupName,

    -- * Destructuring the Response
    GetDeploymentGroupResponse (..),
    newGetDeploymentGroupResponse,

    -- * Response Lenses
    getDeploymentGroupResponse_deploymentGroupInfo,
    getDeploymentGroupResponse_httpStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetDeploymentGroup@ operation.
--
-- /See:/ 'newGetDeploymentGroup' smart constructor.
data GetDeploymentGroup = GetDeploymentGroup'
  { -- | The name of an AWS CodeDeploy application associated with the IAM user
    -- or AWS account.
    applicationName :: Core.Text,
    -- | The name of a deployment group for the specified application.
    deploymentGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDeploymentGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'getDeploymentGroup_applicationName' - The name of an AWS CodeDeploy application associated with the IAM user
-- or AWS account.
--
-- 'deploymentGroupName', 'getDeploymentGroup_deploymentGroupName' - The name of a deployment group for the specified application.
newGetDeploymentGroup ::
  -- | 'applicationName'
  Core.Text ->
  -- | 'deploymentGroupName'
  Core.Text ->
  GetDeploymentGroup
newGetDeploymentGroup
  pApplicationName_
  pDeploymentGroupName_ =
    GetDeploymentGroup'
      { applicationName =
          pApplicationName_,
        deploymentGroupName = pDeploymentGroupName_
      }

-- | The name of an AWS CodeDeploy application associated with the IAM user
-- or AWS account.
getDeploymentGroup_applicationName :: Lens.Lens' GetDeploymentGroup Core.Text
getDeploymentGroup_applicationName = Lens.lens (\GetDeploymentGroup' {applicationName} -> applicationName) (\s@GetDeploymentGroup' {} a -> s {applicationName = a} :: GetDeploymentGroup)

-- | The name of a deployment group for the specified application.
getDeploymentGroup_deploymentGroupName :: Lens.Lens' GetDeploymentGroup Core.Text
getDeploymentGroup_deploymentGroupName = Lens.lens (\GetDeploymentGroup' {deploymentGroupName} -> deploymentGroupName) (\s@GetDeploymentGroup' {} a -> s {deploymentGroupName = a} :: GetDeploymentGroup)

instance Core.AWSRequest GetDeploymentGroup where
  type
    AWSResponse GetDeploymentGroup =
      GetDeploymentGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeploymentGroupResponse'
            Core.<$> (x Core..?> "deploymentGroupInfo")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDeploymentGroup

instance Core.NFData GetDeploymentGroup

instance Core.ToHeaders GetDeploymentGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.GetDeploymentGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetDeploymentGroup where
  toJSON GetDeploymentGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("applicationName" Core..= applicationName),
            Core.Just
              ("deploymentGroupName" Core..= deploymentGroupName)
          ]
      )

instance Core.ToPath GetDeploymentGroup where
  toPath = Core.const "/"

instance Core.ToQuery GetDeploymentGroup where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @GetDeploymentGroup@ operation.
--
-- /See:/ 'newGetDeploymentGroupResponse' smart constructor.
data GetDeploymentGroupResponse = GetDeploymentGroupResponse'
  { -- | Information about the deployment group.
    deploymentGroupInfo :: Core.Maybe DeploymentGroupInfo,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDeploymentGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentGroupInfo', 'getDeploymentGroupResponse_deploymentGroupInfo' - Information about the deployment group.
--
-- 'httpStatus', 'getDeploymentGroupResponse_httpStatus' - The response's http status code.
newGetDeploymentGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDeploymentGroupResponse
newGetDeploymentGroupResponse pHttpStatus_ =
  GetDeploymentGroupResponse'
    { deploymentGroupInfo =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the deployment group.
getDeploymentGroupResponse_deploymentGroupInfo :: Lens.Lens' GetDeploymentGroupResponse (Core.Maybe DeploymentGroupInfo)
getDeploymentGroupResponse_deploymentGroupInfo = Lens.lens (\GetDeploymentGroupResponse' {deploymentGroupInfo} -> deploymentGroupInfo) (\s@GetDeploymentGroupResponse' {} a -> s {deploymentGroupInfo = a} :: GetDeploymentGroupResponse)

-- | The response's http status code.
getDeploymentGroupResponse_httpStatus :: Lens.Lens' GetDeploymentGroupResponse Core.Int
getDeploymentGroupResponse_httpStatus = Lens.lens (\GetDeploymentGroupResponse' {httpStatus} -> httpStatus) (\s@GetDeploymentGroupResponse' {} a -> s {httpStatus = a} :: GetDeploymentGroupResponse)

instance Core.NFData GetDeploymentGroupResponse
