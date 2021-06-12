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
-- Module      : Network.AWS.CodeDeploy.DeleteDeploymentGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a deployment group.
module Network.AWS.CodeDeploy.DeleteDeploymentGroup
  ( -- * Creating a Request
    DeleteDeploymentGroup (..),
    newDeleteDeploymentGroup,

    -- * Request Lenses
    deleteDeploymentGroup_applicationName,
    deleteDeploymentGroup_deploymentGroupName,

    -- * Destructuring the Response
    DeleteDeploymentGroupResponse (..),
    newDeleteDeploymentGroupResponse,

    -- * Response Lenses
    deleteDeploymentGroupResponse_hooksNotCleanedUp,
    deleteDeploymentGroupResponse_httpStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteDeploymentGroup@ operation.
--
-- /See:/ 'newDeleteDeploymentGroup' smart constructor.
data DeleteDeploymentGroup = DeleteDeploymentGroup'
  { -- | The name of an AWS CodeDeploy application associated with the IAM user
    -- or AWS account.
    applicationName :: Core.Text,
    -- | The name of a deployment group for the specified application.
    deploymentGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDeploymentGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'deleteDeploymentGroup_applicationName' - The name of an AWS CodeDeploy application associated with the IAM user
-- or AWS account.
--
-- 'deploymentGroupName', 'deleteDeploymentGroup_deploymentGroupName' - The name of a deployment group for the specified application.
newDeleteDeploymentGroup ::
  -- | 'applicationName'
  Core.Text ->
  -- | 'deploymentGroupName'
  Core.Text ->
  DeleteDeploymentGroup
newDeleteDeploymentGroup
  pApplicationName_
  pDeploymentGroupName_ =
    DeleteDeploymentGroup'
      { applicationName =
          pApplicationName_,
        deploymentGroupName = pDeploymentGroupName_
      }

-- | The name of an AWS CodeDeploy application associated with the IAM user
-- or AWS account.
deleteDeploymentGroup_applicationName :: Lens.Lens' DeleteDeploymentGroup Core.Text
deleteDeploymentGroup_applicationName = Lens.lens (\DeleteDeploymentGroup' {applicationName} -> applicationName) (\s@DeleteDeploymentGroup' {} a -> s {applicationName = a} :: DeleteDeploymentGroup)

-- | The name of a deployment group for the specified application.
deleteDeploymentGroup_deploymentGroupName :: Lens.Lens' DeleteDeploymentGroup Core.Text
deleteDeploymentGroup_deploymentGroupName = Lens.lens (\DeleteDeploymentGroup' {deploymentGroupName} -> deploymentGroupName) (\s@DeleteDeploymentGroup' {} a -> s {deploymentGroupName = a} :: DeleteDeploymentGroup)

instance Core.AWSRequest DeleteDeploymentGroup where
  type
    AWSResponse DeleteDeploymentGroup =
      DeleteDeploymentGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDeploymentGroupResponse'
            Core.<$> (x Core..?> "hooksNotCleanedUp" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteDeploymentGroup

instance Core.NFData DeleteDeploymentGroup

instance Core.ToHeaders DeleteDeploymentGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.DeleteDeploymentGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteDeploymentGroup where
  toJSON DeleteDeploymentGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("applicationName" Core..= applicationName),
            Core.Just
              ("deploymentGroupName" Core..= deploymentGroupName)
          ]
      )

instance Core.ToPath DeleteDeploymentGroup where
  toPath = Core.const "/"

instance Core.ToQuery DeleteDeploymentGroup where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @DeleteDeploymentGroup@ operation.
--
-- /See:/ 'newDeleteDeploymentGroupResponse' smart constructor.
data DeleteDeploymentGroupResponse = DeleteDeploymentGroupResponse'
  { -- | If the output contains no data, and the corresponding deployment group
    -- contained at least one Auto Scaling group, AWS CodeDeploy successfully
    -- removed all corresponding Auto Scaling lifecycle event hooks from the
    -- Amazon EC2 instances in the Auto Scaling group. If the output contains
    -- data, AWS CodeDeploy could not remove some Auto Scaling lifecycle event
    -- hooks from the Amazon EC2 instances in the Auto Scaling group.
    hooksNotCleanedUp :: Core.Maybe [AutoScalingGroup],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDeploymentGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hooksNotCleanedUp', 'deleteDeploymentGroupResponse_hooksNotCleanedUp' - If the output contains no data, and the corresponding deployment group
-- contained at least one Auto Scaling group, AWS CodeDeploy successfully
-- removed all corresponding Auto Scaling lifecycle event hooks from the
-- Amazon EC2 instances in the Auto Scaling group. If the output contains
-- data, AWS CodeDeploy could not remove some Auto Scaling lifecycle event
-- hooks from the Amazon EC2 instances in the Auto Scaling group.
--
-- 'httpStatus', 'deleteDeploymentGroupResponse_httpStatus' - The response's http status code.
newDeleteDeploymentGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteDeploymentGroupResponse
newDeleteDeploymentGroupResponse pHttpStatus_ =
  DeleteDeploymentGroupResponse'
    { hooksNotCleanedUp =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the output contains no data, and the corresponding deployment group
-- contained at least one Auto Scaling group, AWS CodeDeploy successfully
-- removed all corresponding Auto Scaling lifecycle event hooks from the
-- Amazon EC2 instances in the Auto Scaling group. If the output contains
-- data, AWS CodeDeploy could not remove some Auto Scaling lifecycle event
-- hooks from the Amazon EC2 instances in the Auto Scaling group.
deleteDeploymentGroupResponse_hooksNotCleanedUp :: Lens.Lens' DeleteDeploymentGroupResponse (Core.Maybe [AutoScalingGroup])
deleteDeploymentGroupResponse_hooksNotCleanedUp = Lens.lens (\DeleteDeploymentGroupResponse' {hooksNotCleanedUp} -> hooksNotCleanedUp) (\s@DeleteDeploymentGroupResponse' {} a -> s {hooksNotCleanedUp = a} :: DeleteDeploymentGroupResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
deleteDeploymentGroupResponse_httpStatus :: Lens.Lens' DeleteDeploymentGroupResponse Core.Int
deleteDeploymentGroupResponse_httpStatus = Lens.lens (\DeleteDeploymentGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteDeploymentGroupResponse' {} a -> s {httpStatus = a} :: DeleteDeploymentGroupResponse)

instance Core.NFData DeleteDeploymentGroupResponse
