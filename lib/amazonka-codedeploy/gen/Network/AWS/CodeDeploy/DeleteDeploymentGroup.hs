{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.DeleteDeploymentGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a deployment group.
module Network.AWS.CodeDeploy.DeleteDeploymentGroup
  ( -- * Creating a request
    DeleteDeploymentGroup (..),
    mkDeleteDeploymentGroup,

    -- ** Request lenses
    ddgApplicationName,
    ddgDeploymentGroupName,

    -- * Destructuring the response
    DeleteDeploymentGroupResponse (..),
    mkDeleteDeploymentGroupResponse,

    -- ** Response lenses
    ddgrsHooksNotCleanedUp,
    ddgrsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @DeleteDeploymentGroup@ operation.
--
-- /See:/ 'mkDeleteDeploymentGroup' smart constructor.
data DeleteDeploymentGroup = DeleteDeploymentGroup'
  { -- | The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
    applicationName :: Lude.Text,
    -- | The name of a deployment group for the specified application.
    deploymentGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDeploymentGroup' with the minimum fields required to make a request.
--
-- * 'applicationName' - The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
-- * 'deploymentGroupName' - The name of a deployment group for the specified application.
mkDeleteDeploymentGroup ::
  -- | 'applicationName'
  Lude.Text ->
  -- | 'deploymentGroupName'
  Lude.Text ->
  DeleteDeploymentGroup
mkDeleteDeploymentGroup pApplicationName_ pDeploymentGroupName_ =
  DeleteDeploymentGroup'
    { applicationName = pApplicationName_,
      deploymentGroupName = pDeploymentGroupName_
    }

-- | The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddgApplicationName :: Lens.Lens' DeleteDeploymentGroup Lude.Text
ddgApplicationName = Lens.lens (applicationName :: DeleteDeploymentGroup -> Lude.Text) (\s a -> s {applicationName = a} :: DeleteDeploymentGroup)
{-# DEPRECATED ddgApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The name of a deployment group for the specified application.
--
-- /Note:/ Consider using 'deploymentGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddgDeploymentGroupName :: Lens.Lens' DeleteDeploymentGroup Lude.Text
ddgDeploymentGroupName = Lens.lens (deploymentGroupName :: DeleteDeploymentGroup -> Lude.Text) (\s a -> s {deploymentGroupName = a} :: DeleteDeploymentGroup)
{-# DEPRECATED ddgDeploymentGroupName "Use generic-lens or generic-optics with 'deploymentGroupName' instead." #-}

instance Lude.AWSRequest DeleteDeploymentGroup where
  type Rs DeleteDeploymentGroup = DeleteDeploymentGroupResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteDeploymentGroupResponse'
            Lude.<$> (x Lude..?> "hooksNotCleanedUp" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDeploymentGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeDeploy_20141006.DeleteDeploymentGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteDeploymentGroup where
  toJSON DeleteDeploymentGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("applicationName" Lude..= applicationName),
            Lude.Just ("deploymentGroupName" Lude..= deploymentGroupName)
          ]
      )

instance Lude.ToPath DeleteDeploymentGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDeploymentGroup where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @DeleteDeploymentGroup@ operation.
--
-- /See:/ 'mkDeleteDeploymentGroupResponse' smart constructor.
data DeleteDeploymentGroupResponse = DeleteDeploymentGroupResponse'
  { -- | If the output contains no data, and the corresponding deployment group contained at least one Auto Scaling group, AWS CodeDeploy successfully removed all corresponding Auto Scaling lifecycle event hooks from the Amazon EC2 instances in the Auto Scaling group. If the output contains data, AWS CodeDeploy could not remove some Auto Scaling lifecycle event hooks from the Amazon EC2 instances in the Auto Scaling group.
    hooksNotCleanedUp :: Lude.Maybe [AutoScalingGroup],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDeploymentGroupResponse' with the minimum fields required to make a request.
--
-- * 'hooksNotCleanedUp' - If the output contains no data, and the corresponding deployment group contained at least one Auto Scaling group, AWS CodeDeploy successfully removed all corresponding Auto Scaling lifecycle event hooks from the Amazon EC2 instances in the Auto Scaling group. If the output contains data, AWS CodeDeploy could not remove some Auto Scaling lifecycle event hooks from the Amazon EC2 instances in the Auto Scaling group.
-- * 'responseStatus' - The response status code.
mkDeleteDeploymentGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDeploymentGroupResponse
mkDeleteDeploymentGroupResponse pResponseStatus_ =
  DeleteDeploymentGroupResponse'
    { hooksNotCleanedUp = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the output contains no data, and the corresponding deployment group contained at least one Auto Scaling group, AWS CodeDeploy successfully removed all corresponding Auto Scaling lifecycle event hooks from the Amazon EC2 instances in the Auto Scaling group. If the output contains data, AWS CodeDeploy could not remove some Auto Scaling lifecycle event hooks from the Amazon EC2 instances in the Auto Scaling group.
--
-- /Note:/ Consider using 'hooksNotCleanedUp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddgrsHooksNotCleanedUp :: Lens.Lens' DeleteDeploymentGroupResponse (Lude.Maybe [AutoScalingGroup])
ddgrsHooksNotCleanedUp = Lens.lens (hooksNotCleanedUp :: DeleteDeploymentGroupResponse -> Lude.Maybe [AutoScalingGroup]) (\s a -> s {hooksNotCleanedUp = a} :: DeleteDeploymentGroupResponse)
{-# DEPRECATED ddgrsHooksNotCleanedUp "Use generic-lens or generic-optics with 'hooksNotCleanedUp' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddgrsResponseStatus :: Lens.Lens' DeleteDeploymentGroupResponse Lude.Int
ddgrsResponseStatus = Lens.lens (responseStatus :: DeleteDeploymentGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDeploymentGroupResponse)
{-# DEPRECATED ddgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
