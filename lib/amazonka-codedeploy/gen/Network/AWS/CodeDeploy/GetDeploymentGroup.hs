{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.GetDeploymentGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a deployment group.
module Network.AWS.CodeDeploy.GetDeploymentGroup
  ( -- * Creating a request
    GetDeploymentGroup (..),
    mkGetDeploymentGroup,

    -- ** Request lenses
    gdgApplicationName,
    gdgDeploymentGroupName,

    -- * Destructuring the response
    GetDeploymentGroupResponse (..),
    mkGetDeploymentGroupResponse,

    -- ** Response lenses
    gdgrsDeploymentGroupInfo,
    gdgrsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @GetDeploymentGroup@ operation.
--
-- /See:/ 'mkGetDeploymentGroup' smart constructor.
data GetDeploymentGroup = GetDeploymentGroup'
  { -- | The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
    applicationName :: Lude.Text,
    -- | The name of a deployment group for the specified application.
    deploymentGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDeploymentGroup' with the minimum fields required to make a request.
--
-- * 'applicationName' - The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
-- * 'deploymentGroupName' - The name of a deployment group for the specified application.
mkGetDeploymentGroup ::
  -- | 'applicationName'
  Lude.Text ->
  -- | 'deploymentGroupName'
  Lude.Text ->
  GetDeploymentGroup
mkGetDeploymentGroup pApplicationName_ pDeploymentGroupName_ =
  GetDeploymentGroup'
    { applicationName = pApplicationName_,
      deploymentGroupName = pDeploymentGroupName_
    }

-- | The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdgApplicationName :: Lens.Lens' GetDeploymentGroup Lude.Text
gdgApplicationName = Lens.lens (applicationName :: GetDeploymentGroup -> Lude.Text) (\s a -> s {applicationName = a} :: GetDeploymentGroup)
{-# DEPRECATED gdgApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The name of a deployment group for the specified application.
--
-- /Note:/ Consider using 'deploymentGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdgDeploymentGroupName :: Lens.Lens' GetDeploymentGroup Lude.Text
gdgDeploymentGroupName = Lens.lens (deploymentGroupName :: GetDeploymentGroup -> Lude.Text) (\s a -> s {deploymentGroupName = a} :: GetDeploymentGroup)
{-# DEPRECATED gdgDeploymentGroupName "Use generic-lens or generic-optics with 'deploymentGroupName' instead." #-}

instance Lude.AWSRequest GetDeploymentGroup where
  type Rs GetDeploymentGroup = GetDeploymentGroupResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDeploymentGroupResponse'
            Lude.<$> (x Lude..?> "deploymentGroupInfo")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDeploymentGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeDeploy_20141006.GetDeploymentGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDeploymentGroup where
  toJSON GetDeploymentGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("applicationName" Lude..= applicationName),
            Lude.Just ("deploymentGroupName" Lude..= deploymentGroupName)
          ]
      )

instance Lude.ToPath GetDeploymentGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDeploymentGroup where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @GetDeploymentGroup@ operation.
--
-- /See:/ 'mkGetDeploymentGroupResponse' smart constructor.
data GetDeploymentGroupResponse = GetDeploymentGroupResponse'
  { -- | Information about the deployment group.
    deploymentGroupInfo :: Lude.Maybe DeploymentGroupInfo,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDeploymentGroupResponse' with the minimum fields required to make a request.
--
-- * 'deploymentGroupInfo' - Information about the deployment group.
-- * 'responseStatus' - The response status code.
mkGetDeploymentGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDeploymentGroupResponse
mkGetDeploymentGroupResponse pResponseStatus_ =
  GetDeploymentGroupResponse'
    { deploymentGroupInfo = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the deployment group.
--
-- /Note:/ Consider using 'deploymentGroupInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdgrsDeploymentGroupInfo :: Lens.Lens' GetDeploymentGroupResponse (Lude.Maybe DeploymentGroupInfo)
gdgrsDeploymentGroupInfo = Lens.lens (deploymentGroupInfo :: GetDeploymentGroupResponse -> Lude.Maybe DeploymentGroupInfo) (\s a -> s {deploymentGroupInfo = a} :: GetDeploymentGroupResponse)
{-# DEPRECATED gdgrsDeploymentGroupInfo "Use generic-lens or generic-optics with 'deploymentGroupInfo' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdgrsResponseStatus :: Lens.Lens' GetDeploymentGroupResponse Lude.Int
gdgrsResponseStatus = Lens.lens (responseStatus :: GetDeploymentGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDeploymentGroupResponse)
{-# DEPRECATED gdgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
