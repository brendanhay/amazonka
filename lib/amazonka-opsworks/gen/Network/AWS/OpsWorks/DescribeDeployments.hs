{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeDeployments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a description of a specified set of deployments.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeDeployments
  ( -- * Creating a request
    DescribeDeployments (..),
    mkDescribeDeployments,

    -- ** Request lenses
    ddAppId,
    ddDeploymentIds,
    ddStackId,

    -- * Destructuring the response
    DescribeDeploymentsResponse (..),
    mkDescribeDeploymentsResponse,

    -- ** Response lenses
    ddrsDeployments,
    ddrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeDeployments' smart constructor.
data DescribeDeployments = DescribeDeployments'
  { appId ::
      Lude.Maybe Lude.Text,
    deploymentIds :: Lude.Maybe [Lude.Text],
    stackId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDeployments' with the minimum fields required to make a request.
--
-- * 'appId' - The app ID. If you include this parameter, the command returns a description of the commands associated with the specified app.
-- * 'deploymentIds' - An array of deployment IDs to be described. If you include this parameter, the command returns a description of the specified deployments. Otherwise, it returns a description of every deployment.
-- * 'stackId' - The stack ID. If you include this parameter, the command returns a description of the commands associated with the specified stack.
mkDescribeDeployments ::
  DescribeDeployments
mkDescribeDeployments =
  DescribeDeployments'
    { appId = Lude.Nothing,
      deploymentIds = Lude.Nothing,
      stackId = Lude.Nothing
    }

-- | The app ID. If you include this parameter, the command returns a description of the commands associated with the specified app.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddAppId :: Lens.Lens' DescribeDeployments (Lude.Maybe Lude.Text)
ddAppId = Lens.lens (appId :: DescribeDeployments -> Lude.Maybe Lude.Text) (\s a -> s {appId = a} :: DescribeDeployments)
{-# DEPRECATED ddAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

-- | An array of deployment IDs to be described. If you include this parameter, the command returns a description of the specified deployments. Otherwise, it returns a description of every deployment.
--
-- /Note:/ Consider using 'deploymentIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDeploymentIds :: Lens.Lens' DescribeDeployments (Lude.Maybe [Lude.Text])
ddDeploymentIds = Lens.lens (deploymentIds :: DescribeDeployments -> Lude.Maybe [Lude.Text]) (\s a -> s {deploymentIds = a} :: DescribeDeployments)
{-# DEPRECATED ddDeploymentIds "Use generic-lens or generic-optics with 'deploymentIds' instead." #-}

-- | The stack ID. If you include this parameter, the command returns a description of the commands associated with the specified stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddStackId :: Lens.Lens' DescribeDeployments (Lude.Maybe Lude.Text)
ddStackId = Lens.lens (stackId :: DescribeDeployments -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: DescribeDeployments)
{-# DEPRECATED ddStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

instance Lude.AWSRequest DescribeDeployments where
  type Rs DescribeDeployments = DescribeDeploymentsResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeDeploymentsResponse'
            Lude.<$> (x Lude..?> "Deployments" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDeployments where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.DescribeDeployments" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeDeployments where
  toJSON DescribeDeployments' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AppId" Lude..=) Lude.<$> appId,
            ("DeploymentIds" Lude..=) Lude.<$> deploymentIds,
            ("StackId" Lude..=) Lude.<$> stackId
          ]
      )

instance Lude.ToPath DescribeDeployments where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDeployments where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @DescribeDeployments@ request.
--
-- /See:/ 'mkDescribeDeploymentsResponse' smart constructor.
data DescribeDeploymentsResponse = DescribeDeploymentsResponse'
  { deployments ::
      Lude.Maybe [Deployment],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDeploymentsResponse' with the minimum fields required to make a request.
--
-- * 'deployments' - An array of @Deployment@ objects that describe the deployments.
-- * 'responseStatus' - The response status code.
mkDescribeDeploymentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDeploymentsResponse
mkDescribeDeploymentsResponse pResponseStatus_ =
  DescribeDeploymentsResponse'
    { deployments = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @Deployment@ objects that describe the deployments.
--
-- /Note:/ Consider using 'deployments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsDeployments :: Lens.Lens' DescribeDeploymentsResponse (Lude.Maybe [Deployment])
ddrsDeployments = Lens.lens (deployments :: DescribeDeploymentsResponse -> Lude.Maybe [Deployment]) (\s a -> s {deployments = a} :: DescribeDeploymentsResponse)
{-# DEPRECATED ddrsDeployments "Use generic-lens or generic-optics with 'deployments' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsResponseStatus :: Lens.Lens' DescribeDeploymentsResponse Lude.Int
ddrsResponseStatus = Lens.lens (responseStatus :: DescribeDeploymentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDeploymentsResponse)
{-# DEPRECATED ddrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
