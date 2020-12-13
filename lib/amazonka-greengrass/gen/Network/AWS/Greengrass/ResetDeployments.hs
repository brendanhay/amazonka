{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ResetDeployments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets a group's deployments.
module Network.AWS.Greengrass.ResetDeployments
  ( -- * Creating a request
    ResetDeployments (..),
    mkResetDeployments,

    -- ** Request lenses
    rdAmznClientToken,
    rdForce,
    rdGroupId,

    -- * Destructuring the response
    ResetDeploymentsResponse (..),
    mkResetDeploymentsResponse,

    -- ** Response lenses
    rdrsDeploymentId,
    rdrsDeploymentARN,
    rdrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Information needed to reset deployments.
--
-- /See:/ 'mkResetDeployments' smart constructor.
data ResetDeployments = ResetDeployments'
  { -- | A client token used to correlate requests and responses.
    amznClientToken :: Lude.Maybe Lude.Text,
    -- | If true, performs a best-effort only core reset.
    force :: Lude.Maybe Lude.Bool,
    -- | The ID of the Greengrass group.
    groupId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResetDeployments' with the minimum fields required to make a request.
--
-- * 'amznClientToken' - A client token used to correlate requests and responses.
-- * 'force' - If true, performs a best-effort only core reset.
-- * 'groupId' - The ID of the Greengrass group.
mkResetDeployments ::
  -- | 'groupId'
  Lude.Text ->
  ResetDeployments
mkResetDeployments pGroupId_ =
  ResetDeployments'
    { amznClientToken = Lude.Nothing,
      force = Lude.Nothing,
      groupId = pGroupId_
    }

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdAmznClientToken :: Lens.Lens' ResetDeployments (Lude.Maybe Lude.Text)
rdAmznClientToken = Lens.lens (amznClientToken :: ResetDeployments -> Lude.Maybe Lude.Text) (\s a -> s {amznClientToken = a} :: ResetDeployments)
{-# DEPRECATED rdAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | If true, performs a best-effort only core reset.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdForce :: Lens.Lens' ResetDeployments (Lude.Maybe Lude.Bool)
rdForce = Lens.lens (force :: ResetDeployments -> Lude.Maybe Lude.Bool) (\s a -> s {force = a} :: ResetDeployments)
{-# DEPRECATED rdForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdGroupId :: Lens.Lens' ResetDeployments Lude.Text
rdGroupId = Lens.lens (groupId :: ResetDeployments -> Lude.Text) (\s a -> s {groupId = a} :: ResetDeployments)
{-# DEPRECATED rdGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

instance Lude.AWSRequest ResetDeployments where
  type Rs ResetDeployments = ResetDeploymentsResponse
  request = Req.postJSON greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          ResetDeploymentsResponse'
            Lude.<$> (x Lude..?> "DeploymentId")
            Lude.<*> (x Lude..?> "DeploymentArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ResetDeployments where
  toHeaders ResetDeployments' {..} =
    Lude.mconcat
      [ "X-Amzn-Client-Token" Lude.=# amznClientToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON ResetDeployments where
  toJSON ResetDeployments' {..} =
    Lude.object (Lude.catMaybes [("Force" Lude..=) Lude.<$> force])

instance Lude.ToPath ResetDeployments where
  toPath ResetDeployments' {..} =
    Lude.mconcat
      ["/greengrass/groups/", Lude.toBS groupId, "/deployments/$reset"]

instance Lude.ToQuery ResetDeployments where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkResetDeploymentsResponse' smart constructor.
data ResetDeploymentsResponse = ResetDeploymentsResponse'
  { -- | The ID of the deployment.
    deploymentId :: Lude.Maybe Lude.Text,
    -- | The ARN of the deployment.
    deploymentARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResetDeploymentsResponse' with the minimum fields required to make a request.
--
-- * 'deploymentId' - The ID of the deployment.
-- * 'deploymentARN' - The ARN of the deployment.
-- * 'responseStatus' - The response status code.
mkResetDeploymentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ResetDeploymentsResponse
mkResetDeploymentsResponse pResponseStatus_ =
  ResetDeploymentsResponse'
    { deploymentId = Lude.Nothing,
      deploymentARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdrsDeploymentId :: Lens.Lens' ResetDeploymentsResponse (Lude.Maybe Lude.Text)
rdrsDeploymentId = Lens.lens (deploymentId :: ResetDeploymentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {deploymentId = a} :: ResetDeploymentsResponse)
{-# DEPRECATED rdrsDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | The ARN of the deployment.
--
-- /Note:/ Consider using 'deploymentARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdrsDeploymentARN :: Lens.Lens' ResetDeploymentsResponse (Lude.Maybe Lude.Text)
rdrsDeploymentARN = Lens.lens (deploymentARN :: ResetDeploymentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {deploymentARN = a} :: ResetDeploymentsResponse)
{-# DEPRECATED rdrsDeploymentARN "Use generic-lens or generic-optics with 'deploymentARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdrsResponseStatus :: Lens.Lens' ResetDeploymentsResponse Lude.Int
rdrsResponseStatus = Lens.lens (responseStatus :: ResetDeploymentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ResetDeploymentsResponse)
{-# DEPRECATED rdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
