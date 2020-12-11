{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a deployment. ''CreateDeployment'' requests are idempotent with respect to the ''X-Amzn-Client-Token'' token and the request parameters.
module Network.AWS.Greengrass.CreateDeployment
  ( -- * Creating a request
    CreateDeployment (..),
    mkCreateDeployment,

    -- ** Request lenses
    cdDeploymentId,
    cdAmznClientToken,
    cdGroupVersionId,
    cdGroupId,
    cdDeploymentType,

    -- * Destructuring the response
    CreateDeploymentResponse (..),
    mkCreateDeploymentResponse,

    -- ** Response lenses
    cdrsDeploymentId,
    cdrsDeploymentARN,
    cdrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDeployment' smart constructor.
data CreateDeployment = CreateDeployment'
  { deploymentId ::
      Lude.Maybe Lude.Text,
    amznClientToken :: Lude.Maybe Lude.Text,
    groupVersionId :: Lude.Maybe Lude.Text,
    groupId :: Lude.Text,
    deploymentType :: DeploymentType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDeployment' with the minimum fields required to make a request.
--
-- * 'amznClientToken' - A client token used to correlate requests and responses.
-- * 'deploymentId' - The ID of the deployment if you wish to redeploy a previous deployment.
-- * 'deploymentType' - The type of deployment. When used for ''CreateDeployment'', only ''NewDeployment'' and ''Redeployment'' are valid.
-- * 'groupId' - The ID of the Greengrass group.
-- * 'groupVersionId' - The ID of the group version to be deployed.
mkCreateDeployment ::
  -- | 'groupId'
  Lude.Text ->
  -- | 'deploymentType'
  DeploymentType ->
  CreateDeployment
mkCreateDeployment pGroupId_ pDeploymentType_ =
  CreateDeployment'
    { deploymentId = Lude.Nothing,
      amznClientToken = Lude.Nothing,
      groupVersionId = Lude.Nothing,
      groupId = pGroupId_,
      deploymentType = pDeploymentType_
    }

-- | The ID of the deployment if you wish to redeploy a previous deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDeploymentId :: Lens.Lens' CreateDeployment (Lude.Maybe Lude.Text)
cdDeploymentId = Lens.lens (deploymentId :: CreateDeployment -> Lude.Maybe Lude.Text) (\s a -> s {deploymentId = a} :: CreateDeployment)
{-# DEPRECATED cdDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdAmznClientToken :: Lens.Lens' CreateDeployment (Lude.Maybe Lude.Text)
cdAmznClientToken = Lens.lens (amznClientToken :: CreateDeployment -> Lude.Maybe Lude.Text) (\s a -> s {amznClientToken = a} :: CreateDeployment)
{-# DEPRECATED cdAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | The ID of the group version to be deployed.
--
-- /Note:/ Consider using 'groupVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdGroupVersionId :: Lens.Lens' CreateDeployment (Lude.Maybe Lude.Text)
cdGroupVersionId = Lens.lens (groupVersionId :: CreateDeployment -> Lude.Maybe Lude.Text) (\s a -> s {groupVersionId = a} :: CreateDeployment)
{-# DEPRECATED cdGroupVersionId "Use generic-lens or generic-optics with 'groupVersionId' instead." #-}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdGroupId :: Lens.Lens' CreateDeployment Lude.Text
cdGroupId = Lens.lens (groupId :: CreateDeployment -> Lude.Text) (\s a -> s {groupId = a} :: CreateDeployment)
{-# DEPRECATED cdGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The type of deployment. When used for ''CreateDeployment'', only ''NewDeployment'' and ''Redeployment'' are valid.
--
-- /Note:/ Consider using 'deploymentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDeploymentType :: Lens.Lens' CreateDeployment DeploymentType
cdDeploymentType = Lens.lens (deploymentType :: CreateDeployment -> DeploymentType) (\s a -> s {deploymentType = a} :: CreateDeployment)
{-# DEPRECATED cdDeploymentType "Use generic-lens or generic-optics with 'deploymentType' instead." #-}

instance Lude.AWSRequest CreateDeployment where
  type Rs CreateDeployment = CreateDeploymentResponse
  request = Req.postJSON greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDeploymentResponse'
            Lude.<$> (x Lude..?> "DeploymentId")
            Lude.<*> (x Lude..?> "DeploymentArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDeployment where
  toHeaders CreateDeployment' {..} =
    Lude.mconcat
      [ "X-Amzn-Client-Token" Lude.=# amznClientToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON CreateDeployment where
  toJSON CreateDeployment' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DeploymentId" Lude..=) Lude.<$> deploymentId,
            ("GroupVersionId" Lude..=) Lude.<$> groupVersionId,
            Lude.Just ("DeploymentType" Lude..= deploymentType)
          ]
      )

instance Lude.ToPath CreateDeployment where
  toPath CreateDeployment' {..} =
    Lude.mconcat
      ["/greengrass/groups/", Lude.toBS groupId, "/deployments"]

instance Lude.ToQuery CreateDeployment where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateDeploymentResponse' smart constructor.
data CreateDeploymentResponse = CreateDeploymentResponse'
  { deploymentId ::
      Lude.Maybe Lude.Text,
    deploymentARN :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateDeploymentResponse' with the minimum fields required to make a request.
--
-- * 'deploymentARN' - The ARN of the deployment.
-- * 'deploymentId' - The ID of the deployment.
-- * 'responseStatus' - The response status code.
mkCreateDeploymentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDeploymentResponse
mkCreateDeploymentResponse pResponseStatus_ =
  CreateDeploymentResponse'
    { deploymentId = Lude.Nothing,
      deploymentARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsDeploymentId :: Lens.Lens' CreateDeploymentResponse (Lude.Maybe Lude.Text)
cdrsDeploymentId = Lens.lens (deploymentId :: CreateDeploymentResponse -> Lude.Maybe Lude.Text) (\s a -> s {deploymentId = a} :: CreateDeploymentResponse)
{-# DEPRECATED cdrsDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | The ARN of the deployment.
--
-- /Note:/ Consider using 'deploymentARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsDeploymentARN :: Lens.Lens' CreateDeploymentResponse (Lude.Maybe Lude.Text)
cdrsDeploymentARN = Lens.lens (deploymentARN :: CreateDeploymentResponse -> Lude.Maybe Lude.Text) (\s a -> s {deploymentARN = a} :: CreateDeploymentResponse)
{-# DEPRECATED cdrsDeploymentARN "Use generic-lens or generic-optics with 'deploymentARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsResponseStatus :: Lens.Lens' CreateDeploymentResponse Lude.Int
cdrsResponseStatus = Lens.lens (responseStatus :: CreateDeploymentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDeploymentResponse)
{-# DEPRECATED cdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
