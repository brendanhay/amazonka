{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetDeploymentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the status of a deployment.
module Network.AWS.Greengrass.GetDeploymentStatus
  ( -- * Creating a request
    GetDeploymentStatus (..),
    mkGetDeploymentStatus,

    -- ** Request lenses
    gdsGroupId,
    gdsDeploymentId,

    -- * Destructuring the response
    GetDeploymentStatusResponse (..),
    mkGetDeploymentStatusResponse,

    -- ** Response lenses
    gdsrsDeploymentType,
    gdsrsErrorDetails,
    gdsrsDeploymentStatus,
    gdsrsUpdatedAt,
    gdsrsErrorMessage,
    gdsrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDeploymentStatus' smart constructor.
data GetDeploymentStatus = GetDeploymentStatus'
  { groupId ::
      Lude.Text,
    deploymentId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDeploymentStatus' with the minimum fields required to make a request.
--
-- * 'deploymentId' - The ID of the deployment.
-- * 'groupId' - The ID of the Greengrass group.
mkGetDeploymentStatus ::
  -- | 'groupId'
  Lude.Text ->
  -- | 'deploymentId'
  Lude.Text ->
  GetDeploymentStatus
mkGetDeploymentStatus pGroupId_ pDeploymentId_ =
  GetDeploymentStatus'
    { groupId = pGroupId_,
      deploymentId = pDeploymentId_
    }

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsGroupId :: Lens.Lens' GetDeploymentStatus Lude.Text
gdsGroupId = Lens.lens (groupId :: GetDeploymentStatus -> Lude.Text) (\s a -> s {groupId = a} :: GetDeploymentStatus)
{-# DEPRECATED gdsGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The ID of the deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsDeploymentId :: Lens.Lens' GetDeploymentStatus Lude.Text
gdsDeploymentId = Lens.lens (deploymentId :: GetDeploymentStatus -> Lude.Text) (\s a -> s {deploymentId = a} :: GetDeploymentStatus)
{-# DEPRECATED gdsDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

instance Lude.AWSRequest GetDeploymentStatus where
  type Rs GetDeploymentStatus = GetDeploymentStatusResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDeploymentStatusResponse'
            Lude.<$> (x Lude..?> "DeploymentType")
            Lude.<*> (x Lude..?> "ErrorDetails" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "DeploymentStatus")
            Lude.<*> (x Lude..?> "UpdatedAt")
            Lude.<*> (x Lude..?> "ErrorMessage")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDeploymentStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetDeploymentStatus where
  toPath GetDeploymentStatus' {..} =
    Lude.mconcat
      [ "/greengrass/groups/",
        Lude.toBS groupId,
        "/deployments/",
        Lude.toBS deploymentId,
        "/status"
      ]

instance Lude.ToQuery GetDeploymentStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDeploymentStatusResponse' smart constructor.
data GetDeploymentStatusResponse = GetDeploymentStatusResponse'
  { deploymentType ::
      Lude.Maybe DeploymentType,
    errorDetails ::
      Lude.Maybe [ErrorDetail],
    deploymentStatus ::
      Lude.Maybe Lude.Text,
    updatedAt :: Lude.Maybe Lude.Text,
    errorMessage ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetDeploymentStatusResponse' with the minimum fields required to make a request.
--
-- * 'deploymentStatus' - The status of the deployment: ''InProgress'', ''Building'', ''Success'', or ''Failure''.
-- * 'deploymentType' - The type of the deployment.
-- * 'errorDetails' - Error details
-- * 'errorMessage' - Error message
-- * 'responseStatus' - The response status code.
-- * 'updatedAt' - The time, in milliseconds since the epoch, when the deployment status was updated.
mkGetDeploymentStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDeploymentStatusResponse
mkGetDeploymentStatusResponse pResponseStatus_ =
  GetDeploymentStatusResponse'
    { deploymentType = Lude.Nothing,
      errorDetails = Lude.Nothing,
      deploymentStatus = Lude.Nothing,
      updatedAt = Lude.Nothing,
      errorMessage = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The type of the deployment.
--
-- /Note:/ Consider using 'deploymentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsDeploymentType :: Lens.Lens' GetDeploymentStatusResponse (Lude.Maybe DeploymentType)
gdsrsDeploymentType = Lens.lens (deploymentType :: GetDeploymentStatusResponse -> Lude.Maybe DeploymentType) (\s a -> s {deploymentType = a} :: GetDeploymentStatusResponse)
{-# DEPRECATED gdsrsDeploymentType "Use generic-lens or generic-optics with 'deploymentType' instead." #-}

-- | Error details
--
-- /Note:/ Consider using 'errorDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsErrorDetails :: Lens.Lens' GetDeploymentStatusResponse (Lude.Maybe [ErrorDetail])
gdsrsErrorDetails = Lens.lens (errorDetails :: GetDeploymentStatusResponse -> Lude.Maybe [ErrorDetail]) (\s a -> s {errorDetails = a} :: GetDeploymentStatusResponse)
{-# DEPRECATED gdsrsErrorDetails "Use generic-lens or generic-optics with 'errorDetails' instead." #-}

-- | The status of the deployment: ''InProgress'', ''Building'', ''Success'', or ''Failure''.
--
-- /Note:/ Consider using 'deploymentStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsDeploymentStatus :: Lens.Lens' GetDeploymentStatusResponse (Lude.Maybe Lude.Text)
gdsrsDeploymentStatus = Lens.lens (deploymentStatus :: GetDeploymentStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {deploymentStatus = a} :: GetDeploymentStatusResponse)
{-# DEPRECATED gdsrsDeploymentStatus "Use generic-lens or generic-optics with 'deploymentStatus' instead." #-}

-- | The time, in milliseconds since the epoch, when the deployment status was updated.
--
-- /Note:/ Consider using 'updatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsUpdatedAt :: Lens.Lens' GetDeploymentStatusResponse (Lude.Maybe Lude.Text)
gdsrsUpdatedAt = Lens.lens (updatedAt :: GetDeploymentStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {updatedAt = a} :: GetDeploymentStatusResponse)
{-# DEPRECATED gdsrsUpdatedAt "Use generic-lens or generic-optics with 'updatedAt' instead." #-}

-- | Error message
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsErrorMessage :: Lens.Lens' GetDeploymentStatusResponse (Lude.Maybe Lude.Text)
gdsrsErrorMessage = Lens.lens (errorMessage :: GetDeploymentStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: GetDeploymentStatusResponse)
{-# DEPRECATED gdsrsErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsResponseStatus :: Lens.Lens' GetDeploymentStatusResponse Lude.Int
gdsrsResponseStatus = Lens.lens (responseStatus :: GetDeploymentStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDeploymentStatusResponse)
{-# DEPRECATED gdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
