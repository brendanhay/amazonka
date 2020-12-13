{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetBulkDeploymentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the status of a bulk deployment.
module Network.AWS.Greengrass.GetBulkDeploymentStatus
  ( -- * Creating a request
    GetBulkDeploymentStatus (..),
    mkGetBulkDeploymentStatus,

    -- ** Request lenses
    gbdsBulkDeploymentId,

    -- * Destructuring the response
    GetBulkDeploymentStatusResponse (..),
    mkGetBulkDeploymentStatusResponse,

    -- ** Response lenses
    gbdsrsCreatedAt,
    gbdsrsErrorDetails,
    gbdsrsBulkDeploymentStatus,
    gbdsrsErrorMessage,
    gbdsrsBulkDeploymentMetrics,
    gbdsrsTags,
    gbdsrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetBulkDeploymentStatus' smart constructor.
newtype GetBulkDeploymentStatus = GetBulkDeploymentStatus'
  { -- | The ID of the bulk deployment.
    bulkDeploymentId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBulkDeploymentStatus' with the minimum fields required to make a request.
--
-- * 'bulkDeploymentId' - The ID of the bulk deployment.
mkGetBulkDeploymentStatus ::
  -- | 'bulkDeploymentId'
  Lude.Text ->
  GetBulkDeploymentStatus
mkGetBulkDeploymentStatus pBulkDeploymentId_ =
  GetBulkDeploymentStatus' {bulkDeploymentId = pBulkDeploymentId_}

-- | The ID of the bulk deployment.
--
-- /Note:/ Consider using 'bulkDeploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbdsBulkDeploymentId :: Lens.Lens' GetBulkDeploymentStatus Lude.Text
gbdsBulkDeploymentId = Lens.lens (bulkDeploymentId :: GetBulkDeploymentStatus -> Lude.Text) (\s a -> s {bulkDeploymentId = a} :: GetBulkDeploymentStatus)
{-# DEPRECATED gbdsBulkDeploymentId "Use generic-lens or generic-optics with 'bulkDeploymentId' instead." #-}

instance Lude.AWSRequest GetBulkDeploymentStatus where
  type Rs GetBulkDeploymentStatus = GetBulkDeploymentStatusResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetBulkDeploymentStatusResponse'
            Lude.<$> (x Lude..?> "CreatedAt")
            Lude.<*> (x Lude..?> "ErrorDetails" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "BulkDeploymentStatus")
            Lude.<*> (x Lude..?> "ErrorMessage")
            Lude.<*> (x Lude..?> "BulkDeploymentMetrics")
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBulkDeploymentStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetBulkDeploymentStatus where
  toPath GetBulkDeploymentStatus' {..} =
    Lude.mconcat
      [ "/greengrass/bulk/deployments/",
        Lude.toBS bulkDeploymentId,
        "/status"
      ]

instance Lude.ToQuery GetBulkDeploymentStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetBulkDeploymentStatusResponse' smart constructor.
data GetBulkDeploymentStatusResponse = GetBulkDeploymentStatusResponse'
  { -- | The time, in ISO format, when the deployment was created.
    createdAt :: Lude.Maybe Lude.Text,
    -- | Error details
    errorDetails :: Lude.Maybe [ErrorDetail],
    -- | The status of the bulk deployment.
    bulkDeploymentStatus :: Lude.Maybe BulkDeploymentStatus,
    -- | Error message
    errorMessage :: Lude.Maybe Lude.Text,
    -- | Relevant metrics on input records processed during bulk deployment.
    bulkDeploymentMetrics :: Lude.Maybe BulkDeploymentMetrics,
    -- | Tag(s) attached to the resource arn.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBulkDeploymentStatusResponse' with the minimum fields required to make a request.
--
-- * 'createdAt' - The time, in ISO format, when the deployment was created.
-- * 'errorDetails' - Error details
-- * 'bulkDeploymentStatus' - The status of the bulk deployment.
-- * 'errorMessage' - Error message
-- * 'bulkDeploymentMetrics' - Relevant metrics on input records processed during bulk deployment.
-- * 'tags' - Tag(s) attached to the resource arn.
-- * 'responseStatus' - The response status code.
mkGetBulkDeploymentStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBulkDeploymentStatusResponse
mkGetBulkDeploymentStatusResponse pResponseStatus_ =
  GetBulkDeploymentStatusResponse'
    { createdAt = Lude.Nothing,
      errorDetails = Lude.Nothing,
      bulkDeploymentStatus = Lude.Nothing,
      errorMessage = Lude.Nothing,
      bulkDeploymentMetrics = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The time, in ISO format, when the deployment was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbdsrsCreatedAt :: Lens.Lens' GetBulkDeploymentStatusResponse (Lude.Maybe Lude.Text)
gbdsrsCreatedAt = Lens.lens (createdAt :: GetBulkDeploymentStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {createdAt = a} :: GetBulkDeploymentStatusResponse)
{-# DEPRECATED gbdsrsCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | Error details
--
-- /Note:/ Consider using 'errorDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbdsrsErrorDetails :: Lens.Lens' GetBulkDeploymentStatusResponse (Lude.Maybe [ErrorDetail])
gbdsrsErrorDetails = Lens.lens (errorDetails :: GetBulkDeploymentStatusResponse -> Lude.Maybe [ErrorDetail]) (\s a -> s {errorDetails = a} :: GetBulkDeploymentStatusResponse)
{-# DEPRECATED gbdsrsErrorDetails "Use generic-lens or generic-optics with 'errorDetails' instead." #-}

-- | The status of the bulk deployment.
--
-- /Note:/ Consider using 'bulkDeploymentStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbdsrsBulkDeploymentStatus :: Lens.Lens' GetBulkDeploymentStatusResponse (Lude.Maybe BulkDeploymentStatus)
gbdsrsBulkDeploymentStatus = Lens.lens (bulkDeploymentStatus :: GetBulkDeploymentStatusResponse -> Lude.Maybe BulkDeploymentStatus) (\s a -> s {bulkDeploymentStatus = a} :: GetBulkDeploymentStatusResponse)
{-# DEPRECATED gbdsrsBulkDeploymentStatus "Use generic-lens or generic-optics with 'bulkDeploymentStatus' instead." #-}

-- | Error message
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbdsrsErrorMessage :: Lens.Lens' GetBulkDeploymentStatusResponse (Lude.Maybe Lude.Text)
gbdsrsErrorMessage = Lens.lens (errorMessage :: GetBulkDeploymentStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: GetBulkDeploymentStatusResponse)
{-# DEPRECATED gbdsrsErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | Relevant metrics on input records processed during bulk deployment.
--
-- /Note:/ Consider using 'bulkDeploymentMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbdsrsBulkDeploymentMetrics :: Lens.Lens' GetBulkDeploymentStatusResponse (Lude.Maybe BulkDeploymentMetrics)
gbdsrsBulkDeploymentMetrics = Lens.lens (bulkDeploymentMetrics :: GetBulkDeploymentStatusResponse -> Lude.Maybe BulkDeploymentMetrics) (\s a -> s {bulkDeploymentMetrics = a} :: GetBulkDeploymentStatusResponse)
{-# DEPRECATED gbdsrsBulkDeploymentMetrics "Use generic-lens or generic-optics with 'bulkDeploymentMetrics' instead." #-}

-- | Tag(s) attached to the resource arn.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbdsrsTags :: Lens.Lens' GetBulkDeploymentStatusResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gbdsrsTags = Lens.lens (tags :: GetBulkDeploymentStatusResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: GetBulkDeploymentStatusResponse)
{-# DEPRECATED gbdsrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbdsrsResponseStatus :: Lens.Lens' GetBulkDeploymentStatusResponse Lude.Int
gbdsrsResponseStatus = Lens.lens (responseStatus :: GetBulkDeploymentStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBulkDeploymentStatusResponse)
{-# DEPRECATED gbdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
