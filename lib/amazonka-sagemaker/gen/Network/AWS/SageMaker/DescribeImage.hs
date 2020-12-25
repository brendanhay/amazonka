{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a SageMaker image.
module Network.AWS.SageMaker.DescribeImage
  ( -- * Creating a request
    DescribeImage (..),
    mkDescribeImage,

    -- ** Request lenses
    diImageName,

    -- * Destructuring the response
    DescribeImageResponse (..),
    mkDescribeImageResponse,

    -- ** Response lenses
    dirfrsCreationTime,
    dirfrsDescription,
    dirfrsDisplayName,
    dirfrsFailureReason,
    dirfrsImageArn,
    dirfrsImageName,
    dirfrsImageStatus,
    dirfrsLastModifiedTime,
    dirfrsRoleArn,
    dirfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeImage' smart constructor.
newtype DescribeImage = DescribeImage'
  { -- | The name of the image to describe.
    imageName :: Types.ImageName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeImage' value with any optional fields omitted.
mkDescribeImage ::
  -- | 'imageName'
  Types.ImageName ->
  DescribeImage
mkDescribeImage imageName = DescribeImage' {imageName}

-- | The name of the image to describe.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diImageName :: Lens.Lens' DescribeImage Types.ImageName
diImageName = Lens.field @"imageName"
{-# DEPRECATED diImageName "Use generic-lens or generic-optics with 'imageName' instead." #-}

instance Core.FromJSON DescribeImage where
  toJSON DescribeImage {..} =
    Core.object
      (Core.catMaybes [Core.Just ("ImageName" Core..= imageName)])

instance Core.AWSRequest DescribeImage where
  type Rs DescribeImage = DescribeImageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DescribeImage")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeImageResponse'
            Core.<$> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "DisplayName")
            Core.<*> (x Core..:? "FailureReason")
            Core.<*> (x Core..:? "ImageArn")
            Core.<*> (x Core..:? "ImageName")
            Core.<*> (x Core..:? "ImageStatus")
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "RoleArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeImageResponse' smart constructor.
data DescribeImageResponse = DescribeImageResponse'
  { -- | When the image was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The description of the image.
    description :: Core.Maybe Types.Description,
    -- | The name of the image as displayed.
    displayName :: Core.Maybe Types.ImageDisplayName,
    -- | When a create, update, or delete operation fails, the reason for the failure.
    failureReason :: Core.Maybe Types.FailureReason,
    -- | The Amazon Resource Name (ARN) of the image.
    imageArn :: Core.Maybe Types.ImageArn,
    -- | The name of the image.
    imageName :: Core.Maybe Types.ImageName,
    -- | The status of the image.
    imageStatus :: Core.Maybe Types.ImageStatus,
    -- | When the image was last modified.
    lastModifiedTime :: Core.Maybe Core.NominalDiffTime,
    -- | The Amazon Resource Name (ARN) of the IAM role that enables Amazon SageMaker to perform tasks on your behalf.
    roleArn :: Core.Maybe Types.RoleArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeImageResponse' value with any optional fields omitted.
mkDescribeImageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeImageResponse
mkDescribeImageResponse responseStatus =
  DescribeImageResponse'
    { creationTime = Core.Nothing,
      description = Core.Nothing,
      displayName = Core.Nothing,
      failureReason = Core.Nothing,
      imageArn = Core.Nothing,
      imageName = Core.Nothing,
      imageStatus = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      roleArn = Core.Nothing,
      responseStatus
    }

-- | When the image was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsCreationTime :: Lens.Lens' DescribeImageResponse (Core.Maybe Core.NominalDiffTime)
dirfrsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED dirfrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The description of the image.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsDescription :: Lens.Lens' DescribeImageResponse (Core.Maybe Types.Description)
dirfrsDescription = Lens.field @"description"
{-# DEPRECATED dirfrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the image as displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsDisplayName :: Lens.Lens' DescribeImageResponse (Core.Maybe Types.ImageDisplayName)
dirfrsDisplayName = Lens.field @"displayName"
{-# DEPRECATED dirfrsDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | When a create, update, or delete operation fails, the reason for the failure.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsFailureReason :: Lens.Lens' DescribeImageResponse (Core.Maybe Types.FailureReason)
dirfrsFailureReason = Lens.field @"failureReason"
{-# DEPRECATED dirfrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The Amazon Resource Name (ARN) of the image.
--
-- /Note:/ Consider using 'imageArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsImageArn :: Lens.Lens' DescribeImageResponse (Core.Maybe Types.ImageArn)
dirfrsImageArn = Lens.field @"imageArn"
{-# DEPRECATED dirfrsImageArn "Use generic-lens or generic-optics with 'imageArn' instead." #-}

-- | The name of the image.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsImageName :: Lens.Lens' DescribeImageResponse (Core.Maybe Types.ImageName)
dirfrsImageName = Lens.field @"imageName"
{-# DEPRECATED dirfrsImageName "Use generic-lens or generic-optics with 'imageName' instead." #-}

-- | The status of the image.
--
-- /Note:/ Consider using 'imageStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsImageStatus :: Lens.Lens' DescribeImageResponse (Core.Maybe Types.ImageStatus)
dirfrsImageStatus = Lens.field @"imageStatus"
{-# DEPRECATED dirfrsImageStatus "Use generic-lens or generic-optics with 'imageStatus' instead." #-}

-- | When the image was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsLastModifiedTime :: Lens.Lens' DescribeImageResponse (Core.Maybe Core.NominalDiffTime)
dirfrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED dirfrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role that enables Amazon SageMaker to perform tasks on your behalf.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsRoleArn :: Lens.Lens' DescribeImageResponse (Core.Maybe Types.RoleArn)
dirfrsRoleArn = Lens.field @"roleArn"
{-# DEPRECATED dirfrsRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsResponseStatus :: Lens.Lens' DescribeImageResponse Core.Int
dirfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dirfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
