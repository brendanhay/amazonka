{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeImageVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a version of a SageMaker image.
module Network.AWS.SageMaker.DescribeImageVersion
  ( -- * Creating a request
    DescribeImageVersion (..),
    mkDescribeImageVersion,

    -- ** Request lenses
    dImageName,
    dVersion,

    -- * Destructuring the response
    DescribeImageVersionResponse (..),
    mkDescribeImageVersionResponse,

    -- ** Response lenses
    divrfrsBaseImage,
    divrfrsContainerImage,
    divrfrsCreationTime,
    divrfrsFailureReason,
    divrfrsImageArn,
    divrfrsImageVersionArn,
    divrfrsImageVersionStatus,
    divrfrsLastModifiedTime,
    divrfrsVersion,
    divrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeImageVersion' smart constructor.
data DescribeImageVersion = DescribeImageVersion'
  { -- | The name of the image.
    imageName :: Types.ImageName,
    -- | The version of the image. If not specified, the latest version is described.
    version :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeImageVersion' value with any optional fields omitted.
mkDescribeImageVersion ::
  -- | 'imageName'
  Types.ImageName ->
  DescribeImageVersion
mkDescribeImageVersion imageName =
  DescribeImageVersion' {imageName, version = Core.Nothing}

-- | The name of the image.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dImageName :: Lens.Lens' DescribeImageVersion Types.ImageName
dImageName = Lens.field @"imageName"
{-# DEPRECATED dImageName "Use generic-lens or generic-optics with 'imageName' instead." #-}

-- | The version of the image. If not specified, the latest version is described.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dVersion :: Lens.Lens' DescribeImageVersion (Core.Maybe Core.Natural)
dVersion = Lens.field @"version"
{-# DEPRECATED dVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON DescribeImageVersion where
  toJSON DescribeImageVersion {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ImageName" Core..= imageName),
            ("Version" Core..=) Core.<$> version
          ]
      )

instance Core.AWSRequest DescribeImageVersion where
  type Rs DescribeImageVersion = DescribeImageVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DescribeImageVersion")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeImageVersionResponse'
            Core.<$> (x Core..:? "BaseImage")
            Core.<*> (x Core..:? "ContainerImage")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "FailureReason")
            Core.<*> (x Core..:? "ImageArn")
            Core.<*> (x Core..:? "ImageVersionArn")
            Core.<*> (x Core..:? "ImageVersionStatus")
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "Version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeImageVersionResponse' smart constructor.
data DescribeImageVersionResponse = DescribeImageVersionResponse'
  { -- | The registry path of the container image on which this image version is based.
    baseImage :: Core.Maybe Types.ImageBaseImage,
    -- | The registry path of the container image that contains this image version.
    containerImage :: Core.Maybe Types.ImageContainerImage,
    -- | When the version was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | When a create or delete operation fails, the reason for the failure.
    failureReason :: Core.Maybe Types.FailureReason,
    -- | The Amazon Resource Name (ARN) of the image the version is based on.
    imageArn :: Core.Maybe Types.ImageArn,
    -- | The ARN of the version.
    imageVersionArn :: Core.Maybe Types.ImageVersionArn,
    -- | The status of the version.
    imageVersionStatus :: Core.Maybe Types.ImageVersionStatus,
    -- | When the version was last modified.
    lastModifiedTime :: Core.Maybe Core.NominalDiffTime,
    -- | The version number.
    version :: Core.Maybe Core.Natural,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeImageVersionResponse' value with any optional fields omitted.
mkDescribeImageVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeImageVersionResponse
mkDescribeImageVersionResponse responseStatus =
  DescribeImageVersionResponse'
    { baseImage = Core.Nothing,
      containerImage = Core.Nothing,
      creationTime = Core.Nothing,
      failureReason = Core.Nothing,
      imageArn = Core.Nothing,
      imageVersionArn = Core.Nothing,
      imageVersionStatus = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      version = Core.Nothing,
      responseStatus
    }

-- | The registry path of the container image on which this image version is based.
--
-- /Note:/ Consider using 'baseImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divrfrsBaseImage :: Lens.Lens' DescribeImageVersionResponse (Core.Maybe Types.ImageBaseImage)
divrfrsBaseImage = Lens.field @"baseImage"
{-# DEPRECATED divrfrsBaseImage "Use generic-lens or generic-optics with 'baseImage' instead." #-}

-- | The registry path of the container image that contains this image version.
--
-- /Note:/ Consider using 'containerImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divrfrsContainerImage :: Lens.Lens' DescribeImageVersionResponse (Core.Maybe Types.ImageContainerImage)
divrfrsContainerImage = Lens.field @"containerImage"
{-# DEPRECATED divrfrsContainerImage "Use generic-lens or generic-optics with 'containerImage' instead." #-}

-- | When the version was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divrfrsCreationTime :: Lens.Lens' DescribeImageVersionResponse (Core.Maybe Core.NominalDiffTime)
divrfrsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED divrfrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | When a create or delete operation fails, the reason for the failure.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divrfrsFailureReason :: Lens.Lens' DescribeImageVersionResponse (Core.Maybe Types.FailureReason)
divrfrsFailureReason = Lens.field @"failureReason"
{-# DEPRECATED divrfrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The Amazon Resource Name (ARN) of the image the version is based on.
--
-- /Note:/ Consider using 'imageArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divrfrsImageArn :: Lens.Lens' DescribeImageVersionResponse (Core.Maybe Types.ImageArn)
divrfrsImageArn = Lens.field @"imageArn"
{-# DEPRECATED divrfrsImageArn "Use generic-lens or generic-optics with 'imageArn' instead." #-}

-- | The ARN of the version.
--
-- /Note:/ Consider using 'imageVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divrfrsImageVersionArn :: Lens.Lens' DescribeImageVersionResponse (Core.Maybe Types.ImageVersionArn)
divrfrsImageVersionArn = Lens.field @"imageVersionArn"
{-# DEPRECATED divrfrsImageVersionArn "Use generic-lens or generic-optics with 'imageVersionArn' instead." #-}

-- | The status of the version.
--
-- /Note:/ Consider using 'imageVersionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divrfrsImageVersionStatus :: Lens.Lens' DescribeImageVersionResponse (Core.Maybe Types.ImageVersionStatus)
divrfrsImageVersionStatus = Lens.field @"imageVersionStatus"
{-# DEPRECATED divrfrsImageVersionStatus "Use generic-lens or generic-optics with 'imageVersionStatus' instead." #-}

-- | When the version was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divrfrsLastModifiedTime :: Lens.Lens' DescribeImageVersionResponse (Core.Maybe Core.NominalDiffTime)
divrfrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED divrfrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The version number.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divrfrsVersion :: Lens.Lens' DescribeImageVersionResponse (Core.Maybe Core.Natural)
divrfrsVersion = Lens.field @"version"
{-# DEPRECATED divrfrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divrfrsResponseStatus :: Lens.Lens' DescribeImageVersionResponse Core.Int
divrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED divrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
