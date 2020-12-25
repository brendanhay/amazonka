{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.StartImageScan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an image vulnerability scan. An image scan can only be started once per day on an individual image. This limit includes if an image was scanned on initial push. For more information, see <https://docs.aws.amazon.com/AmazonECR/latest/userguide/image-scanning.html Image Scanning> in the /Amazon Elastic Container Registry User Guide/ .
module Network.AWS.ECR.StartImageScan
  ( -- * Creating a request
    StartImageScan (..),
    mkStartImageScan,

    -- ** Request lenses
    sisRepositoryName,
    sisImageId,
    sisRegistryId,

    -- * Destructuring the response
    StartImageScanResponse (..),
    mkStartImageScanResponse,

    -- ** Response lenses
    sisrrsImageId,
    sisrrsImageScanStatus,
    sisrrsRegistryId,
    sisrrsRepositoryName,
    sisrrsResponseStatus,
  )
where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartImageScan' smart constructor.
data StartImageScan = StartImageScan'
  { -- | The name of the repository that contains the images to scan.
    repositoryName :: Types.RepositoryName,
    imageId :: Types.ImageIdentifier,
    -- | The AWS account ID associated with the registry that contains the repository in which to start an image scan request. If you do not specify a registry, the default registry is assumed.
    registryId :: Core.Maybe Types.RegistryId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartImageScan' value with any optional fields omitted.
mkStartImageScan ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  -- | 'imageId'
  Types.ImageIdentifier ->
  StartImageScan
mkStartImageScan repositoryName imageId =
  StartImageScan'
    { repositoryName,
      imageId,
      registryId = Core.Nothing
    }

-- | The name of the repository that contains the images to scan.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisRepositoryName :: Lens.Lens' StartImageScan Types.RepositoryName
sisRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED sisRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisImageId :: Lens.Lens' StartImageScan Types.ImageIdentifier
sisImageId = Lens.field @"imageId"
{-# DEPRECATED sisImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The AWS account ID associated with the registry that contains the repository in which to start an image scan request. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisRegistryId :: Lens.Lens' StartImageScan (Core.Maybe Types.RegistryId)
sisRegistryId = Lens.field @"registryId"
{-# DEPRECATED sisRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

instance Core.FromJSON StartImageScan where
  toJSON StartImageScan {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just ("imageId" Core..= imageId),
            ("registryId" Core..=) Core.<$> registryId
          ]
      )

instance Core.AWSRequest StartImageScan where
  type Rs StartImageScan = StartImageScanResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerRegistry_V20150921.StartImageScan"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartImageScanResponse'
            Core.<$> (x Core..:? "imageId")
            Core.<*> (x Core..:? "imageScanStatus")
            Core.<*> (x Core..:? "registryId")
            Core.<*> (x Core..:? "repositoryName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartImageScanResponse' smart constructor.
data StartImageScanResponse = StartImageScanResponse'
  { imageId :: Core.Maybe Types.ImageIdentifier,
    -- | The current state of the scan.
    imageScanStatus :: Core.Maybe Types.ImageScanStatus,
    -- | The registry ID associated with the request.
    registryId :: Core.Maybe Types.RegistryId,
    -- | The repository name associated with the request.
    repositoryName :: Core.Maybe Types.RepositoryName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartImageScanResponse' value with any optional fields omitted.
mkStartImageScanResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartImageScanResponse
mkStartImageScanResponse responseStatus =
  StartImageScanResponse'
    { imageId = Core.Nothing,
      imageScanStatus = Core.Nothing,
      registryId = Core.Nothing,
      repositoryName = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisrrsImageId :: Lens.Lens' StartImageScanResponse (Core.Maybe Types.ImageIdentifier)
sisrrsImageId = Lens.field @"imageId"
{-# DEPRECATED sisrrsImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The current state of the scan.
--
-- /Note:/ Consider using 'imageScanStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisrrsImageScanStatus :: Lens.Lens' StartImageScanResponse (Core.Maybe Types.ImageScanStatus)
sisrrsImageScanStatus = Lens.field @"imageScanStatus"
{-# DEPRECATED sisrrsImageScanStatus "Use generic-lens or generic-optics with 'imageScanStatus' instead." #-}

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisrrsRegistryId :: Lens.Lens' StartImageScanResponse (Core.Maybe Types.RegistryId)
sisrrsRegistryId = Lens.field @"registryId"
{-# DEPRECATED sisrrsRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisrrsRepositoryName :: Lens.Lens' StartImageScanResponse (Core.Maybe Types.RepositoryName)
sisrrsRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED sisrrsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisrrsResponseStatus :: Lens.Lens' StartImageScanResponse Core.Int
sisrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sisrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
