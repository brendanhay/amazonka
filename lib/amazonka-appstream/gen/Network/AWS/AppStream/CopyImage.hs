{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.CopyImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the image within the same region or to a new region within the same AWS account. Note that any tags you added to the image will not be copied.
module Network.AWS.AppStream.CopyImage
  ( -- * Creating a request
    CopyImage (..),
    mkCopyImage,

    -- ** Request lenses
    ciSourceImageName,
    ciDestinationImageName,
    ciDestinationRegion,
    ciDestinationImageDescription,

    -- * Destructuring the response
    CopyImageResponse (..),
    mkCopyImageResponse,

    -- ** Response lenses
    cirrsDestinationImageName,
    cirrsResponseStatus,
  )
where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCopyImage' smart constructor.
data CopyImage = CopyImage'
  { -- | The name of the image to copy.
    sourceImageName :: Types.SourceImageName,
    -- | The name that the image will have when it is copied to the destination.
    destinationImageName :: Types.DestinationImageName,
    -- | The destination region to which the image will be copied. This parameter is required, even if you are copying an image within the same region.
    destinationRegion :: Types.RegionName,
    -- | The description that the image will have when it is copied to the destination.
    destinationImageDescription :: Core.Maybe Types.DestinationImageDescription
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopyImage' value with any optional fields omitted.
mkCopyImage ::
  -- | 'sourceImageName'
  Types.SourceImageName ->
  -- | 'destinationImageName'
  Types.DestinationImageName ->
  -- | 'destinationRegion'
  Types.RegionName ->
  CopyImage
mkCopyImage sourceImageName destinationImageName destinationRegion =
  CopyImage'
    { sourceImageName,
      destinationImageName,
      destinationRegion,
      destinationImageDescription = Core.Nothing
    }

-- | The name of the image to copy.
--
-- /Note:/ Consider using 'sourceImageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciSourceImageName :: Lens.Lens' CopyImage Types.SourceImageName
ciSourceImageName = Lens.field @"sourceImageName"
{-# DEPRECATED ciSourceImageName "Use generic-lens or generic-optics with 'sourceImageName' instead." #-}

-- | The name that the image will have when it is copied to the destination.
--
-- /Note:/ Consider using 'destinationImageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciDestinationImageName :: Lens.Lens' CopyImage Types.DestinationImageName
ciDestinationImageName = Lens.field @"destinationImageName"
{-# DEPRECATED ciDestinationImageName "Use generic-lens or generic-optics with 'destinationImageName' instead." #-}

-- | The destination region to which the image will be copied. This parameter is required, even if you are copying an image within the same region.
--
-- /Note:/ Consider using 'destinationRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciDestinationRegion :: Lens.Lens' CopyImage Types.RegionName
ciDestinationRegion = Lens.field @"destinationRegion"
{-# DEPRECATED ciDestinationRegion "Use generic-lens or generic-optics with 'destinationRegion' instead." #-}

-- | The description that the image will have when it is copied to the destination.
--
-- /Note:/ Consider using 'destinationImageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciDestinationImageDescription :: Lens.Lens' CopyImage (Core.Maybe Types.DestinationImageDescription)
ciDestinationImageDescription = Lens.field @"destinationImageDescription"
{-# DEPRECATED ciDestinationImageDescription "Use generic-lens or generic-optics with 'destinationImageDescription' instead." #-}

instance Core.FromJSON CopyImage where
  toJSON CopyImage {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SourceImageName" Core..= sourceImageName),
            Core.Just ("DestinationImageName" Core..= destinationImageName),
            Core.Just ("DestinationRegion" Core..= destinationRegion),
            ("DestinationImageDescription" Core..=)
              Core.<$> destinationImageDescription
          ]
      )

instance Core.AWSRequest CopyImage where
  type Rs CopyImage = CopyImageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "PhotonAdminProxyService.CopyImage")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CopyImageResponse'
            Core.<$> (x Core..:? "DestinationImageName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCopyImageResponse' smart constructor.
data CopyImageResponse = CopyImageResponse'
  { -- | The name of the destination image.
    destinationImageName :: Core.Maybe Types.Name,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopyImageResponse' value with any optional fields omitted.
mkCopyImageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CopyImageResponse
mkCopyImageResponse responseStatus =
  CopyImageResponse'
    { destinationImageName = Core.Nothing,
      responseStatus
    }

-- | The name of the destination image.
--
-- /Note:/ Consider using 'destinationImageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirrsDestinationImageName :: Lens.Lens' CopyImageResponse (Core.Maybe Types.Name)
cirrsDestinationImageName = Lens.field @"destinationImageName"
{-# DEPRECATED cirrsDestinationImageName "Use generic-lens or generic-optics with 'destinationImageName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirrsResponseStatus :: Lens.Lens' CopyImageResponse Core.Int
cirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
