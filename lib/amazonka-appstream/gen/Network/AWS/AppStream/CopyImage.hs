{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CopyImage (..)
    , mkCopyImage
    -- ** Request lenses
    , ciSourceImageName
    , ciDestinationImageName
    , ciDestinationRegion
    , ciDestinationImageDescription

    -- * Destructuring the response
    , CopyImageResponse (..)
    , mkCopyImageResponse
    -- ** Response lenses
    , cirrsDestinationImageName
    , cirrsResponseStatus
    ) where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCopyImage' smart constructor.
data CopyImage = CopyImage'
  { sourceImageName :: Types.SourceImageName
    -- ^ The name of the image to copy.
  , destinationImageName :: Types.DestinationImageName
    -- ^ The name that the image will have when it is copied to the destination.
  , destinationRegion :: Types.RegionName
    -- ^ The destination region to which the image will be copied. This parameter is required, even if you are copying an image within the same region.
  , destinationImageDescription :: Core.Maybe Types.DestinationImageDescription
    -- ^ The description that the image will have when it is copied to the destination.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopyImage' value with any optional fields omitted.
mkCopyImage
    :: Types.SourceImageName -- ^ 'sourceImageName'
    -> Types.DestinationImageName -- ^ 'destinationImageName'
    -> Types.RegionName -- ^ 'destinationRegion'
    -> CopyImage
mkCopyImage sourceImageName destinationImageName destinationRegion
  = CopyImage'{sourceImageName, destinationImageName,
               destinationRegion, destinationImageDescription = Core.Nothing}

-- | The name of the image to copy.
--
-- /Note:/ Consider using 'sourceImageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciSourceImageName :: Lens.Lens' CopyImage Types.SourceImageName
ciSourceImageName = Lens.field @"sourceImageName"
{-# INLINEABLE ciSourceImageName #-}
{-# DEPRECATED sourceImageName "Use generic-lens or generic-optics with 'sourceImageName' instead"  #-}

-- | The name that the image will have when it is copied to the destination.
--
-- /Note:/ Consider using 'destinationImageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciDestinationImageName :: Lens.Lens' CopyImage Types.DestinationImageName
ciDestinationImageName = Lens.field @"destinationImageName"
{-# INLINEABLE ciDestinationImageName #-}
{-# DEPRECATED destinationImageName "Use generic-lens or generic-optics with 'destinationImageName' instead"  #-}

-- | The destination region to which the image will be copied. This parameter is required, even if you are copying an image within the same region.
--
-- /Note:/ Consider using 'destinationRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciDestinationRegion :: Lens.Lens' CopyImage Types.RegionName
ciDestinationRegion = Lens.field @"destinationRegion"
{-# INLINEABLE ciDestinationRegion #-}
{-# DEPRECATED destinationRegion "Use generic-lens or generic-optics with 'destinationRegion' instead"  #-}

-- | The description that the image will have when it is copied to the destination.
--
-- /Note:/ Consider using 'destinationImageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciDestinationImageDescription :: Lens.Lens' CopyImage (Core.Maybe Types.DestinationImageDescription)
ciDestinationImageDescription = Lens.field @"destinationImageDescription"
{-# INLINEABLE ciDestinationImageDescription #-}
{-# DEPRECATED destinationImageDescription "Use generic-lens or generic-optics with 'destinationImageDescription' instead"  #-}

instance Core.ToQuery CopyImage where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CopyImage where
        toHeaders CopyImage{..}
          = Core.pure ("X-Amz-Target", "PhotonAdminProxyService.CopyImage")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CopyImage where
        toJSON CopyImage{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SourceImageName" Core..= sourceImageName),
                  Core.Just ("DestinationImageName" Core..= destinationImageName),
                  Core.Just ("DestinationRegion" Core..= destinationRegion),
                  ("DestinationImageDescription" Core..=) Core.<$>
                    destinationImageDescription])

instance Core.AWSRequest CopyImage where
        type Rs CopyImage = CopyImageResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CopyImageResponse' Core.<$>
                   (x Core..:? "DestinationImageName") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCopyImageResponse' smart constructor.
data CopyImageResponse = CopyImageResponse'
  { destinationImageName :: Core.Maybe Types.Name
    -- ^ The name of the destination image.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopyImageResponse' value with any optional fields omitted.
mkCopyImageResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CopyImageResponse
mkCopyImageResponse responseStatus
  = CopyImageResponse'{destinationImageName = Core.Nothing,
                       responseStatus}

-- | The name of the destination image.
--
-- /Note:/ Consider using 'destinationImageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirrsDestinationImageName :: Lens.Lens' CopyImageResponse (Core.Maybe Types.Name)
cirrsDestinationImageName = Lens.field @"destinationImageName"
{-# INLINEABLE cirrsDestinationImageName #-}
{-# DEPRECATED destinationImageName "Use generic-lens or generic-optics with 'destinationImageName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirrsResponseStatus :: Lens.Lens' CopyImageResponse Core.Int
cirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
