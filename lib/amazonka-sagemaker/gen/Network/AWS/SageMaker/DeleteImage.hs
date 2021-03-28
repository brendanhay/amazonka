{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a SageMaker image and all versions of the image. The container images aren't deleted.
module Network.AWS.SageMaker.DeleteImage
    (
    -- * Creating a request
      DeleteImage (..)
    , mkDeleteImage
    -- ** Request lenses
    , difImageName

    -- * Destructuring the response
    , DeleteImageResponse (..)
    , mkDeleteImageResponse
    -- ** Response lenses
    , dirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDeleteImage' smart constructor.
newtype DeleteImage = DeleteImage'
  { imageName :: Types.ImageName
    -- ^ The name of the image to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteImage' value with any optional fields omitted.
mkDeleteImage
    :: Types.ImageName -- ^ 'imageName'
    -> DeleteImage
mkDeleteImage imageName = DeleteImage'{imageName}

-- | The name of the image to delete.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difImageName :: Lens.Lens' DeleteImage Types.ImageName
difImageName = Lens.field @"imageName"
{-# INLINEABLE difImageName #-}
{-# DEPRECATED imageName "Use generic-lens or generic-optics with 'imageName' instead"  #-}

instance Core.ToQuery DeleteImage where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteImage where
        toHeaders DeleteImage{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DeleteImage") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteImage where
        toJSON DeleteImage{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ImageName" Core..= imageName)])

instance Core.AWSRequest DeleteImage where
        type Rs DeleteImage = DeleteImageResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteImageResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteImageResponse' smart constructor.
newtype DeleteImageResponse = DeleteImageResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteImageResponse' value with any optional fields omitted.
mkDeleteImageResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteImageResponse
mkDeleteImageResponse responseStatus
  = DeleteImageResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsResponseStatus :: Lens.Lens' DeleteImageResponse Core.Int
dirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
