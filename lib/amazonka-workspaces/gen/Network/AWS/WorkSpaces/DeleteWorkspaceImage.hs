{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DeleteWorkspaceImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified image from your account. To delete an image, you must first delete any bundles that are associated with the image and unshare the image if it is shared with other accounts. 
module Network.AWS.WorkSpaces.DeleteWorkspaceImage
    (
    -- * Creating a request
      DeleteWorkspaceImage (..)
    , mkDeleteWorkspaceImage
    -- ** Request lenses
    , dwiImageId

    -- * Destructuring the response
    , DeleteWorkspaceImageResponse (..)
    , mkDeleteWorkspaceImageResponse
    -- ** Response lenses
    , dwirfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkDeleteWorkspaceImage' smart constructor.
newtype DeleteWorkspaceImage = DeleteWorkspaceImage'
  { imageId :: Types.WorkspaceImageId
    -- ^ The identifier of the image.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteWorkspaceImage' value with any optional fields omitted.
mkDeleteWorkspaceImage
    :: Types.WorkspaceImageId -- ^ 'imageId'
    -> DeleteWorkspaceImage
mkDeleteWorkspaceImage imageId = DeleteWorkspaceImage'{imageId}

-- | The identifier of the image.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwiImageId :: Lens.Lens' DeleteWorkspaceImage Types.WorkspaceImageId
dwiImageId = Lens.field @"imageId"
{-# INLINEABLE dwiImageId #-}
{-# DEPRECATED imageId "Use generic-lens or generic-optics with 'imageId' instead"  #-}

instance Core.ToQuery DeleteWorkspaceImage where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteWorkspaceImage where
        toHeaders DeleteWorkspaceImage{..}
          = Core.pure
              ("X-Amz-Target", "WorkspacesService.DeleteWorkspaceImage")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteWorkspaceImage where
        toJSON DeleteWorkspaceImage{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ImageId" Core..= imageId)])

instance Core.AWSRequest DeleteWorkspaceImage where
        type Rs DeleteWorkspaceImage = DeleteWorkspaceImageResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteWorkspaceImageResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteWorkspaceImageResponse' smart constructor.
newtype DeleteWorkspaceImageResponse = DeleteWorkspaceImageResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteWorkspaceImageResponse' value with any optional fields omitted.
mkDeleteWorkspaceImageResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteWorkspaceImageResponse
mkDeleteWorkspaceImageResponse responseStatus
  = DeleteWorkspaceImageResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwirfrsResponseStatus :: Lens.Lens' DeleteWorkspaceImageResponse Core.Int
dwirfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dwirfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
