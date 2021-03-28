{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DeleteImageBuilder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified image builder and releases the capacity.
module Network.AWS.AppStream.DeleteImageBuilder
    (
    -- * Creating a request
      DeleteImageBuilder (..)
    , mkDeleteImageBuilder
    -- ** Request lenses
    , dibName

    -- * Destructuring the response
    , DeleteImageBuilderResponse (..)
    , mkDeleteImageBuilderResponse
    -- ** Response lenses
    , dibrrsImageBuilder
    , dibrrsResponseStatus
    ) where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteImageBuilder' smart constructor.
newtype DeleteImageBuilder = DeleteImageBuilder'
  { name :: Types.Name
    -- ^ The name of the image builder.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteImageBuilder' value with any optional fields omitted.
mkDeleteImageBuilder
    :: Types.Name -- ^ 'name'
    -> DeleteImageBuilder
mkDeleteImageBuilder name = DeleteImageBuilder'{name}

-- | The name of the image builder.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dibName :: Lens.Lens' DeleteImageBuilder Types.Name
dibName = Lens.field @"name"
{-# INLINEABLE dibName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery DeleteImageBuilder where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteImageBuilder where
        toHeaders DeleteImageBuilder{..}
          = Core.pure
              ("X-Amz-Target", "PhotonAdminProxyService.DeleteImageBuilder")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteImageBuilder where
        toJSON DeleteImageBuilder{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DeleteImageBuilder where
        type Rs DeleteImageBuilder = DeleteImageBuilderResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteImageBuilderResponse' Core.<$>
                   (x Core..:? "ImageBuilder") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteImageBuilderResponse' smart constructor.
data DeleteImageBuilderResponse = DeleteImageBuilderResponse'
  { imageBuilder :: Core.Maybe Types.ImageBuilder
    -- ^ Information about the image builder.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteImageBuilderResponse' value with any optional fields omitted.
mkDeleteImageBuilderResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteImageBuilderResponse
mkDeleteImageBuilderResponse responseStatus
  = DeleteImageBuilderResponse'{imageBuilder = Core.Nothing,
                                responseStatus}

-- | Information about the image builder.
--
-- /Note:/ Consider using 'imageBuilder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dibrrsImageBuilder :: Lens.Lens' DeleteImageBuilderResponse (Core.Maybe Types.ImageBuilder)
dibrrsImageBuilder = Lens.field @"imageBuilder"
{-# INLINEABLE dibrrsImageBuilder #-}
{-# DEPRECATED imageBuilder "Use generic-lens or generic-optics with 'imageBuilder' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dibrrsResponseStatus :: Lens.Lens' DeleteImageBuilderResponse Core.Int
dibrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dibrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
