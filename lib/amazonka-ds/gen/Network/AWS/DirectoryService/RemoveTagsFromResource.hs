{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.RemoveTagsFromResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags from a directory.
module Network.AWS.DirectoryService.RemoveTagsFromResource
    (
    -- * Creating a request
      RemoveTagsFromResource (..)
    , mkRemoveTagsFromResource
    -- ** Request lenses
    , rtfrResourceId
    , rtfrTagKeys

    -- * Destructuring the response
    , RemoveTagsFromResourceResponse (..)
    , mkRemoveTagsFromResourceResponse
    -- ** Response lenses
    , rtfrrrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemoveTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
  { resourceId :: Types.ResourceId
    -- ^ Identifier (ID) of the directory from which to remove the tag.
  , tagKeys :: [Types.TagKey]
    -- ^ The tag key (name) of the tag to be removed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTagsFromResource' value with any optional fields omitted.
mkRemoveTagsFromResource
    :: Types.ResourceId -- ^ 'resourceId'
    -> RemoveTagsFromResource
mkRemoveTagsFromResource resourceId
  = RemoveTagsFromResource'{resourceId, tagKeys = Core.mempty}

-- | Identifier (ID) of the directory from which to remove the tag.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrResourceId :: Lens.Lens' RemoveTagsFromResource Types.ResourceId
rtfrResourceId = Lens.field @"resourceId"
{-# INLINEABLE rtfrResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The tag key (name) of the tag to be removed.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrTagKeys :: Lens.Lens' RemoveTagsFromResource [Types.TagKey]
rtfrTagKeys = Lens.field @"tagKeys"
{-# INLINEABLE rtfrTagKeys #-}
{-# DEPRECATED tagKeys "Use generic-lens or generic-optics with 'tagKeys' instead"  #-}

instance Core.ToQuery RemoveTagsFromResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RemoveTagsFromResource where
        toHeaders RemoveTagsFromResource{..}
          = Core.pure
              ("X-Amz-Target",
               "DirectoryService_20150416.RemoveTagsFromResource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RemoveTagsFromResource where
        toJSON RemoveTagsFromResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceId" Core..= resourceId),
                  Core.Just ("TagKeys" Core..= tagKeys)])

instance Core.AWSRequest RemoveTagsFromResource where
        type Rs RemoveTagsFromResource = RemoveTagsFromResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 RemoveTagsFromResourceResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRemoveTagsFromResourceResponse' smart constructor.
newtype RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTagsFromResourceResponse' value with any optional fields omitted.
mkRemoveTagsFromResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RemoveTagsFromResourceResponse
mkRemoveTagsFromResourceResponse responseStatus
  = RemoveTagsFromResourceResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrrrsResponseStatus :: Lens.Lens' RemoveTagsFromResourceResponse Core.Int
rtfrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rtfrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
