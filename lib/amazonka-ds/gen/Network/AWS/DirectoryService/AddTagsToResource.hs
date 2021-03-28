{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.AddTagsToResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or overwrites one or more tags for the specified directory. Each directory can have a maximum of 50 tags. Each tag consists of a key and optional value. Tag keys must be unique to each resource.
module Network.AWS.DirectoryService.AddTagsToResource
    (
    -- * Creating a request
      AddTagsToResource (..)
    , mkAddTagsToResource
    -- ** Request lenses
    , attrResourceId
    , attrTags

    -- * Destructuring the response
    , AddTagsToResourceResponse (..)
    , mkAddTagsToResourceResponse
    -- ** Response lenses
    , attrrrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAddTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
  { resourceId :: Types.ResourceId
    -- ^ Identifier (ID) for the directory to which to add the tag.
  , tags :: [Types.Tag]
    -- ^ The tags to be assigned to the directory.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddTagsToResource' value with any optional fields omitted.
mkAddTagsToResource
    :: Types.ResourceId -- ^ 'resourceId'
    -> AddTagsToResource
mkAddTagsToResource resourceId
  = AddTagsToResource'{resourceId, tags = Core.mempty}

-- | Identifier (ID) for the directory to which to add the tag.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrResourceId :: Lens.Lens' AddTagsToResource Types.ResourceId
attrResourceId = Lens.field @"resourceId"
{-# INLINEABLE attrResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The tags to be assigned to the directory.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrTags :: Lens.Lens' AddTagsToResource [Types.Tag]
attrTags = Lens.field @"tags"
{-# INLINEABLE attrTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery AddTagsToResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AddTagsToResource where
        toHeaders AddTagsToResource{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.AddTagsToResource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AddTagsToResource where
        toJSON AddTagsToResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceId" Core..= resourceId),
                  Core.Just ("Tags" Core..= tags)])

instance Core.AWSRequest AddTagsToResource where
        type Rs AddTagsToResource = AddTagsToResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AddTagsToResourceResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAddTagsToResourceResponse' smart constructor.
newtype AddTagsToResourceResponse = AddTagsToResourceResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AddTagsToResourceResponse' value with any optional fields omitted.
mkAddTagsToResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AddTagsToResourceResponse
mkAddTagsToResourceResponse responseStatus
  = AddTagsToResourceResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrrrsResponseStatus :: Lens.Lens' AddTagsToResourceResponse Core.Int
attrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE attrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
