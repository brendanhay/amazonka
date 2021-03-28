{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.RemoveTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from a trail.
module Network.AWS.CloudTrail.RemoveTags
    (
    -- * Creating a request
      RemoveTags (..)
    , mkRemoveTags
    -- ** Request lenses
    , rResourceId
    , rTagsList

    -- * Destructuring the response
    , RemoveTagsResponse (..)
    , mkRemoveTagsResponse
    -- ** Response lenses
    , rtrrsResponseStatus
    ) where

import qualified Network.AWS.CloudTrail.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Specifies the tags to remove from a trail.
--
-- /See:/ 'mkRemoveTags' smart constructor.
data RemoveTags = RemoveTags'
  { resourceId :: Core.Text
    -- ^ Specifies the ARN of the trail from which tags should be removed. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@ 
  , tagsList :: Core.Maybe [Types.Tag]
    -- ^ Specifies a list of tags to be removed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTags' value with any optional fields omitted.
mkRemoveTags
    :: Core.Text -- ^ 'resourceId'
    -> RemoveTags
mkRemoveTags resourceId
  = RemoveTags'{resourceId, tagsList = Core.Nothing}

-- | Specifies the ARN of the trail from which tags should be removed. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@ 
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResourceId :: Lens.Lens' RemoveTags Core.Text
rResourceId = Lens.field @"resourceId"
{-# INLINEABLE rResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | Specifies a list of tags to be removed.
--
-- /Note:/ Consider using 'tagsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTagsList :: Lens.Lens' RemoveTags (Core.Maybe [Types.Tag])
rTagsList = Lens.field @"tagsList"
{-# INLINEABLE rTagsList #-}
{-# DEPRECATED tagsList "Use generic-lens or generic-optics with 'tagsList' instead"  #-}

instance Core.ToQuery RemoveTags where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RemoveTags where
        toHeaders RemoveTags{..}
          = Core.pure
              ("X-Amz-Target",
               "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.RemoveTags")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RemoveTags where
        toJSON RemoveTags{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceId" Core..= resourceId),
                  ("TagsList" Core..=) Core.<$> tagsList])

instance Core.AWSRequest RemoveTags where
        type Rs RemoveTags = RemoveTagsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 RemoveTagsResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Returns the objects or data listed below if successful. Otherwise, returns an error.
--
-- /See:/ 'mkRemoveTagsResponse' smart constructor.
newtype RemoveTagsResponse = RemoveTagsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTagsResponse' value with any optional fields omitted.
mkRemoveTagsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RemoveTagsResponse
mkRemoveTagsResponse responseStatus
  = RemoveTagsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrrsResponseStatus :: Lens.Lens' RemoveTagsResponse Core.Int
rtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
