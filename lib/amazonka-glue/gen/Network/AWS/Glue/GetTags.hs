{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of tags associated with a resource.
module Network.AWS.Glue.GetTags
    (
    -- * Creating a request
      GetTags (..)
    , mkGetTags
    -- ** Request lenses
    , gtResourceArn

    -- * Destructuring the response
    , GetTagsResponse (..)
    , mkGetTagsResponse
    -- ** Response lenses
    , gtrrsTags
    , gtrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTags' smart constructor.
newtype GetTags = GetTags'
  { resourceArn :: Types.GlueResourceArn
    -- ^ The Amazon Resource Name (ARN) of the resource for which to retrieve tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetTags' value with any optional fields omitted.
mkGetTags
    :: Types.GlueResourceArn -- ^ 'resourceArn'
    -> GetTags
mkGetTags resourceArn = GetTags'{resourceArn}

-- | The Amazon Resource Name (ARN) of the resource for which to retrieve tags.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtResourceArn :: Lens.Lens' GetTags Types.GlueResourceArn
gtResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE gtResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

instance Core.ToQuery GetTags where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetTags where
        toHeaders GetTags{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetTags") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetTags where
        toJSON GetTags{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ResourceArn" Core..= resourceArn)])

instance Core.AWSRequest GetTags where
        type Rs GetTags = GetTagsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetTagsResponse' Core.<$>
                   (x Core..:? "Tags") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetTagsResponse' smart constructor.
data GetTagsResponse = GetTagsResponse'
  { tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ The requested tags.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTagsResponse' value with any optional fields omitted.
mkGetTagsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetTagsResponse
mkGetTagsResponse responseStatus
  = GetTagsResponse'{tags = Core.Nothing, responseStatus}

-- | The requested tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsTags :: Lens.Lens' GetTagsResponse (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
gtrrsTags = Lens.field @"tags"
{-# INLINEABLE gtrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsResponseStatus :: Lens.Lens' GetTagsResponse Core.Int
gtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
