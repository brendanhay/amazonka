{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.ListTagsLogGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags for the specified log group.
module Network.AWS.CloudWatchLogs.ListTagsLogGroup
    (
    -- * Creating a request
      ListTagsLogGroup (..)
    , mkListTagsLogGroup
    -- ** Request lenses
    , ltlgLogGroupName

    -- * Destructuring the response
    , ListTagsLogGroupResponse (..)
    , mkListTagsLogGroupResponse
    -- ** Response lenses
    , ltlgrrsTags
    , ltlgrrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTagsLogGroup' smart constructor.
newtype ListTagsLogGroup = ListTagsLogGroup'
  { logGroupName :: Types.LogGroupName
    -- ^ The name of the log group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsLogGroup' value with any optional fields omitted.
mkListTagsLogGroup
    :: Types.LogGroupName -- ^ 'logGroupName'
    -> ListTagsLogGroup
mkListTagsLogGroup logGroupName = ListTagsLogGroup'{logGroupName}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlgLogGroupName :: Lens.Lens' ListTagsLogGroup Types.LogGroupName
ltlgLogGroupName = Lens.field @"logGroupName"
{-# INLINEABLE ltlgLogGroupName #-}
{-# DEPRECATED logGroupName "Use generic-lens or generic-optics with 'logGroupName' instead"  #-}

instance Core.ToQuery ListTagsLogGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTagsLogGroup where
        toHeaders ListTagsLogGroup{..}
          = Core.pure ("X-Amz-Target", "Logs_20140328.ListTagsLogGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListTagsLogGroup where
        toJSON ListTagsLogGroup{..}
          = Core.object
              (Core.catMaybes [Core.Just ("logGroupName" Core..= logGroupName)])

instance Core.AWSRequest ListTagsLogGroup where
        type Rs ListTagsLogGroup = ListTagsLogGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTagsLogGroupResponse' Core.<$>
                   (x Core..:? "tags") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListTagsLogGroupResponse' smart constructor.
data ListTagsLogGroupResponse = ListTagsLogGroupResponse'
  { tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ The tags for the log group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsLogGroupResponse' value with any optional fields omitted.
mkListTagsLogGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTagsLogGroupResponse
mkListTagsLogGroupResponse responseStatus
  = ListTagsLogGroupResponse'{tags = Core.Nothing, responseStatus}

-- | The tags for the log group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlgrrsTags :: Lens.Lens' ListTagsLogGroupResponse (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
ltlgrrsTags = Lens.field @"tags"
{-# INLINEABLE ltlgrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlgrrsResponseStatus :: Lens.Lens' ListTagsLogGroupResponse Core.Int
ltlgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltlgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
