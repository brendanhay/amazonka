{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.ListTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags for the trail in the current region.
--
-- This operation returns paginated results.
module Network.AWS.CloudTrail.ListTags
    (
    -- * Creating a request
      ListTags (..)
    , mkListTags
    -- ** Request lenses
    , ltResourceIdList
    , ltNextToken

    -- * Destructuring the response
    , ListTagsResponse (..)
    , mkListTagsResponse
    -- ** Response lenses
    , ltrrsNextToken
    , ltrrsResourceTagList
    , ltrrsResponseStatus
    ) where

import qualified Network.AWS.CloudTrail.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Specifies a list of trail tags to return.
--
-- /See:/ 'mkListTags' smart constructor.
data ListTags = ListTags'
  { resourceIdList :: [Core.Text]
    -- ^ Specifies a list of trail ARNs whose tags will be listed. The list has a limit of 20 ARNs. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@ 
  , nextToken :: Core.Maybe Core.Text
    -- ^ Reserved for future use.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTags' value with any optional fields omitted.
mkListTags
    :: ListTags
mkListTags
  = ListTags'{resourceIdList = Core.mempty, nextToken = Core.Nothing}

-- | Specifies a list of trail ARNs whose tags will be listed. The list has a limit of 20 ARNs. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@ 
--
-- /Note:/ Consider using 'resourceIdList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltResourceIdList :: Lens.Lens' ListTags [Core.Text]
ltResourceIdList = Lens.field @"resourceIdList"
{-# INLINEABLE ltResourceIdList #-}
{-# DEPRECATED resourceIdList "Use generic-lens or generic-optics with 'resourceIdList' instead"  #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltNextToken :: Lens.Lens' ListTags (Core.Maybe Core.Text)
ltNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListTags where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTags where
        toHeaders ListTags{..}
          = Core.pure
              ("X-Amz-Target",
               "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.ListTags")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListTags where
        toJSON ListTags{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceIdList" Core..= resourceIdList),
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListTags where
        type Rs ListTags = ListTagsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTagsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "ResourceTagList"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListTags where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"resourceTagList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Returns the objects or data listed below if successful. Otherwise, returns an error.
--
-- /See:/ 'mkListTagsResponse' smart constructor.
data ListTagsResponse = ListTagsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ Reserved for future use.
  , resourceTagList :: Core.Maybe [Types.ResourceTag]
    -- ^ A list of resource tags.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsResponse' value with any optional fields omitted.
mkListTagsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTagsResponse
mkListTagsResponse responseStatus
  = ListTagsResponse'{nextToken = Core.Nothing,
                      resourceTagList = Core.Nothing, responseStatus}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsNextToken :: Lens.Lens' ListTagsResponse (Core.Maybe Core.Text)
ltrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of resource tags.
--
-- /Note:/ Consider using 'resourceTagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsResourceTagList :: Lens.Lens' ListTagsResponse (Core.Maybe [Types.ResourceTag])
ltrrsResourceTagList = Lens.field @"resourceTagList"
{-# INLINEABLE ltrrsResourceTagList #-}
{-# DEPRECATED resourceTagList "Use generic-lens or generic-optics with 'resourceTagList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsResponseStatus :: Lens.Lens' ListTagsResponse Core.Int
ltrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
