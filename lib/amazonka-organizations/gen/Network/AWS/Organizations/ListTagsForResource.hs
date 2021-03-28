{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.ListTagsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists tags that are attached to the specified resource.
--
-- You can attach tags to the following resources in AWS Organizations.
--
--     * AWS account
--
--
--     * Organization root
--
--
--     * Organizational unit (OU)
--
--
--     * Policy (any type)
--
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListTagsForResource
    (
    -- * Creating a request
      ListTagsForResource (..)
    , mkListTagsForResource
    -- ** Request lenses
    , ltfrResourceId
    , ltfrNextToken

    -- * Destructuring the response
    , ListTagsForResourceResponse (..)
    , mkListTagsForResourceResponse
    -- ** Response lenses
    , ltfrrrsNextToken
    , ltfrrrsTags
    , ltfrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { resourceId :: Types.TaggableResourceId
    -- ^ The ID of the resource with the tags to list.
--
-- You can specify any of the following taggable resources.
--
--     * AWS account – specify the account ID number.
--
--
--     * Organizational unit – specify the OU ID that begins with @ou-@ and looks similar to: @ou-/1a2b-34uvwxyz/ @ 
--
--
--     * Root – specify the root ID that begins with @r-@ and looks similar to: @r-/1a2b/ @ 
--
--
--     * Policy – specify the policy ID that begins with @p-@ andlooks similar to: @p-/12abcdefg3/ @ 
--
--
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForResource' value with any optional fields omitted.
mkListTagsForResource
    :: Types.TaggableResourceId -- ^ 'resourceId'
    -> ListTagsForResource
mkListTagsForResource resourceId
  = ListTagsForResource'{resourceId, nextToken = Core.Nothing}

-- | The ID of the resource with the tags to list.
--
-- You can specify any of the following taggable resources.
--
--     * AWS account – specify the account ID number.
--
--
--     * Organizational unit – specify the OU ID that begins with @ou-@ and looks similar to: @ou-/1a2b-34uvwxyz/ @ 
--
--
--     * Root – specify the root ID that begins with @r-@ and looks similar to: @r-/1a2b/ @ 
--
--
--     * Policy – specify the policy ID that begins with @p-@ andlooks similar to: @p-/12abcdefg3/ @ 
--
--
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrResourceId :: Lens.Lens' ListTagsForResource Types.TaggableResourceId
ltfrResourceId = Lens.field @"resourceId"
{-# INLINEABLE ltfrResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrNextToken :: Lens.Lens' ListTagsForResource (Core.Maybe Types.NextToken)
ltfrNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltfrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListTagsForResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTagsForResource where
        toHeaders ListTagsForResource{..}
          = Core.pure
              ("X-Amz-Target", "AWSOrganizationsV20161128.ListTagsForResource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListTagsForResource where
        toJSON ListTagsForResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceId" Core..= resourceId),
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListTagsForResource where
        type Rs ListTagsForResource = ListTagsForResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTagsForResourceResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Tags" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListTagsForResource where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"tags" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags that are assigned to the resource.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForResourceResponse' value with any optional fields omitted.
mkListTagsForResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTagsForResourceResponse
mkListTagsForResourceResponse responseStatus
  = ListTagsForResourceResponse'{nextToken = Core.Nothing,
                                 tags = Core.Nothing, responseStatus}

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrrsNextToken :: Lens.Lens' ListTagsForResourceResponse (Core.Maybe Types.NextToken)
ltfrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltfrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The tags that are assigned to the resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrrsTags :: Lens.Lens' ListTagsForResourceResponse (Core.Maybe [Types.Tag])
ltfrrrsTags = Lens.field @"tags"
{-# INLINEABLE ltfrrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrrsResponseStatus :: Lens.Lens' ListTagsForResourceResponse Core.Int
ltfrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltfrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
