{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.ListParents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the root or organizational units (OUs) that serve as the immediate parent of the specified child OU or account. This operation, along with 'ListChildren' enables you to traverse the tree structure that makes up this root.
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListParents
    (
    -- * Creating a request
      ListParents (..)
    , mkListParents
    -- ** Request lenses
    , lChildId
    , lMaxResults
    , lNextToken

    -- * Destructuring the response
    , ListParentsResponse (..)
    , mkListParentsResponse
    -- ** Response lenses
    , lrsNextToken
    , lrsParents
    , lrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListParents' smart constructor.
data ListParents = ListParents'
  { childId :: Types.ChildId
    -- ^ The unique identifier (ID) of the OU or account whose parent containers you want to list. Don't specify a root.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a child ID string requires one of the following:
--
--     * __Account__ - A string that consists of exactly 12 digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that contains the OU). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListParents' value with any optional fields omitted.
mkListParents
    :: Types.ChildId -- ^ 'childId'
    -> ListParents
mkListParents childId
  = ListParents'{childId, maxResults = Core.Nothing,
                 nextToken = Core.Nothing}

-- | The unique identifier (ID) of the OU or account whose parent containers you want to list. Don't specify a root.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a child ID string requires one of the following:
--
--     * __Account__ - A string that consists of exactly 12 digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that contains the OU). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
--
-- /Note:/ Consider using 'childId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lChildId :: Lens.Lens' ListParents Types.ChildId
lChildId = Lens.field @"childId"
{-# INLINEABLE lChildId #-}
{-# DEPRECATED childId "Use generic-lens or generic-optics with 'childId' instead"  #-}

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMaxResults :: Lens.Lens' ListParents (Core.Maybe Core.Natural)
lMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lNextToken :: Lens.Lens' ListParents (Core.Maybe Types.NextToken)
lNextToken = Lens.field @"nextToken"
{-# INLINEABLE lNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListParents where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListParents where
        toHeaders ListParents{..}
          = Core.pure
              ("X-Amz-Target", "AWSOrganizationsV20161128.ListParents")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListParents where
        toJSON ListParents{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ChildId" Core..= childId),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListParents where
        type Rs ListParents = ListParentsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListParentsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Parents" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListParents where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"parents" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListParentsResponse' smart constructor.
data ListParentsResponse = ListParentsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
  , parents :: Core.Maybe [Types.Parent]
    -- ^ A list of parents for the specified child account or OU.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListParentsResponse' value with any optional fields omitted.
mkListParentsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListParentsResponse
mkListParentsResponse responseStatus
  = ListParentsResponse'{nextToken = Core.Nothing,
                         parents = Core.Nothing, responseStatus}

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListParentsResponse (Core.Maybe Types.NextToken)
lrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of parents for the specified child account or OU.
--
-- /Note:/ Consider using 'parents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsParents :: Lens.Lens' ListParentsResponse (Core.Maybe [Types.Parent])
lrsParents = Lens.field @"parents"
{-# INLINEABLE lrsParents #-}
{-# DEPRECATED parents "Use generic-lens or generic-optics with 'parents' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListParentsResponse Core.Int
lrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
