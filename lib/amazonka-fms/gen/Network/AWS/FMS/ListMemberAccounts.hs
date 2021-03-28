{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.ListMemberAccounts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a @MemberAccounts@ object that lists the member accounts in the administrator's AWS organization.
--
-- The @ListMemberAccounts@ must be submitted by the account that is set as the AWS Firewall Manager administrator.
--
-- This operation returns paginated results.
module Network.AWS.FMS.ListMemberAccounts
    (
    -- * Creating a request
      ListMemberAccounts (..)
    , mkListMemberAccounts
    -- ** Request lenses
    , lmaMaxResults
    , lmaNextToken

    -- * Destructuring the response
    , ListMemberAccountsResponse (..)
    , mkListMemberAccountsResponse
    -- ** Response lenses
    , lmarrsMemberAccounts
    , lmarrsNextToken
    , lmarrsResponseStatus
    ) where

import qualified Network.AWS.FMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListMemberAccounts' smart constructor.
data ListMemberAccounts = ListMemberAccounts'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ Specifies the number of member account IDs that you want AWS Firewall Manager to return for this request. If you have more IDs than the number that you specify for @MaxResults@ , the response includes a @NextToken@ value that you can use to get another batch of member account IDs.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If you specify a value for @MaxResults@ and you have more account IDs than the number that you specify for @MaxResults@ , AWS Firewall Manager returns a @NextToken@ value in the response that allows you to list another group of IDs. For the second and subsequent @ListMemberAccountsRequest@ requests, specify the value of @NextToken@ from the previous response to get information about another batch of member account IDs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListMemberAccounts' value with any optional fields omitted.
mkListMemberAccounts
    :: ListMemberAccounts
mkListMemberAccounts
  = ListMemberAccounts'{maxResults = Core.Nothing,
                        nextToken = Core.Nothing}

-- | Specifies the number of member account IDs that you want AWS Firewall Manager to return for this request. If you have more IDs than the number that you specify for @MaxResults@ , the response includes a @NextToken@ value that you can use to get another batch of member account IDs.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmaMaxResults :: Lens.Lens' ListMemberAccounts (Core.Maybe Core.Natural)
lmaMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lmaMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If you specify a value for @MaxResults@ and you have more account IDs than the number that you specify for @MaxResults@ , AWS Firewall Manager returns a @NextToken@ value in the response that allows you to list another group of IDs. For the second and subsequent @ListMemberAccountsRequest@ requests, specify the value of @NextToken@ from the previous response to get information about another batch of member account IDs.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmaNextToken :: Lens.Lens' ListMemberAccounts (Core.Maybe Types.PaginationToken)
lmaNextToken = Lens.field @"nextToken"
{-# INLINEABLE lmaNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListMemberAccounts where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListMemberAccounts where
        toHeaders ListMemberAccounts{..}
          = Core.pure ("X-Amz-Target", "AWSFMS_20180101.ListMemberAccounts")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListMemberAccounts where
        toJSON ListMemberAccounts{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListMemberAccounts where
        type Rs ListMemberAccounts = ListMemberAccountsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListMemberAccountsResponse' Core.<$>
                   (x Core..:? "MemberAccounts") Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListMemberAccounts where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"memberAccounts" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListMemberAccountsResponse' smart constructor.
data ListMemberAccountsResponse = ListMemberAccountsResponse'
  { memberAccounts :: Core.Maybe [Types.AWSAccountId]
    -- ^ An array of account IDs.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If you have more member account IDs than the number that you specified for @MaxResults@ in the request, the response includes a @NextToken@ value. To list more IDs, submit another @ListMemberAccounts@ request, and specify the @NextToken@ value from the response in the @NextToken@ value in the next request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListMemberAccountsResponse' value with any optional fields omitted.
mkListMemberAccountsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListMemberAccountsResponse
mkListMemberAccountsResponse responseStatus
  = ListMemberAccountsResponse'{memberAccounts = Core.Nothing,
                                nextToken = Core.Nothing, responseStatus}

-- | An array of account IDs.
--
-- /Note:/ Consider using 'memberAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmarrsMemberAccounts :: Lens.Lens' ListMemberAccountsResponse (Core.Maybe [Types.AWSAccountId])
lmarrsMemberAccounts = Lens.field @"memberAccounts"
{-# INLINEABLE lmarrsMemberAccounts #-}
{-# DEPRECATED memberAccounts "Use generic-lens or generic-optics with 'memberAccounts' instead"  #-}

-- | If you have more member account IDs than the number that you specified for @MaxResults@ in the request, the response includes a @NextToken@ value. To list more IDs, submit another @ListMemberAccounts@ request, and specify the @NextToken@ value from the response in the @NextToken@ value in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmarrsNextToken :: Lens.Lens' ListMemberAccountsResponse (Core.Maybe Types.PaginationToken)
lmarrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lmarrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmarrsResponseStatus :: Lens.Lens' ListMemberAccountsResponse Core.Int
lmarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lmarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
