{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.ListAccountsForParent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the accounts in an organization that are contained by the specified target root or organizational unit (OU). If you specify the root, you get a list of all the accounts that aren't in any OU. If you specify an OU, you get a list of all the accounts in only that OU and not in any child OUs. To get a list of all accounts in the organization, use the 'ListAccounts' operation.
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListAccountsForParent
    (
    -- * Creating a request
      ListAccountsForParent (..)
    , mkListAccountsForParent
    -- ** Request lenses
    , lafpParentId
    , lafpMaxResults
    , lafpNextToken

    -- * Destructuring the response
    , ListAccountsForParentResponse (..)
    , mkListAccountsForParentResponse
    -- ** Response lenses
    , lafprrsAccounts
    , lafprrsNextToken
    , lafprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAccountsForParent' smart constructor.
data ListAccountsForParent = ListAccountsForParent'
  { parentId :: Types.ParentId
    -- ^ The unique identifier (ID) for the parent root or organization unit (OU) whose accounts you want to list.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAccountsForParent' value with any optional fields omitted.
mkListAccountsForParent
    :: Types.ParentId -- ^ 'parentId'
    -> ListAccountsForParent
mkListAccountsForParent parentId
  = ListAccountsForParent'{parentId, maxResults = Core.Nothing,
                           nextToken = Core.Nothing}

-- | The unique identifier (ID) for the parent root or organization unit (OU) whose accounts you want to list.
--
-- /Note:/ Consider using 'parentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafpParentId :: Lens.Lens' ListAccountsForParent Types.ParentId
lafpParentId = Lens.field @"parentId"
{-# INLINEABLE lafpParentId #-}
{-# DEPRECATED parentId "Use generic-lens or generic-optics with 'parentId' instead"  #-}

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafpMaxResults :: Lens.Lens' ListAccountsForParent (Core.Maybe Core.Natural)
lafpMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lafpMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafpNextToken :: Lens.Lens' ListAccountsForParent (Core.Maybe Types.NextToken)
lafpNextToken = Lens.field @"nextToken"
{-# INLINEABLE lafpNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListAccountsForParent where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListAccountsForParent where
        toHeaders ListAccountsForParent{..}
          = Core.pure
              ("X-Amz-Target", "AWSOrganizationsV20161128.ListAccountsForParent")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListAccountsForParent where
        toJSON ListAccountsForParent{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ParentId" Core..= parentId),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListAccountsForParent where
        type Rs ListAccountsForParent = ListAccountsForParentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListAccountsForParentResponse' Core.<$>
                   (x Core..:? "Accounts") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListAccountsForParent where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"accounts" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListAccountsForParentResponse' smart constructor.
data ListAccountsForParentResponse = ListAccountsForParentResponse'
  { accounts :: Core.Maybe [Types.Account]
    -- ^ A list of the accounts in the specified root or OU.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListAccountsForParentResponse' value with any optional fields omitted.
mkListAccountsForParentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListAccountsForParentResponse
mkListAccountsForParentResponse responseStatus
  = ListAccountsForParentResponse'{accounts = Core.Nothing,
                                   nextToken = Core.Nothing, responseStatus}

-- | A list of the accounts in the specified root or OU.
--
-- /Note:/ Consider using 'accounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafprrsAccounts :: Lens.Lens' ListAccountsForParentResponse (Core.Maybe [Types.Account])
lafprrsAccounts = Lens.field @"accounts"
{-# INLINEABLE lafprrsAccounts #-}
{-# DEPRECATED accounts "Use generic-lens or generic-optics with 'accounts' instead"  #-}

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafprrsNextToken :: Lens.Lens' ListAccountsForParentResponse (Core.Maybe Types.NextToken)
lafprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lafprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafprrsResponseStatus :: Lens.Lens' ListAccountsForParentResponse Core.Int
lafprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lafprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
