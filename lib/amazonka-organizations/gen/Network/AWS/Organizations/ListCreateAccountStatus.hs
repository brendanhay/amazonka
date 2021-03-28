{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.ListCreateAccountStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the account creation requests that match the specified status that is currently being tracked for the organization.
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListCreateAccountStatus
    (
    -- * Creating a request
      ListCreateAccountStatus (..)
    , mkListCreateAccountStatus
    -- ** Request lenses
    , lcasMaxResults
    , lcasNextToken
    , lcasStates

    -- * Destructuring the response
    , ListCreateAccountStatusResponse (..)
    , mkListCreateAccountStatusResponse
    -- ** Response lenses
    , lcasrrsCreateAccountStatuses
    , lcasrrsNextToken
    , lcasrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListCreateAccountStatus' smart constructor.
data ListCreateAccountStatus = ListCreateAccountStatus'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
  , states :: Core.Maybe [Types.CreateAccountState]
    -- ^ A list of one or more states that you want included in the response. If this parameter isn't present, all requests are included in the response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCreateAccountStatus' value with any optional fields omitted.
mkListCreateAccountStatus
    :: ListCreateAccountStatus
mkListCreateAccountStatus
  = ListCreateAccountStatus'{maxResults = Core.Nothing,
                             nextToken = Core.Nothing, states = Core.Nothing}

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcasMaxResults :: Lens.Lens' ListCreateAccountStatus (Core.Maybe Core.Natural)
lcasMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lcasMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcasNextToken :: Lens.Lens' ListCreateAccountStatus (Core.Maybe Types.NextToken)
lcasNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcasNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of one or more states that you want included in the response. If this parameter isn't present, all requests are included in the response.
--
-- /Note:/ Consider using 'states' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcasStates :: Lens.Lens' ListCreateAccountStatus (Core.Maybe [Types.CreateAccountState])
lcasStates = Lens.field @"states"
{-# INLINEABLE lcasStates #-}
{-# DEPRECATED states "Use generic-lens or generic-optics with 'states' instead"  #-}

instance Core.ToQuery ListCreateAccountStatus where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListCreateAccountStatus where
        toHeaders ListCreateAccountStatus{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSOrganizationsV20161128.ListCreateAccountStatus")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListCreateAccountStatus where
        toJSON ListCreateAccountStatus{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("States" Core..=) Core.<$> states])

instance Core.AWSRequest ListCreateAccountStatus where
        type Rs ListCreateAccountStatus = ListCreateAccountStatusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListCreateAccountStatusResponse' Core.<$>
                   (x Core..:? "CreateAccountStatuses") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListCreateAccountStatus where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"createAccountStatuses" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListCreateAccountStatusResponse' smart constructor.
data ListCreateAccountStatusResponse = ListCreateAccountStatusResponse'
  { createAccountStatuses :: Core.Maybe [Types.CreateAccountStatus]
    -- ^ A list of objects with details about the requests. Certain elements, such as the accountId number, are present in the output only after the account has been successfully created.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListCreateAccountStatusResponse' value with any optional fields omitted.
mkListCreateAccountStatusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListCreateAccountStatusResponse
mkListCreateAccountStatusResponse responseStatus
  = ListCreateAccountStatusResponse'{createAccountStatuses =
                                       Core.Nothing,
                                     nextToken = Core.Nothing, responseStatus}

-- | A list of objects with details about the requests. Certain elements, such as the accountId number, are present in the output only after the account has been successfully created.
--
-- /Note:/ Consider using 'createAccountStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcasrrsCreateAccountStatuses :: Lens.Lens' ListCreateAccountStatusResponse (Core.Maybe [Types.CreateAccountStatus])
lcasrrsCreateAccountStatuses = Lens.field @"createAccountStatuses"
{-# INLINEABLE lcasrrsCreateAccountStatuses #-}
{-# DEPRECATED createAccountStatuses "Use generic-lens or generic-optics with 'createAccountStatuses' instead"  #-}

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcasrrsNextToken :: Lens.Lens' ListCreateAccountStatusResponse (Core.Maybe Types.NextToken)
lcasrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcasrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcasrrsResponseStatus :: Lens.Lens' ListCreateAccountStatusResponse Core.Int
lcasrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lcasrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
