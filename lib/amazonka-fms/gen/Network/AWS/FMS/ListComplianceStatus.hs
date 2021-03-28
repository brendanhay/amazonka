{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.ListComplianceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @PolicyComplianceStatus@ objects. Use @PolicyComplianceStatus@ to get a summary of which member accounts are protected by the specified policy. 
--
-- This operation returns paginated results.
module Network.AWS.FMS.ListComplianceStatus
    (
    -- * Creating a request
      ListComplianceStatus (..)
    , mkListComplianceStatus
    -- ** Request lenses
    , lcsPolicyId
    , lcsMaxResults
    , lcsNextToken

    -- * Destructuring the response
    , ListComplianceStatusResponse (..)
    , mkListComplianceStatusResponse
    -- ** Response lenses
    , lcsrrsNextToken
    , lcsrrsPolicyComplianceStatusList
    , lcsrrsResponseStatus
    ) where

import qualified Network.AWS.FMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListComplianceStatus' smart constructor.
data ListComplianceStatus = ListComplianceStatus'
  { policyId :: Types.PolicyId
    -- ^ The ID of the AWS Firewall Manager policy that you want the details for.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ Specifies the number of @PolicyComplianceStatus@ objects that you want AWS Firewall Manager to return for this request. If you have more @PolicyComplianceStatus@ objects than the number that you specify for @MaxResults@ , the response includes a @NextToken@ value that you can use to get another batch of @PolicyComplianceStatus@ objects.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If you specify a value for @MaxResults@ and you have more @PolicyComplianceStatus@ objects than the number that you specify for @MaxResults@ , AWS Firewall Manager returns a @NextToken@ value in the response that allows you to list another group of @PolicyComplianceStatus@ objects. For the second and subsequent @ListComplianceStatus@ requests, specify the value of @NextToken@ from the previous response to get information about another batch of @PolicyComplianceStatus@ objects.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListComplianceStatus' value with any optional fields omitted.
mkListComplianceStatus
    :: Types.PolicyId -- ^ 'policyId'
    -> ListComplianceStatus
mkListComplianceStatus policyId
  = ListComplianceStatus'{policyId, maxResults = Core.Nothing,
                          nextToken = Core.Nothing}

-- | The ID of the AWS Firewall Manager policy that you want the details for.
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsPolicyId :: Lens.Lens' ListComplianceStatus Types.PolicyId
lcsPolicyId = Lens.field @"policyId"
{-# INLINEABLE lcsPolicyId #-}
{-# DEPRECATED policyId "Use generic-lens or generic-optics with 'policyId' instead"  #-}

-- | Specifies the number of @PolicyComplianceStatus@ objects that you want AWS Firewall Manager to return for this request. If you have more @PolicyComplianceStatus@ objects than the number that you specify for @MaxResults@ , the response includes a @NextToken@ value that you can use to get another batch of @PolicyComplianceStatus@ objects.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsMaxResults :: Lens.Lens' ListComplianceStatus (Core.Maybe Core.Natural)
lcsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lcsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If you specify a value for @MaxResults@ and you have more @PolicyComplianceStatus@ objects than the number that you specify for @MaxResults@ , AWS Firewall Manager returns a @NextToken@ value in the response that allows you to list another group of @PolicyComplianceStatus@ objects. For the second and subsequent @ListComplianceStatus@ requests, specify the value of @NextToken@ from the previous response to get information about another batch of @PolicyComplianceStatus@ objects.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsNextToken :: Lens.Lens' ListComplianceStatus (Core.Maybe Types.PaginationToken)
lcsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListComplianceStatus where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListComplianceStatus where
        toHeaders ListComplianceStatus{..}
          = Core.pure
              ("X-Amz-Target", "AWSFMS_20180101.ListComplianceStatus")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListComplianceStatus where
        toJSON ListComplianceStatus{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("PolicyId" Core..= policyId),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListComplianceStatus where
        type Rs ListComplianceStatus = ListComplianceStatusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListComplianceStatusResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*>
                     x Core..:? "PolicyComplianceStatusList"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListComplianceStatus where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"policyComplianceStatusList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListComplianceStatusResponse' smart constructor.
data ListComplianceStatusResponse = ListComplianceStatusResponse'
  { nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If you have more @PolicyComplianceStatus@ objects than the number that you specified for @MaxResults@ in the request, the response includes a @NextToken@ value. To list more @PolicyComplianceStatus@ objects, submit another @ListComplianceStatus@ request, and specify the @NextToken@ value from the response in the @NextToken@ value in the next request.
  , policyComplianceStatusList :: Core.Maybe [Types.PolicyComplianceStatus]
    -- ^ An array of @PolicyComplianceStatus@ objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListComplianceStatusResponse' value with any optional fields omitted.
mkListComplianceStatusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListComplianceStatusResponse
mkListComplianceStatusResponse responseStatus
  = ListComplianceStatusResponse'{nextToken = Core.Nothing,
                                  policyComplianceStatusList = Core.Nothing, responseStatus}

-- | If you have more @PolicyComplianceStatus@ objects than the number that you specified for @MaxResults@ in the request, the response includes a @NextToken@ value. To list more @PolicyComplianceStatus@ objects, submit another @ListComplianceStatus@ request, and specify the @NextToken@ value from the response in the @NextToken@ value in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsrrsNextToken :: Lens.Lens' ListComplianceStatusResponse (Core.Maybe Types.PaginationToken)
lcsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | An array of @PolicyComplianceStatus@ objects.
--
-- /Note:/ Consider using 'policyComplianceStatusList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsrrsPolicyComplianceStatusList :: Lens.Lens' ListComplianceStatusResponse (Core.Maybe [Types.PolicyComplianceStatus])
lcsrrsPolicyComplianceStatusList = Lens.field @"policyComplianceStatusList"
{-# INLINEABLE lcsrrsPolicyComplianceStatusList #-}
{-# DEPRECATED policyComplianceStatusList "Use generic-lens or generic-optics with 'policyComplianceStatusList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsrrsResponseStatus :: Lens.Lens' ListComplianceStatusResponse Core.Int
lcsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lcsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
