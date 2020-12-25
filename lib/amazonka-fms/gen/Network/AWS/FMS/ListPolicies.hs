{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.ListPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @PolicySummary@ objects.
--
-- This operation returns paginated results.
module Network.AWS.FMS.ListPolicies
  ( -- * Creating a request
    ListPolicies (..),
    mkListPolicies,

    -- ** Request lenses
    lpMaxResults,
    lpNextToken,

    -- * Destructuring the response
    ListPoliciesResponse (..),
    mkListPoliciesResponse,

    -- ** Response lenses
    lprrsNextToken,
    lprrsPolicyList,
    lprrsResponseStatus,
  )
where

import qualified Network.AWS.FMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListPolicies' smart constructor.
data ListPolicies = ListPolicies'
  { -- | Specifies the number of @PolicySummary@ objects that you want AWS Firewall Manager to return for this request. If you have more @PolicySummary@ objects than the number that you specify for @MaxResults@ , the response includes a @NextToken@ value that you can use to get another batch of @PolicySummary@ objects.
    maxResults :: Core.Maybe Core.Natural,
    -- | If you specify a value for @MaxResults@ and you have more @PolicySummary@ objects than the number that you specify for @MaxResults@ , AWS Firewall Manager returns a @NextToken@ value in the response that allows you to list another group of @PolicySummary@ objects. For the second and subsequent @ListPolicies@ requests, specify the value of @NextToken@ from the previous response to get information about another batch of @PolicySummary@ objects.
    nextToken :: Core.Maybe Types.PaginationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPolicies' value with any optional fields omitted.
mkListPolicies ::
  ListPolicies
mkListPolicies =
  ListPolicies'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Specifies the number of @PolicySummary@ objects that you want AWS Firewall Manager to return for this request. If you have more @PolicySummary@ objects than the number that you specify for @MaxResults@ , the response includes a @NextToken@ value that you can use to get another batch of @PolicySummary@ objects.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMaxResults :: Lens.Lens' ListPolicies (Core.Maybe Core.Natural)
lpMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If you specify a value for @MaxResults@ and you have more @PolicySummary@ objects than the number that you specify for @MaxResults@ , AWS Firewall Manager returns a @NextToken@ value in the response that allows you to list another group of @PolicySummary@ objects. For the second and subsequent @ListPolicies@ requests, specify the value of @NextToken@ from the previous response to get information about another batch of @PolicySummary@ objects.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpNextToken :: Lens.Lens' ListPolicies (Core.Maybe Types.PaginationToken)
lpNextToken = Lens.field @"nextToken"
{-# DEPRECATED lpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListPolicies where
  toJSON ListPolicies {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListPolicies where
  type Rs ListPolicies = ListPoliciesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSFMS_20180101.ListPolicies")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPoliciesResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "PolicyList")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListPolicies where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"policyList" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListPoliciesResponse' smart constructor.
data ListPoliciesResponse = ListPoliciesResponse'
  { -- | If you have more @PolicySummary@ objects than the number that you specified for @MaxResults@ in the request, the response includes a @NextToken@ value. To list more @PolicySummary@ objects, submit another @ListPolicies@ request, and specify the @NextToken@ value from the response in the @NextToken@ value in the next request.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | An array of @PolicySummary@ objects.
    policyList :: Core.Maybe [Types.PolicySummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPoliciesResponse' value with any optional fields omitted.
mkListPoliciesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListPoliciesResponse
mkListPoliciesResponse responseStatus =
  ListPoliciesResponse'
    { nextToken = Core.Nothing,
      policyList = Core.Nothing,
      responseStatus
    }

-- | If you have more @PolicySummary@ objects than the number that you specified for @MaxResults@ in the request, the response includes a @NextToken@ value. To list more @PolicySummary@ objects, submit another @ListPolicies@ request, and specify the @NextToken@ value from the response in the @NextToken@ value in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsNextToken :: Lens.Lens' ListPoliciesResponse (Core.Maybe Types.PaginationToken)
lprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of @PolicySummary@ objects.
--
-- /Note:/ Consider using 'policyList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsPolicyList :: Lens.Lens' ListPoliciesResponse (Core.Maybe [Types.PolicySummary])
lprrsPolicyList = Lens.field @"policyList"
{-# DEPRECATED lprrsPolicyList "Use generic-lens or generic-optics with 'policyList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsResponseStatus :: Lens.Lens' ListPoliciesResponse Core.Int
lprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
