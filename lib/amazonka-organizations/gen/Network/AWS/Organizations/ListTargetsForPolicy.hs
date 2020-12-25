{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.ListTargetsForPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the roots, organizational units (OUs), and accounts that the specified policy is attached to.
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListTargetsForPolicy
  ( -- * Creating a request
    ListTargetsForPolicy (..),
    mkListTargetsForPolicy,

    -- ** Request lenses
    ltfpPolicyId,
    ltfpMaxResults,
    ltfpNextToken,

    -- * Destructuring the response
    ListTargetsForPolicyResponse (..),
    mkListTargetsForPolicyResponse,

    -- ** Response lenses
    ltfprrsNextToken,
    ltfprrsTargets,
    ltfprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTargetsForPolicy' smart constructor.
data ListTargetsForPolicy = ListTargetsForPolicy'
  { -- | The unique identifier (ID) of the policy whose attachments you want to know.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
    policyId :: Types.PolicyId,
    -- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTargetsForPolicy' value with any optional fields omitted.
mkListTargetsForPolicy ::
  -- | 'policyId'
  Types.PolicyId ->
  ListTargetsForPolicy
mkListTargetsForPolicy policyId =
  ListTargetsForPolicy'
    { policyId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The unique identifier (ID) of the policy whose attachments you want to know.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfpPolicyId :: Lens.Lens' ListTargetsForPolicy Types.PolicyId
ltfpPolicyId = Lens.field @"policyId"
{-# DEPRECATED ltfpPolicyId "Use generic-lens or generic-optics with 'policyId' instead." #-}

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfpMaxResults :: Lens.Lens' ListTargetsForPolicy (Core.Maybe Core.Natural)
ltfpMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ltfpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfpNextToken :: Lens.Lens' ListTargetsForPolicy (Core.Maybe Types.NextToken)
ltfpNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltfpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListTargetsForPolicy where
  toJSON ListTargetsForPolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PolicyId" Core..= policyId),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListTargetsForPolicy where
  type Rs ListTargetsForPolicy = ListTargetsForPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSOrganizationsV20161128.ListTargetsForPolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTargetsForPolicyResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Targets")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListTargetsForPolicy where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"targets" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListTargetsForPolicyResponse' smart constructor.
data ListTargetsForPolicyResponse = ListTargetsForPolicyResponse'
  { -- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of structures, each of which contains details about one of the entities to which the specified policy is attached.
    targets :: Core.Maybe [Types.PolicyTargetSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTargetsForPolicyResponse' value with any optional fields omitted.
mkListTargetsForPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListTargetsForPolicyResponse
mkListTargetsForPolicyResponse responseStatus =
  ListTargetsForPolicyResponse'
    { nextToken = Core.Nothing,
      targets = Core.Nothing,
      responseStatus
    }

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfprrsNextToken :: Lens.Lens' ListTargetsForPolicyResponse (Core.Maybe Types.NextToken)
ltfprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltfprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of structures, each of which contains details about one of the entities to which the specified policy is attached.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfprrsTargets :: Lens.Lens' ListTargetsForPolicyResponse (Core.Maybe [Types.PolicyTargetSummary])
ltfprrsTargets = Lens.field @"targets"
{-# DEPRECATED ltfprrsTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfprrsResponseStatus :: Lens.Lens' ListTargetsForPolicyResponse Core.Int
ltfprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltfprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
