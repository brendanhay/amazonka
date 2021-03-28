{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.ListPoliciesForTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the policies that are directly attached to the specified target root, organizational unit (OU), or account. You must specify the policy type that you want included in the returned list.
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListPoliciesForTarget
    (
    -- * Creating a request
      ListPoliciesForTarget (..)
    , mkListPoliciesForTarget
    -- ** Request lenses
    , lpftTargetId
    , lpftFilter
    , lpftMaxResults
    , lpftNextToken

    -- * Destructuring the response
    , ListPoliciesForTargetResponse (..)
    , mkListPoliciesForTargetResponse
    -- ** Response lenses
    , lpftrrsNextToken
    , lpftrrsPolicies
    , lpftrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListPoliciesForTarget' smart constructor.
data ListPoliciesForTarget = ListPoliciesForTarget'
  { targetId :: Types.PolicyTargetId
    -- ^ The unique identifier (ID) of the root, organizational unit, or account whose policies you want to list.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a target ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Account__ - A string that consists of exactly 12 digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
  , filter :: Types.PolicyType
    -- ^ The type of policy that you want to include in the returned list. You must specify one of the following values:
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY> 
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY> 
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY> 
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY> 
--
--
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPoliciesForTarget' value with any optional fields omitted.
mkListPoliciesForTarget
    :: Types.PolicyTargetId -- ^ 'targetId'
    -> Types.PolicyType -- ^ 'filter'
    -> ListPoliciesForTarget
mkListPoliciesForTarget targetId filter
  = ListPoliciesForTarget'{targetId, filter,
                           maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The unique identifier (ID) of the root, organizational unit, or account whose policies you want to list.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a target ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Account__ - A string that consists of exactly 12 digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
--
-- /Note:/ Consider using 'targetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpftTargetId :: Lens.Lens' ListPoliciesForTarget Types.PolicyTargetId
lpftTargetId = Lens.field @"targetId"
{-# INLINEABLE lpftTargetId #-}
{-# DEPRECATED targetId "Use generic-lens or generic-optics with 'targetId' instead"  #-}

-- | The type of policy that you want to include in the returned list. You must specify one of the following values:
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY> 
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY> 
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY> 
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY> 
--
--
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpftFilter :: Lens.Lens' ListPoliciesForTarget Types.PolicyType
lpftFilter = Lens.field @"filter"
{-# INLINEABLE lpftFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpftMaxResults :: Lens.Lens' ListPoliciesForTarget (Core.Maybe Core.Natural)
lpftMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lpftMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpftNextToken :: Lens.Lens' ListPoliciesForTarget (Core.Maybe Types.NextToken)
lpftNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpftNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListPoliciesForTarget where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListPoliciesForTarget where
        toHeaders ListPoliciesForTarget{..}
          = Core.pure
              ("X-Amz-Target", "AWSOrganizationsV20161128.ListPoliciesForTarget")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListPoliciesForTarget where
        toJSON ListPoliciesForTarget{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TargetId" Core..= targetId),
                  Core.Just ("Filter" Core..= filter),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListPoliciesForTarget where
        type Rs ListPoliciesForTarget = ListPoliciesForTargetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListPoliciesForTargetResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Policies" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListPoliciesForTarget where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"policies" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListPoliciesForTargetResponse' smart constructor.
data ListPoliciesForTargetResponse = ListPoliciesForTargetResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
  , policies :: Core.Maybe [Types.PolicySummary]
    -- ^ The list of policies that match the criteria in the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPoliciesForTargetResponse' value with any optional fields omitted.
mkListPoliciesForTargetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListPoliciesForTargetResponse
mkListPoliciesForTargetResponse responseStatus
  = ListPoliciesForTargetResponse'{nextToken = Core.Nothing,
                                   policies = Core.Nothing, responseStatus}

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpftrrsNextToken :: Lens.Lens' ListPoliciesForTargetResponse (Core.Maybe Types.NextToken)
lpftrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpftrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The list of policies that match the criteria in the request.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpftrrsPolicies :: Lens.Lens' ListPoliciesForTargetResponse (Core.Maybe [Types.PolicySummary])
lpftrrsPolicies = Lens.field @"policies"
{-# INLINEABLE lpftrrsPolicies #-}
{-# DEPRECATED policies "Use generic-lens or generic-optics with 'policies' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpftrrsResponseStatus :: Lens.Lens' ListPoliciesForTargetResponse Core.Int
lpftrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lpftrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
