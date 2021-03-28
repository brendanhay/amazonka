{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the managed policies that are available in your AWS account, including your own customer-defined managed policies and all AWS managed policies.
--
-- You can filter the list of policies that is returned using the optional @OnlyAttached@ , @Scope@ , and @PathPrefix@ parameters. For example, to list only the customer managed policies in your AWS account, set @Scope@ to @Local@ . To list only AWS managed policies, set @Scope@ to @AWS@ .
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters.
-- For more information about managed policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListPolicies
    (
    -- * Creating a request
      ListPolicies (..)
    , mkListPolicies
    -- ** Request lenses
    , lpMarker
    , lpMaxItems
    , lpOnlyAttached
    , lpPathPrefix
    , lpPolicyUsageFilter
    , lpScope

    -- * Destructuring the response
    , ListPoliciesResponse (..)
    , mkListPoliciesResponse
    -- ** Response lenses
    , lprrsIsTruncated
    , lprrsMarker
    , lprrsPolicies
    , lprrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListPolicies' smart constructor.
data ListPolicies = ListPolicies'
  { marker :: Core.Maybe Types.MarkerType
    -- ^ Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
  , maxItems :: Core.Maybe Core.Natural
    -- ^ Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
  , onlyAttached :: Core.Maybe Core.Bool
    -- ^ A flag to filter the results to only the attached policies.
--
-- When @OnlyAttached@ is @true@ , the returned list contains only the policies that are attached to an IAM user, group, or role. When @OnlyAttached@ is @false@ , or when the parameter is not included, all policies are returned.
  , pathPrefix :: Core.Maybe Types.PolicyPathType
    -- ^ The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all policies. This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
  , policyUsageFilter :: Core.Maybe Types.PolicyUsageType
    -- ^ The policy usage method to use for filtering the results.
--
-- To list only permissions policies, set @PolicyUsageFilter@ to @PermissionsPolicy@ . To list only the policies used to set permissions boundaries, set the value to @PermissionsBoundary@ .
-- This parameter is optional. If it is not included, all policies are returned. 
  , scope :: Core.Maybe Types.PolicyScopeType
    -- ^ The scope to use for filtering the results.
--
-- To list only AWS managed policies, set @Scope@ to @AWS@ . To list only the customer managed policies in your AWS account, set @Scope@ to @Local@ .
-- This parameter is optional. If it is not included, or if it is set to @All@ , all policies are returned.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPolicies' value with any optional fields omitted.
mkListPolicies
    :: ListPolicies
mkListPolicies
  = ListPolicies'{marker = Core.Nothing, maxItems = Core.Nothing,
                  onlyAttached = Core.Nothing, pathPrefix = Core.Nothing,
                  policyUsageFilter = Core.Nothing, scope = Core.Nothing}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMarker :: Lens.Lens' ListPolicies (Core.Maybe Types.MarkerType)
lpMarker = Lens.field @"marker"
{-# INLINEABLE lpMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMaxItems :: Lens.Lens' ListPolicies (Core.Maybe Core.Natural)
lpMaxItems = Lens.field @"maxItems"
{-# INLINEABLE lpMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

-- | A flag to filter the results to only the attached policies.
--
-- When @OnlyAttached@ is @true@ , the returned list contains only the policies that are attached to an IAM user, group, or role. When @OnlyAttached@ is @false@ , or when the parameter is not included, all policies are returned.
--
-- /Note:/ Consider using 'onlyAttached' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpOnlyAttached :: Lens.Lens' ListPolicies (Core.Maybe Core.Bool)
lpOnlyAttached = Lens.field @"onlyAttached"
{-# INLINEABLE lpOnlyAttached #-}
{-# DEPRECATED onlyAttached "Use generic-lens or generic-optics with 'onlyAttached' instead"  #-}

-- | The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all policies. This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'pathPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpPathPrefix :: Lens.Lens' ListPolicies (Core.Maybe Types.PolicyPathType)
lpPathPrefix = Lens.field @"pathPrefix"
{-# INLINEABLE lpPathPrefix #-}
{-# DEPRECATED pathPrefix "Use generic-lens or generic-optics with 'pathPrefix' instead"  #-}

-- | The policy usage method to use for filtering the results.
--
-- To list only permissions policies, set @PolicyUsageFilter@ to @PermissionsPolicy@ . To list only the policies used to set permissions boundaries, set the value to @PermissionsBoundary@ .
-- This parameter is optional. If it is not included, all policies are returned. 
--
-- /Note:/ Consider using 'policyUsageFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpPolicyUsageFilter :: Lens.Lens' ListPolicies (Core.Maybe Types.PolicyUsageType)
lpPolicyUsageFilter = Lens.field @"policyUsageFilter"
{-# INLINEABLE lpPolicyUsageFilter #-}
{-# DEPRECATED policyUsageFilter "Use generic-lens or generic-optics with 'policyUsageFilter' instead"  #-}

-- | The scope to use for filtering the results.
--
-- To list only AWS managed policies, set @Scope@ to @AWS@ . To list only the customer managed policies in your AWS account, set @Scope@ to @Local@ .
-- This parameter is optional. If it is not included, or if it is set to @All@ , all policies are returned.
--
-- /Note:/ Consider using 'scope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpScope :: Lens.Lens' ListPolicies (Core.Maybe Types.PolicyScopeType)
lpScope = Lens.field @"scope"
{-# INLINEABLE lpScope #-}
{-# DEPRECATED scope "Use generic-lens or generic-optics with 'scope' instead"  #-}

instance Core.ToQuery ListPolicies where
        toQuery ListPolicies{..}
          = Core.toQueryPair "Action" ("ListPolicies" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxItems") maxItems
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "OnlyAttached")
                onlyAttached
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PathPrefix") pathPrefix
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PolicyUsageFilter")
                policyUsageFilter
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Scope") scope

instance Core.ToHeaders ListPolicies where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListPolicies where
        type Rs ListPolicies = ListPoliciesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "ListPoliciesResult"
              (\ s h x ->
                 ListPoliciesResponse' Core.<$>
                   (x Core..@? "IsTruncated") Core.<*> x Core..@? "Marker" Core.<*>
                     x Core..@? "Policies" Core..<@> Core.parseXMLList "member"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListPolicies where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"isTruncated") = Core.Nothing
          | Core.isNothing (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Contains the response to a successful 'ListPolicies' request. 
--
-- /See:/ 'mkListPoliciesResponse' smart constructor.
data ListPoliciesResponse = ListPoliciesResponse'
  { isTruncated :: Core.Maybe Core.Bool
    -- ^ A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
  , marker :: Core.Maybe Types.Marker
    -- ^ When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
  , policies :: Core.Maybe [Types.Policy]
    -- ^ A list of policies.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListPoliciesResponse' value with any optional fields omitted.
mkListPoliciesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListPoliciesResponse
mkListPoliciesResponse responseStatus
  = ListPoliciesResponse'{isTruncated = Core.Nothing,
                          marker = Core.Nothing, policies = Core.Nothing, responseStatus}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsIsTruncated :: Lens.Lens' ListPoliciesResponse (Core.Maybe Core.Bool)
lprrsIsTruncated = Lens.field @"isTruncated"
{-# INLINEABLE lprrsIsTruncated #-}
{-# DEPRECATED isTruncated "Use generic-lens or generic-optics with 'isTruncated' instead"  #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsMarker :: Lens.Lens' ListPoliciesResponse (Core.Maybe Types.Marker)
lprrsMarker = Lens.field @"marker"
{-# INLINEABLE lprrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | A list of policies.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsPolicies :: Lens.Lens' ListPoliciesResponse (Core.Maybe [Types.Policy])
lprrsPolicies = Lens.field @"policies"
{-# INLINEABLE lprrsPolicies #-}
{-# DEPRECATED policies "Use generic-lens or generic-optics with 'policies' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsResponseStatus :: Lens.Lens' ListPoliciesResponse Core.Int
lprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
