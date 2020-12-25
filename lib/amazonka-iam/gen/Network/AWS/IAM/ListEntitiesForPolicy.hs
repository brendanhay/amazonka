{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListEntitiesForPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all IAM users, groups, and roles that the specified managed policy is attached to.
--
-- You can use the optional @EntityFilter@ parameter to limit the results to a particular type of entity (users, groups, or roles). For example, to list only the roles that are attached to the specified policy, set @EntityFilter@ to @Role@ .
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListEntitiesForPolicy
  ( -- * Creating a request
    ListEntitiesForPolicy (..),
    mkListEntitiesForPolicy,

    -- ** Request lenses
    lefpPolicyArn,
    lefpEntityFilter,
    lefpMarker,
    lefpMaxItems,
    lefpPathPrefix,
    lefpPolicyUsageFilter,

    -- * Destructuring the response
    ListEntitiesForPolicyResponse (..),
    mkListEntitiesForPolicyResponse,

    -- ** Response lenses
    lefprrsIsTruncated,
    lefprrsMarker,
    lefprrsPolicyGroups,
    lefprrsPolicyRoles,
    lefprrsPolicyUsers,
    lefprrsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListEntitiesForPolicy' smart constructor.
data ListEntitiesForPolicy = ListEntitiesForPolicy'
  { -- | The Amazon Resource Name (ARN) of the IAM policy for which you want the versions.
    --
    -- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
    policyArn :: Types.PolicyArn,
    -- | The entity type to use for filtering the results.
    --
    -- For example, when @EntityFilter@ is @Role@ , only the roles that are attached to the specified policy are returned. This parameter is optional. If it is not included, all attached entities (users, groups, and roles) are returned. The argument for this parameter must be one of the valid values listed below.
    entityFilter :: Core.Maybe Types.EntityType,
    -- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
    marker :: Core.Maybe Types.MarkerType,
    -- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
    --
    -- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
    maxItems :: Core.Maybe Core.Natural,
    -- | The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all entities.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
    pathPrefix :: Core.Maybe Types.PathPrefix,
    -- | The policy usage method to use for filtering the results.
    --
    -- To list only permissions policies, set @PolicyUsageFilter@ to @PermissionsPolicy@ . To list only the policies used to set permissions boundaries, set the value to @PermissionsBoundary@ .
    -- This parameter is optional. If it is not included, all policies are returned.
    policyUsageFilter :: Core.Maybe Types.PolicyUsageType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListEntitiesForPolicy' value with any optional fields omitted.
mkListEntitiesForPolicy ::
  -- | 'policyArn'
  Types.PolicyArn ->
  ListEntitiesForPolicy
mkListEntitiesForPolicy policyArn =
  ListEntitiesForPolicy'
    { policyArn,
      entityFilter = Core.Nothing,
      marker = Core.Nothing,
      maxItems = Core.Nothing,
      pathPrefix = Core.Nothing,
      policyUsageFilter = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the IAM policy for which you want the versions.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lefpPolicyArn :: Lens.Lens' ListEntitiesForPolicy Types.PolicyArn
lefpPolicyArn = Lens.field @"policyArn"
{-# DEPRECATED lefpPolicyArn "Use generic-lens or generic-optics with 'policyArn' instead." #-}

-- | The entity type to use for filtering the results.
--
-- For example, when @EntityFilter@ is @Role@ , only the roles that are attached to the specified policy are returned. This parameter is optional. If it is not included, all attached entities (users, groups, and roles) are returned. The argument for this parameter must be one of the valid values listed below.
--
-- /Note:/ Consider using 'entityFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lefpEntityFilter :: Lens.Lens' ListEntitiesForPolicy (Core.Maybe Types.EntityType)
lefpEntityFilter = Lens.field @"entityFilter"
{-# DEPRECATED lefpEntityFilter "Use generic-lens or generic-optics with 'entityFilter' instead." #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lefpMarker :: Lens.Lens' ListEntitiesForPolicy (Core.Maybe Types.MarkerType)
lefpMarker = Lens.field @"marker"
{-# DEPRECATED lefpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lefpMaxItems :: Lens.Lens' ListEntitiesForPolicy (Core.Maybe Core.Natural)
lefpMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lefpMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all entities.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'pathPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lefpPathPrefix :: Lens.Lens' ListEntitiesForPolicy (Core.Maybe Types.PathPrefix)
lefpPathPrefix = Lens.field @"pathPrefix"
{-# DEPRECATED lefpPathPrefix "Use generic-lens or generic-optics with 'pathPrefix' instead." #-}

-- | The policy usage method to use for filtering the results.
--
-- To list only permissions policies, set @PolicyUsageFilter@ to @PermissionsPolicy@ . To list only the policies used to set permissions boundaries, set the value to @PermissionsBoundary@ .
-- This parameter is optional. If it is not included, all policies are returned.
--
-- /Note:/ Consider using 'policyUsageFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lefpPolicyUsageFilter :: Lens.Lens' ListEntitiesForPolicy (Core.Maybe Types.PolicyUsageType)
lefpPolicyUsageFilter = Lens.field @"policyUsageFilter"
{-# DEPRECATED lefpPolicyUsageFilter "Use generic-lens or generic-optics with 'policyUsageFilter' instead." #-}

instance Core.AWSRequest ListEntitiesForPolicy where
  type Rs ListEntitiesForPolicy = ListEntitiesForPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ListEntitiesForPolicy")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "PolicyArn" policyArn)
                Core.<> (Core.toQueryValue "EntityFilter" Core.<$> entityFilter)
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxItems" Core.<$> maxItems)
                Core.<> (Core.toQueryValue "PathPrefix" Core.<$> pathPrefix)
                Core.<> ( Core.toQueryValue "PolicyUsageFilter"
                            Core.<$> policyUsageFilter
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListEntitiesForPolicyResult"
      ( \s h x ->
          ListEntitiesForPolicyResponse'
            Core.<$> (x Core..@? "IsTruncated")
            Core.<*> (x Core..@? "Marker")
            Core.<*> (x Core..@? "PolicyGroups" Core..<@> Core.parseXMLList "member")
            Core.<*> (x Core..@? "PolicyRoles" Core..<@> Core.parseXMLList "member")
            Core.<*> (x Core..@? "PolicyUsers" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListEntitiesForPolicy where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"isTruncated") = Core.Nothing
    | Core.isNothing (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Contains the response to a successful 'ListEntitiesForPolicy' request.
--
-- /See:/ 'mkListEntitiesForPolicyResponse' smart constructor.
data ListEntitiesForPolicyResponse = ListEntitiesForPolicyResponse'
  { -- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
    isTruncated :: Core.Maybe Core.Bool,
    -- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
    marker :: Core.Maybe Types.ResponseMarkerType,
    -- | A list of IAM groups that the policy is attached to.
    policyGroups :: Core.Maybe [Types.PolicyGroup],
    -- | A list of IAM roles that the policy is attached to.
    policyRoles :: Core.Maybe [Types.PolicyRole],
    -- | A list of IAM users that the policy is attached to.
    policyUsers :: Core.Maybe [Types.PolicyUser],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListEntitiesForPolicyResponse' value with any optional fields omitted.
mkListEntitiesForPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListEntitiesForPolicyResponse
mkListEntitiesForPolicyResponse responseStatus =
  ListEntitiesForPolicyResponse'
    { isTruncated = Core.Nothing,
      marker = Core.Nothing,
      policyGroups = Core.Nothing,
      policyRoles = Core.Nothing,
      policyUsers = Core.Nothing,
      responseStatus
    }

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lefprrsIsTruncated :: Lens.Lens' ListEntitiesForPolicyResponse (Core.Maybe Core.Bool)
lefprrsIsTruncated = Lens.field @"isTruncated"
{-# DEPRECATED lefprrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lefprrsMarker :: Lens.Lens' ListEntitiesForPolicyResponse (Core.Maybe Types.ResponseMarkerType)
lefprrsMarker = Lens.field @"marker"
{-# DEPRECATED lefprrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of IAM groups that the policy is attached to.
--
-- /Note:/ Consider using 'policyGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lefprrsPolicyGroups :: Lens.Lens' ListEntitiesForPolicyResponse (Core.Maybe [Types.PolicyGroup])
lefprrsPolicyGroups = Lens.field @"policyGroups"
{-# DEPRECATED lefprrsPolicyGroups "Use generic-lens or generic-optics with 'policyGroups' instead." #-}

-- | A list of IAM roles that the policy is attached to.
--
-- /Note:/ Consider using 'policyRoles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lefprrsPolicyRoles :: Lens.Lens' ListEntitiesForPolicyResponse (Core.Maybe [Types.PolicyRole])
lefprrsPolicyRoles = Lens.field @"policyRoles"
{-# DEPRECATED lefprrsPolicyRoles "Use generic-lens or generic-optics with 'policyRoles' instead." #-}

-- | A list of IAM users that the policy is attached to.
--
-- /Note:/ Consider using 'policyUsers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lefprrsPolicyUsers :: Lens.Lens' ListEntitiesForPolicyResponse (Core.Maybe [Types.PolicyUser])
lefprrsPolicyUsers = Lens.field @"policyUsers"
{-# DEPRECATED lefprrsPolicyUsers "Use generic-lens or generic-optics with 'policyUsers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lefprrsResponseStatus :: Lens.Lens' ListEntitiesForPolicyResponse Core.Int
lefprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lefprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
