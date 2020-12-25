{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListRolePolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the names of the inline policies that are embedded in the specified IAM role.
--
-- An IAM role can also have managed policies attached to it. To list the managed policies that are attached to a role, use 'ListAttachedRolePolicies' . For more information about policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters. If there are no inline policies embedded with the specified role, the operation returns an empty list.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListRolePolicies
  ( -- * Creating a request
    ListRolePolicies (..),
    mkListRolePolicies,

    -- ** Request lenses
    lrpRoleName,
    lrpMarker,
    lrpMaxItems,

    -- * Destructuring the response
    ListRolePoliciesResponse (..),
    mkListRolePoliciesResponse,

    -- ** Response lenses
    lrprrsPolicyNames,
    lrprrsIsTruncated,
    lrprrsMarker,
    lrprrsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListRolePolicies' smart constructor.
data ListRolePolicies = ListRolePolicies'
  { -- | The name of the role to list policies for.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    roleName :: Types.RoleName,
    -- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
    marker :: Core.Maybe Types.MarkerType,
    -- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
    --
    -- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
    maxItems :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRolePolicies' value with any optional fields omitted.
mkListRolePolicies ::
  -- | 'roleName'
  Types.RoleName ->
  ListRolePolicies
mkListRolePolicies roleName =
  ListRolePolicies'
    { roleName,
      marker = Core.Nothing,
      maxItems = Core.Nothing
    }

-- | The name of the role to list policies for.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpRoleName :: Lens.Lens' ListRolePolicies Types.RoleName
lrpRoleName = Lens.field @"roleName"
{-# DEPRECATED lrpRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpMarker :: Lens.Lens' ListRolePolicies (Core.Maybe Types.MarkerType)
lrpMarker = Lens.field @"marker"
{-# DEPRECATED lrpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpMaxItems :: Lens.Lens' ListRolePolicies (Core.Maybe Core.Natural)
lrpMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lrpMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Core.AWSRequest ListRolePolicies where
  type Rs ListRolePolicies = ListRolePoliciesResponse
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
            ( Core.pure ("Action", "ListRolePolicies")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "RoleName" roleName)
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxItems" Core.<$> maxItems)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListRolePoliciesResult"
      ( \s h x ->
          ListRolePoliciesResponse'
            Core.<$> ( x Core..@? "PolicyNames" Core..@! Core.mempty
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (x Core..@? "IsTruncated")
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListRolePolicies where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"isTruncated") = Core.Nothing
    | Core.isNothing (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Contains the response to a successful 'ListRolePolicies' request.
--
-- /See:/ 'mkListRolePoliciesResponse' smart constructor.
data ListRolePoliciesResponse = ListRolePoliciesResponse'
  { -- | A list of policy names.
    policyNames :: [Types.PolicyNameType],
    -- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
    isTruncated :: Core.Maybe Core.Bool,
    -- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
    marker :: Core.Maybe Types.ResponseMarkerType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRolePoliciesResponse' value with any optional fields omitted.
mkListRolePoliciesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListRolePoliciesResponse
mkListRolePoliciesResponse responseStatus =
  ListRolePoliciesResponse'
    { policyNames = Core.mempty,
      isTruncated = Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | A list of policy names.
--
-- /Note:/ Consider using 'policyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprrsPolicyNames :: Lens.Lens' ListRolePoliciesResponse [Types.PolicyNameType]
lrprrsPolicyNames = Lens.field @"policyNames"
{-# DEPRECATED lrprrsPolicyNames "Use generic-lens or generic-optics with 'policyNames' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprrsIsTruncated :: Lens.Lens' ListRolePoliciesResponse (Core.Maybe Core.Bool)
lrprrsIsTruncated = Lens.field @"isTruncated"
{-# DEPRECATED lrprrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprrsMarker :: Lens.Lens' ListRolePoliciesResponse (Core.Maybe Types.ResponseMarkerType)
lrprrsMarker = Lens.field @"marker"
{-# DEPRECATED lrprrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprrsResponseStatus :: Lens.Lens' ListRolePoliciesResponse Core.Int
lrprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lrprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
