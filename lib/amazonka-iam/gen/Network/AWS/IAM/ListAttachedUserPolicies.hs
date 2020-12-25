{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListAttachedUserPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all managed policies that are attached to the specified IAM user.
--
-- An IAM user can also have inline policies embedded with it. To list the inline policies for a user, use the 'ListUserPolicies' API. For information about policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters. You can use the @PathPrefix@ parameter to limit the list of policies to only those matching the specified path prefix. If there are no policies attached to the specified group (or none that match the specified path prefix), the operation returns an empty list.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListAttachedUserPolicies
  ( -- * Creating a request
    ListAttachedUserPolicies (..),
    mkListAttachedUserPolicies,

    -- ** Request lenses
    laupUserName,
    laupMarker,
    laupMaxItems,
    laupPathPrefix,

    -- * Destructuring the response
    ListAttachedUserPoliciesResponse (..),
    mkListAttachedUserPoliciesResponse,

    -- ** Response lenses
    lauprrsAttachedPolicies,
    lauprrsIsTruncated,
    lauprrsMarker,
    lauprrsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAttachedUserPolicies' smart constructor.
data ListAttachedUserPolicies = ListAttachedUserPolicies'
  { -- | The name (friendly name, not ARN) of the user to list attached policies for.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Types.UserName,
    -- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
    marker :: Core.Maybe Types.MarkerType,
    -- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
    --
    -- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
    maxItems :: Core.Maybe Core.Natural,
    -- | The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all policies.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
    pathPrefix :: Core.Maybe Types.PolicyPathType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAttachedUserPolicies' value with any optional fields omitted.
mkListAttachedUserPolicies ::
  -- | 'userName'
  Types.UserName ->
  ListAttachedUserPolicies
mkListAttachedUserPolicies userName =
  ListAttachedUserPolicies'
    { userName,
      marker = Core.Nothing,
      maxItems = Core.Nothing,
      pathPrefix = Core.Nothing
    }

-- | The name (friendly name, not ARN) of the user to list attached policies for.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laupUserName :: Lens.Lens' ListAttachedUserPolicies Types.UserName
laupUserName = Lens.field @"userName"
{-# DEPRECATED laupUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laupMarker :: Lens.Lens' ListAttachedUserPolicies (Core.Maybe Types.MarkerType)
laupMarker = Lens.field @"marker"
{-# DEPRECATED laupMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laupMaxItems :: Lens.Lens' ListAttachedUserPolicies (Core.Maybe Core.Natural)
laupMaxItems = Lens.field @"maxItems"
{-# DEPRECATED laupMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all policies.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'pathPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laupPathPrefix :: Lens.Lens' ListAttachedUserPolicies (Core.Maybe Types.PolicyPathType)
laupPathPrefix = Lens.field @"pathPrefix"
{-# DEPRECATED laupPathPrefix "Use generic-lens or generic-optics with 'pathPrefix' instead." #-}

instance Core.AWSRequest ListAttachedUserPolicies where
  type Rs ListAttachedUserPolicies = ListAttachedUserPoliciesResponse
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
            ( Core.pure ("Action", "ListAttachedUserPolicies")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "UserName" userName)
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxItems" Core.<$> maxItems)
                Core.<> (Core.toQueryValue "PathPrefix" Core.<$> pathPrefix)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListAttachedUserPoliciesResult"
      ( \s h x ->
          ListAttachedUserPoliciesResponse'
            Core.<$> ( x Core..@? "AttachedPolicies"
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (x Core..@? "IsTruncated")
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListAttachedUserPolicies where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"isTruncated") = Core.Nothing
    | Core.isNothing (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Contains the response to a successful 'ListAttachedUserPolicies' request.
--
-- /See:/ 'mkListAttachedUserPoliciesResponse' smart constructor.
data ListAttachedUserPoliciesResponse = ListAttachedUserPoliciesResponse'
  { -- | A list of the attached policies.
    attachedPolicies :: Core.Maybe [Types.AttachedPolicy],
    -- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
    isTruncated :: Core.Maybe Core.Bool,
    -- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
    marker :: Core.Maybe Types.ResponseMarkerType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAttachedUserPoliciesResponse' value with any optional fields omitted.
mkListAttachedUserPoliciesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListAttachedUserPoliciesResponse
mkListAttachedUserPoliciesResponse responseStatus =
  ListAttachedUserPoliciesResponse'
    { attachedPolicies =
        Core.Nothing,
      isTruncated = Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | A list of the attached policies.
--
-- /Note:/ Consider using 'attachedPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lauprrsAttachedPolicies :: Lens.Lens' ListAttachedUserPoliciesResponse (Core.Maybe [Types.AttachedPolicy])
lauprrsAttachedPolicies = Lens.field @"attachedPolicies"
{-# DEPRECATED lauprrsAttachedPolicies "Use generic-lens or generic-optics with 'attachedPolicies' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lauprrsIsTruncated :: Lens.Lens' ListAttachedUserPoliciesResponse (Core.Maybe Core.Bool)
lauprrsIsTruncated = Lens.field @"isTruncated"
{-# DEPRECATED lauprrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lauprrsMarker :: Lens.Lens' ListAttachedUserPoliciesResponse (Core.Maybe Types.ResponseMarkerType)
lauprrsMarker = Lens.field @"marker"
{-# DEPRECATED lauprrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lauprrsResponseStatus :: Lens.Lens' ListAttachedUserPoliciesResponse Core.Int
lauprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lauprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
