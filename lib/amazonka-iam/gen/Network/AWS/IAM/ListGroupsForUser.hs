{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListGroupsForUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the IAM groups that the specified IAM user belongs to.
--
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListGroupsForUser
  ( -- * Creating a request
    ListGroupsForUser (..),
    mkListGroupsForUser,

    -- ** Request lenses
    lgfuUserName,
    lgfuMarker,
    lgfuMaxItems,

    -- * Destructuring the response
    ListGroupsForUserResponse (..),
    mkListGroupsForUserResponse,

    -- ** Response lenses
    lgfurrsGroups,
    lgfurrsIsTruncated,
    lgfurrsMarker,
    lgfurrsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListGroupsForUser' smart constructor.
data ListGroupsForUser = ListGroupsForUser'
  { -- | The name of the user to list groups for.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Types.UserName,
    -- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
    marker :: Core.Maybe Types.MarkerType,
    -- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
    --
    -- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
    maxItems :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListGroupsForUser' value with any optional fields omitted.
mkListGroupsForUser ::
  -- | 'userName'
  Types.UserName ->
  ListGroupsForUser
mkListGroupsForUser userName =
  ListGroupsForUser'
    { userName,
      marker = Core.Nothing,
      maxItems = Core.Nothing
    }

-- | The name of the user to list groups for.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgfuUserName :: Lens.Lens' ListGroupsForUser Types.UserName
lgfuUserName = Lens.field @"userName"
{-# DEPRECATED lgfuUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgfuMarker :: Lens.Lens' ListGroupsForUser (Core.Maybe Types.MarkerType)
lgfuMarker = Lens.field @"marker"
{-# DEPRECATED lgfuMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgfuMaxItems :: Lens.Lens' ListGroupsForUser (Core.Maybe Core.Natural)
lgfuMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lgfuMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Core.AWSRequest ListGroupsForUser where
  type Rs ListGroupsForUser = ListGroupsForUserResponse
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
            ( Core.pure ("Action", "ListGroupsForUser")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "UserName" userName)
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxItems" Core.<$> maxItems)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListGroupsForUserResult"
      ( \s h x ->
          ListGroupsForUserResponse'
            Core.<$> ( x Core..@? "Groups" Core..@! Core.mempty
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (x Core..@? "IsTruncated")
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListGroupsForUser where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"isTruncated") = Core.Nothing
    | Core.isNothing (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Contains the response to a successful 'ListGroupsForUser' request.
--
-- /See:/ 'mkListGroupsForUserResponse' smart constructor.
data ListGroupsForUserResponse = ListGroupsForUserResponse'
  { -- | A list of groups.
    groups :: [Types.Group],
    -- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
    isTruncated :: Core.Maybe Core.Bool,
    -- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
    marker :: Core.Maybe Types.ResponseMarkerType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListGroupsForUserResponse' value with any optional fields omitted.
mkListGroupsForUserResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListGroupsForUserResponse
mkListGroupsForUserResponse responseStatus =
  ListGroupsForUserResponse'
    { groups = Core.mempty,
      isTruncated = Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | A list of groups.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgfurrsGroups :: Lens.Lens' ListGroupsForUserResponse [Types.Group]
lgfurrsGroups = Lens.field @"groups"
{-# DEPRECATED lgfurrsGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgfurrsIsTruncated :: Lens.Lens' ListGroupsForUserResponse (Core.Maybe Core.Bool)
lgfurrsIsTruncated = Lens.field @"isTruncated"
{-# DEPRECATED lgfurrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgfurrsMarker :: Lens.Lens' ListGroupsForUserResponse (Core.Maybe Types.ResponseMarkerType)
lgfurrsMarker = Lens.field @"marker"
{-# DEPRECATED lgfurrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgfurrsResponseStatus :: Lens.Lens' ListGroupsForUserResponse Core.Int
lgfurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lgfurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
