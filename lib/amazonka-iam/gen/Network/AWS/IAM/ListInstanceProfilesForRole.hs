{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListInstanceProfilesForRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the instance profiles that have the specified associated IAM role. If there are none, the operation returns an empty list. For more information about instance profiles, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles> .
--
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListInstanceProfilesForRole
  ( -- * Creating a request
    ListInstanceProfilesForRole (..),
    mkListInstanceProfilesForRole,

    -- ** Request lenses
    lipfrRoleName,
    lipfrMarker,
    lipfrMaxItems,

    -- * Destructuring the response
    ListInstanceProfilesForRoleResponse (..),
    mkListInstanceProfilesForRoleResponse,

    -- ** Response lenses
    lipfrrrsInstanceProfiles,
    lipfrrrsIsTruncated,
    lipfrrrsMarker,
    lipfrrrsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListInstanceProfilesForRole' smart constructor.
data ListInstanceProfilesForRole = ListInstanceProfilesForRole'
  { -- | The name of the role to list instance profiles for.
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

-- | Creates a 'ListInstanceProfilesForRole' value with any optional fields omitted.
mkListInstanceProfilesForRole ::
  -- | 'roleName'
  Types.RoleName ->
  ListInstanceProfilesForRole
mkListInstanceProfilesForRole roleName =
  ListInstanceProfilesForRole'
    { roleName,
      marker = Core.Nothing,
      maxItems = Core.Nothing
    }

-- | The name of the role to list instance profiles for.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipfrRoleName :: Lens.Lens' ListInstanceProfilesForRole Types.RoleName
lipfrRoleName = Lens.field @"roleName"
{-# DEPRECATED lipfrRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipfrMarker :: Lens.Lens' ListInstanceProfilesForRole (Core.Maybe Types.MarkerType)
lipfrMarker = Lens.field @"marker"
{-# DEPRECATED lipfrMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipfrMaxItems :: Lens.Lens' ListInstanceProfilesForRole (Core.Maybe Core.Natural)
lipfrMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lipfrMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Core.AWSRequest ListInstanceProfilesForRole where
  type
    Rs ListInstanceProfilesForRole =
      ListInstanceProfilesForRoleResponse
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
            ( Core.pure ("Action", "ListInstanceProfilesForRole")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "RoleName" roleName)
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxItems" Core.<$> maxItems)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListInstanceProfilesForRoleResult"
      ( \s h x ->
          ListInstanceProfilesForRoleResponse'
            Core.<$> ( x Core..@? "InstanceProfiles" Core..@! Core.mempty
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (x Core..@? "IsTruncated")
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListInstanceProfilesForRole where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"isTruncated") = Core.Nothing
    | Core.isNothing (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Contains the response to a successful 'ListInstanceProfilesForRole' request.
--
-- /See:/ 'mkListInstanceProfilesForRoleResponse' smart constructor.
data ListInstanceProfilesForRoleResponse = ListInstanceProfilesForRoleResponse'
  { -- | A list of instance profiles.
    instanceProfiles :: [Types.InstanceProfile],
    -- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
    isTruncated :: Core.Maybe Core.Bool,
    -- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
    marker :: Core.Maybe Types.Marker,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListInstanceProfilesForRoleResponse' value with any optional fields omitted.
mkListInstanceProfilesForRoleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListInstanceProfilesForRoleResponse
mkListInstanceProfilesForRoleResponse responseStatus =
  ListInstanceProfilesForRoleResponse'
    { instanceProfiles =
        Core.mempty,
      isTruncated = Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | A list of instance profiles.
--
-- /Note:/ Consider using 'instanceProfiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipfrrrsInstanceProfiles :: Lens.Lens' ListInstanceProfilesForRoleResponse [Types.InstanceProfile]
lipfrrrsInstanceProfiles = Lens.field @"instanceProfiles"
{-# DEPRECATED lipfrrrsInstanceProfiles "Use generic-lens or generic-optics with 'instanceProfiles' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipfrrrsIsTruncated :: Lens.Lens' ListInstanceProfilesForRoleResponse (Core.Maybe Core.Bool)
lipfrrrsIsTruncated = Lens.field @"isTruncated"
{-# DEPRECATED lipfrrrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipfrrrsMarker :: Lens.Lens' ListInstanceProfilesForRoleResponse (Core.Maybe Types.Marker)
lipfrrrsMarker = Lens.field @"marker"
{-# DEPRECATED lipfrrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipfrrrsResponseStatus :: Lens.Lens' ListInstanceProfilesForRoleResponse Core.Int
lipfrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lipfrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
