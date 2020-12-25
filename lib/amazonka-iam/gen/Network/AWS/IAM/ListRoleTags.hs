{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListRoleTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags that are attached to the specified role. The returned list of tags is sorted by tag key. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
module Network.AWS.IAM.ListRoleTags
  ( -- * Creating a request
    ListRoleTags (..),
    mkListRoleTags,

    -- ** Request lenses
    lrtRoleName,
    lrtMarker,
    lrtMaxItems,

    -- * Destructuring the response
    ListRoleTagsResponse (..),
    mkListRoleTagsResponse,

    -- ** Response lenses
    lrtrrsTags,
    lrtrrsIsTruncated,
    lrtrrsMarker,
    lrtrrsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListRoleTags' smart constructor.
data ListRoleTags = ListRoleTags'
  { -- | The name of the IAM role for which you want to see the list of tags.
    --
    -- This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    roleName :: Types.RoleNameType,
    -- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
    marker :: Core.Maybe Types.MarkerType,
    -- | (Optional) Use this only when paginating results to indicate the maximum number of items that you want in the response. If additional items exist beyond the maximum that you specify, the @IsTruncated@ response element is @true@ .
    --
    -- If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when more results are available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
    maxItems :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRoleTags' value with any optional fields omitted.
mkListRoleTags ::
  -- | 'roleName'
  Types.RoleNameType ->
  ListRoleTags
mkListRoleTags roleName =
  ListRoleTags'
    { roleName,
      marker = Core.Nothing,
      maxItems = Core.Nothing
    }

-- | The name of the IAM role for which you want to see the list of tags.
--
-- This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtRoleName :: Lens.Lens' ListRoleTags Types.RoleNameType
lrtRoleName = Lens.field @"roleName"
{-# DEPRECATED lrtRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtMarker :: Lens.Lens' ListRoleTags (Core.Maybe Types.MarkerType)
lrtMarker = Lens.field @"marker"
{-# DEPRECATED lrtMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | (Optional) Use this only when paginating results to indicate the maximum number of items that you want in the response. If additional items exist beyond the maximum that you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when more results are available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtMaxItems :: Lens.Lens' ListRoleTags (Core.Maybe Core.Natural)
lrtMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lrtMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Core.AWSRequest ListRoleTags where
  type Rs ListRoleTags = ListRoleTagsResponse
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
            ( Core.pure ("Action", "ListRoleTags")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "RoleName" roleName)
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxItems" Core.<$> maxItems)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListRoleTagsResult"
      ( \s h x ->
          ListRoleTagsResponse'
            Core.<$> ( x Core..@? "Tags" Core..@! Core.mempty
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (x Core..@? "IsTruncated")
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListRoleTagsResponse' smart constructor.
data ListRoleTagsResponse = ListRoleTagsResponse'
  { -- | The list of tags currently that is attached to the role. Each tag consists of a key name and an associated value. If no tags are attached to the specified role, the response contains an empty list.
    tags :: [Types.Tag],
    -- | A flag that indicates whether there are more items to return. If your results were truncated, you can use the @Marker@ request parameter to make a subsequent pagination request that retrieves more items. Note that IAM might return fewer than the @MaxItems@ number of results even when more results are available. Check @IsTruncated@ after every call to ensure that you receive all of your results.
    isTruncated :: Core.Maybe Core.Bool,
    -- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
    marker :: Core.Maybe Types.Marker,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRoleTagsResponse' value with any optional fields omitted.
mkListRoleTagsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListRoleTagsResponse
mkListRoleTagsResponse responseStatus =
  ListRoleTagsResponse'
    { tags = Core.mempty,
      isTruncated = Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | The list of tags currently that is attached to the role. Each tag consists of a key name and an associated value. If no tags are attached to the specified role, the response contains an empty list.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtrrsTags :: Lens.Lens' ListRoleTagsResponse [Types.Tag]
lrtrrsTags = Lens.field @"tags"
{-# DEPRECATED lrtrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can use the @Marker@ request parameter to make a subsequent pagination request that retrieves more items. Note that IAM might return fewer than the @MaxItems@ number of results even when more results are available. Check @IsTruncated@ after every call to ensure that you receive all of your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtrrsIsTruncated :: Lens.Lens' ListRoleTagsResponse (Core.Maybe Core.Bool)
lrtrrsIsTruncated = Lens.field @"isTruncated"
{-# DEPRECATED lrtrrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtrrsMarker :: Lens.Lens' ListRoleTagsResponse (Core.Maybe Types.Marker)
lrtrrsMarker = Lens.field @"marker"
{-# DEPRECATED lrtrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtrrsResponseStatus :: Lens.Lens' ListRoleTagsResponse Core.Int
lrtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lrtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
