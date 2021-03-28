{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListRoleTags (..)
    , mkListRoleTags
    -- ** Request lenses
    , lrtRoleName
    , lrtMarker
    , lrtMaxItems

    -- * Destructuring the response
    , ListRoleTagsResponse (..)
    , mkListRoleTagsResponse
    -- ** Response lenses
    , lrtrrsTags
    , lrtrrsIsTruncated
    , lrtrrsMarker
    , lrtrrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListRoleTags' smart constructor.
data ListRoleTags = ListRoleTags'
  { roleName :: Types.RoleNameType
    -- ^ The name of the IAM role for which you want to see the list of tags.
--
-- This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  , marker :: Core.Maybe Types.MarkerType
    -- ^ Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
  , maxItems :: Core.Maybe Core.Natural
    -- ^ (Optional) Use this only when paginating results to indicate the maximum number of items that you want in the response. If additional items exist beyond the maximum that you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when more results are available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRoleTags' value with any optional fields omitted.
mkListRoleTags
    :: Types.RoleNameType -- ^ 'roleName'
    -> ListRoleTags
mkListRoleTags roleName
  = ListRoleTags'{roleName, marker = Core.Nothing,
                  maxItems = Core.Nothing}

-- | The name of the IAM role for which you want to see the list of tags.
--
-- This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtRoleName :: Lens.Lens' ListRoleTags Types.RoleNameType
lrtRoleName = Lens.field @"roleName"
{-# INLINEABLE lrtRoleName #-}
{-# DEPRECATED roleName "Use generic-lens or generic-optics with 'roleName' instead"  #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtMarker :: Lens.Lens' ListRoleTags (Core.Maybe Types.MarkerType)
lrtMarker = Lens.field @"marker"
{-# INLINEABLE lrtMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | (Optional) Use this only when paginating results to indicate the maximum number of items that you want in the response. If additional items exist beyond the maximum that you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when more results are available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtMaxItems :: Lens.Lens' ListRoleTags (Core.Maybe Core.Natural)
lrtMaxItems = Lens.field @"maxItems"
{-# INLINEABLE lrtMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

instance Core.ToQuery ListRoleTags where
        toQuery ListRoleTags{..}
          = Core.toQueryPair "Action" ("ListRoleTags" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "RoleName" roleName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxItems") maxItems

instance Core.ToHeaders ListRoleTags where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListRoleTags where
        type Rs ListRoleTags = ListRoleTagsResponse
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
          = Response.receiveXMLWrapper "ListRoleTagsResult"
              (\ s h x ->
                 ListRoleTagsResponse' Core.<$>
                   (x Core..@ "Tags" Core..@! Core.mempty Core..<@>
                      Core.parseXMLList "member")
                     Core.<*> x Core..@? "IsTruncated"
                     Core.<*> x Core..@? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListRoleTagsResponse' smart constructor.
data ListRoleTagsResponse = ListRoleTagsResponse'
  { tags :: [Types.Tag]
    -- ^ The list of tags currently that is attached to the role. Each tag consists of a key name and an associated value. If no tags are attached to the specified role, the response contains an empty list.
  , isTruncated :: Core.Maybe Core.Bool
    -- ^ A flag that indicates whether there are more items to return. If your results were truncated, you can use the @Marker@ request parameter to make a subsequent pagination request that retrieves more items. Note that IAM might return fewer than the @MaxItems@ number of results even when more results are available. Check @IsTruncated@ after every call to ensure that you receive all of your results.
  , marker :: Core.Maybe Types.Marker
    -- ^ When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRoleTagsResponse' value with any optional fields omitted.
mkListRoleTagsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListRoleTagsResponse
mkListRoleTagsResponse responseStatus
  = ListRoleTagsResponse'{tags = Core.mempty,
                          isTruncated = Core.Nothing, marker = Core.Nothing, responseStatus}

-- | The list of tags currently that is attached to the role. Each tag consists of a key name and an associated value. If no tags are attached to the specified role, the response contains an empty list.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtrrsTags :: Lens.Lens' ListRoleTagsResponse [Types.Tag]
lrtrrsTags = Lens.field @"tags"
{-# INLINEABLE lrtrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can use the @Marker@ request parameter to make a subsequent pagination request that retrieves more items. Note that IAM might return fewer than the @MaxItems@ number of results even when more results are available. Check @IsTruncated@ after every call to ensure that you receive all of your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtrrsIsTruncated :: Lens.Lens' ListRoleTagsResponse (Core.Maybe Core.Bool)
lrtrrsIsTruncated = Lens.field @"isTruncated"
{-# INLINEABLE lrtrrsIsTruncated #-}
{-# DEPRECATED isTruncated "Use generic-lens or generic-optics with 'isTruncated' instead"  #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtrrsMarker :: Lens.Lens' ListRoleTagsResponse (Core.Maybe Types.Marker)
lrtrrsMarker = Lens.field @"marker"
{-# INLINEABLE lrtrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtrrsResponseStatus :: Lens.Lens' ListRoleTagsResponse Core.Int
lrtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
