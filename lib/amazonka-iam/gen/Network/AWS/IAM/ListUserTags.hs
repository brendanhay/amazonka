{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListUserTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags that are attached to the specified user. The returned list of tags is sorted by tag key. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
module Network.AWS.IAM.ListUserTags
    (
    -- * Creating a request
      ListUserTags (..)
    , mkListUserTags
    -- ** Request lenses
    , lutUserName
    , lutMarker
    , lutMaxItems

    -- * Destructuring the response
    , ListUserTagsResponse (..)
    , mkListUserTagsResponse
    -- ** Response lenses
    , lutrrsTags
    , lutrrsIsTruncated
    , lutrrsMarker
    , lutrrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListUserTags' smart constructor.
data ListUserTags = ListUserTags'
  { userName :: Types.UserName
    -- ^ The name of the IAM user whose tags you want to see.
--
-- This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@-
  , marker :: Core.Maybe Types.MarkerType
    -- ^ Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
  , maxItems :: Core.Maybe Core.Natural
    -- ^ (Optional) Use this only when paginating results to indicate the maximum number of items that you want in the response. If additional items exist beyond the maximum that you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when more results are available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListUserTags' value with any optional fields omitted.
mkListUserTags
    :: Types.UserName -- ^ 'userName'
    -> ListUserTags
mkListUserTags userName
  = ListUserTags'{userName, marker = Core.Nothing,
                  maxItems = Core.Nothing}

-- | The name of the IAM user whose tags you want to see.
--
-- This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lutUserName :: Lens.Lens' ListUserTags Types.UserName
lutUserName = Lens.field @"userName"
{-# INLINEABLE lutUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lutMarker :: Lens.Lens' ListUserTags (Core.Maybe Types.MarkerType)
lutMarker = Lens.field @"marker"
{-# INLINEABLE lutMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | (Optional) Use this only when paginating results to indicate the maximum number of items that you want in the response. If additional items exist beyond the maximum that you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when more results are available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lutMaxItems :: Lens.Lens' ListUserTags (Core.Maybe Core.Natural)
lutMaxItems = Lens.field @"maxItems"
{-# INLINEABLE lutMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

instance Core.ToQuery ListUserTags where
        toQuery ListUserTags{..}
          = Core.toQueryPair "Action" ("ListUserTags" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "UserName" userName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxItems") maxItems

instance Core.ToHeaders ListUserTags where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListUserTags where
        type Rs ListUserTags = ListUserTagsResponse
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
          = Response.receiveXMLWrapper "ListUserTagsResult"
              (\ s h x ->
                 ListUserTagsResponse' Core.<$>
                   (x Core..@ "Tags" Core..@! Core.mempty Core..<@>
                      Core.parseXMLList "member")
                     Core.<*> x Core..@? "IsTruncated"
                     Core.<*> x Core..@? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListUserTagsResponse' smart constructor.
data ListUserTagsResponse = ListUserTagsResponse'
  { tags :: [Types.Tag]
    -- ^ The list of tags that are currently attached to the user. Each tag consists of a key name and an associated value. If no tags are attached to the specified user, the response contains an empty list.
  , isTruncated :: Core.Maybe Core.Bool
    -- ^ A flag that indicates whether there are more items to return. If your results were truncated, you can use the @Marker@ request parameter to make a subsequent pagination request that retrieves more items. Note that IAM might return fewer than the @MaxItems@ number of results even when more results are available. Check @IsTruncated@ after every call to ensure that you receive all of your results.
  , marker :: Core.Maybe Types.ResponseMarkerType
    -- ^ When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListUserTagsResponse' value with any optional fields omitted.
mkListUserTagsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListUserTagsResponse
mkListUserTagsResponse responseStatus
  = ListUserTagsResponse'{tags = Core.mempty,
                          isTruncated = Core.Nothing, marker = Core.Nothing, responseStatus}

-- | The list of tags that are currently attached to the user. Each tag consists of a key name and an associated value. If no tags are attached to the specified user, the response contains an empty list.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lutrrsTags :: Lens.Lens' ListUserTagsResponse [Types.Tag]
lutrrsTags = Lens.field @"tags"
{-# INLINEABLE lutrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can use the @Marker@ request parameter to make a subsequent pagination request that retrieves more items. Note that IAM might return fewer than the @MaxItems@ number of results even when more results are available. Check @IsTruncated@ after every call to ensure that you receive all of your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lutrrsIsTruncated :: Lens.Lens' ListUserTagsResponse (Core.Maybe Core.Bool)
lutrrsIsTruncated = Lens.field @"isTruncated"
{-# INLINEABLE lutrrsIsTruncated #-}
{-# DEPRECATED isTruncated "Use generic-lens or generic-optics with 'isTruncated' instead"  #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lutrrsMarker :: Lens.Lens' ListUserTagsResponse (Core.Maybe Types.ResponseMarkerType)
lutrrsMarker = Lens.field @"marker"
{-# INLINEABLE lutrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lutrrsResponseStatus :: Lens.Lens' ListUserTagsResponse Core.Int
lutrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lutrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
