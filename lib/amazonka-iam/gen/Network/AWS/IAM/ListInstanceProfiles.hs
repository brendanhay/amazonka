{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListInstanceProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the instance profiles that have the specified path prefix. If there are none, the operation returns an empty list. For more information about instance profiles, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles> .
--
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListInstanceProfiles
    (
    -- * Creating a request
      ListInstanceProfiles (..)
    , mkListInstanceProfiles
    -- ** Request lenses
    , lipMarker
    , lipMaxItems
    , lipPathPrefix

    -- * Destructuring the response
    , ListInstanceProfilesResponse (..)
    , mkListInstanceProfilesResponse
    -- ** Response lenses
    , liprrsInstanceProfiles
    , liprrsIsTruncated
    , liprrsMarker
    , liprrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListInstanceProfiles' smart constructor.
data ListInstanceProfiles = ListInstanceProfiles'
  { marker :: Core.Maybe Types.MarkerType
    -- ^ Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
  , maxItems :: Core.Maybe Core.Natural
    -- ^ Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
  , pathPrefix :: Core.Maybe Types.PathPrefix
    -- ^ The path prefix for filtering the results. For example, the prefix @/application_abc/component_xyz/@ gets all instance profiles whose path starts with @/application_abc/component_xyz/@ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/), listing all instance profiles. This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListInstanceProfiles' value with any optional fields omitted.
mkListInstanceProfiles
    :: ListInstanceProfiles
mkListInstanceProfiles
  = ListInstanceProfiles'{marker = Core.Nothing,
                          maxItems = Core.Nothing, pathPrefix = Core.Nothing}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipMarker :: Lens.Lens' ListInstanceProfiles (Core.Maybe Types.MarkerType)
lipMarker = Lens.field @"marker"
{-# INLINEABLE lipMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipMaxItems :: Lens.Lens' ListInstanceProfiles (Core.Maybe Core.Natural)
lipMaxItems = Lens.field @"maxItems"
{-# INLINEABLE lipMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

-- | The path prefix for filtering the results. For example, the prefix @/application_abc/component_xyz/@ gets all instance profiles whose path starts with @/application_abc/component_xyz/@ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/), listing all instance profiles. This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'pathPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipPathPrefix :: Lens.Lens' ListInstanceProfiles (Core.Maybe Types.PathPrefix)
lipPathPrefix = Lens.field @"pathPrefix"
{-# INLINEABLE lipPathPrefix #-}
{-# DEPRECATED pathPrefix "Use generic-lens or generic-optics with 'pathPrefix' instead"  #-}

instance Core.ToQuery ListInstanceProfiles where
        toQuery ListInstanceProfiles{..}
          = Core.toQueryPair "Action" ("ListInstanceProfiles" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxItems") maxItems
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PathPrefix") pathPrefix

instance Core.ToHeaders ListInstanceProfiles where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListInstanceProfiles where
        type Rs ListInstanceProfiles = ListInstanceProfilesResponse
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
          = Response.receiveXMLWrapper "ListInstanceProfilesResult"
              (\ s h x ->
                 ListInstanceProfilesResponse' Core.<$>
                   (x Core..@ "InstanceProfiles" Core..@! Core.mempty Core..<@>
                      Core.parseXMLList "member")
                     Core.<*> x Core..@? "IsTruncated"
                     Core.<*> x Core..@? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListInstanceProfiles where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"isTruncated") = Core.Nothing
          | Core.isNothing (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Contains the response to a successful 'ListInstanceProfiles' request. 
--
-- /See:/ 'mkListInstanceProfilesResponse' smart constructor.
data ListInstanceProfilesResponse = ListInstanceProfilesResponse'
  { instanceProfiles :: [Types.InstanceProfile]
    -- ^ A list of instance profiles.
  , isTruncated :: Core.Maybe Core.Bool
    -- ^ A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
  , marker :: Core.Maybe Types.ResponseMarkerType
    -- ^ When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListInstanceProfilesResponse' value with any optional fields omitted.
mkListInstanceProfilesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListInstanceProfilesResponse
mkListInstanceProfilesResponse responseStatus
  = ListInstanceProfilesResponse'{instanceProfiles = Core.mempty,
                                  isTruncated = Core.Nothing, marker = Core.Nothing, responseStatus}

-- | A list of instance profiles.
--
-- /Note:/ Consider using 'instanceProfiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprrsInstanceProfiles :: Lens.Lens' ListInstanceProfilesResponse [Types.InstanceProfile]
liprrsInstanceProfiles = Lens.field @"instanceProfiles"
{-# INLINEABLE liprrsInstanceProfiles #-}
{-# DEPRECATED instanceProfiles "Use generic-lens or generic-optics with 'instanceProfiles' instead"  #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprrsIsTruncated :: Lens.Lens' ListInstanceProfilesResponse (Core.Maybe Core.Bool)
liprrsIsTruncated = Lens.field @"isTruncated"
{-# INLINEABLE liprrsIsTruncated #-}
{-# DEPRECATED isTruncated "Use generic-lens or generic-optics with 'isTruncated' instead"  #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprrsMarker :: Lens.Lens' ListInstanceProfilesResponse (Core.Maybe Types.ResponseMarkerType)
liprrsMarker = Lens.field @"marker"
{-# INLINEABLE liprrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprrsResponseStatus :: Lens.Lens' ListInstanceProfilesResponse Core.Int
liprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE liprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
