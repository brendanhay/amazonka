{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of IAM users that are in the specified IAM group. You can paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- This operation returns paginated results.
module Network.AWS.IAM.GetGroup
    (
    -- * Creating a request
      GetGroup (..)
    , mkGetGroup
    -- ** Request lenses
    , ggGroupName
    , ggMarker
    , ggMaxItems

    -- * Destructuring the response
    , GetGroupResponse (..)
    , mkGetGroupResponse
    -- ** Response lenses
    , ggrrsGroup
    , ggrrsUsers
    , ggrrsIsTruncated
    , ggrrsMarker
    , ggrrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetGroup' smart constructor.
data GetGroup = GetGroup'
  { groupName :: Types.GroupNameType
    -- ^ The name of the group.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  , marker :: Core.Maybe Types.MarkerType
    -- ^ Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
  , maxItems :: Core.Maybe Core.Natural
    -- ^ Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGroup' value with any optional fields omitted.
mkGetGroup
    :: Types.GroupNameType -- ^ 'groupName'
    -> GetGroup
mkGetGroup groupName
  = GetGroup'{groupName, marker = Core.Nothing,
              maxItems = Core.Nothing}

-- | The name of the group.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggGroupName :: Lens.Lens' GetGroup Types.GroupNameType
ggGroupName = Lens.field @"groupName"
{-# INLINEABLE ggGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggMarker :: Lens.Lens' GetGroup (Core.Maybe Types.MarkerType)
ggMarker = Lens.field @"marker"
{-# INLINEABLE ggMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggMaxItems :: Lens.Lens' GetGroup (Core.Maybe Core.Natural)
ggMaxItems = Lens.field @"maxItems"
{-# INLINEABLE ggMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

instance Core.ToQuery GetGroup where
        toQuery GetGroup{..}
          = Core.toQueryPair "Action" ("GetGroup" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "GroupName" groupName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxItems") maxItems

instance Core.ToHeaders GetGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetGroup where
        type Rs GetGroup = GetGroupResponse
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
          = Response.receiveXMLWrapper "GetGroupResult"
              (\ s h x ->
                 GetGroupResponse' Core.<$>
                   (x Core..@ "Group") Core.<*>
                     x Core..@ "Users" Core..@! Core.mempty Core..<@>
                       Core.parseXMLList "member"
                     Core.<*> x Core..@? "IsTruncated"
                     Core.<*> x Core..@? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetGroup where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"isTruncated") = Core.Nothing
          | Core.isNothing (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Contains the response to a successful 'GetGroup' request. 
--
-- /See:/ 'mkGetGroupResponse' smart constructor.
data GetGroupResponse = GetGroupResponse'
  { group :: Types.Group
    -- ^ A structure that contains details about the group.
  , users :: [Types.User]
    -- ^ A list of users in the group.
  , isTruncated :: Core.Maybe Core.Bool
    -- ^ A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
  , marker :: Core.Maybe Types.ResponseMarkerType
    -- ^ When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetGroupResponse' value with any optional fields omitted.
mkGetGroupResponse
    :: Types.Group -- ^ 'group'
    -> Core.Int -- ^ 'responseStatus'
    -> GetGroupResponse
mkGetGroupResponse group responseStatus
  = GetGroupResponse'{group, users = Core.mempty,
                      isTruncated = Core.Nothing, marker = Core.Nothing, responseStatus}

-- | A structure that contains details about the group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrrsGroup :: Lens.Lens' GetGroupResponse Types.Group
ggrrsGroup = Lens.field @"group"
{-# INLINEABLE ggrrsGroup #-}
{-# DEPRECATED group "Use generic-lens or generic-optics with 'group' instead"  #-}

-- | A list of users in the group.
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrrsUsers :: Lens.Lens' GetGroupResponse [Types.User]
ggrrsUsers = Lens.field @"users"
{-# INLINEABLE ggrrsUsers #-}
{-# DEPRECATED users "Use generic-lens or generic-optics with 'users' instead"  #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrrsIsTruncated :: Lens.Lens' GetGroupResponse (Core.Maybe Core.Bool)
ggrrsIsTruncated = Lens.field @"isTruncated"
{-# INLINEABLE ggrrsIsTruncated #-}
{-# DEPRECATED isTruncated "Use generic-lens or generic-optics with 'isTruncated' instead"  #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrrsMarker :: Lens.Lens' GetGroupResponse (Core.Maybe Types.ResponseMarkerType)
ggrrsMarker = Lens.field @"marker"
{-# INLINEABLE ggrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrrsResponseStatus :: Lens.Lens' GetGroupResponse Core.Int
ggrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ggrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
