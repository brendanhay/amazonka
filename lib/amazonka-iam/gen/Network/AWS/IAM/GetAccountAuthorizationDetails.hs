{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetAccountAuthorizationDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about all IAM users, groups, roles, and policies in your AWS account, including their relationships to one another. Use this API to obtain a snapshot of the configuration of IAM permissions (users, groups, roles, and policies) in your account.
--
-- You can optionally filter the results using the @Filter@ parameter. You can paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- This operation returns paginated results.
module Network.AWS.IAM.GetAccountAuthorizationDetails
    (
    -- * Creating a request
      GetAccountAuthorizationDetails (..)
    , mkGetAccountAuthorizationDetails
    -- ** Request lenses
    , gaadFilter
    , gaadMarker
    , gaadMaxItems

    -- * Destructuring the response
    , GetAccountAuthorizationDetailsResponse (..)
    , mkGetAccountAuthorizationDetailsResponse
    -- ** Response lenses
    , gaadrrsGroupDetailList
    , gaadrrsIsTruncated
    , gaadrrsMarker
    , gaadrrsPolicies
    , gaadrrsRoleDetailList
    , gaadrrsUserDetailList
    , gaadrrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAccountAuthorizationDetails' smart constructor.
data GetAccountAuthorizationDetails = GetAccountAuthorizationDetails'
  { filter :: Core.Maybe [Types.EntityType]
    -- ^ A list of entity types used to filter the results. Only the entities that match the types you specify are included in the output. Use the value @LocalManagedPolicy@ to include customer managed policies.
--
-- The format for this parameter is a comma-separated (if more than one) list of strings. Each string value in the list must be one of the valid values listed below.
  , marker :: Core.Maybe Types.Marker
    -- ^ Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
  , maxItems :: Core.Maybe Core.Natural
    -- ^ Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAccountAuthorizationDetails' value with any optional fields omitted.
mkGetAccountAuthorizationDetails
    :: GetAccountAuthorizationDetails
mkGetAccountAuthorizationDetails
  = GetAccountAuthorizationDetails'{filter = Core.Nothing,
                                    marker = Core.Nothing, maxItems = Core.Nothing}

-- | A list of entity types used to filter the results. Only the entities that match the types you specify are included in the output. Use the value @LocalManagedPolicy@ to include customer managed policies.
--
-- The format for this parameter is a comma-separated (if more than one) list of strings. Each string value in the list must be one of the valid values listed below.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaadFilter :: Lens.Lens' GetAccountAuthorizationDetails (Core.Maybe [Types.EntityType])
gaadFilter = Lens.field @"filter"
{-# INLINEABLE gaadFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaadMarker :: Lens.Lens' GetAccountAuthorizationDetails (Core.Maybe Types.Marker)
gaadMarker = Lens.field @"marker"
{-# INLINEABLE gaadMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaadMaxItems :: Lens.Lens' GetAccountAuthorizationDetails (Core.Maybe Core.Natural)
gaadMaxItems = Lens.field @"maxItems"
{-# INLINEABLE gaadMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

instance Core.ToQuery GetAccountAuthorizationDetails where
        toQuery GetAccountAuthorizationDetails{..}
          = Core.toQueryPair "Action"
              ("GetAccountAuthorizationDetails" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<>
              Core.toQueryPair "Filter"
                (Core.maybe Core.mempty (Core.toQueryList "member") filter)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxItems") maxItems

instance Core.ToHeaders GetAccountAuthorizationDetails where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetAccountAuthorizationDetails where
        type Rs GetAccountAuthorizationDetails =
             GetAccountAuthorizationDetailsResponse
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
          = Response.receiveXMLWrapper "GetAccountAuthorizationDetailsResult"
              (\ s h x ->
                 GetAccountAuthorizationDetailsResponse' Core.<$>
                   (x Core..@? "GroupDetailList" Core..<@> Core.parseXMLList "member")
                     Core.<*> x Core..@? "IsTruncated"
                     Core.<*> x Core..@? "Marker"
                     Core.<*> x Core..@? "Policies" Core..<@> Core.parseXMLList "member"
                     Core.<*>
                     x Core..@? "RoleDetailList" Core..<@> Core.parseXMLList "member"
                     Core.<*>
                     x Core..@? "UserDetailList" Core..<@> Core.parseXMLList "member"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetAccountAuthorizationDetails where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"isTruncated") = Core.Nothing
          | Core.isNothing (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Contains the response to a successful 'GetAccountAuthorizationDetails' request. 
--
-- /See:/ 'mkGetAccountAuthorizationDetailsResponse' smart constructor.
data GetAccountAuthorizationDetailsResponse = GetAccountAuthorizationDetailsResponse'
  { groupDetailList :: Core.Maybe [Types.GroupDetail]
    -- ^ A list containing information about IAM groups.
  , isTruncated :: Core.Maybe Core.Bool
    -- ^ A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
  , marker :: Core.Maybe Types.ResponseMarkerType
    -- ^ When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
  , policies :: Core.Maybe [Types.ManagedPolicyDetail]
    -- ^ A list containing information about managed policies.
  , roleDetailList :: Core.Maybe [Types.RoleDetail]
    -- ^ A list containing information about IAM roles.
  , userDetailList :: Core.Maybe [Types.UserDetail]
    -- ^ A list containing information about IAM users.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetAccountAuthorizationDetailsResponse' value with any optional fields omitted.
mkGetAccountAuthorizationDetailsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetAccountAuthorizationDetailsResponse
mkGetAccountAuthorizationDetailsResponse responseStatus
  = GetAccountAuthorizationDetailsResponse'{groupDetailList =
                                              Core.Nothing,
                                            isTruncated = Core.Nothing, marker = Core.Nothing,
                                            policies = Core.Nothing, roleDetailList = Core.Nothing,
                                            userDetailList = Core.Nothing, responseStatus}

-- | A list containing information about IAM groups.
--
-- /Note:/ Consider using 'groupDetailList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaadrrsGroupDetailList :: Lens.Lens' GetAccountAuthorizationDetailsResponse (Core.Maybe [Types.GroupDetail])
gaadrrsGroupDetailList = Lens.field @"groupDetailList"
{-# INLINEABLE gaadrrsGroupDetailList #-}
{-# DEPRECATED groupDetailList "Use generic-lens or generic-optics with 'groupDetailList' instead"  #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaadrrsIsTruncated :: Lens.Lens' GetAccountAuthorizationDetailsResponse (Core.Maybe Core.Bool)
gaadrrsIsTruncated = Lens.field @"isTruncated"
{-# INLINEABLE gaadrrsIsTruncated #-}
{-# DEPRECATED isTruncated "Use generic-lens or generic-optics with 'isTruncated' instead"  #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaadrrsMarker :: Lens.Lens' GetAccountAuthorizationDetailsResponse (Core.Maybe Types.ResponseMarkerType)
gaadrrsMarker = Lens.field @"marker"
{-# INLINEABLE gaadrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | A list containing information about managed policies.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaadrrsPolicies :: Lens.Lens' GetAccountAuthorizationDetailsResponse (Core.Maybe [Types.ManagedPolicyDetail])
gaadrrsPolicies = Lens.field @"policies"
{-# INLINEABLE gaadrrsPolicies #-}
{-# DEPRECATED policies "Use generic-lens or generic-optics with 'policies' instead"  #-}

-- | A list containing information about IAM roles.
--
-- /Note:/ Consider using 'roleDetailList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaadrrsRoleDetailList :: Lens.Lens' GetAccountAuthorizationDetailsResponse (Core.Maybe [Types.RoleDetail])
gaadrrsRoleDetailList = Lens.field @"roleDetailList"
{-# INLINEABLE gaadrrsRoleDetailList #-}
{-# DEPRECATED roleDetailList "Use generic-lens or generic-optics with 'roleDetailList' instead"  #-}

-- | A list containing information about IAM users.
--
-- /Note:/ Consider using 'userDetailList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaadrrsUserDetailList :: Lens.Lens' GetAccountAuthorizationDetailsResponse (Core.Maybe [Types.UserDetail])
gaadrrsUserDetailList = Lens.field @"userDetailList"
{-# INLINEABLE gaadrrsUserDetailList #-}
{-# DEPRECATED userDetailList "Use generic-lens or generic-optics with 'userDetailList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaadrrsResponseStatus :: Lens.Lens' GetAccountAuthorizationDetailsResponse Core.Int
gaadrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gaadrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
