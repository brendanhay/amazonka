{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DescribeResourcePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the permissions of a specified resource.
--
-- This operation returns paginated results.
module Network.AWS.WorkDocs.DescribeResourcePermissions
    (
    -- * Creating a request
      DescribeResourcePermissions (..)
    , mkDescribeResourcePermissions
    -- ** Request lenses
    , drpResourceId
    , drpAuthenticationToken
    , drpLimit
    , drpMarker
    , drpPrincipalId

    -- * Destructuring the response
    , DescribeResourcePermissionsResponse (..)
    , mkDescribeResourcePermissionsResponse
    -- ** Response lenses
    , drprrsMarker
    , drprrsPrincipals
    , drprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkDescribeResourcePermissions' smart constructor.
data DescribeResourcePermissions = DescribeResourcePermissions'
  { resourceId :: Types.ResourceIdType
    -- ^ The ID of the resource.
  , authenticationToken :: Core.Maybe Types.AuthenticationHeaderType
    -- ^ Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return with this call.
  , marker :: Core.Maybe Types.PageMarkerType
    -- ^ The marker for the next set of results. (You received this marker from a previous call)
  , principalId :: Core.Maybe Types.IdType
    -- ^ The ID of the principal to filter permissions by.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeResourcePermissions' value with any optional fields omitted.
mkDescribeResourcePermissions
    :: Types.ResourceIdType -- ^ 'resourceId'
    -> DescribeResourcePermissions
mkDescribeResourcePermissions resourceId
  = DescribeResourcePermissions'{resourceId,
                                 authenticationToken = Core.Nothing, limit = Core.Nothing,
                                 marker = Core.Nothing, principalId = Core.Nothing}

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpResourceId :: Lens.Lens' DescribeResourcePermissions Types.ResourceIdType
drpResourceId = Lens.field @"resourceId"
{-# INLINEABLE drpResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpAuthenticationToken :: Lens.Lens' DescribeResourcePermissions (Core.Maybe Types.AuthenticationHeaderType)
drpAuthenticationToken = Lens.field @"authenticationToken"
{-# INLINEABLE drpAuthenticationToken #-}
{-# DEPRECATED authenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead"  #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpLimit :: Lens.Lens' DescribeResourcePermissions (Core.Maybe Core.Natural)
drpLimit = Lens.field @"limit"
{-# INLINEABLE drpLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The marker for the next set of results. (You received this marker from a previous call)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpMarker :: Lens.Lens' DescribeResourcePermissions (Core.Maybe Types.PageMarkerType)
drpMarker = Lens.field @"marker"
{-# INLINEABLE drpMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The ID of the principal to filter permissions by.
--
-- /Note:/ Consider using 'principalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpPrincipalId :: Lens.Lens' DescribeResourcePermissions (Core.Maybe Types.IdType)
drpPrincipalId = Lens.field @"principalId"
{-# INLINEABLE drpPrincipalId #-}
{-# DEPRECATED principalId "Use generic-lens or generic-optics with 'principalId' instead"  #-}

instance Core.ToQuery DescribeResourcePermissions where
        toQuery DescribeResourcePermissions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "limit") limit Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "principalId") principalId

instance Core.ToHeaders DescribeResourcePermissions where
        toHeaders DescribeResourcePermissions{..}
          = Core.toHeaders "Authentication" authenticationToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DescribeResourcePermissions where
        type Rs DescribeResourcePermissions =
             DescribeResourcePermissionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/api/v1/resources/" Core.<> Core.toText resourceId Core.<>
                             "/permissions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeResourcePermissionsResponse' Core.<$>
                   (x Core..:? "Marker") Core.<*> x Core..:? "Principals" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeResourcePermissions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"principals" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | /See:/ 'mkDescribeResourcePermissionsResponse' smart constructor.
data DescribeResourcePermissionsResponse = DescribeResourcePermissionsResponse'
  { marker :: Core.Maybe Types.Marker
    -- ^ The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
  , principals :: Core.Maybe [Types.Principal]
    -- ^ The principals.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeResourcePermissionsResponse' value with any optional fields omitted.
mkDescribeResourcePermissionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeResourcePermissionsResponse
mkDescribeResourcePermissionsResponse responseStatus
  = DescribeResourcePermissionsResponse'{marker = Core.Nothing,
                                         principals = Core.Nothing, responseStatus}

-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprrsMarker :: Lens.Lens' DescribeResourcePermissionsResponse (Core.Maybe Types.Marker)
drprrsMarker = Lens.field @"marker"
{-# INLINEABLE drprrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The principals.
--
-- /Note:/ Consider using 'principals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprrsPrincipals :: Lens.Lens' DescribeResourcePermissionsResponse (Core.Maybe [Types.Principal])
drprrsPrincipals = Lens.field @"principals"
{-# INLINEABLE drprrsPrincipals #-}
{-# DEPRECATED principals "Use generic-lens or generic-optics with 'principals' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprrsResponseStatus :: Lens.Lens' DescribeResourcePermissionsResponse Core.Int
drprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
