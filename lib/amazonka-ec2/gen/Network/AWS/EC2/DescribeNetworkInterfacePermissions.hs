{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeNetworkInterfacePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the permissions for your network interfaces. 
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeNetworkInterfacePermissions
    (
    -- * Creating a request
      DescribeNetworkInterfacePermissions (..)
    , mkDescribeNetworkInterfacePermissions
    -- ** Request lenses
    , dnipFilters
    , dnipMaxResults
    , dnipNetworkInterfacePermissionIds
    , dnipNextToken

    -- * Destructuring the response
    , DescribeNetworkInterfacePermissionsResponse (..)
    , mkDescribeNetworkInterfacePermissionsResponse
    -- ** Response lenses
    , dniprfrsNetworkInterfacePermissions
    , dniprfrsNextToken
    , dniprfrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeNetworkInterfacePermissions.
--
-- /See:/ 'mkDescribeNetworkInterfacePermissions' smart constructor.
data DescribeNetworkInterfacePermissions = DescribeNetworkInterfacePermissions'
  { filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters.
--
--
--     * @network-interface-permission.network-interface-permission-id@ - The ID of the permission.
--
--
--     * @network-interface-permission.network-interface-id@ - The ID of the network interface.
--
--
--     * @network-interface-permission.aws-account-id@ - The AWS account ID.
--
--
--     * @network-interface-permission.aws-service@ - The AWS service.
--
--
--     * @network-interface-permission.permission@ - The type of permission (@INSTANCE-ATTACH@ | @EIP-ASSOCIATE@ ).
--
--
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. If this parameter is not specified, up to 50 results are returned by default.
  , networkInterfacePermissionIds :: Core.Maybe [Types.NetworkInterfacePermissionId]
    -- ^ One or more network interface permission IDs.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to request the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeNetworkInterfacePermissions' value with any optional fields omitted.
mkDescribeNetworkInterfacePermissions
    :: DescribeNetworkInterfacePermissions
mkDescribeNetworkInterfacePermissions
  = DescribeNetworkInterfacePermissions'{filters = Core.Nothing,
                                         maxResults = Core.Nothing,
                                         networkInterfacePermissionIds = Core.Nothing,
                                         nextToken = Core.Nothing}

-- | One or more filters.
--
--
--     * @network-interface-permission.network-interface-permission-id@ - The ID of the permission.
--
--
--     * @network-interface-permission.network-interface-id@ - The ID of the network interface.
--
--
--     * @network-interface-permission.aws-account-id@ - The AWS account ID.
--
--
--     * @network-interface-permission.aws-service@ - The AWS service.
--
--
--     * @network-interface-permission.permission@ - The type of permission (@INSTANCE-ATTACH@ | @EIP-ASSOCIATE@ ).
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnipFilters :: Lens.Lens' DescribeNetworkInterfacePermissions (Core.Maybe [Types.Filter])
dnipFilters = Lens.field @"filters"
{-# INLINEABLE dnipFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. If this parameter is not specified, up to 50 results are returned by default.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnipMaxResults :: Lens.Lens' DescribeNetworkInterfacePermissions (Core.Maybe Core.Natural)
dnipMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dnipMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | One or more network interface permission IDs.
--
-- /Note:/ Consider using 'networkInterfacePermissionIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnipNetworkInterfacePermissionIds :: Lens.Lens' DescribeNetworkInterfacePermissions (Core.Maybe [Types.NetworkInterfacePermissionId])
dnipNetworkInterfacePermissionIds = Lens.field @"networkInterfacePermissionIds"
{-# INLINEABLE dnipNetworkInterfacePermissionIds #-}
{-# DEPRECATED networkInterfacePermissionIds "Use generic-lens or generic-optics with 'networkInterfacePermissionIds' instead"  #-}

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnipNextToken :: Lens.Lens' DescribeNetworkInterfacePermissions (Core.Maybe Core.Text)
dnipNextToken = Lens.field @"nextToken"
{-# INLINEABLE dnipNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeNetworkInterfacePermissions where
        toQuery DescribeNetworkInterfacePermissions{..}
          = Core.toQueryPair "Action"
              ("DescribeNetworkInterfacePermissions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryList "NetworkInterfacePermissionId")
                networkInterfacePermissionIds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeNetworkInterfacePermissions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeNetworkInterfacePermissions where
        type Rs DescribeNetworkInterfacePermissions =
             DescribeNetworkInterfacePermissionsResponse
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
          = Response.receiveXML
              (\ s h x ->
                 DescribeNetworkInterfacePermissionsResponse' Core.<$>
                   (x Core..@? "networkInterfacePermissions" Core..<@>
                      Core.parseXMLList "item")
                     Core.<*> x Core..@? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeNetworkInterfacePermissions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"networkInterfacePermissions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Contains the output for DescribeNetworkInterfacePermissions.
--
-- /See:/ 'mkDescribeNetworkInterfacePermissionsResponse' smart constructor.
data DescribeNetworkInterfacePermissionsResponse = DescribeNetworkInterfacePermissionsResponse'
  { networkInterfacePermissions :: Core.Maybe [Types.NetworkInterfacePermission]
    -- ^ The network interface permissions.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeNetworkInterfacePermissionsResponse' value with any optional fields omitted.
mkDescribeNetworkInterfacePermissionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeNetworkInterfacePermissionsResponse
mkDescribeNetworkInterfacePermissionsResponse responseStatus
  = DescribeNetworkInterfacePermissionsResponse'{networkInterfacePermissions
                                                   = Core.Nothing,
                                                 nextToken = Core.Nothing, responseStatus}

-- | The network interface permissions.
--
-- /Note:/ Consider using 'networkInterfacePermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniprfrsNetworkInterfacePermissions :: Lens.Lens' DescribeNetworkInterfacePermissionsResponse (Core.Maybe [Types.NetworkInterfacePermission])
dniprfrsNetworkInterfacePermissions = Lens.field @"networkInterfacePermissions"
{-# INLINEABLE dniprfrsNetworkInterfacePermissions #-}
{-# DEPRECATED networkInterfacePermissions "Use generic-lens or generic-optics with 'networkInterfacePermissions' instead"  #-}

-- | The token to use to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniprfrsNextToken :: Lens.Lens' DescribeNetworkInterfacePermissionsResponse (Core.Maybe Core.Text)
dniprfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dniprfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniprfrsResponseStatus :: Lens.Lens' DescribeNetworkInterfacePermissionsResponse Core.Int
dniprfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dniprfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
