{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVpcEndpointServicePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the principals (service consumers) that are permitted to discover your VPC endpoint service.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVpcEndpointServicePermissions
    (
    -- * Creating a request
      DescribeVpcEndpointServicePermissions (..)
    , mkDescribeVpcEndpointServicePermissions
    -- ** Request lenses
    , dvespServiceId
    , dvespDryRun
    , dvespFilters
    , dvespMaxResults
    , dvespNextToken

    -- * Destructuring the response
    , DescribeVpcEndpointServicePermissionsResponse (..)
    , mkDescribeVpcEndpointServicePermissionsResponse
    -- ** Response lenses
    , dvesprrsAllowedPrincipals
    , dvesprrsNextToken
    , dvesprrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeVpcEndpointServicePermissions' smart constructor.
data DescribeVpcEndpointServicePermissions = DescribeVpcEndpointServicePermissions'
  { serviceId :: Types.VpcEndpointServiceId
    -- ^ The ID of the service.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters.
--
--
--     * @principal@ - The ARN of the principal.
--
--
--     * @principal-type@ - The principal type (@All@ | @Service@ | @OrganizationUnit@ | @Account@ | @User@ | @Role@ ).
--
--
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1,000; if @MaxResults@ is given a value larger than 1,000, only 1,000 results are returned.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to retrieve the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVpcEndpointServicePermissions' value with any optional fields omitted.
mkDescribeVpcEndpointServicePermissions
    :: Types.VpcEndpointServiceId -- ^ 'serviceId'
    -> DescribeVpcEndpointServicePermissions
mkDescribeVpcEndpointServicePermissions serviceId
  = DescribeVpcEndpointServicePermissions'{serviceId,
                                           dryRun = Core.Nothing, filters = Core.Nothing,
                                           maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The ID of the service.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvespServiceId :: Lens.Lens' DescribeVpcEndpointServicePermissions Types.VpcEndpointServiceId
dvespServiceId = Lens.field @"serviceId"
{-# INLINEABLE dvespServiceId #-}
{-# DEPRECATED serviceId "Use generic-lens or generic-optics with 'serviceId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvespDryRun :: Lens.Lens' DescribeVpcEndpointServicePermissions (Core.Maybe Core.Bool)
dvespDryRun = Lens.field @"dryRun"
{-# INLINEABLE dvespDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | One or more filters.
--
--
--     * @principal@ - The ARN of the principal.
--
--
--     * @principal-type@ - The principal type (@All@ | @Service@ | @OrganizationUnit@ | @Account@ | @User@ | @Role@ ).
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvespFilters :: Lens.Lens' DescribeVpcEndpointServicePermissions (Core.Maybe [Types.Filter])
dvespFilters = Lens.field @"filters"
{-# INLINEABLE dvespFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1,000; if @MaxResults@ is given a value larger than 1,000, only 1,000 results are returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvespMaxResults :: Lens.Lens' DescribeVpcEndpointServicePermissions (Core.Maybe Core.Int)
dvespMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dvespMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvespNextToken :: Lens.Lens' DescribeVpcEndpointServicePermissions (Core.Maybe Core.Text)
dvespNextToken = Lens.field @"nextToken"
{-# INLINEABLE dvespNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeVpcEndpointServicePermissions where
        toQuery DescribeVpcEndpointServicePermissions{..}
          = Core.toQueryPair "Action"
              ("DescribeVpcEndpointServicePermissions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ServiceId" serviceId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeVpcEndpointServicePermissions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeVpcEndpointServicePermissions
         where
        type Rs DescribeVpcEndpointServicePermissions =
             DescribeVpcEndpointServicePermissionsResponse
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
                 DescribeVpcEndpointServicePermissionsResponse' Core.<$>
                   (x Core..@? "allowedPrincipals" Core..<@> Core.parseXMLList "item")
                     Core.<*> x Core..@? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeVpcEndpointServicePermissions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"allowedPrincipals" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeVpcEndpointServicePermissionsResponse' smart constructor.
data DescribeVpcEndpointServicePermissionsResponse = DescribeVpcEndpointServicePermissionsResponse'
  { allowedPrincipals :: Core.Maybe [Types.AllowedPrincipal]
    -- ^ Information about one or more allowed principals.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVpcEndpointServicePermissionsResponse' value with any optional fields omitted.
mkDescribeVpcEndpointServicePermissionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeVpcEndpointServicePermissionsResponse
mkDescribeVpcEndpointServicePermissionsResponse responseStatus
  = DescribeVpcEndpointServicePermissionsResponse'{allowedPrincipals
                                                     = Core.Nothing,
                                                   nextToken = Core.Nothing, responseStatus}

-- | Information about one or more allowed principals.
--
-- /Note:/ Consider using 'allowedPrincipals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvesprrsAllowedPrincipals :: Lens.Lens' DescribeVpcEndpointServicePermissionsResponse (Core.Maybe [Types.AllowedPrincipal])
dvesprrsAllowedPrincipals = Lens.field @"allowedPrincipals"
{-# INLINEABLE dvesprrsAllowedPrincipals #-}
{-# DEPRECATED allowedPrincipals "Use generic-lens or generic-optics with 'allowedPrincipals' instead"  #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvesprrsNextToken :: Lens.Lens' DescribeVpcEndpointServicePermissionsResponse (Core.Maybe Core.Text)
dvesprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dvesprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvesprrsResponseStatus :: Lens.Lens' DescribeVpcEndpointServicePermissionsResponse Core.Int
dvesprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dvesprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
