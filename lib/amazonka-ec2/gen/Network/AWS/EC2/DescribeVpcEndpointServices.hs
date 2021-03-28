{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVpcEndpointServices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes available services to which you can create a VPC endpoint.
--
-- When the service provider and the consumer have different accounts multiple Availability Zones, and the consumer views the VPC endpoint service information, the response only includes the common Availability Zones. For example, when the service provider account uses @us-east-1a@ and @us-east-1c@ and the consumer uses @us-east-1a@ and us-east-1a and us-east-1b, the response includes the VPC endpoint services in the common Availability Zone, @us-east-1a@ .
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVpcEndpointServices
    (
    -- * Creating a request
      DescribeVpcEndpointServices (..)
    , mkDescribeVpcEndpointServices
    -- ** Request lenses
    , dvesDryRun
    , dvesFilters
    , dvesMaxResults
    , dvesNextToken
    , dvesServiceNames

    -- * Destructuring the response
    , DescribeVpcEndpointServicesResponse (..)
    , mkDescribeVpcEndpointServicesResponse
    -- ** Response lenses
    , dvesrrsNextToken
    , dvesrrsServiceDetails
    , dvesrrsServiceNames
    , dvesrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeVpcEndpointServices.
--
-- /See:/ 'mkDescribeVpcEndpointServices' smart constructor.
data DescribeVpcEndpointServices = DescribeVpcEndpointServices'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters.
--
--
--     * @service-name@ - The name of the service.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of items to return for this request. The request returns a token that you can specify in a subsequent call to get the next set of results.
--
-- Constraint: If the value is greater than 1,000, we return only 1,000 items.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of items to return. (You received this token from a prior call.)
  , serviceNames :: Core.Maybe [Core.Text]
    -- ^ One or more service names.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVpcEndpointServices' value with any optional fields omitted.
mkDescribeVpcEndpointServices
    :: DescribeVpcEndpointServices
mkDescribeVpcEndpointServices
  = DescribeVpcEndpointServices'{dryRun = Core.Nothing,
                                 filters = Core.Nothing, maxResults = Core.Nothing,
                                 nextToken = Core.Nothing, serviceNames = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvesDryRun :: Lens.Lens' DescribeVpcEndpointServices (Core.Maybe Core.Bool)
dvesDryRun = Lens.field @"dryRun"
{-# INLINEABLE dvesDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | One or more filters.
--
--
--     * @service-name@ - The name of the service.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvesFilters :: Lens.Lens' DescribeVpcEndpointServices (Core.Maybe [Types.Filter])
dvesFilters = Lens.field @"filters"
{-# INLINEABLE dvesFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of items to return for this request. The request returns a token that you can specify in a subsequent call to get the next set of results.
--
-- Constraint: If the value is greater than 1,000, we return only 1,000 items.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvesMaxResults :: Lens.Lens' DescribeVpcEndpointServices (Core.Maybe Core.Int)
dvesMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dvesMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of items to return. (You received this token from a prior call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvesNextToken :: Lens.Lens' DescribeVpcEndpointServices (Core.Maybe Core.Text)
dvesNextToken = Lens.field @"nextToken"
{-# INLINEABLE dvesNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | One or more service names.
--
-- /Note:/ Consider using 'serviceNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvesServiceNames :: Lens.Lens' DescribeVpcEndpointServices (Core.Maybe [Core.Text])
dvesServiceNames = Lens.field @"serviceNames"
{-# INLINEABLE dvesServiceNames #-}
{-# DEPRECATED serviceNames "Use generic-lens or generic-optics with 'serviceNames' instead"  #-}

instance Core.ToQuery DescribeVpcEndpointServices where
        toQuery DescribeVpcEndpointServices{..}
          = Core.toQueryPair "Action"
              ("DescribeVpcEndpointServices" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "ServiceName")
                serviceNames

instance Core.ToHeaders DescribeVpcEndpointServices where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeVpcEndpointServices where
        type Rs DescribeVpcEndpointServices =
             DescribeVpcEndpointServicesResponse
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
                 DescribeVpcEndpointServicesResponse' Core.<$>
                   (x Core..@? "nextToken") Core.<*>
                     x Core..@? "serviceDetailSet" Core..<@> Core.parseXMLList "item"
                     Core.<*>
                     x Core..@? "serviceNameSet" Core..<@> Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeVpcEndpointServices where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"serviceDetails" Core.. Lens._Just)
            = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"serviceNames" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Contains the output of DescribeVpcEndpointServices.
--
-- /See:/ 'mkDescribeVpcEndpointServicesResponse' smart constructor.
data DescribeVpcEndpointServicesResponse = DescribeVpcEndpointServicesResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
  , serviceDetails :: Core.Maybe [Types.ServiceDetail]
    -- ^ Information about the service.
  , serviceNames :: Core.Maybe [Core.Text]
    -- ^ A list of supported services.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVpcEndpointServicesResponse' value with any optional fields omitted.
mkDescribeVpcEndpointServicesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeVpcEndpointServicesResponse
mkDescribeVpcEndpointServicesResponse responseStatus
  = DescribeVpcEndpointServicesResponse'{nextToken = Core.Nothing,
                                         serviceDetails = Core.Nothing, serviceNames = Core.Nothing,
                                         responseStatus}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvesrrsNextToken :: Lens.Lens' DescribeVpcEndpointServicesResponse (Core.Maybe Core.Text)
dvesrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dvesrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the service.
--
-- /Note:/ Consider using 'serviceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvesrrsServiceDetails :: Lens.Lens' DescribeVpcEndpointServicesResponse (Core.Maybe [Types.ServiceDetail])
dvesrrsServiceDetails = Lens.field @"serviceDetails"
{-# INLINEABLE dvesrrsServiceDetails #-}
{-# DEPRECATED serviceDetails "Use generic-lens or generic-optics with 'serviceDetails' instead"  #-}

-- | A list of supported services.
--
-- /Note:/ Consider using 'serviceNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvesrrsServiceNames :: Lens.Lens' DescribeVpcEndpointServicesResponse (Core.Maybe [Core.Text])
dvesrrsServiceNames = Lens.field @"serviceNames"
{-# INLINEABLE dvesrrsServiceNames #-}
{-# DEPRECATED serviceNames "Use generic-lens or generic-optics with 'serviceNames' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvesrrsResponseStatus :: Lens.Lens' DescribeVpcEndpointServicesResponse Core.Int
dvesrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dvesrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
