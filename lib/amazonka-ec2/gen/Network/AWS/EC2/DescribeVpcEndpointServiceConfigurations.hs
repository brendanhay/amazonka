{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVpcEndpointServiceConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the VPC endpoint service configurations in your account (your services).
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVpcEndpointServiceConfigurations
    (
    -- * Creating a request
      DescribeVpcEndpointServiceConfigurations (..)
    , mkDescribeVpcEndpointServiceConfigurations
    -- ** Request lenses
    , dvescDryRun
    , dvescFilters
    , dvescMaxResults
    , dvescNextToken
    , dvescServiceIds

    -- * Destructuring the response
    , DescribeVpcEndpointServiceConfigurationsResponse (..)
    , mkDescribeVpcEndpointServiceConfigurationsResponse
    -- ** Response lenses
    , dvescrrsNextToken
    , dvescrrsServiceConfigurations
    , dvescrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeVpcEndpointServiceConfigurations' smart constructor.
data DescribeVpcEndpointServiceConfigurations = DescribeVpcEndpointServiceConfigurations'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters.
--
--
--     * @service-name@ - The name of the service.
--
--
--     * @service-id@ - The ID of the service.
--
--
--     * @service-state@ - The state of the service (@Pending@ | @Available@ | @Deleting@ | @Deleted@ | @Failed@ ). 
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1,000; if @MaxResults@ is given a value larger than 1,000, only 1,000 results are returned.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to retrieve the next page of results.
  , serviceIds :: Core.Maybe [Types.VpcEndpointServiceId]
    -- ^ The IDs of one or more services.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVpcEndpointServiceConfigurations' value with any optional fields omitted.
mkDescribeVpcEndpointServiceConfigurations
    :: DescribeVpcEndpointServiceConfigurations
mkDescribeVpcEndpointServiceConfigurations
  = DescribeVpcEndpointServiceConfigurations'{dryRun = Core.Nothing,
                                              filters = Core.Nothing, maxResults = Core.Nothing,
                                              nextToken = Core.Nothing, serviceIds = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvescDryRun :: Lens.Lens' DescribeVpcEndpointServiceConfigurations (Core.Maybe Core.Bool)
dvescDryRun = Lens.field @"dryRun"
{-# INLINEABLE dvescDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | One or more filters.
--
--
--     * @service-name@ - The name of the service.
--
--
--     * @service-id@ - The ID of the service.
--
--
--     * @service-state@ - The state of the service (@Pending@ | @Available@ | @Deleting@ | @Deleted@ | @Failed@ ). 
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
dvescFilters :: Lens.Lens' DescribeVpcEndpointServiceConfigurations (Core.Maybe [Types.Filter])
dvescFilters = Lens.field @"filters"
{-# INLINEABLE dvescFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1,000; if @MaxResults@ is given a value larger than 1,000, only 1,000 results are returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvescMaxResults :: Lens.Lens' DescribeVpcEndpointServiceConfigurations (Core.Maybe Core.Int)
dvescMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dvescMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvescNextToken :: Lens.Lens' DescribeVpcEndpointServiceConfigurations (Core.Maybe Core.Text)
dvescNextToken = Lens.field @"nextToken"
{-# INLINEABLE dvescNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The IDs of one or more services.
--
-- /Note:/ Consider using 'serviceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvescServiceIds :: Lens.Lens' DescribeVpcEndpointServiceConfigurations (Core.Maybe [Types.VpcEndpointServiceId])
dvescServiceIds = Lens.field @"serviceIds"
{-# INLINEABLE dvescServiceIds #-}
{-# DEPRECATED serviceIds "Use generic-lens or generic-optics with 'serviceIds' instead"  #-}

instance Core.ToQuery DescribeVpcEndpointServiceConfigurations
         where
        toQuery DescribeVpcEndpointServiceConfigurations{..}
          = Core.toQueryPair "Action"
              ("DescribeVpcEndpointServiceConfigurations" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "ServiceId") serviceIds

instance Core.ToHeaders DescribeVpcEndpointServiceConfigurations
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeVpcEndpointServiceConfigurations
         where
        type Rs DescribeVpcEndpointServiceConfigurations =
             DescribeVpcEndpointServiceConfigurationsResponse
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
                 DescribeVpcEndpointServiceConfigurationsResponse' Core.<$>
                   (x Core..@? "nextToken") Core.<*>
                     x Core..@? "serviceConfigurationSet" Core..<@>
                       Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeVpcEndpointServiceConfigurations
         where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"serviceConfigurations" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeVpcEndpointServiceConfigurationsResponse' smart constructor.
data DescribeVpcEndpointServiceConfigurationsResponse = DescribeVpcEndpointServiceConfigurationsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , serviceConfigurations :: Core.Maybe [Types.ServiceConfiguration]
    -- ^ Information about one or more services.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVpcEndpointServiceConfigurationsResponse' value with any optional fields omitted.
mkDescribeVpcEndpointServiceConfigurationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeVpcEndpointServiceConfigurationsResponse
mkDescribeVpcEndpointServiceConfigurationsResponse responseStatus
  = DescribeVpcEndpointServiceConfigurationsResponse'{nextToken =
                                                        Core.Nothing,
                                                      serviceConfigurations = Core.Nothing,
                                                      responseStatus}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvescrrsNextToken :: Lens.Lens' DescribeVpcEndpointServiceConfigurationsResponse (Core.Maybe Core.Text)
dvescrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dvescrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about one or more services.
--
-- /Note:/ Consider using 'serviceConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvescrrsServiceConfigurations :: Lens.Lens' DescribeVpcEndpointServiceConfigurationsResponse (Core.Maybe [Types.ServiceConfiguration])
dvescrrsServiceConfigurations = Lens.field @"serviceConfigurations"
{-# INLINEABLE dvescrrsServiceConfigurations #-}
{-# DEPRECATED serviceConfigurations "Use generic-lens or generic-optics with 'serviceConfigurations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvescrrsResponseStatus :: Lens.Lens' DescribeVpcEndpointServiceConfigurationsResponse Core.Int
dvescrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dvescrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
