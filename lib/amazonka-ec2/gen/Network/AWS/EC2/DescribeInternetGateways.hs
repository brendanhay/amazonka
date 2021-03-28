{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeInternetGateways
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your internet gateways.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeInternetGateways
    (
    -- * Creating a request
      DescribeInternetGateways (..)
    , mkDescribeInternetGateways
    -- ** Request lenses
    , dDryRun
    , dFilters
    , dInternetGatewayIds
    , dMaxResults
    , dNextToken

    -- * Destructuring the response
    , DescribeInternetGatewaysResponse (..)
    , mkDescribeInternetGatewaysResponse
    -- ** Response lenses
    , digrrsInternetGateways
    , digrrsNextToken
    , digrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeInternetGateways' smart constructor.
data DescribeInternetGateways = DescribeInternetGateways'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters.
--
--
--     * @attachment.state@ - The current state of the attachment between the gateway and the VPC (@available@ ). Present only if a VPC is attached.
--
--
--     * @attachment.vpc-id@ - The ID of an attached VPC.
--
--
--     * @internet-gateway-id@ - The ID of the Internet gateway.
--
--
--     * @owner-id@ - The ID of the AWS account that owns the internet gateway.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
  , internetGatewayIds :: Core.Maybe [Types.InternetGatewayId]
    -- ^ One or more internet gateway IDs.
--
-- Default: Describes all your internet gateways.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInternetGateways' value with any optional fields omitted.
mkDescribeInternetGateways
    :: DescribeInternetGateways
mkDescribeInternetGateways
  = DescribeInternetGateways'{dryRun = Core.Nothing,
                              filters = Core.Nothing, internetGatewayIds = Core.Nothing,
                              maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDryRun :: Lens.Lens' DescribeInternetGateways (Core.Maybe Core.Bool)
dDryRun = Lens.field @"dryRun"
{-# INLINEABLE dDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | One or more filters.
--
--
--     * @attachment.state@ - The current state of the attachment between the gateway and the VPC (@available@ ). Present only if a VPC is attached.
--
--
--     * @attachment.vpc-id@ - The ID of an attached VPC.
--
--
--     * @internet-gateway-id@ - The ID of the Internet gateway.
--
--
--     * @owner-id@ - The ID of the AWS account that owns the internet gateway.
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
dFilters :: Lens.Lens' DescribeInternetGateways (Core.Maybe [Types.Filter])
dFilters = Lens.field @"filters"
{-# INLINEABLE dFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | One or more internet gateway IDs.
--
-- Default: Describes all your internet gateways.
--
-- /Note:/ Consider using 'internetGatewayIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dInternetGatewayIds :: Lens.Lens' DescribeInternetGateways (Core.Maybe [Types.InternetGatewayId])
dInternetGatewayIds = Lens.field @"internetGatewayIds"
{-# INLINEABLE dInternetGatewayIds #-}
{-# DEPRECATED internetGatewayIds "Use generic-lens or generic-optics with 'internetGatewayIds' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaxResults :: Lens.Lens' DescribeInternetGateways (Core.Maybe Core.Natural)
dMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNextToken :: Lens.Lens' DescribeInternetGateways (Core.Maybe Core.Text)
dNextToken = Lens.field @"nextToken"
{-# INLINEABLE dNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeInternetGateways where
        toQuery DescribeInternetGateways{..}
          = Core.toQueryPair "Action"
              ("DescribeInternetGateways" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "InternetGatewayId")
                internetGatewayIds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeInternetGateways where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeInternetGateways where
        type Rs DescribeInternetGateways = DescribeInternetGatewaysResponse
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
                 DescribeInternetGatewaysResponse' Core.<$>
                   (x Core..@? "internetGatewaySet" Core..<@>
                      Core.parseXMLList "item")
                     Core.<*> x Core..@? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeInternetGateways where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"internetGateways" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeInternetGatewaysResponse' smart constructor.
data DescribeInternetGatewaysResponse = DescribeInternetGatewaysResponse'
  { internetGateways :: Core.Maybe [Types.InternetGateway]
    -- ^ Information about one or more internet gateways.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInternetGatewaysResponse' value with any optional fields omitted.
mkDescribeInternetGatewaysResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeInternetGatewaysResponse
mkDescribeInternetGatewaysResponse responseStatus
  = DescribeInternetGatewaysResponse'{internetGateways =
                                        Core.Nothing,
                                      nextToken = Core.Nothing, responseStatus}

-- | Information about one or more internet gateways.
--
-- /Note:/ Consider using 'internetGateways' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digrrsInternetGateways :: Lens.Lens' DescribeInternetGatewaysResponse (Core.Maybe [Types.InternetGateway])
digrrsInternetGateways = Lens.field @"internetGateways"
{-# INLINEABLE digrrsInternetGateways #-}
{-# DEPRECATED internetGateways "Use generic-lens or generic-optics with 'internetGateways' instead"  #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digrrsNextToken :: Lens.Lens' DescribeInternetGatewaysResponse (Core.Maybe Core.Text)
digrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE digrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digrrsResponseStatus :: Lens.Lens' DescribeInternetGatewaysResponse Core.Int
digrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE digrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
