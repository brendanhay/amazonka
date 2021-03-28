{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified tags for your EC2 resources.
--
-- For more information about tags, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging Your Resources> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTags
    (
    -- * Creating a request
      DescribeTags (..)
    , mkDescribeTags
    -- ** Request lenses
    , dtDryRun
    , dtFilters
    , dtMaxResults
    , dtNextToken

    -- * Destructuring the response
    , DescribeTagsResponse (..)
    , mkDescribeTagsResponse
    -- ** Response lenses
    , dtrrsNextToken
    , dtrrsTags
    , dtrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeTags' smart constructor.
data DescribeTags = DescribeTags'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ The filters.
--
--
--     * @key@ - The tag key.
--
--
--     * @resource-id@ - The ID of the resource.
--
--
--     * @resource-type@ - The resource type (@customer-gateway@ | @dedicated-host@ | @dhcp-options@ | @elastic-ip@ | @fleet@ | @fpga-image@ | @host-reservation@ | @image@ | @instance@ | @internet-gateway@ | @key-pair@ | @launch-template@ | @natgateway@ | @network-acl@ | @network-interface@ | @placement-group@ | @reserved-instances@ | @route-table@ | @security-group@ | @snapshot@ | @spot-instances-request@ | @subnet@ | @volume@ | @vpc@ | @vpc-endpoint@ | @vpc-endpoint-service@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ ).
--
--
--     * @tag@ :<key> - The key/value combination of the tag. For example, specify "tag:Owner" for the filter name and "TeamA" for the filter value to find resources with the tag "Owner=TeamA".
--
--
--     * @value@ - The tag value.
--
--
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of results to return in a single call. This value can be between 5 and 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to retrieve the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTags' value with any optional fields omitted.
mkDescribeTags
    :: DescribeTags
mkDescribeTags
  = DescribeTags'{dryRun = Core.Nothing, filters = Core.Nothing,
                  maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtDryRun :: Lens.Lens' DescribeTags (Core.Maybe Core.Bool)
dtDryRun = Lens.field @"dryRun"
{-# INLINEABLE dtDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The filters.
--
--
--     * @key@ - The tag key.
--
--
--     * @resource-id@ - The ID of the resource.
--
--
--     * @resource-type@ - The resource type (@customer-gateway@ | @dedicated-host@ | @dhcp-options@ | @elastic-ip@ | @fleet@ | @fpga-image@ | @host-reservation@ | @image@ | @instance@ | @internet-gateway@ | @key-pair@ | @launch-template@ | @natgateway@ | @network-acl@ | @network-interface@ | @placement-group@ | @reserved-instances@ | @route-table@ | @security-group@ | @snapshot@ | @spot-instances-request@ | @subnet@ | @volume@ | @vpc@ | @vpc-endpoint@ | @vpc-endpoint-service@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ ).
--
--
--     * @tag@ :<key> - The key/value combination of the tag. For example, specify "tag:Owner" for the filter name and "TeamA" for the filter value to find resources with the tag "Owner=TeamA".
--
--
--     * @value@ - The tag value.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtFilters :: Lens.Lens' DescribeTags (Core.Maybe [Types.Filter])
dtFilters = Lens.field @"filters"
{-# INLINEABLE dtFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return in a single call. This value can be between 5 and 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtMaxResults :: Lens.Lens' DescribeTags (Core.Maybe Core.Int)
dtMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dtMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtNextToken :: Lens.Lens' DescribeTags (Core.Maybe Core.Text)
dtNextToken = Lens.field @"nextToken"
{-# INLINEABLE dtNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeTags where
        toQuery DescribeTags{..}
          = Core.toQueryPair "Action" ("DescribeTags" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeTags where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeTags where
        type Rs DescribeTags = DescribeTagsResponse
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
                 DescribeTagsResponse' Core.<$>
                   (x Core..@? "nextToken") Core.<*>
                     x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeTags where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"tags" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , tags :: Core.Maybe [Types.TagDescription]
    -- ^ The tags.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTagsResponse' value with any optional fields omitted.
mkDescribeTagsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeTagsResponse
mkDescribeTagsResponse responseStatus
  = DescribeTagsResponse'{nextToken = Core.Nothing,
                          tags = Core.Nothing, responseStatus}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsNextToken :: Lens.Lens' DescribeTagsResponse (Core.Maybe Core.Text)
dtrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dtrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsTags :: Lens.Lens' DescribeTagsResponse (Core.Maybe [Types.TagDescription])
dtrrsTags = Lens.field @"tags"
{-# INLINEABLE dtrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsResponseStatus :: Lens.Lens' DescribeTagsResponse Core.Int
dtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
