{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeDhcpOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your DHCP options sets.
--
-- For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_DHCP_Options.html DHCP Options Sets> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeDhcpOptions
    (
    -- * Creating a request
      DescribeDhcpOptions (..)
    , mkDescribeDhcpOptions
    -- ** Request lenses
    , ddoDhcpOptionsIds
    , ddoDryRun
    , ddoFilters
    , ddoMaxResults
    , ddoNextToken

    -- * Destructuring the response
    , DescribeDhcpOptionsResponse (..)
    , mkDescribeDhcpOptionsResponse
    -- ** Response lenses
    , ddorrsDhcpOptions
    , ddorrsNextToken
    , ddorrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeDhcpOptions' smart constructor.
data DescribeDhcpOptions = DescribeDhcpOptions'
  { dhcpOptionsIds :: Core.Maybe [Types.DhcpOptionsId]
    -- ^ The IDs of one or more DHCP options sets.
--
-- Default: Describes all your DHCP options sets.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters.
--
--
--     * @dhcp-options-id@ - The ID of a DHCP options set.
--
--
--     * @key@ - The key for one of the options (for example, @domain-name@ ).
--
--
--     * @value@ - The value for one of the options.
--
--
--     * @owner-id@ - The ID of the AWS account that owns the DHCP options set.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDhcpOptions' value with any optional fields omitted.
mkDescribeDhcpOptions
    :: DescribeDhcpOptions
mkDescribeDhcpOptions
  = DescribeDhcpOptions'{dhcpOptionsIds = Core.Nothing,
                         dryRun = Core.Nothing, filters = Core.Nothing,
                         maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The IDs of one or more DHCP options sets.
--
-- Default: Describes all your DHCP options sets.
--
-- /Note:/ Consider using 'dhcpOptionsIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddoDhcpOptionsIds :: Lens.Lens' DescribeDhcpOptions (Core.Maybe [Types.DhcpOptionsId])
ddoDhcpOptionsIds = Lens.field @"dhcpOptionsIds"
{-# INLINEABLE ddoDhcpOptionsIds #-}
{-# DEPRECATED dhcpOptionsIds "Use generic-lens or generic-optics with 'dhcpOptionsIds' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddoDryRun :: Lens.Lens' DescribeDhcpOptions (Core.Maybe Core.Bool)
ddoDryRun = Lens.field @"dryRun"
{-# INLINEABLE ddoDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | One or more filters.
--
--
--     * @dhcp-options-id@ - The ID of a DHCP options set.
--
--
--     * @key@ - The key for one of the options (for example, @domain-name@ ).
--
--
--     * @value@ - The value for one of the options.
--
--
--     * @owner-id@ - The ID of the AWS account that owns the DHCP options set.
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
ddoFilters :: Lens.Lens' DescribeDhcpOptions (Core.Maybe [Types.Filter])
ddoFilters = Lens.field @"filters"
{-# INLINEABLE ddoFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddoMaxResults :: Lens.Lens' DescribeDhcpOptions (Core.Maybe Core.Natural)
ddoMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ddoMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddoNextToken :: Lens.Lens' DescribeDhcpOptions (Core.Maybe Core.Text)
ddoNextToken = Lens.field @"nextToken"
{-# INLINEABLE ddoNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeDhcpOptions where
        toQuery DescribeDhcpOptions{..}
          = Core.toQueryPair "Action" ("DescribeDhcpOptions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "DhcpOptionsId")
                dhcpOptionsIds
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeDhcpOptions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeDhcpOptions where
        type Rs DescribeDhcpOptions = DescribeDhcpOptionsResponse
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
                 DescribeDhcpOptionsResponse' Core.<$>
                   (x Core..@? "dhcpOptionsSet" Core..<@> Core.parseXMLList "item")
                     Core.<*> x Core..@? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeDhcpOptions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"dhcpOptions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeDhcpOptionsResponse' smart constructor.
data DescribeDhcpOptionsResponse = DescribeDhcpOptionsResponse'
  { dhcpOptions :: Core.Maybe [Types.DhcpOptions]
    -- ^ Information about one or more DHCP options sets.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDhcpOptionsResponse' value with any optional fields omitted.
mkDescribeDhcpOptionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDhcpOptionsResponse
mkDescribeDhcpOptionsResponse responseStatus
  = DescribeDhcpOptionsResponse'{dhcpOptions = Core.Nothing,
                                 nextToken = Core.Nothing, responseStatus}

-- | Information about one or more DHCP options sets.
--
-- /Note:/ Consider using 'dhcpOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddorrsDhcpOptions :: Lens.Lens' DescribeDhcpOptionsResponse (Core.Maybe [Types.DhcpOptions])
ddorrsDhcpOptions = Lens.field @"dhcpOptions"
{-# INLINEABLE ddorrsDhcpOptions #-}
{-# DEPRECATED dhcpOptions "Use generic-lens or generic-optics with 'dhcpOptions' instead"  #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddorrsNextToken :: Lens.Lens' DescribeDhcpOptionsResponse (Core.Maybe Core.Text)
ddorrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ddorrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddorrsResponseStatus :: Lens.Lens' DescribeDhcpOptionsResponse Core.Int
ddorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
