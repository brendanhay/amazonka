{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVpcClassicLinkDnsSupport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the ClassicLink DNS support status of one or more VPCs. If enabled, the DNS hostname of a linked EC2-Classic instance resolves to its private IP address when addressed from an instance in the VPC to which it's linked. Similarly, the DNS hostname of an instance in a VPC resolves to its private IP address when addressed from a linked EC2-Classic instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVpcClassicLinkDnsSupport
    (
    -- * Creating a request
      DescribeVpcClassicLinkDnsSupport (..)
    , mkDescribeVpcClassicLinkDnsSupport
    -- ** Request lenses
    , dvcldsMaxResults
    , dvcldsNextToken
    , dvcldsVpcIds

    -- * Destructuring the response
    , DescribeVpcClassicLinkDnsSupportResponse (..)
    , mkDescribeVpcClassicLinkDnsSupportResponse
    -- ** Response lenses
    , dvcldsrrsNextToken
    , dvcldsrrsVpcs
    , dvcldsrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeVpcClassicLinkDnsSupport' smart constructor.
data DescribeVpcClassicLinkDnsSupport = DescribeVpcClassicLinkDnsSupport'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next page of results.
  , vpcIds :: Core.Maybe [Types.VpcId]
    -- ^ One or more VPC IDs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVpcClassicLinkDnsSupport' value with any optional fields omitted.
mkDescribeVpcClassicLinkDnsSupport
    :: DescribeVpcClassicLinkDnsSupport
mkDescribeVpcClassicLinkDnsSupport
  = DescribeVpcClassicLinkDnsSupport'{maxResults = Core.Nothing,
                                      nextToken = Core.Nothing, vpcIds = Core.Nothing}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcldsMaxResults :: Lens.Lens' DescribeVpcClassicLinkDnsSupport (Core.Maybe Core.Natural)
dvcldsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dvcldsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcldsNextToken :: Lens.Lens' DescribeVpcClassicLinkDnsSupport (Core.Maybe Types.NextToken)
dvcldsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dvcldsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | One or more VPC IDs.
--
-- /Note:/ Consider using 'vpcIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcldsVpcIds :: Lens.Lens' DescribeVpcClassicLinkDnsSupport (Core.Maybe [Types.VpcId])
dvcldsVpcIds = Lens.field @"vpcIds"
{-# INLINEABLE dvcldsVpcIds #-}
{-# DEPRECATED vpcIds "Use generic-lens or generic-optics with 'vpcIds' instead"  #-}

instance Core.ToQuery DescribeVpcClassicLinkDnsSupport where
        toQuery DescribeVpcClassicLinkDnsSupport{..}
          = Core.toQueryPair "Action"
              ("DescribeVpcClassicLinkDnsSupport" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<> Core.maybe Core.mempty (Core.toQueryList "VpcIds") vpcIds

instance Core.ToHeaders DescribeVpcClassicLinkDnsSupport where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeVpcClassicLinkDnsSupport where
        type Rs DescribeVpcClassicLinkDnsSupport =
             DescribeVpcClassicLinkDnsSupportResponse
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
                 DescribeVpcClassicLinkDnsSupportResponse' Core.<$>
                   (x Core..@? "nextToken") Core.<*>
                     x Core..@? "vpcs" Core..<@> Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeVpcClassicLinkDnsSupport where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"vpcs" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeVpcClassicLinkDnsSupportResponse' smart constructor.
data DescribeVpcClassicLinkDnsSupportResponse = DescribeVpcClassicLinkDnsSupportResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , vpcs :: Core.Maybe [Types.ClassicLinkDnsSupport]
    -- ^ Information about the ClassicLink DNS support status of the VPCs.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVpcClassicLinkDnsSupportResponse' value with any optional fields omitted.
mkDescribeVpcClassicLinkDnsSupportResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeVpcClassicLinkDnsSupportResponse
mkDescribeVpcClassicLinkDnsSupportResponse responseStatus
  = DescribeVpcClassicLinkDnsSupportResponse'{nextToken =
                                                Core.Nothing,
                                              vpcs = Core.Nothing, responseStatus}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcldsrrsNextToken :: Lens.Lens' DescribeVpcClassicLinkDnsSupportResponse (Core.Maybe Types.NextToken)
dvcldsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dvcldsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the ClassicLink DNS support status of the VPCs.
--
-- /Note:/ Consider using 'vpcs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcldsrrsVpcs :: Lens.Lens' DescribeVpcClassicLinkDnsSupportResponse (Core.Maybe [Types.ClassicLinkDnsSupport])
dvcldsrrsVpcs = Lens.field @"vpcs"
{-# INLINEABLE dvcldsrrsVpcs #-}
{-# DEPRECATED vpcs "Use generic-lens or generic-optics with 'vpcs' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcldsrrsResponseStatus :: Lens.Lens' DescribeVpcClassicLinkDnsSupportResponse Core.Int
dvcldsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dvcldsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
