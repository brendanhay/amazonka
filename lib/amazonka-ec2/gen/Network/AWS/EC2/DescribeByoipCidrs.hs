{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeByoipCidrs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the IP address ranges that were specified in calls to 'ProvisionByoipCidr' .
--
-- To describe the address pools that were created when you provisioned the address ranges, use 'DescribePublicIpv4Pools' or 'DescribeIpv6Pools' .
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeByoipCidrs
    (
    -- * Creating a request
      DescribeByoipCidrs (..)
    , mkDescribeByoipCidrs
    -- ** Request lenses
    , dbcMaxResults
    , dbcDryRun
    , dbcNextToken

    -- * Destructuring the response
    , DescribeByoipCidrsResponse (..)
    , mkDescribeByoipCidrsResponse
    -- ** Response lenses
    , dbcrrsByoipCidrs
    , dbcrrsNextToken
    , dbcrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeByoipCidrs' smart constructor.
data DescribeByoipCidrs = DescribeByoipCidrs'
  { maxResults :: Core.Natural
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeByoipCidrs' value with any optional fields omitted.
mkDescribeByoipCidrs
    :: Core.Natural -- ^ 'maxResults'
    -> DescribeByoipCidrs
mkDescribeByoipCidrs maxResults
  = DescribeByoipCidrs'{maxResults, dryRun = Core.Nothing,
                        nextToken = Core.Nothing}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcMaxResults :: Lens.Lens' DescribeByoipCidrs Core.Natural
dbcMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dbcMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcDryRun :: Lens.Lens' DescribeByoipCidrs (Core.Maybe Core.Bool)
dbcDryRun = Lens.field @"dryRun"
{-# INLINEABLE dbcDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcNextToken :: Lens.Lens' DescribeByoipCidrs (Core.Maybe Types.NextToken)
dbcNextToken = Lens.field @"nextToken"
{-# INLINEABLE dbcNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeByoipCidrs where
        toQuery DescribeByoipCidrs{..}
          = Core.toQueryPair "Action" ("DescribeByoipCidrs" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "MaxResults" maxResults
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeByoipCidrs where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeByoipCidrs where
        type Rs DescribeByoipCidrs = DescribeByoipCidrsResponse
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
                 DescribeByoipCidrsResponse' Core.<$>
                   (x Core..@? "byoipCidrSet" Core..<@> Core.parseXMLList "item")
                     Core.<*> x Core..@? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeByoipCidrs where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"byoipCidrs" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeByoipCidrsResponse' smart constructor.
data DescribeByoipCidrsResponse = DescribeByoipCidrsResponse'
  { byoipCidrs :: Core.Maybe [Types.ByoipCidr]
    -- ^ Information about your address ranges.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeByoipCidrsResponse' value with any optional fields omitted.
mkDescribeByoipCidrsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeByoipCidrsResponse
mkDescribeByoipCidrsResponse responseStatus
  = DescribeByoipCidrsResponse'{byoipCidrs = Core.Nothing,
                                nextToken = Core.Nothing, responseStatus}

-- | Information about your address ranges.
--
-- /Note:/ Consider using 'byoipCidrs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcrrsByoipCidrs :: Lens.Lens' DescribeByoipCidrsResponse (Core.Maybe [Types.ByoipCidr])
dbcrrsByoipCidrs = Lens.field @"byoipCidrs"
{-# INLINEABLE dbcrrsByoipCidrs #-}
{-# DEPRECATED byoipCidrs "Use generic-lens or generic-optics with 'byoipCidrs' instead"  #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcrrsNextToken :: Lens.Lens' DescribeByoipCidrsResponse (Core.Maybe Core.Text)
dbcrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dbcrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcrrsResponseStatus :: Lens.Lens' DescribeByoipCidrsResponse Core.Int
dbcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dbcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
