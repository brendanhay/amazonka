{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetAssociatedIpv6PoolCidrs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the IPv6 CIDR block associations for a specified IPv6 address pool.
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetAssociatedIpv6PoolCidrs
    (
    -- * Creating a request
      GetAssociatedIpv6PoolCidrs (..)
    , mkGetAssociatedIpv6PoolCidrs
    -- ** Request lenses
    , gaipcPoolId
    , gaipcDryRun
    , gaipcMaxResults
    , gaipcNextToken

    -- * Destructuring the response
    , GetAssociatedIpv6PoolCidrsResponse (..)
    , mkGetAssociatedIpv6PoolCidrsResponse
    -- ** Response lenses
    , gaipcrrsIpv6CidrAssociations
    , gaipcrrsNextToken
    , gaipcrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAssociatedIpv6PoolCidrs' smart constructor.
data GetAssociatedIpv6PoolCidrs = GetAssociatedIpv6PoolCidrs'
  { poolId :: Types.Ipv6PoolEc2Id
    -- ^ The ID of the IPv6 address pool.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAssociatedIpv6PoolCidrs' value with any optional fields omitted.
mkGetAssociatedIpv6PoolCidrs
    :: Types.Ipv6PoolEc2Id -- ^ 'poolId'
    -> GetAssociatedIpv6PoolCidrs
mkGetAssociatedIpv6PoolCidrs poolId
  = GetAssociatedIpv6PoolCidrs'{poolId, dryRun = Core.Nothing,
                                maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The ID of the IPv6 address pool.
--
-- /Note:/ Consider using 'poolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaipcPoolId :: Lens.Lens' GetAssociatedIpv6PoolCidrs Types.Ipv6PoolEc2Id
gaipcPoolId = Lens.field @"poolId"
{-# INLINEABLE gaipcPoolId #-}
{-# DEPRECATED poolId "Use generic-lens or generic-optics with 'poolId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaipcDryRun :: Lens.Lens' GetAssociatedIpv6PoolCidrs (Core.Maybe Core.Bool)
gaipcDryRun = Lens.field @"dryRun"
{-# INLINEABLE gaipcDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaipcMaxResults :: Lens.Lens' GetAssociatedIpv6PoolCidrs (Core.Maybe Core.Natural)
gaipcMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gaipcMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaipcNextToken :: Lens.Lens' GetAssociatedIpv6PoolCidrs (Core.Maybe Types.NextToken)
gaipcNextToken = Lens.field @"nextToken"
{-# INLINEABLE gaipcNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetAssociatedIpv6PoolCidrs where
        toQuery GetAssociatedIpv6PoolCidrs{..}
          = Core.toQueryPair "Action"
              ("GetAssociatedIpv6PoolCidrs" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "PoolId" poolId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders GetAssociatedIpv6PoolCidrs where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetAssociatedIpv6PoolCidrs where
        type Rs GetAssociatedIpv6PoolCidrs =
             GetAssociatedIpv6PoolCidrsResponse
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
                 GetAssociatedIpv6PoolCidrsResponse' Core.<$>
                   (x Core..@? "ipv6CidrAssociationSet" Core..<@>
                      Core.parseXMLList "item")
                     Core.<*> x Core..@? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetAssociatedIpv6PoolCidrs where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"ipv6CidrAssociations" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetAssociatedIpv6PoolCidrsResponse' smart constructor.
data GetAssociatedIpv6PoolCidrsResponse = GetAssociatedIpv6PoolCidrsResponse'
  { ipv6CidrAssociations :: Core.Maybe [Types.Ipv6CidrAssociation]
    -- ^ Information about the IPv6 CIDR block associations.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAssociatedIpv6PoolCidrsResponse' value with any optional fields omitted.
mkGetAssociatedIpv6PoolCidrsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetAssociatedIpv6PoolCidrsResponse
mkGetAssociatedIpv6PoolCidrsResponse responseStatus
  = GetAssociatedIpv6PoolCidrsResponse'{ipv6CidrAssociations =
                                          Core.Nothing,
                                        nextToken = Core.Nothing, responseStatus}

-- | Information about the IPv6 CIDR block associations.
--
-- /Note:/ Consider using 'ipv6CidrAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaipcrrsIpv6CidrAssociations :: Lens.Lens' GetAssociatedIpv6PoolCidrsResponse (Core.Maybe [Types.Ipv6CidrAssociation])
gaipcrrsIpv6CidrAssociations = Lens.field @"ipv6CidrAssociations"
{-# INLINEABLE gaipcrrsIpv6CidrAssociations #-}
{-# DEPRECATED ipv6CidrAssociations "Use generic-lens or generic-optics with 'ipv6CidrAssociations' instead"  #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaipcrrsNextToken :: Lens.Lens' GetAssociatedIpv6PoolCidrsResponse (Core.Maybe Core.Text)
gaipcrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gaipcrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaipcrrsResponseStatus :: Lens.Lens' GetAssociatedIpv6PoolCidrsResponse Core.Int
gaipcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gaipcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
