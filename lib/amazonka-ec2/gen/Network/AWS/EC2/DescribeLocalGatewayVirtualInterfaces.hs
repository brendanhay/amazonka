{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeLocalGatewayVirtualInterfaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified local gateway virtual interfaces.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeLocalGatewayVirtualInterfaces
    (
    -- * Creating a request
      DescribeLocalGatewayVirtualInterfaces (..)
    , mkDescribeLocalGatewayVirtualInterfaces
    -- ** Request lenses
    , dlgviDryRun
    , dlgviFilters
    , dlgviLocalGatewayVirtualInterfaceIds
    , dlgviMaxResults
    , dlgviNextToken

    -- * Destructuring the response
    , DescribeLocalGatewayVirtualInterfacesResponse (..)
    , mkDescribeLocalGatewayVirtualInterfacesResponse
    -- ** Response lenses
    , dlgvirrsLocalGatewayVirtualInterfaces
    , dlgvirrsNextToken
    , dlgvirrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLocalGatewayVirtualInterfaces' smart constructor.
data DescribeLocalGatewayVirtualInterfaces = DescribeLocalGatewayVirtualInterfaces'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters.
  , localGatewayVirtualInterfaceIds :: Core.Maybe [Types.LocalGatewayVirtualInterfaceId]
    -- ^ The IDs of the virtual interfaces.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLocalGatewayVirtualInterfaces' value with any optional fields omitted.
mkDescribeLocalGatewayVirtualInterfaces
    :: DescribeLocalGatewayVirtualInterfaces
mkDescribeLocalGatewayVirtualInterfaces
  = DescribeLocalGatewayVirtualInterfaces'{dryRun = Core.Nothing,
                                           filters = Core.Nothing,
                                           localGatewayVirtualInterfaceIds = Core.Nothing,
                                           maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgviDryRun :: Lens.Lens' DescribeLocalGatewayVirtualInterfaces (Core.Maybe Core.Bool)
dlgviDryRun = Lens.field @"dryRun"
{-# INLINEABLE dlgviDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | One or more filters.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgviFilters :: Lens.Lens' DescribeLocalGatewayVirtualInterfaces (Core.Maybe [Types.Filter])
dlgviFilters = Lens.field @"filters"
{-# INLINEABLE dlgviFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The IDs of the virtual interfaces.
--
-- /Note:/ Consider using 'localGatewayVirtualInterfaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgviLocalGatewayVirtualInterfaceIds :: Lens.Lens' DescribeLocalGatewayVirtualInterfaces (Core.Maybe [Types.LocalGatewayVirtualInterfaceId])
dlgviLocalGatewayVirtualInterfaceIds = Lens.field @"localGatewayVirtualInterfaceIds"
{-# INLINEABLE dlgviLocalGatewayVirtualInterfaceIds #-}
{-# DEPRECATED localGatewayVirtualInterfaceIds "Use generic-lens or generic-optics with 'localGatewayVirtualInterfaceIds' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgviMaxResults :: Lens.Lens' DescribeLocalGatewayVirtualInterfaces (Core.Maybe Core.Natural)
dlgviMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dlgviMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgviNextToken :: Lens.Lens' DescribeLocalGatewayVirtualInterfaces (Core.Maybe Core.Text)
dlgviNextToken = Lens.field @"nextToken"
{-# INLINEABLE dlgviNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeLocalGatewayVirtualInterfaces where
        toQuery DescribeLocalGatewayVirtualInterfaces{..}
          = Core.toQueryPair "Action"
              ("DescribeLocalGatewayVirtualInterfaces" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryList "LocalGatewayVirtualInterfaceId")
                localGatewayVirtualInterfaceIds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeLocalGatewayVirtualInterfaces where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeLocalGatewayVirtualInterfaces
         where
        type Rs DescribeLocalGatewayVirtualInterfaces =
             DescribeLocalGatewayVirtualInterfacesResponse
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
                 DescribeLocalGatewayVirtualInterfacesResponse' Core.<$>
                   (x Core..@? "localGatewayVirtualInterfaceSet" Core..<@>
                      Core.parseXMLList "item")
                     Core.<*> x Core..@? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeLocalGatewayVirtualInterfaces where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"localGatewayVirtualInterfaces" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeLocalGatewayVirtualInterfacesResponse' smart constructor.
data DescribeLocalGatewayVirtualInterfacesResponse = DescribeLocalGatewayVirtualInterfacesResponse'
  { localGatewayVirtualInterfaces :: Core.Maybe [Types.LocalGatewayVirtualInterface]
    -- ^ Information about the virtual interfaces.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLocalGatewayVirtualInterfacesResponse' value with any optional fields omitted.
mkDescribeLocalGatewayVirtualInterfacesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeLocalGatewayVirtualInterfacesResponse
mkDescribeLocalGatewayVirtualInterfacesResponse responseStatus
  = DescribeLocalGatewayVirtualInterfacesResponse'{localGatewayVirtualInterfaces
                                                     = Core.Nothing,
                                                   nextToken = Core.Nothing, responseStatus}

-- | Information about the virtual interfaces.
--
-- /Note:/ Consider using 'localGatewayVirtualInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgvirrsLocalGatewayVirtualInterfaces :: Lens.Lens' DescribeLocalGatewayVirtualInterfacesResponse (Core.Maybe [Types.LocalGatewayVirtualInterface])
dlgvirrsLocalGatewayVirtualInterfaces = Lens.field @"localGatewayVirtualInterfaces"
{-# INLINEABLE dlgvirrsLocalGatewayVirtualInterfaces #-}
{-# DEPRECATED localGatewayVirtualInterfaces "Use generic-lens or generic-optics with 'localGatewayVirtualInterfaces' instead"  #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgvirrsNextToken :: Lens.Lens' DescribeLocalGatewayVirtualInterfacesResponse (Core.Maybe Core.Text)
dlgvirrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dlgvirrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgvirrsResponseStatus :: Lens.Lens' DescribeLocalGatewayVirtualInterfacesResponse Core.Int
dlgvirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dlgvirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
