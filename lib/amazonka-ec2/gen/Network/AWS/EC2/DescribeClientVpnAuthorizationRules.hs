{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeClientVpnAuthorizationRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the authorization rules for a specified Client VPN endpoint.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeClientVpnAuthorizationRules
    (
    -- * Creating a request
      DescribeClientVpnAuthorizationRules (..)
    , mkDescribeClientVpnAuthorizationRules
    -- ** Request lenses
    , dcvarClientVpnEndpointId
    , dcvarDryRun
    , dcvarFilters
    , dcvarMaxResults
    , dcvarNextToken

    -- * Destructuring the response
    , DescribeClientVpnAuthorizationRulesResponse (..)
    , mkDescribeClientVpnAuthorizationRulesResponse
    -- ** Response lenses
    , dcvarrrsAuthorizationRules
    , dcvarrrsNextToken
    , dcvarrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeClientVpnAuthorizationRules' smart constructor.
data DescribeClientVpnAuthorizationRules = DescribeClientVpnAuthorizationRules'
  { clientVpnEndpointId :: Types.ClientVpnEndpointId
    -- ^ The ID of the Client VPN endpoint.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters. Filter names and values are case-sensitive.
--
--
--     * @description@ - The description of the authorization rule.
--
--
--     * @destination-cidr@ - The CIDR of the network to which the authorization rule applies.
--
--
--     * @group-id@ - The ID of the Active Directory group to which the authorization rule grants access.
--
--
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to retrieve the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClientVpnAuthorizationRules' value with any optional fields omitted.
mkDescribeClientVpnAuthorizationRules
    :: Types.ClientVpnEndpointId -- ^ 'clientVpnEndpointId'
    -> DescribeClientVpnAuthorizationRules
mkDescribeClientVpnAuthorizationRules clientVpnEndpointId
  = DescribeClientVpnAuthorizationRules'{clientVpnEndpointId,
                                         dryRun = Core.Nothing, filters = Core.Nothing,
                                         maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The ID of the Client VPN endpoint.
--
-- /Note:/ Consider using 'clientVpnEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvarClientVpnEndpointId :: Lens.Lens' DescribeClientVpnAuthorizationRules Types.ClientVpnEndpointId
dcvarClientVpnEndpointId = Lens.field @"clientVpnEndpointId"
{-# INLINEABLE dcvarClientVpnEndpointId #-}
{-# DEPRECATED clientVpnEndpointId "Use generic-lens or generic-optics with 'clientVpnEndpointId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvarDryRun :: Lens.Lens' DescribeClientVpnAuthorizationRules (Core.Maybe Core.Bool)
dcvarDryRun = Lens.field @"dryRun"
{-# INLINEABLE dcvarDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | One or more filters. Filter names and values are case-sensitive.
--
--
--     * @description@ - The description of the authorization rule.
--
--
--     * @destination-cidr@ - The CIDR of the network to which the authorization rule applies.
--
--
--     * @group-id@ - The ID of the Active Directory group to which the authorization rule grants access.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvarFilters :: Lens.Lens' DescribeClientVpnAuthorizationRules (Core.Maybe [Types.Filter])
dcvarFilters = Lens.field @"filters"
{-# INLINEABLE dcvarFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvarMaxResults :: Lens.Lens' DescribeClientVpnAuthorizationRules (Core.Maybe Core.Natural)
dcvarMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dcvarMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvarNextToken :: Lens.Lens' DescribeClientVpnAuthorizationRules (Core.Maybe Types.NextToken)
dcvarNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcvarNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeClientVpnAuthorizationRules where
        toQuery DescribeClientVpnAuthorizationRules{..}
          = Core.toQueryPair "Action"
              ("DescribeClientVpnAuthorizationRules" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ClientVpnEndpointId" clientVpnEndpointId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeClientVpnAuthorizationRules where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeClientVpnAuthorizationRules where
        type Rs DescribeClientVpnAuthorizationRules =
             DescribeClientVpnAuthorizationRulesResponse
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
                 DescribeClientVpnAuthorizationRulesResponse' Core.<$>
                   (x Core..@? "authorizationRule" Core..<@> Core.parseXMLList "item")
                     Core.<*> x Core..@? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeClientVpnAuthorizationRules where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"authorizationRules" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeClientVpnAuthorizationRulesResponse' smart constructor.
data DescribeClientVpnAuthorizationRulesResponse = DescribeClientVpnAuthorizationRulesResponse'
  { authorizationRules :: Core.Maybe [Types.AuthorizationRule]
    -- ^ Information about the authorization rules.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClientVpnAuthorizationRulesResponse' value with any optional fields omitted.
mkDescribeClientVpnAuthorizationRulesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeClientVpnAuthorizationRulesResponse
mkDescribeClientVpnAuthorizationRulesResponse responseStatus
  = DescribeClientVpnAuthorizationRulesResponse'{authorizationRules =
                                                   Core.Nothing,
                                                 nextToken = Core.Nothing, responseStatus}

-- | Information about the authorization rules.
--
-- /Note:/ Consider using 'authorizationRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvarrrsAuthorizationRules :: Lens.Lens' DescribeClientVpnAuthorizationRulesResponse (Core.Maybe [Types.AuthorizationRule])
dcvarrrsAuthorizationRules = Lens.field @"authorizationRules"
{-# INLINEABLE dcvarrrsAuthorizationRules #-}
{-# DEPRECATED authorizationRules "Use generic-lens or generic-optics with 'authorizationRules' instead"  #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvarrrsNextToken :: Lens.Lens' DescribeClientVpnAuthorizationRulesResponse (Core.Maybe Types.NextToken)
dcvarrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcvarrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvarrrsResponseStatus :: Lens.Lens' DescribeClientVpnAuthorizationRulesResponse Core.Int
dcvarrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcvarrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
