{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribePrincipalIdFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the ID format settings for the root user and all IAM roles and IAM users that have explicitly specified a longer ID (17-character ID) preference. 
--
-- By default, all IAM roles and IAM users default to the same ID settings as the root user, unless they explicitly override the settings. This request is useful for identifying those IAM users and IAM roles that have overridden the default ID settings.
-- The following resource types support longer IDs: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @reservation@ | @route-table@ | @route-table-association@ | @security-group@ | @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ . 
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribePrincipalIdFormat
    (
    -- * Creating a request
      DescribePrincipalIdFormat (..)
    , mkDescribePrincipalIdFormat
    -- ** Request lenses
    , dpifDryRun
    , dpifMaxResults
    , dpifNextToken
    , dpifResources

    -- * Destructuring the response
    , DescribePrincipalIdFormatResponse (..)
    , mkDescribePrincipalIdFormatResponse
    -- ** Response lenses
    , dpifrrsNextToken
    , dpifrrsPrincipals
    , dpifrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribePrincipalIdFormat' smart constructor.
data DescribePrincipalIdFormat = DescribePrincipalIdFormat'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned NextToken value. 
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to request the next page of results.
  , resources :: Core.Maybe [Core.Text]
    -- ^ The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @reservation@ | @route-table@ | @route-table-association@ | @security-group@ | @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePrincipalIdFormat' value with any optional fields omitted.
mkDescribePrincipalIdFormat
    :: DescribePrincipalIdFormat
mkDescribePrincipalIdFormat
  = DescribePrincipalIdFormat'{dryRun = Core.Nothing,
                               maxResults = Core.Nothing, nextToken = Core.Nothing,
                               resources = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpifDryRun :: Lens.Lens' DescribePrincipalIdFormat (Core.Maybe Core.Bool)
dpifDryRun = Lens.field @"dryRun"
{-# INLINEABLE dpifDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned NextToken value. 
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpifMaxResults :: Lens.Lens' DescribePrincipalIdFormat (Core.Maybe Core.Natural)
dpifMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dpifMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpifNextToken :: Lens.Lens' DescribePrincipalIdFormat (Core.Maybe Core.Text)
dpifNextToken = Lens.field @"nextToken"
{-# INLINEABLE dpifNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @reservation@ | @route-table@ | @route-table-association@ | @security-group@ | @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ 
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpifResources :: Lens.Lens' DescribePrincipalIdFormat (Core.Maybe [Core.Text])
dpifResources = Lens.field @"resources"
{-# INLINEABLE dpifResources #-}
{-# DEPRECATED resources "Use generic-lens or generic-optics with 'resources' instead"  #-}

instance Core.ToQuery DescribePrincipalIdFormat where
        toQuery DescribePrincipalIdFormat{..}
          = Core.toQueryPair "Action"
              ("DescribePrincipalIdFormat" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "Resource") resources

instance Core.ToHeaders DescribePrincipalIdFormat where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribePrincipalIdFormat where
        type Rs DescribePrincipalIdFormat =
             DescribePrincipalIdFormatResponse
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
                 DescribePrincipalIdFormatResponse' Core.<$>
                   (x Core..@? "nextToken") Core.<*>
                     x Core..@? "principalSet" Core..<@> Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribePrincipalIdFormat where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"principals" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribePrincipalIdFormatResponse' smart constructor.
data DescribePrincipalIdFormatResponse = DescribePrincipalIdFormatResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is null when there are no more results to return.
  , principals :: Core.Maybe [Types.PrincipalIdFormat]
    -- ^ Information about the ID format settings for the ARN.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribePrincipalIdFormatResponse' value with any optional fields omitted.
mkDescribePrincipalIdFormatResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribePrincipalIdFormatResponse
mkDescribePrincipalIdFormatResponse responseStatus
  = DescribePrincipalIdFormatResponse'{nextToken = Core.Nothing,
                                       principals = Core.Nothing, responseStatus}

-- | The token to use to retrieve the next page of results. This value is null when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpifrrsNextToken :: Lens.Lens' DescribePrincipalIdFormatResponse (Core.Maybe Core.Text)
dpifrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dpifrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the ID format settings for the ARN.
--
-- /Note:/ Consider using 'principals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpifrrsPrincipals :: Lens.Lens' DescribePrincipalIdFormatResponse (Core.Maybe [Types.PrincipalIdFormat])
dpifrrsPrincipals = Lens.field @"principals"
{-# INLINEABLE dpifrrsPrincipals #-}
{-# DEPRECATED principals "Use generic-lens or generic-optics with 'principals' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpifrrsResponseStatus :: Lens.Lens' DescribePrincipalIdFormatResponse Core.Int
dpifrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dpifrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
