{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribePrincipalIdFormat (..),
    mkDescribePrincipalIdFormat,

    -- ** Request lenses
    dpifDryRun,
    dpifMaxResults,
    dpifNextToken,
    dpifResources,

    -- * Destructuring the response
    DescribePrincipalIdFormatResponse (..),
    mkDescribePrincipalIdFormatResponse,

    -- ** Response lenses
    dpifrrsNextToken,
    dpifrrsPrincipals,
    dpifrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribePrincipalIdFormat' smart constructor.
data DescribePrincipalIdFormat = DescribePrincipalIdFormat'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned NextToken value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token to request the next page of results.
    nextToken :: Core.Maybe Types.String,
    -- | The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @reservation@ | @route-table@ | @route-table-association@ | @security-group@ | @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@
    resources :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePrincipalIdFormat' value with any optional fields omitted.
mkDescribePrincipalIdFormat ::
  DescribePrincipalIdFormat
mkDescribePrincipalIdFormat =
  DescribePrincipalIdFormat'
    { dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      resources = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpifDryRun :: Lens.Lens' DescribePrincipalIdFormat (Core.Maybe Core.Bool)
dpifDryRun = Lens.field @"dryRun"
{-# DEPRECATED dpifDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned NextToken value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpifMaxResults :: Lens.Lens' DescribePrincipalIdFormat (Core.Maybe Core.Natural)
dpifMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dpifMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpifNextToken :: Lens.Lens' DescribePrincipalIdFormat (Core.Maybe Types.String)
dpifNextToken = Lens.field @"nextToken"
{-# DEPRECATED dpifNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @reservation@ | @route-table@ | @route-table-association@ | @security-group@ | @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpifResources :: Lens.Lens' DescribePrincipalIdFormat (Core.Maybe [Types.String])
dpifResources = Lens.field @"resources"
{-# DEPRECATED dpifResources "Use generic-lens or generic-optics with 'resources' instead." #-}

instance Core.AWSRequest DescribePrincipalIdFormat where
  type
    Rs DescribePrincipalIdFormat =
      DescribePrincipalIdFormatResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribePrincipalIdFormat")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> (Core.toQueryList "Resource" Core.<$> resources)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribePrincipalIdFormatResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> (x Core..@? "principalSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribePrincipalIdFormat where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"principals" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribePrincipalIdFormatResponse' smart constructor.
data DescribePrincipalIdFormatResponse = DescribePrincipalIdFormatResponse'
  { -- | The token to use to retrieve the next page of results. This value is null when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | Information about the ID format settings for the ARN.
    principals :: Core.Maybe [Types.PrincipalIdFormat],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribePrincipalIdFormatResponse' value with any optional fields omitted.
mkDescribePrincipalIdFormatResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribePrincipalIdFormatResponse
mkDescribePrincipalIdFormatResponse responseStatus =
  DescribePrincipalIdFormatResponse'
    { nextToken = Core.Nothing,
      principals = Core.Nothing,
      responseStatus
    }

-- | The token to use to retrieve the next page of results. This value is null when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpifrrsNextToken :: Lens.Lens' DescribePrincipalIdFormatResponse (Core.Maybe Types.String)
dpifrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dpifrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the ID format settings for the ARN.
--
-- /Note:/ Consider using 'principals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpifrrsPrincipals :: Lens.Lens' DescribePrincipalIdFormatResponse (Core.Maybe [Types.PrincipalIdFormat])
dpifrrsPrincipals = Lens.field @"principals"
{-# DEPRECATED dpifrrsPrincipals "Use generic-lens or generic-optics with 'principals' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpifrrsResponseStatus :: Lens.Lens' DescribePrincipalIdFormatResponse Core.Int
dpifrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dpifrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
