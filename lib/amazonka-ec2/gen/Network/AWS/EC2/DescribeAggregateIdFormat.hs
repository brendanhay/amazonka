{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeAggregateIdFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the longer ID format settings for all resource types in a specific Region. This request is useful for performing a quick audit to determine whether a specific Region is fully opted in for longer IDs (17-character IDs).
--
-- This request only returns information about resource types that support longer IDs.
-- The following resource types support longer IDs: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @reservation@ | @route-table@ | @route-table-association@ | @security-group@ | @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ .
module Network.AWS.EC2.DescribeAggregateIdFormat
  ( -- * Creating a request
    DescribeAggregateIdFormat (..),
    mkDescribeAggregateIdFormat,

    -- ** Request lenses
    daifDryRun,

    -- * Destructuring the response
    DescribeAggregateIdFormatResponse (..),
    mkDescribeAggregateIdFormatResponse,

    -- ** Response lenses
    daifrrsStatuses,
    daifrrsUseLongIdsAggregated,
    daifrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAggregateIdFormat' smart constructor.
newtype DescribeAggregateIdFormat = DescribeAggregateIdFormat'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAggregateIdFormat' value with any optional fields omitted.
mkDescribeAggregateIdFormat ::
  DescribeAggregateIdFormat
mkDescribeAggregateIdFormat =
  DescribeAggregateIdFormat' {dryRun = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daifDryRun :: Lens.Lens' DescribeAggregateIdFormat (Core.Maybe Core.Bool)
daifDryRun = Lens.field @"dryRun"
{-# DEPRECATED daifDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DescribeAggregateIdFormat where
  type
    Rs DescribeAggregateIdFormat =
      DescribeAggregateIdFormatResponse
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
            ( Core.pure ("Action", "DescribeAggregateIdFormat")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeAggregateIdFormatResponse'
            Core.<$> (x Core..@? "statusSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (x Core..@? "useLongIdsAggregated")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeAggregateIdFormatResponse' smart constructor.
data DescribeAggregateIdFormatResponse = DescribeAggregateIdFormatResponse'
  { -- | Information about each resource's ID format.
    statuses :: Core.Maybe [Types.IdFormat],
    -- | Indicates whether all resource types in the Region are configured to use longer IDs. This value is only @true@ if all users are configured to use longer IDs for all resources types in the Region.
    useLongIdsAggregated :: Core.Maybe Core.Bool,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeAggregateIdFormatResponse' value with any optional fields omitted.
mkDescribeAggregateIdFormatResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAggregateIdFormatResponse
mkDescribeAggregateIdFormatResponse responseStatus =
  DescribeAggregateIdFormatResponse'
    { statuses = Core.Nothing,
      useLongIdsAggregated = Core.Nothing,
      responseStatus
    }

-- | Information about each resource's ID format.
--
-- /Note:/ Consider using 'statuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daifrrsStatuses :: Lens.Lens' DescribeAggregateIdFormatResponse (Core.Maybe [Types.IdFormat])
daifrrsStatuses = Lens.field @"statuses"
{-# DEPRECATED daifrrsStatuses "Use generic-lens or generic-optics with 'statuses' instead." #-}

-- | Indicates whether all resource types in the Region are configured to use longer IDs. This value is only @true@ if all users are configured to use longer IDs for all resources types in the Region.
--
-- /Note:/ Consider using 'useLongIdsAggregated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daifrrsUseLongIdsAggregated :: Lens.Lens' DescribeAggregateIdFormatResponse (Core.Maybe Core.Bool)
daifrrsUseLongIdsAggregated = Lens.field @"useLongIdsAggregated"
{-# DEPRECATED daifrrsUseLongIdsAggregated "Use generic-lens or generic-optics with 'useLongIdsAggregated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daifrrsResponseStatus :: Lens.Lens' DescribeAggregateIdFormatResponse Core.Int
daifrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED daifrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
