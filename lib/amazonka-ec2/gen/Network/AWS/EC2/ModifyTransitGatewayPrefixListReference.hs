{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyTransitGatewayPrefixListReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a reference (route) to a prefix list in a specified transit gateway route table.
module Network.AWS.EC2.ModifyTransitGatewayPrefixListReference
  ( -- * Creating a request
    ModifyTransitGatewayPrefixListReference (..),
    mkModifyTransitGatewayPrefixListReference,

    -- ** Request lenses
    mtgplrTransitGatewayRouteTableId,
    mtgplrPrefixListId,
    mtgplrBlackhole,
    mtgplrDryRun,
    mtgplrTransitGatewayAttachmentId,

    -- * Destructuring the response
    ModifyTransitGatewayPrefixListReferenceResponse (..),
    mkModifyTransitGatewayPrefixListReferenceResponse,

    -- ** Response lenses
    mtgplrrrsTransitGatewayPrefixListReference,
    mtgplrrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyTransitGatewayPrefixListReference' smart constructor.
data ModifyTransitGatewayPrefixListReference = ModifyTransitGatewayPrefixListReference'
  { -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Types.TransitGatewayRouteTableId,
    -- | The ID of the prefix list.
    prefixListId :: Types.PrefixListResourceId,
    -- | Indicates whether to drop traffic that matches this route.
    blackhole :: Core.Maybe Core.Bool,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the attachment to which traffic is routed.
    transitGatewayAttachmentId :: Core.Maybe Types.TransitGatewayAttachmentId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyTransitGatewayPrefixListReference' value with any optional fields omitted.
mkModifyTransitGatewayPrefixListReference ::
  -- | 'transitGatewayRouteTableId'
  Types.TransitGatewayRouteTableId ->
  -- | 'prefixListId'
  Types.PrefixListResourceId ->
  ModifyTransitGatewayPrefixListReference
mkModifyTransitGatewayPrefixListReference
  transitGatewayRouteTableId
  prefixListId =
    ModifyTransitGatewayPrefixListReference'
      { transitGatewayRouteTableId,
        prefixListId,
        blackhole = Core.Nothing,
        dryRun = Core.Nothing,
        transitGatewayAttachmentId = Core.Nothing
      }

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgplrTransitGatewayRouteTableId :: Lens.Lens' ModifyTransitGatewayPrefixListReference Types.TransitGatewayRouteTableId
mtgplrTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# DEPRECATED mtgplrTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

-- | The ID of the prefix list.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgplrPrefixListId :: Lens.Lens' ModifyTransitGatewayPrefixListReference Types.PrefixListResourceId
mtgplrPrefixListId = Lens.field @"prefixListId"
{-# DEPRECATED mtgplrPrefixListId "Use generic-lens or generic-optics with 'prefixListId' instead." #-}

-- | Indicates whether to drop traffic that matches this route.
--
-- /Note:/ Consider using 'blackhole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgplrBlackhole :: Lens.Lens' ModifyTransitGatewayPrefixListReference (Core.Maybe Core.Bool)
mtgplrBlackhole = Lens.field @"blackhole"
{-# DEPRECATED mtgplrBlackhole "Use generic-lens or generic-optics with 'blackhole' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgplrDryRun :: Lens.Lens' ModifyTransitGatewayPrefixListReference (Core.Maybe Core.Bool)
mtgplrDryRun = Lens.field @"dryRun"
{-# DEPRECATED mtgplrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the attachment to which traffic is routed.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgplrTransitGatewayAttachmentId :: Lens.Lens' ModifyTransitGatewayPrefixListReference (Core.Maybe Types.TransitGatewayAttachmentId)
mtgplrTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# DEPRECATED mtgplrTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

instance Core.AWSRequest ModifyTransitGatewayPrefixListReference where
  type
    Rs ModifyTransitGatewayPrefixListReference =
      ModifyTransitGatewayPrefixListReferenceResponse
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
            ( Core.pure ("Action", "ModifyTransitGatewayPrefixListReference")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> ( Core.toQueryValue
                            "TransitGatewayRouteTableId"
                            transitGatewayRouteTableId
                        )
                Core.<> (Core.toQueryValue "PrefixListId" prefixListId)
                Core.<> (Core.toQueryValue "Blackhole" Core.<$> blackhole)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> ( Core.toQueryValue "TransitGatewayAttachmentId"
                            Core.<$> transitGatewayAttachmentId
                        )
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyTransitGatewayPrefixListReferenceResponse'
            Core.<$> (x Core..@? "transitGatewayPrefixListReference")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyTransitGatewayPrefixListReferenceResponse' smart constructor.
data ModifyTransitGatewayPrefixListReferenceResponse = ModifyTransitGatewayPrefixListReferenceResponse'
  { -- | Information about the prefix list reference.
    transitGatewayPrefixListReference :: Core.Maybe Types.TransitGatewayPrefixListReference,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyTransitGatewayPrefixListReferenceResponse' value with any optional fields omitted.
mkModifyTransitGatewayPrefixListReferenceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyTransitGatewayPrefixListReferenceResponse
mkModifyTransitGatewayPrefixListReferenceResponse responseStatus =
  ModifyTransitGatewayPrefixListReferenceResponse'
    { transitGatewayPrefixListReference =
        Core.Nothing,
      responseStatus
    }

-- | Information about the prefix list reference.
--
-- /Note:/ Consider using 'transitGatewayPrefixListReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgplrrrsTransitGatewayPrefixListReference :: Lens.Lens' ModifyTransitGatewayPrefixListReferenceResponse (Core.Maybe Types.TransitGatewayPrefixListReference)
mtgplrrrsTransitGatewayPrefixListReference = Lens.field @"transitGatewayPrefixListReference"
{-# DEPRECATED mtgplrrrsTransitGatewayPrefixListReference "Use generic-lens or generic-optics with 'transitGatewayPrefixListReference' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgplrrrsResponseStatus :: Lens.Lens' ModifyTransitGatewayPrefixListReferenceResponse Core.Int
mtgplrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mtgplrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
