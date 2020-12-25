{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateRouteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a route table for the specified VPC. After you create a route table, you can add routes and associate the table with a subnet.
--
-- For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Route_Tables.html Route Tables> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.CreateRouteTable
  ( -- * Creating a request
    CreateRouteTable (..),
    mkCreateRouteTable,

    -- ** Request lenses
    crtVpcId,
    crtDryRun,
    crtTagSpecifications,

    -- * Destructuring the response
    CreateRouteTableResponse (..),
    mkCreateRouteTableResponse,

    -- ** Response lenses
    crtrrsRouteTable,
    crtrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateRouteTable' smart constructor.
data CreateRouteTable = CreateRouteTable'
  { -- | The ID of the VPC.
    vpcId :: Types.VpcId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The tags to assign to the route table.
    tagSpecifications :: Core.Maybe [Types.TagSpecification]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRouteTable' value with any optional fields omitted.
mkCreateRouteTable ::
  -- | 'vpcId'
  Types.VpcId ->
  CreateRouteTable
mkCreateRouteTable vpcId =
  CreateRouteTable'
    { vpcId,
      dryRun = Core.Nothing,
      tagSpecifications = Core.Nothing
    }

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtVpcId :: Lens.Lens' CreateRouteTable Types.VpcId
crtVpcId = Lens.field @"vpcId"
{-# DEPRECATED crtVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtDryRun :: Lens.Lens' CreateRouteTable (Core.Maybe Core.Bool)
crtDryRun = Lens.field @"dryRun"
{-# DEPRECATED crtDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The tags to assign to the route table.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtTagSpecifications :: Lens.Lens' CreateRouteTable (Core.Maybe [Types.TagSpecification])
crtTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED crtTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

instance Core.AWSRequest CreateRouteTable where
  type Rs CreateRouteTable = CreateRouteTableResponse
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
            ( Core.pure ("Action", "CreateRouteTable")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "VpcId" vpcId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "TagSpecification" Core.<$> tagSpecifications)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateRouteTableResponse'
            Core.<$> (x Core..@? "routeTable") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateRouteTableResponse' smart constructor.
data CreateRouteTableResponse = CreateRouteTableResponse'
  { -- | Information about the route table.
    routeTable :: Core.Maybe Types.RouteTable,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRouteTableResponse' value with any optional fields omitted.
mkCreateRouteTableResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateRouteTableResponse
mkCreateRouteTableResponse responseStatus =
  CreateRouteTableResponse'
    { routeTable = Core.Nothing,
      responseStatus
    }

-- | Information about the route table.
--
-- /Note:/ Consider using 'routeTable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtrrsRouteTable :: Lens.Lens' CreateRouteTableResponse (Core.Maybe Types.RouteTable)
crtrrsRouteTable = Lens.field @"routeTable"
{-# DEPRECATED crtrrsRouteTable "Use generic-lens or generic-optics with 'routeTable' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtrrsResponseStatus :: Lens.Lens' CreateRouteTableResponse Core.Int
crtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
