{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ReplaceRouteTableAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the route table associated with a given subnet, internet gateway, or virtual private gateway in a VPC. After the operation completes, the subnet or gateway uses the routes in the new route table. For more information about route tables, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Route_Tables.html Route Tables> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- You can also use this operation to change which table is the main route table in the VPC. Specify the main route table's association ID and the route table ID of the new main route table.
module Network.AWS.EC2.ReplaceRouteTableAssociation
  ( -- * Creating a request
    ReplaceRouteTableAssociation (..),
    mkReplaceRouteTableAssociation,

    -- ** Request lenses
    rrtaAssociationId,
    rrtaRouteTableId,
    rrtaDryRun,

    -- * Destructuring the response
    ReplaceRouteTableAssociationResponse (..),
    mkReplaceRouteTableAssociationResponse,

    -- ** Response lenses
    rrtarrsAssociationState,
    rrtarrsNewAssociationId,
    rrtarrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkReplaceRouteTableAssociation' smart constructor.
data ReplaceRouteTableAssociation = ReplaceRouteTableAssociation'
  { -- | The association ID.
    associationId :: Types.RouteTableAssociationId,
    -- | The ID of the new route table to associate with the subnet.
    routeTableId :: Types.RouteTableId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplaceRouteTableAssociation' value with any optional fields omitted.
mkReplaceRouteTableAssociation ::
  -- | 'associationId'
  Types.RouteTableAssociationId ->
  -- | 'routeTableId'
  Types.RouteTableId ->
  ReplaceRouteTableAssociation
mkReplaceRouteTableAssociation associationId routeTableId =
  ReplaceRouteTableAssociation'
    { associationId,
      routeTableId,
      dryRun = Core.Nothing
    }

-- | The association ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrtaAssociationId :: Lens.Lens' ReplaceRouteTableAssociation Types.RouteTableAssociationId
rrtaAssociationId = Lens.field @"associationId"
{-# DEPRECATED rrtaAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The ID of the new route table to associate with the subnet.
--
-- /Note:/ Consider using 'routeTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrtaRouteTableId :: Lens.Lens' ReplaceRouteTableAssociation Types.RouteTableId
rrtaRouteTableId = Lens.field @"routeTableId"
{-# DEPRECATED rrtaRouteTableId "Use generic-lens or generic-optics with 'routeTableId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrtaDryRun :: Lens.Lens' ReplaceRouteTableAssociation (Core.Maybe Core.Bool)
rrtaDryRun = Lens.field @"dryRun"
{-# DEPRECATED rrtaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest ReplaceRouteTableAssociation where
  type
    Rs ReplaceRouteTableAssociation =
      ReplaceRouteTableAssociationResponse
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
            ( Core.pure ("Action", "ReplaceRouteTableAssociation")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "AssociationId" associationId)
                Core.<> (Core.toQueryValue "RouteTableId" routeTableId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ReplaceRouteTableAssociationResponse'
            Core.<$> (x Core..@? "associationState")
            Core.<*> (x Core..@? "newAssociationId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkReplaceRouteTableAssociationResponse' smart constructor.
data ReplaceRouteTableAssociationResponse = ReplaceRouteTableAssociationResponse'
  { -- | The state of the association.
    associationState :: Core.Maybe Types.RouteTableAssociationState,
    -- | The ID of the new association.
    newAssociationId :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplaceRouteTableAssociationResponse' value with any optional fields omitted.
mkReplaceRouteTableAssociationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ReplaceRouteTableAssociationResponse
mkReplaceRouteTableAssociationResponse responseStatus =
  ReplaceRouteTableAssociationResponse'
    { associationState =
        Core.Nothing,
      newAssociationId = Core.Nothing,
      responseStatus
    }

-- | The state of the association.
--
-- /Note:/ Consider using 'associationState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrtarrsAssociationState :: Lens.Lens' ReplaceRouteTableAssociationResponse (Core.Maybe Types.RouteTableAssociationState)
rrtarrsAssociationState = Lens.field @"associationState"
{-# DEPRECATED rrtarrsAssociationState "Use generic-lens or generic-optics with 'associationState' instead." #-}

-- | The ID of the new association.
--
-- /Note:/ Consider using 'newAssociationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrtarrsNewAssociationId :: Lens.Lens' ReplaceRouteTableAssociationResponse (Core.Maybe Types.String)
rrtarrsNewAssociationId = Lens.field @"newAssociationId"
{-# DEPRECATED rrtarrsNewAssociationId "Use generic-lens or generic-optics with 'newAssociationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrtarrsResponseStatus :: Lens.Lens' ReplaceRouteTableAssociationResponse Core.Int
rrtarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rrtarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
