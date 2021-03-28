{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ReplaceRouteTableAssociation (..)
    , mkReplaceRouteTableAssociation
    -- ** Request lenses
    , rrtaAssociationId
    , rrtaRouteTableId
    , rrtaDryRun

    -- * Destructuring the response
    , ReplaceRouteTableAssociationResponse (..)
    , mkReplaceRouteTableAssociationResponse
    -- ** Response lenses
    , rrtarrsAssociationState
    , rrtarrsNewAssociationId
    , rrtarrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkReplaceRouteTableAssociation' smart constructor.
data ReplaceRouteTableAssociation = ReplaceRouteTableAssociation'
  { associationId :: Types.RouteTableAssociationId
    -- ^ The association ID.
  , routeTableId :: Types.RouteTableId
    -- ^ The ID of the new route table to associate with the subnet.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplaceRouteTableAssociation' value with any optional fields omitted.
mkReplaceRouteTableAssociation
    :: Types.RouteTableAssociationId -- ^ 'associationId'
    -> Types.RouteTableId -- ^ 'routeTableId'
    -> ReplaceRouteTableAssociation
mkReplaceRouteTableAssociation associationId routeTableId
  = ReplaceRouteTableAssociation'{associationId, routeTableId,
                                  dryRun = Core.Nothing}

-- | The association ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrtaAssociationId :: Lens.Lens' ReplaceRouteTableAssociation Types.RouteTableAssociationId
rrtaAssociationId = Lens.field @"associationId"
{-# INLINEABLE rrtaAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

-- | The ID of the new route table to associate with the subnet.
--
-- /Note:/ Consider using 'routeTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrtaRouteTableId :: Lens.Lens' ReplaceRouteTableAssociation Types.RouteTableId
rrtaRouteTableId = Lens.field @"routeTableId"
{-# INLINEABLE rrtaRouteTableId #-}
{-# DEPRECATED routeTableId "Use generic-lens or generic-optics with 'routeTableId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrtaDryRun :: Lens.Lens' ReplaceRouteTableAssociation (Core.Maybe Core.Bool)
rrtaDryRun = Lens.field @"dryRun"
{-# INLINEABLE rrtaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery ReplaceRouteTableAssociation where
        toQuery ReplaceRouteTableAssociation{..}
          = Core.toQueryPair "Action"
              ("ReplaceRouteTableAssociation" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "AssociationId" associationId
              Core.<> Core.toQueryPair "RouteTableId" routeTableId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders ReplaceRouteTableAssociation where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ReplaceRouteTableAssociation where
        type Rs ReplaceRouteTableAssociation =
             ReplaceRouteTableAssociationResponse
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
                 ReplaceRouteTableAssociationResponse' Core.<$>
                   (x Core..@? "associationState") Core.<*>
                     x Core..@? "newAssociationId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkReplaceRouteTableAssociationResponse' smart constructor.
data ReplaceRouteTableAssociationResponse = ReplaceRouteTableAssociationResponse'
  { associationState :: Core.Maybe Types.RouteTableAssociationState
    -- ^ The state of the association.
  , newAssociationId :: Core.Maybe Core.Text
    -- ^ The ID of the new association.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplaceRouteTableAssociationResponse' value with any optional fields omitted.
mkReplaceRouteTableAssociationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ReplaceRouteTableAssociationResponse
mkReplaceRouteTableAssociationResponse responseStatus
  = ReplaceRouteTableAssociationResponse'{associationState =
                                            Core.Nothing,
                                          newAssociationId = Core.Nothing, responseStatus}

-- | The state of the association.
--
-- /Note:/ Consider using 'associationState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrtarrsAssociationState :: Lens.Lens' ReplaceRouteTableAssociationResponse (Core.Maybe Types.RouteTableAssociationState)
rrtarrsAssociationState = Lens.field @"associationState"
{-# INLINEABLE rrtarrsAssociationState #-}
{-# DEPRECATED associationState "Use generic-lens or generic-optics with 'associationState' instead"  #-}

-- | The ID of the new association.
--
-- /Note:/ Consider using 'newAssociationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrtarrsNewAssociationId :: Lens.Lens' ReplaceRouteTableAssociationResponse (Core.Maybe Core.Text)
rrtarrsNewAssociationId = Lens.field @"newAssociationId"
{-# INLINEABLE rrtarrsNewAssociationId #-}
{-# DEPRECATED newAssociationId "Use generic-lens or generic-optics with 'newAssociationId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrtarrsResponseStatus :: Lens.Lens' ReplaceRouteTableAssociationResponse Core.Int
rrtarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rrtarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
