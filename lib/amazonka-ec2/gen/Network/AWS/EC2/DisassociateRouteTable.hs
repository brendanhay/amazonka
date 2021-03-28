{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisassociateRouteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a subnet or gateway from a route table.
--
-- After you perform this action, the subnet no longer uses the routes in the route table. Instead, it uses the routes in the VPC's main route table. For more information about route tables, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Route_Tables.html Route Tables> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.DisassociateRouteTable
    (
    -- * Creating a request
      DisassociateRouteTable (..)
    , mkDisassociateRouteTable
    -- ** Request lenses
    , drtAssociationId
    , drtDryRun

    -- * Destructuring the response
    , DisassociateRouteTableResponse (..)
    , mkDisassociateRouteTableResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateRouteTable' smart constructor.
data DisassociateRouteTable = DisassociateRouteTable'
  { associationId :: Types.RouteTableAssociationId
    -- ^ The association ID representing the current association between the route table and subnet or gateway.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateRouteTable' value with any optional fields omitted.
mkDisassociateRouteTable
    :: Types.RouteTableAssociationId -- ^ 'associationId'
    -> DisassociateRouteTable
mkDisassociateRouteTable associationId
  = DisassociateRouteTable'{associationId, dryRun = Core.Nothing}

-- | The association ID representing the current association between the route table and subnet or gateway.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtAssociationId :: Lens.Lens' DisassociateRouteTable Types.RouteTableAssociationId
drtAssociationId = Lens.field @"associationId"
{-# INLINEABLE drtAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtDryRun :: Lens.Lens' DisassociateRouteTable (Core.Maybe Core.Bool)
drtDryRun = Lens.field @"dryRun"
{-# INLINEABLE drtDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DisassociateRouteTable where
        toQuery DisassociateRouteTable{..}
          = Core.toQueryPair "Action" ("DisassociateRouteTable" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "AssociationId" associationId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DisassociateRouteTable where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DisassociateRouteTable where
        type Rs DisassociateRouteTable = DisassociateRouteTableResponse
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
          = Response.receiveNull DisassociateRouteTableResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateRouteTableResponse' smart constructor.
data DisassociateRouteTableResponse = DisassociateRouteTableResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateRouteTableResponse' value with any optional fields omitted.
mkDisassociateRouteTableResponse
    :: DisassociateRouteTableResponse
mkDisassociateRouteTableResponse = DisassociateRouteTableResponse'
