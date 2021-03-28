{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteRouteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified route table. You must disassociate the route table from any subnets before you can delete it. You can't delete the main route table.
module Network.AWS.EC2.DeleteRouteTable
    (
    -- * Creating a request
      DeleteRouteTable (..)
    , mkDeleteRouteTable
    -- ** Request lenses
    , drtfRouteTableId
    , drtfDryRun

    -- * Destructuring the response
    , DeleteRouteTableResponse (..)
    , mkDeleteRouteTableResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteRouteTable' smart constructor.
data DeleteRouteTable = DeleteRouteTable'
  { routeTableId :: Types.RouteTableId
    -- ^ The ID of the route table.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRouteTable' value with any optional fields omitted.
mkDeleteRouteTable
    :: Types.RouteTableId -- ^ 'routeTableId'
    -> DeleteRouteTable
mkDeleteRouteTable routeTableId
  = DeleteRouteTable'{routeTableId, dryRun = Core.Nothing}

-- | The ID of the route table.
--
-- /Note:/ Consider using 'routeTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtfRouteTableId :: Lens.Lens' DeleteRouteTable Types.RouteTableId
drtfRouteTableId = Lens.field @"routeTableId"
{-# INLINEABLE drtfRouteTableId #-}
{-# DEPRECATED routeTableId "Use generic-lens or generic-optics with 'routeTableId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtfDryRun :: Lens.Lens' DeleteRouteTable (Core.Maybe Core.Bool)
drtfDryRun = Lens.field @"dryRun"
{-# INLINEABLE drtfDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteRouteTable where
        toQuery DeleteRouteTable{..}
          = Core.toQueryPair "Action" ("DeleteRouteTable" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "RouteTableId" routeTableId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteRouteTable where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteRouteTable where
        type Rs DeleteRouteTable = DeleteRouteTableResponse
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
        parseResponse = Response.receiveNull DeleteRouteTableResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteRouteTableResponse' smart constructor.
data DeleteRouteTableResponse = DeleteRouteTableResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRouteTableResponse' value with any optional fields omitted.
mkDeleteRouteTableResponse
    :: DeleteRouteTableResponse
mkDeleteRouteTableResponse = DeleteRouteTableResponse'
