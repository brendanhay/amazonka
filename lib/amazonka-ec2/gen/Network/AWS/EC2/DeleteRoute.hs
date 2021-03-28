{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified route from the specified route table.
module Network.AWS.EC2.DeleteRoute
    (
    -- * Creating a request
      DeleteRoute (..)
    , mkDeleteRoute
    -- ** Request lenses
    , drRouteTableId
    , drDestinationCidrBlock
    , drDestinationIpv6CidrBlock
    , drDestinationPrefixListId
    , drDryRun

    -- * Destructuring the response
    , DeleteRouteResponse (..)
    , mkDeleteRouteResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteRoute' smart constructor.
data DeleteRoute = DeleteRoute'
  { routeTableId :: Types.RouteTableId
    -- ^ The ID of the route table.
  , destinationCidrBlock :: Core.Maybe Core.Text
    -- ^ The IPv4 CIDR range for the route. The value you specify must match the CIDR for the route exactly.
  , destinationIpv6CidrBlock :: Core.Maybe Core.Text
    -- ^ The IPv6 CIDR range for the route. The value you specify must match the CIDR for the route exactly.
  , destinationPrefixListId :: Core.Maybe Types.PrefixListResourceId
    -- ^ The ID of the prefix list for the route.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRoute' value with any optional fields omitted.
mkDeleteRoute
    :: Types.RouteTableId -- ^ 'routeTableId'
    -> DeleteRoute
mkDeleteRoute routeTableId
  = DeleteRoute'{routeTableId, destinationCidrBlock = Core.Nothing,
                 destinationIpv6CidrBlock = Core.Nothing,
                 destinationPrefixListId = Core.Nothing, dryRun = Core.Nothing}

-- | The ID of the route table.
--
-- /Note:/ Consider using 'routeTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drRouteTableId :: Lens.Lens' DeleteRoute Types.RouteTableId
drRouteTableId = Lens.field @"routeTableId"
{-# INLINEABLE drRouteTableId #-}
{-# DEPRECATED routeTableId "Use generic-lens or generic-optics with 'routeTableId' instead"  #-}

-- | The IPv4 CIDR range for the route. The value you specify must match the CIDR for the route exactly.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drDestinationCidrBlock :: Lens.Lens' DeleteRoute (Core.Maybe Core.Text)
drDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# INLINEABLE drDestinationCidrBlock #-}
{-# DEPRECATED destinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead"  #-}

-- | The IPv6 CIDR range for the route. The value you specify must match the CIDR for the route exactly.
--
-- /Note:/ Consider using 'destinationIpv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drDestinationIpv6CidrBlock :: Lens.Lens' DeleteRoute (Core.Maybe Core.Text)
drDestinationIpv6CidrBlock = Lens.field @"destinationIpv6CidrBlock"
{-# INLINEABLE drDestinationIpv6CidrBlock #-}
{-# DEPRECATED destinationIpv6CidrBlock "Use generic-lens or generic-optics with 'destinationIpv6CidrBlock' instead"  #-}

-- | The ID of the prefix list for the route.
--
-- /Note:/ Consider using 'destinationPrefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drDestinationPrefixListId :: Lens.Lens' DeleteRoute (Core.Maybe Types.PrefixListResourceId)
drDestinationPrefixListId = Lens.field @"destinationPrefixListId"
{-# INLINEABLE drDestinationPrefixListId #-}
{-# DEPRECATED destinationPrefixListId "Use generic-lens or generic-optics with 'destinationPrefixListId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drDryRun :: Lens.Lens' DeleteRoute (Core.Maybe Core.Bool)
drDryRun = Lens.field @"dryRun"
{-# INLINEABLE drDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteRoute where
        toQuery DeleteRoute{..}
          = Core.toQueryPair "Action" ("DeleteRoute" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "RouteTableId" routeTableId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DestinationCidrBlock")
                destinationCidrBlock
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "DestinationIpv6CidrBlock")
                destinationIpv6CidrBlock
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DestinationPrefixListId")
                destinationPrefixListId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteRoute where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteRoute where
        type Rs DeleteRoute = DeleteRouteResponse
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
        parseResponse = Response.receiveNull DeleteRouteResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteRouteResponse' smart constructor.
data DeleteRouteResponse = DeleteRouteResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRouteResponse' value with any optional fields omitted.
mkDeleteRouteResponse
    :: DeleteRouteResponse
mkDeleteRouteResponse = DeleteRouteResponse'
