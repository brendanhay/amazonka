{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteVpnConnectionRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified static route associated with a VPN connection between an existing virtual private gateway and a VPN customer gateway. The static route allows traffic to be routed from the virtual private gateway to the VPN customer gateway.
module Network.AWS.EC2.DeleteVpnConnectionRoute
    (
    -- * Creating a request
      DeleteVpnConnectionRoute (..)
    , mkDeleteVpnConnectionRoute
    -- ** Request lenses
    , dvcrDestinationCidrBlock
    , dvcrVpnConnectionId

    -- * Destructuring the response
    , DeleteVpnConnectionRouteResponse (..)
    , mkDeleteVpnConnectionRouteResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeleteVpnConnectionRoute.
--
-- /See:/ 'mkDeleteVpnConnectionRoute' smart constructor.
data DeleteVpnConnectionRoute = DeleteVpnConnectionRoute'
  { destinationCidrBlock :: Core.Text
    -- ^ The CIDR block associated with the local subnet of the customer network.
  , vpnConnectionId :: Types.VpnConnectionId
    -- ^ The ID of the VPN connection.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVpnConnectionRoute' value with any optional fields omitted.
mkDeleteVpnConnectionRoute
    :: Core.Text -- ^ 'destinationCidrBlock'
    -> Types.VpnConnectionId -- ^ 'vpnConnectionId'
    -> DeleteVpnConnectionRoute
mkDeleteVpnConnectionRoute destinationCidrBlock vpnConnectionId
  = DeleteVpnConnectionRoute'{destinationCidrBlock, vpnConnectionId}

-- | The CIDR block associated with the local subnet of the customer network.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcrDestinationCidrBlock :: Lens.Lens' DeleteVpnConnectionRoute Core.Text
dvcrDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# INLINEABLE dvcrDestinationCidrBlock #-}
{-# DEPRECATED destinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead"  #-}

-- | The ID of the VPN connection.
--
-- /Note:/ Consider using 'vpnConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcrVpnConnectionId :: Lens.Lens' DeleteVpnConnectionRoute Types.VpnConnectionId
dvcrVpnConnectionId = Lens.field @"vpnConnectionId"
{-# INLINEABLE dvcrVpnConnectionId #-}
{-# DEPRECATED vpnConnectionId "Use generic-lens or generic-optics with 'vpnConnectionId' instead"  #-}

instance Core.ToQuery DeleteVpnConnectionRoute where
        toQuery DeleteVpnConnectionRoute{..}
          = Core.toQueryPair "Action"
              ("DeleteVpnConnectionRoute" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "DestinationCidrBlock" destinationCidrBlock
              Core.<> Core.toQueryPair "VpnConnectionId" vpnConnectionId

instance Core.ToHeaders DeleteVpnConnectionRoute where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteVpnConnectionRoute where
        type Rs DeleteVpnConnectionRoute = DeleteVpnConnectionRouteResponse
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
          = Response.receiveNull DeleteVpnConnectionRouteResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteVpnConnectionRouteResponse' smart constructor.
data DeleteVpnConnectionRouteResponse = DeleteVpnConnectionRouteResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVpnConnectionRouteResponse' value with any optional fields omitted.
mkDeleteVpnConnectionRouteResponse
    :: DeleteVpnConnectionRouteResponse
mkDeleteVpnConnectionRouteResponse
  = DeleteVpnConnectionRouteResponse'
