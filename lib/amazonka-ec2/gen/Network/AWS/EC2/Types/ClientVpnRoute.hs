{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVpnRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ClientVpnRoute
  ( ClientVpnRoute (..)
  -- * Smart constructor
  , mkClientVpnRoute
  -- * Lenses
  , cvrClientVpnEndpointId
  , cvrDescription
  , cvrDestinationCidr
  , cvrOrigin
  , cvrStatus
  , cvrTargetSubnet
  , cvrType
  ) where

import qualified Network.AWS.EC2.Types.ClientVpnRouteStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a Client VPN endpoint route.
--
-- /See:/ 'mkClientVpnRoute' smart constructor.
data ClientVpnRoute = ClientVpnRoute'
  { clientVpnEndpointId :: Core.Maybe Core.Text
    -- ^ The ID of the Client VPN endpoint with which the route is associated.
  , description :: Core.Maybe Core.Text
    -- ^ A brief description of the route.
  , destinationCidr :: Core.Maybe Core.Text
    -- ^ The IPv4 address range, in CIDR notation, of the route destination.
  , origin :: Core.Maybe Core.Text
    -- ^ Indicates how the route was associated with the Client VPN endpoint. @associate@ indicates that the route was automatically added when the target network was associated with the Client VPN endpoint. @add-route@ indicates that the route was manually added using the __CreateClientVpnRoute__ action.
  , status :: Core.Maybe Types.ClientVpnRouteStatus
    -- ^ The current state of the route.
  , targetSubnet :: Core.Maybe Core.Text
    -- ^ The ID of the subnet through which traffic is routed.
  , type' :: Core.Maybe Core.Text
    -- ^ The route type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClientVpnRoute' value with any optional fields omitted.
mkClientVpnRoute
    :: ClientVpnRoute
mkClientVpnRoute
  = ClientVpnRoute'{clientVpnEndpointId = Core.Nothing,
                    description = Core.Nothing, destinationCidr = Core.Nothing,
                    origin = Core.Nothing, status = Core.Nothing,
                    targetSubnet = Core.Nothing, type' = Core.Nothing}

-- | The ID of the Client VPN endpoint with which the route is associated.
--
-- /Note:/ Consider using 'clientVpnEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrClientVpnEndpointId :: Lens.Lens' ClientVpnRoute (Core.Maybe Core.Text)
cvrClientVpnEndpointId = Lens.field @"clientVpnEndpointId"
{-# INLINEABLE cvrClientVpnEndpointId #-}
{-# DEPRECATED clientVpnEndpointId "Use generic-lens or generic-optics with 'clientVpnEndpointId' instead"  #-}

-- | A brief description of the route.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrDescription :: Lens.Lens' ClientVpnRoute (Core.Maybe Core.Text)
cvrDescription = Lens.field @"description"
{-# INLINEABLE cvrDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The IPv4 address range, in CIDR notation, of the route destination.
--
-- /Note:/ Consider using 'destinationCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrDestinationCidr :: Lens.Lens' ClientVpnRoute (Core.Maybe Core.Text)
cvrDestinationCidr = Lens.field @"destinationCidr"
{-# INLINEABLE cvrDestinationCidr #-}
{-# DEPRECATED destinationCidr "Use generic-lens or generic-optics with 'destinationCidr' instead"  #-}

-- | Indicates how the route was associated with the Client VPN endpoint. @associate@ indicates that the route was automatically added when the target network was associated with the Client VPN endpoint. @add-route@ indicates that the route was manually added using the __CreateClientVpnRoute__ action.
--
-- /Note:/ Consider using 'origin' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrOrigin :: Lens.Lens' ClientVpnRoute (Core.Maybe Core.Text)
cvrOrigin = Lens.field @"origin"
{-# INLINEABLE cvrOrigin #-}
{-# DEPRECATED origin "Use generic-lens or generic-optics with 'origin' instead"  #-}

-- | The current state of the route.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrStatus :: Lens.Lens' ClientVpnRoute (Core.Maybe Types.ClientVpnRouteStatus)
cvrStatus = Lens.field @"status"
{-# INLINEABLE cvrStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The ID of the subnet through which traffic is routed.
--
-- /Note:/ Consider using 'targetSubnet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrTargetSubnet :: Lens.Lens' ClientVpnRoute (Core.Maybe Core.Text)
cvrTargetSubnet = Lens.field @"targetSubnet"
{-# INLINEABLE cvrTargetSubnet #-}
{-# DEPRECATED targetSubnet "Use generic-lens or generic-optics with 'targetSubnet' instead"  #-}

-- | The route type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrType :: Lens.Lens' ClientVpnRoute (Core.Maybe Core.Text)
cvrType = Lens.field @"type'"
{-# INLINEABLE cvrType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromXML ClientVpnRoute where
        parseXML x
          = ClientVpnRoute' Core.<$>
              (x Core..@? "clientVpnEndpointId") Core.<*>
                x Core..@? "description"
                Core.<*> x Core..@? "destinationCidr"
                Core.<*> x Core..@? "origin"
                Core.<*> x Core..@? "status"
                Core.<*> x Core..@? "targetSubnet"
                Core.<*> x Core..@? "type"
