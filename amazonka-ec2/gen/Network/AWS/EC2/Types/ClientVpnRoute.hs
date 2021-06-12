{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVpnRoute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVpnRoute where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ClientVpnRouteStatus
import qualified Network.AWS.Lens as Lens

-- | Information about a Client VPN endpoint route.
--
-- /See:/ 'newClientVpnRoute' smart constructor.
data ClientVpnRoute = ClientVpnRoute'
  { -- | The ID of the Client VPN endpoint with which the route is associated.
    clientVpnEndpointId :: Core.Maybe Core.Text,
    -- | The current state of the route.
    status :: Core.Maybe ClientVpnRouteStatus,
    -- | Indicates how the route was associated with the Client VPN endpoint.
    -- @associate@ indicates that the route was automatically added when the
    -- target network was associated with the Client VPN endpoint. @add-route@
    -- indicates that the route was manually added using the
    -- __CreateClientVpnRoute__ action.
    origin :: Core.Maybe Core.Text,
    -- | The IPv4 address range, in CIDR notation, of the route destination.
    destinationCidr :: Core.Maybe Core.Text,
    -- | A brief description of the route.
    description :: Core.Maybe Core.Text,
    -- | The route type.
    type' :: Core.Maybe Core.Text,
    -- | The ID of the subnet through which traffic is routed.
    targetSubnet :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ClientVpnRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientVpnEndpointId', 'clientVpnRoute_clientVpnEndpointId' - The ID of the Client VPN endpoint with which the route is associated.
--
-- 'status', 'clientVpnRoute_status' - The current state of the route.
--
-- 'origin', 'clientVpnRoute_origin' - Indicates how the route was associated with the Client VPN endpoint.
-- @associate@ indicates that the route was automatically added when the
-- target network was associated with the Client VPN endpoint. @add-route@
-- indicates that the route was manually added using the
-- __CreateClientVpnRoute__ action.
--
-- 'destinationCidr', 'clientVpnRoute_destinationCidr' - The IPv4 address range, in CIDR notation, of the route destination.
--
-- 'description', 'clientVpnRoute_description' - A brief description of the route.
--
-- 'type'', 'clientVpnRoute_type' - The route type.
--
-- 'targetSubnet', 'clientVpnRoute_targetSubnet' - The ID of the subnet through which traffic is routed.
newClientVpnRoute ::
  ClientVpnRoute
newClientVpnRoute =
  ClientVpnRoute'
    { clientVpnEndpointId = Core.Nothing,
      status = Core.Nothing,
      origin = Core.Nothing,
      destinationCidr = Core.Nothing,
      description = Core.Nothing,
      type' = Core.Nothing,
      targetSubnet = Core.Nothing
    }

-- | The ID of the Client VPN endpoint with which the route is associated.
clientVpnRoute_clientVpnEndpointId :: Lens.Lens' ClientVpnRoute (Core.Maybe Core.Text)
clientVpnRoute_clientVpnEndpointId = Lens.lens (\ClientVpnRoute' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@ClientVpnRoute' {} a -> s {clientVpnEndpointId = a} :: ClientVpnRoute)

-- | The current state of the route.
clientVpnRoute_status :: Lens.Lens' ClientVpnRoute (Core.Maybe ClientVpnRouteStatus)
clientVpnRoute_status = Lens.lens (\ClientVpnRoute' {status} -> status) (\s@ClientVpnRoute' {} a -> s {status = a} :: ClientVpnRoute)

-- | Indicates how the route was associated with the Client VPN endpoint.
-- @associate@ indicates that the route was automatically added when the
-- target network was associated with the Client VPN endpoint. @add-route@
-- indicates that the route was manually added using the
-- __CreateClientVpnRoute__ action.
clientVpnRoute_origin :: Lens.Lens' ClientVpnRoute (Core.Maybe Core.Text)
clientVpnRoute_origin = Lens.lens (\ClientVpnRoute' {origin} -> origin) (\s@ClientVpnRoute' {} a -> s {origin = a} :: ClientVpnRoute)

-- | The IPv4 address range, in CIDR notation, of the route destination.
clientVpnRoute_destinationCidr :: Lens.Lens' ClientVpnRoute (Core.Maybe Core.Text)
clientVpnRoute_destinationCidr = Lens.lens (\ClientVpnRoute' {destinationCidr} -> destinationCidr) (\s@ClientVpnRoute' {} a -> s {destinationCidr = a} :: ClientVpnRoute)

-- | A brief description of the route.
clientVpnRoute_description :: Lens.Lens' ClientVpnRoute (Core.Maybe Core.Text)
clientVpnRoute_description = Lens.lens (\ClientVpnRoute' {description} -> description) (\s@ClientVpnRoute' {} a -> s {description = a} :: ClientVpnRoute)

-- | The route type.
clientVpnRoute_type :: Lens.Lens' ClientVpnRoute (Core.Maybe Core.Text)
clientVpnRoute_type = Lens.lens (\ClientVpnRoute' {type'} -> type') (\s@ClientVpnRoute' {} a -> s {type' = a} :: ClientVpnRoute)

-- | The ID of the subnet through which traffic is routed.
clientVpnRoute_targetSubnet :: Lens.Lens' ClientVpnRoute (Core.Maybe Core.Text)
clientVpnRoute_targetSubnet = Lens.lens (\ClientVpnRoute' {targetSubnet} -> targetSubnet) (\s@ClientVpnRoute' {} a -> s {targetSubnet = a} :: ClientVpnRoute)

instance Core.FromXML ClientVpnRoute where
  parseXML x =
    ClientVpnRoute'
      Core.<$> (x Core..@? "clientVpnEndpointId")
      Core.<*> (x Core..@? "status")
      Core.<*> (x Core..@? "origin")
      Core.<*> (x Core..@? "destinationCidr")
      Core.<*> (x Core..@? "description")
      Core.<*> (x Core..@? "type")
      Core.<*> (x Core..@? "targetSubnet")

instance Core.Hashable ClientVpnRoute

instance Core.NFData ClientVpnRoute
