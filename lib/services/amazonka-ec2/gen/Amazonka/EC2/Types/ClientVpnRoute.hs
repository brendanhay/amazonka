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
-- Module      : Amazonka.EC2.Types.ClientVpnRoute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ClientVpnRoute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ClientVpnRouteStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about a Client VPN endpoint route.
--
-- /See:/ 'newClientVpnRoute' smart constructor.
data ClientVpnRoute = ClientVpnRoute'
  { -- | The ID of the Client VPN endpoint with which the route is associated.
    clientVpnEndpointId :: Prelude.Maybe Prelude.Text,
    -- | A brief description of the route.
    description :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 address range, in CIDR notation, of the route destination.
    destinationCidr :: Prelude.Maybe Prelude.Text,
    -- | Indicates how the route was associated with the Client VPN endpoint.
    -- @associate@ indicates that the route was automatically added when the
    -- target network was associated with the Client VPN endpoint. @add-route@
    -- indicates that the route was manually added using the
    -- __CreateClientVpnRoute__ action.
    origin :: Prelude.Maybe Prelude.Text,
    -- | The current state of the route.
    status :: Prelude.Maybe ClientVpnRouteStatus,
    -- | The ID of the subnet through which traffic is routed.
    targetSubnet :: Prelude.Maybe Prelude.Text,
    -- | The route type.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'description', 'clientVpnRoute_description' - A brief description of the route.
--
-- 'destinationCidr', 'clientVpnRoute_destinationCidr' - The IPv4 address range, in CIDR notation, of the route destination.
--
-- 'origin', 'clientVpnRoute_origin' - Indicates how the route was associated with the Client VPN endpoint.
-- @associate@ indicates that the route was automatically added when the
-- target network was associated with the Client VPN endpoint. @add-route@
-- indicates that the route was manually added using the
-- __CreateClientVpnRoute__ action.
--
-- 'status', 'clientVpnRoute_status' - The current state of the route.
--
-- 'targetSubnet', 'clientVpnRoute_targetSubnet' - The ID of the subnet through which traffic is routed.
--
-- 'type'', 'clientVpnRoute_type' - The route type.
newClientVpnRoute ::
  ClientVpnRoute
newClientVpnRoute =
  ClientVpnRoute'
    { clientVpnEndpointId =
        Prelude.Nothing,
      description = Prelude.Nothing,
      destinationCidr = Prelude.Nothing,
      origin = Prelude.Nothing,
      status = Prelude.Nothing,
      targetSubnet = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The ID of the Client VPN endpoint with which the route is associated.
clientVpnRoute_clientVpnEndpointId :: Lens.Lens' ClientVpnRoute (Prelude.Maybe Prelude.Text)
clientVpnRoute_clientVpnEndpointId = Lens.lens (\ClientVpnRoute' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@ClientVpnRoute' {} a -> s {clientVpnEndpointId = a} :: ClientVpnRoute)

-- | A brief description of the route.
clientVpnRoute_description :: Lens.Lens' ClientVpnRoute (Prelude.Maybe Prelude.Text)
clientVpnRoute_description = Lens.lens (\ClientVpnRoute' {description} -> description) (\s@ClientVpnRoute' {} a -> s {description = a} :: ClientVpnRoute)

-- | The IPv4 address range, in CIDR notation, of the route destination.
clientVpnRoute_destinationCidr :: Lens.Lens' ClientVpnRoute (Prelude.Maybe Prelude.Text)
clientVpnRoute_destinationCidr = Lens.lens (\ClientVpnRoute' {destinationCidr} -> destinationCidr) (\s@ClientVpnRoute' {} a -> s {destinationCidr = a} :: ClientVpnRoute)

-- | Indicates how the route was associated with the Client VPN endpoint.
-- @associate@ indicates that the route was automatically added when the
-- target network was associated with the Client VPN endpoint. @add-route@
-- indicates that the route was manually added using the
-- __CreateClientVpnRoute__ action.
clientVpnRoute_origin :: Lens.Lens' ClientVpnRoute (Prelude.Maybe Prelude.Text)
clientVpnRoute_origin = Lens.lens (\ClientVpnRoute' {origin} -> origin) (\s@ClientVpnRoute' {} a -> s {origin = a} :: ClientVpnRoute)

-- | The current state of the route.
clientVpnRoute_status :: Lens.Lens' ClientVpnRoute (Prelude.Maybe ClientVpnRouteStatus)
clientVpnRoute_status = Lens.lens (\ClientVpnRoute' {status} -> status) (\s@ClientVpnRoute' {} a -> s {status = a} :: ClientVpnRoute)

-- | The ID of the subnet through which traffic is routed.
clientVpnRoute_targetSubnet :: Lens.Lens' ClientVpnRoute (Prelude.Maybe Prelude.Text)
clientVpnRoute_targetSubnet = Lens.lens (\ClientVpnRoute' {targetSubnet} -> targetSubnet) (\s@ClientVpnRoute' {} a -> s {targetSubnet = a} :: ClientVpnRoute)

-- | The route type.
clientVpnRoute_type :: Lens.Lens' ClientVpnRoute (Prelude.Maybe Prelude.Text)
clientVpnRoute_type = Lens.lens (\ClientVpnRoute' {type'} -> type') (\s@ClientVpnRoute' {} a -> s {type' = a} :: ClientVpnRoute)

instance Data.FromXML ClientVpnRoute where
  parseXML x =
    ClientVpnRoute'
      Prelude.<$> (x Data..@? "clientVpnEndpointId")
      Prelude.<*> (x Data..@? "description")
      Prelude.<*> (x Data..@? "destinationCidr")
      Prelude.<*> (x Data..@? "origin")
      Prelude.<*> (x Data..@? "status")
      Prelude.<*> (x Data..@? "targetSubnet")
      Prelude.<*> (x Data..@? "type")

instance Prelude.Hashable ClientVpnRoute where
  hashWithSalt _salt ClientVpnRoute' {..} =
    _salt
      `Prelude.hashWithSalt` clientVpnEndpointId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` destinationCidr
      `Prelude.hashWithSalt` origin
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` targetSubnet
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ClientVpnRoute where
  rnf ClientVpnRoute' {..} =
    Prelude.rnf clientVpnEndpointId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf destinationCidr
      `Prelude.seq` Prelude.rnf origin
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf targetSubnet
      `Prelude.seq` Prelude.rnf type'
