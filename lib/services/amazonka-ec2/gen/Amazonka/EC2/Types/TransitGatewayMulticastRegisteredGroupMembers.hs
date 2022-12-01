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
-- Module      : Amazonka.EC2.Types.TransitGatewayMulticastRegisteredGroupMembers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayMulticastRegisteredGroupMembers where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the registered transit gateway multicast group members.
--
-- /See:/ 'newTransitGatewayMulticastRegisteredGroupMembers' smart constructor.
data TransitGatewayMulticastRegisteredGroupMembers = TransitGatewayMulticastRegisteredGroupMembers'
  { -- | The IP address assigned to the transit gateway multicast group.
    groupIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The ID of the registered network interfaces.
    registeredNetworkInterfaceIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayMulticastRegisteredGroupMembers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupIpAddress', 'transitGatewayMulticastRegisteredGroupMembers_groupIpAddress' - The IP address assigned to the transit gateway multicast group.
--
-- 'registeredNetworkInterfaceIds', 'transitGatewayMulticastRegisteredGroupMembers_registeredNetworkInterfaceIds' - The ID of the registered network interfaces.
--
-- 'transitGatewayMulticastDomainId', 'transitGatewayMulticastRegisteredGroupMembers_transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
newTransitGatewayMulticastRegisteredGroupMembers ::
  TransitGatewayMulticastRegisteredGroupMembers
newTransitGatewayMulticastRegisteredGroupMembers =
  TransitGatewayMulticastRegisteredGroupMembers'
    { groupIpAddress =
        Prelude.Nothing,
      registeredNetworkInterfaceIds =
        Prelude.Nothing,
      transitGatewayMulticastDomainId =
        Prelude.Nothing
    }

-- | The IP address assigned to the transit gateway multicast group.
transitGatewayMulticastRegisteredGroupMembers_groupIpAddress :: Lens.Lens' TransitGatewayMulticastRegisteredGroupMembers (Prelude.Maybe Prelude.Text)
transitGatewayMulticastRegisteredGroupMembers_groupIpAddress = Lens.lens (\TransitGatewayMulticastRegisteredGroupMembers' {groupIpAddress} -> groupIpAddress) (\s@TransitGatewayMulticastRegisteredGroupMembers' {} a -> s {groupIpAddress = a} :: TransitGatewayMulticastRegisteredGroupMembers)

-- | The ID of the registered network interfaces.
transitGatewayMulticastRegisteredGroupMembers_registeredNetworkInterfaceIds :: Lens.Lens' TransitGatewayMulticastRegisteredGroupMembers (Prelude.Maybe [Prelude.Text])
transitGatewayMulticastRegisteredGroupMembers_registeredNetworkInterfaceIds = Lens.lens (\TransitGatewayMulticastRegisteredGroupMembers' {registeredNetworkInterfaceIds} -> registeredNetworkInterfaceIds) (\s@TransitGatewayMulticastRegisteredGroupMembers' {} a -> s {registeredNetworkInterfaceIds = a} :: TransitGatewayMulticastRegisteredGroupMembers) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the transit gateway multicast domain.
transitGatewayMulticastRegisteredGroupMembers_transitGatewayMulticastDomainId :: Lens.Lens' TransitGatewayMulticastRegisteredGroupMembers (Prelude.Maybe Prelude.Text)
transitGatewayMulticastRegisteredGroupMembers_transitGatewayMulticastDomainId = Lens.lens (\TransitGatewayMulticastRegisteredGroupMembers' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@TransitGatewayMulticastRegisteredGroupMembers' {} a -> s {transitGatewayMulticastDomainId = a} :: TransitGatewayMulticastRegisteredGroupMembers)

instance
  Core.FromXML
    TransitGatewayMulticastRegisteredGroupMembers
  where
  parseXML x =
    TransitGatewayMulticastRegisteredGroupMembers'
      Prelude.<$> (x Core..@? "groupIpAddress")
        Prelude.<*> ( x Core..@? "registeredNetworkInterfaceIds"
                        Core..!@ Prelude.mempty
                        Prelude.>>= Core.may (Core.parseXMLList "item")
                    )
        Prelude.<*> (x Core..@? "transitGatewayMulticastDomainId")

instance
  Prelude.Hashable
    TransitGatewayMulticastRegisteredGroupMembers
  where
  hashWithSalt
    _salt
    TransitGatewayMulticastRegisteredGroupMembers' {..} =
      _salt `Prelude.hashWithSalt` groupIpAddress
        `Prelude.hashWithSalt` registeredNetworkInterfaceIds
        `Prelude.hashWithSalt` transitGatewayMulticastDomainId

instance
  Prelude.NFData
    TransitGatewayMulticastRegisteredGroupMembers
  where
  rnf
    TransitGatewayMulticastRegisteredGroupMembers' {..} =
      Prelude.rnf groupIpAddress
        `Prelude.seq` Prelude.rnf registeredNetworkInterfaceIds
        `Prelude.seq` Prelude.rnf transitGatewayMulticastDomainId
