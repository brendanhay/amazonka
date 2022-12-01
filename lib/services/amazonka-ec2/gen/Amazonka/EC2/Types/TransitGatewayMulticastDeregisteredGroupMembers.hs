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
-- Module      : Amazonka.EC2.Types.TransitGatewayMulticastDeregisteredGroupMembers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayMulticastDeregisteredGroupMembers where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the deregistered transit gateway multicast group members.
--
-- /See:/ 'newTransitGatewayMulticastDeregisteredGroupMembers' smart constructor.
data TransitGatewayMulticastDeregisteredGroupMembers = TransitGatewayMulticastDeregisteredGroupMembers'
  { -- | The network interface IDs of the deregistered members.
    deregisteredNetworkInterfaceIds :: Prelude.Maybe [Prelude.Text],
    -- | The IP address assigned to the transit gateway multicast group.
    groupIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayMulticastDeregisteredGroupMembers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deregisteredNetworkInterfaceIds', 'transitGatewayMulticastDeregisteredGroupMembers_deregisteredNetworkInterfaceIds' - The network interface IDs of the deregistered members.
--
-- 'groupIpAddress', 'transitGatewayMulticastDeregisteredGroupMembers_groupIpAddress' - The IP address assigned to the transit gateway multicast group.
--
-- 'transitGatewayMulticastDomainId', 'transitGatewayMulticastDeregisteredGroupMembers_transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
newTransitGatewayMulticastDeregisteredGroupMembers ::
  TransitGatewayMulticastDeregisteredGroupMembers
newTransitGatewayMulticastDeregisteredGroupMembers =
  TransitGatewayMulticastDeregisteredGroupMembers'
    { deregisteredNetworkInterfaceIds =
        Prelude.Nothing,
      groupIpAddress =
        Prelude.Nothing,
      transitGatewayMulticastDomainId =
        Prelude.Nothing
    }

-- | The network interface IDs of the deregistered members.
transitGatewayMulticastDeregisteredGroupMembers_deregisteredNetworkInterfaceIds :: Lens.Lens' TransitGatewayMulticastDeregisteredGroupMembers (Prelude.Maybe [Prelude.Text])
transitGatewayMulticastDeregisteredGroupMembers_deregisteredNetworkInterfaceIds = Lens.lens (\TransitGatewayMulticastDeregisteredGroupMembers' {deregisteredNetworkInterfaceIds} -> deregisteredNetworkInterfaceIds) (\s@TransitGatewayMulticastDeregisteredGroupMembers' {} a -> s {deregisteredNetworkInterfaceIds = a} :: TransitGatewayMulticastDeregisteredGroupMembers) Prelude.. Lens.mapping Lens.coerced

-- | The IP address assigned to the transit gateway multicast group.
transitGatewayMulticastDeregisteredGroupMembers_groupIpAddress :: Lens.Lens' TransitGatewayMulticastDeregisteredGroupMembers (Prelude.Maybe Prelude.Text)
transitGatewayMulticastDeregisteredGroupMembers_groupIpAddress = Lens.lens (\TransitGatewayMulticastDeregisteredGroupMembers' {groupIpAddress} -> groupIpAddress) (\s@TransitGatewayMulticastDeregisteredGroupMembers' {} a -> s {groupIpAddress = a} :: TransitGatewayMulticastDeregisteredGroupMembers)

-- | The ID of the transit gateway multicast domain.
transitGatewayMulticastDeregisteredGroupMembers_transitGatewayMulticastDomainId :: Lens.Lens' TransitGatewayMulticastDeregisteredGroupMembers (Prelude.Maybe Prelude.Text)
transitGatewayMulticastDeregisteredGroupMembers_transitGatewayMulticastDomainId = Lens.lens (\TransitGatewayMulticastDeregisteredGroupMembers' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@TransitGatewayMulticastDeregisteredGroupMembers' {} a -> s {transitGatewayMulticastDomainId = a} :: TransitGatewayMulticastDeregisteredGroupMembers)

instance
  Core.FromXML
    TransitGatewayMulticastDeregisteredGroupMembers
  where
  parseXML x =
    TransitGatewayMulticastDeregisteredGroupMembers'
      Prelude.<$> ( x Core..@? "deregisteredNetworkInterfaceIds"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
        Prelude.<*> (x Core..@? "groupIpAddress")
        Prelude.<*> (x Core..@? "transitGatewayMulticastDomainId")

instance
  Prelude.Hashable
    TransitGatewayMulticastDeregisteredGroupMembers
  where
  hashWithSalt
    _salt
    TransitGatewayMulticastDeregisteredGroupMembers' {..} =
      _salt
        `Prelude.hashWithSalt` deregisteredNetworkInterfaceIds
        `Prelude.hashWithSalt` groupIpAddress
        `Prelude.hashWithSalt` transitGatewayMulticastDomainId

instance
  Prelude.NFData
    TransitGatewayMulticastDeregisteredGroupMembers
  where
  rnf
    TransitGatewayMulticastDeregisteredGroupMembers' {..} =
      Prelude.rnf deregisteredNetworkInterfaceIds
        `Prelude.seq` Prelude.rnf groupIpAddress
        `Prelude.seq` Prelude.rnf transitGatewayMulticastDomainId
