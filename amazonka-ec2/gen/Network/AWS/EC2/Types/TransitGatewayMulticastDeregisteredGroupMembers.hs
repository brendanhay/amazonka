{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastDeregisteredGroupMembers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulticastDeregisteredGroupMembers where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the deregistered transit gateway multicast group members.
--
-- /See:/ 'newTransitGatewayMulticastDeregisteredGroupMembers' smart constructor.
data TransitGatewayMulticastDeregisteredGroupMembers = TransitGatewayMulticastDeregisteredGroupMembers'
  { -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Prelude.Maybe Prelude.Text,
    -- | The network interface IDs of the deregistered members.
    deregisteredNetworkInterfaceIds :: Prelude.Maybe [Prelude.Text],
    -- | The IP address assigned to the transit gateway multicast group.
    groupIpAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayMulticastDeregisteredGroupMembers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayMulticastDomainId', 'transitGatewayMulticastDeregisteredGroupMembers_transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- 'deregisteredNetworkInterfaceIds', 'transitGatewayMulticastDeregisteredGroupMembers_deregisteredNetworkInterfaceIds' - The network interface IDs of the deregistered members.
--
-- 'groupIpAddress', 'transitGatewayMulticastDeregisteredGroupMembers_groupIpAddress' - The IP address assigned to the transit gateway multicast group.
newTransitGatewayMulticastDeregisteredGroupMembers ::
  TransitGatewayMulticastDeregisteredGroupMembers
newTransitGatewayMulticastDeregisteredGroupMembers =
  TransitGatewayMulticastDeregisteredGroupMembers'
    { transitGatewayMulticastDomainId =
        Prelude.Nothing,
      deregisteredNetworkInterfaceIds =
        Prelude.Nothing,
      groupIpAddress =
        Prelude.Nothing
    }

-- | The ID of the transit gateway multicast domain.
transitGatewayMulticastDeregisteredGroupMembers_transitGatewayMulticastDomainId :: Lens.Lens' TransitGatewayMulticastDeregisteredGroupMembers (Prelude.Maybe Prelude.Text)
transitGatewayMulticastDeregisteredGroupMembers_transitGatewayMulticastDomainId = Lens.lens (\TransitGatewayMulticastDeregisteredGroupMembers' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@TransitGatewayMulticastDeregisteredGroupMembers' {} a -> s {transitGatewayMulticastDomainId = a} :: TransitGatewayMulticastDeregisteredGroupMembers)

-- | The network interface IDs of the deregistered members.
transitGatewayMulticastDeregisteredGroupMembers_deregisteredNetworkInterfaceIds :: Lens.Lens' TransitGatewayMulticastDeregisteredGroupMembers (Prelude.Maybe [Prelude.Text])
transitGatewayMulticastDeregisteredGroupMembers_deregisteredNetworkInterfaceIds = Lens.lens (\TransitGatewayMulticastDeregisteredGroupMembers' {deregisteredNetworkInterfaceIds} -> deregisteredNetworkInterfaceIds) (\s@TransitGatewayMulticastDeregisteredGroupMembers' {} a -> s {deregisteredNetworkInterfaceIds = a} :: TransitGatewayMulticastDeregisteredGroupMembers) Prelude.. Lens.mapping Prelude._Coerce

-- | The IP address assigned to the transit gateway multicast group.
transitGatewayMulticastDeregisteredGroupMembers_groupIpAddress :: Lens.Lens' TransitGatewayMulticastDeregisteredGroupMembers (Prelude.Maybe Prelude.Text)
transitGatewayMulticastDeregisteredGroupMembers_groupIpAddress = Lens.lens (\TransitGatewayMulticastDeregisteredGroupMembers' {groupIpAddress} -> groupIpAddress) (\s@TransitGatewayMulticastDeregisteredGroupMembers' {} a -> s {groupIpAddress = a} :: TransitGatewayMulticastDeregisteredGroupMembers)

instance
  Prelude.FromXML
    TransitGatewayMulticastDeregisteredGroupMembers
  where
  parseXML x =
    TransitGatewayMulticastDeregisteredGroupMembers'
      Prelude.<$> (x Prelude..@? "transitGatewayMulticastDomainId")
        Prelude.<*> ( x Prelude..@? "deregisteredNetworkInterfaceIds"
                        Prelude..!@ Prelude.mempty
                        Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                    )
        Prelude.<*> (x Prelude..@? "groupIpAddress")

instance
  Prelude.Hashable
    TransitGatewayMulticastDeregisteredGroupMembers

instance
  Prelude.NFData
    TransitGatewayMulticastDeregisteredGroupMembers
