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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes the deregistered transit gateway multicast group members.
--
-- /See:/ 'newTransitGatewayMulticastDeregisteredGroupMembers' smart constructor.
data TransitGatewayMulticastDeregisteredGroupMembers = TransitGatewayMulticastDeregisteredGroupMembers'
  { -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Core.Maybe Core.Text,
    -- | The network interface IDs of the deregistered members.
    deregisteredNetworkInterfaceIds :: Core.Maybe [Core.Text],
    -- | The IP address assigned to the transit gateway multicast group.
    groupIpAddress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      deregisteredNetworkInterfaceIds =
        Core.Nothing,
      groupIpAddress =
        Core.Nothing
    }

-- | The ID of the transit gateway multicast domain.
transitGatewayMulticastDeregisteredGroupMembers_transitGatewayMulticastDomainId :: Lens.Lens' TransitGatewayMulticastDeregisteredGroupMembers (Core.Maybe Core.Text)
transitGatewayMulticastDeregisteredGroupMembers_transitGatewayMulticastDomainId = Lens.lens (\TransitGatewayMulticastDeregisteredGroupMembers' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@TransitGatewayMulticastDeregisteredGroupMembers' {} a -> s {transitGatewayMulticastDomainId = a} :: TransitGatewayMulticastDeregisteredGroupMembers)

-- | The network interface IDs of the deregistered members.
transitGatewayMulticastDeregisteredGroupMembers_deregisteredNetworkInterfaceIds :: Lens.Lens' TransitGatewayMulticastDeregisteredGroupMembers (Core.Maybe [Core.Text])
transitGatewayMulticastDeregisteredGroupMembers_deregisteredNetworkInterfaceIds = Lens.lens (\TransitGatewayMulticastDeregisteredGroupMembers' {deregisteredNetworkInterfaceIds} -> deregisteredNetworkInterfaceIds) (\s@TransitGatewayMulticastDeregisteredGroupMembers' {} a -> s {deregisteredNetworkInterfaceIds = a} :: TransitGatewayMulticastDeregisteredGroupMembers) Core.. Lens.mapping Lens._Coerce

-- | The IP address assigned to the transit gateway multicast group.
transitGatewayMulticastDeregisteredGroupMembers_groupIpAddress :: Lens.Lens' TransitGatewayMulticastDeregisteredGroupMembers (Core.Maybe Core.Text)
transitGatewayMulticastDeregisteredGroupMembers_groupIpAddress = Lens.lens (\TransitGatewayMulticastDeregisteredGroupMembers' {groupIpAddress} -> groupIpAddress) (\s@TransitGatewayMulticastDeregisteredGroupMembers' {} a -> s {groupIpAddress = a} :: TransitGatewayMulticastDeregisteredGroupMembers)

instance
  Core.FromXML
    TransitGatewayMulticastDeregisteredGroupMembers
  where
  parseXML x =
    TransitGatewayMulticastDeregisteredGroupMembers'
      Core.<$> (x Core..@? "transitGatewayMulticastDomainId")
        Core.<*> ( x Core..@? "deregisteredNetworkInterfaceIds"
                     Core..!@ Core.mempty
                     Core.>>= Core.may (Core.parseXMLList "item")
                 )
        Core.<*> (x Core..@? "groupIpAddress")

instance
  Core.Hashable
    TransitGatewayMulticastDeregisteredGroupMembers

instance
  Core.NFData
    TransitGatewayMulticastDeregisteredGroupMembers
