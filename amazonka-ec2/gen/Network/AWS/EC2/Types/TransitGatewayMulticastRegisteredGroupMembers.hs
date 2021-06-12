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
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastRegisteredGroupMembers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulticastRegisteredGroupMembers where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes the registered transit gateway multicast group members.
--
-- /See:/ 'newTransitGatewayMulticastRegisteredGroupMembers' smart constructor.
data TransitGatewayMulticastRegisteredGroupMembers = TransitGatewayMulticastRegisteredGroupMembers'
  { -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Core.Maybe Core.Text,
    -- | The IP address assigned to the transit gateway multicast group.
    groupIpAddress :: Core.Maybe Core.Text,
    -- | The ID of the registered network interfaces.
    registeredNetworkInterfaceIds :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransitGatewayMulticastRegisteredGroupMembers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayMulticastDomainId', 'transitGatewayMulticastRegisteredGroupMembers_transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- 'groupIpAddress', 'transitGatewayMulticastRegisteredGroupMembers_groupIpAddress' - The IP address assigned to the transit gateway multicast group.
--
-- 'registeredNetworkInterfaceIds', 'transitGatewayMulticastRegisteredGroupMembers_registeredNetworkInterfaceIds' - The ID of the registered network interfaces.
newTransitGatewayMulticastRegisteredGroupMembers ::
  TransitGatewayMulticastRegisteredGroupMembers
newTransitGatewayMulticastRegisteredGroupMembers =
  TransitGatewayMulticastRegisteredGroupMembers'
    { transitGatewayMulticastDomainId =
        Core.Nothing,
      groupIpAddress =
        Core.Nothing,
      registeredNetworkInterfaceIds =
        Core.Nothing
    }

-- | The ID of the transit gateway multicast domain.
transitGatewayMulticastRegisteredGroupMembers_transitGatewayMulticastDomainId :: Lens.Lens' TransitGatewayMulticastRegisteredGroupMembers (Core.Maybe Core.Text)
transitGatewayMulticastRegisteredGroupMembers_transitGatewayMulticastDomainId = Lens.lens (\TransitGatewayMulticastRegisteredGroupMembers' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@TransitGatewayMulticastRegisteredGroupMembers' {} a -> s {transitGatewayMulticastDomainId = a} :: TransitGatewayMulticastRegisteredGroupMembers)

-- | The IP address assigned to the transit gateway multicast group.
transitGatewayMulticastRegisteredGroupMembers_groupIpAddress :: Lens.Lens' TransitGatewayMulticastRegisteredGroupMembers (Core.Maybe Core.Text)
transitGatewayMulticastRegisteredGroupMembers_groupIpAddress = Lens.lens (\TransitGatewayMulticastRegisteredGroupMembers' {groupIpAddress} -> groupIpAddress) (\s@TransitGatewayMulticastRegisteredGroupMembers' {} a -> s {groupIpAddress = a} :: TransitGatewayMulticastRegisteredGroupMembers)

-- | The ID of the registered network interfaces.
transitGatewayMulticastRegisteredGroupMembers_registeredNetworkInterfaceIds :: Lens.Lens' TransitGatewayMulticastRegisteredGroupMembers (Core.Maybe [Core.Text])
transitGatewayMulticastRegisteredGroupMembers_registeredNetworkInterfaceIds = Lens.lens (\TransitGatewayMulticastRegisteredGroupMembers' {registeredNetworkInterfaceIds} -> registeredNetworkInterfaceIds) (\s@TransitGatewayMulticastRegisteredGroupMembers' {} a -> s {registeredNetworkInterfaceIds = a} :: TransitGatewayMulticastRegisteredGroupMembers) Core.. Lens.mapping Lens._Coerce

instance
  Core.FromXML
    TransitGatewayMulticastRegisteredGroupMembers
  where
  parseXML x =
    TransitGatewayMulticastRegisteredGroupMembers'
      Core.<$> (x Core..@? "transitGatewayMulticastDomainId")
        Core.<*> (x Core..@? "groupIpAddress")
        Core.<*> ( x Core..@? "registeredNetworkInterfaceIds"
                     Core..!@ Core.mempty
                     Core.>>= Core.may (Core.parseXMLList "item")
                 )

instance
  Core.Hashable
    TransitGatewayMulticastRegisteredGroupMembers

instance
  Core.NFData
    TransitGatewayMulticastRegisteredGroupMembers
