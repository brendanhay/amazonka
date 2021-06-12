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
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastRegisteredGroupSources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulticastRegisteredGroupSources where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes the members registered with the transit gateway multicast
-- group.
--
-- /See:/ 'newTransitGatewayMulticastRegisteredGroupSources' smart constructor.
data TransitGatewayMulticastRegisteredGroupSources = TransitGatewayMulticastRegisteredGroupSources'
  { -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Core.Maybe Core.Text,
    -- | The IP address assigned to the transit gateway multicast group.
    groupIpAddress :: Core.Maybe Core.Text,
    -- | The IDs of the network interfaces members registered with the transit
    -- gateway multicast group.
    registeredNetworkInterfaceIds :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransitGatewayMulticastRegisteredGroupSources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayMulticastDomainId', 'transitGatewayMulticastRegisteredGroupSources_transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- 'groupIpAddress', 'transitGatewayMulticastRegisteredGroupSources_groupIpAddress' - The IP address assigned to the transit gateway multicast group.
--
-- 'registeredNetworkInterfaceIds', 'transitGatewayMulticastRegisteredGroupSources_registeredNetworkInterfaceIds' - The IDs of the network interfaces members registered with the transit
-- gateway multicast group.
newTransitGatewayMulticastRegisteredGroupSources ::
  TransitGatewayMulticastRegisteredGroupSources
newTransitGatewayMulticastRegisteredGroupSources =
  TransitGatewayMulticastRegisteredGroupSources'
    { transitGatewayMulticastDomainId =
        Core.Nothing,
      groupIpAddress =
        Core.Nothing,
      registeredNetworkInterfaceIds =
        Core.Nothing
    }

-- | The ID of the transit gateway multicast domain.
transitGatewayMulticastRegisteredGroupSources_transitGatewayMulticastDomainId :: Lens.Lens' TransitGatewayMulticastRegisteredGroupSources (Core.Maybe Core.Text)
transitGatewayMulticastRegisteredGroupSources_transitGatewayMulticastDomainId = Lens.lens (\TransitGatewayMulticastRegisteredGroupSources' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@TransitGatewayMulticastRegisteredGroupSources' {} a -> s {transitGatewayMulticastDomainId = a} :: TransitGatewayMulticastRegisteredGroupSources)

-- | The IP address assigned to the transit gateway multicast group.
transitGatewayMulticastRegisteredGroupSources_groupIpAddress :: Lens.Lens' TransitGatewayMulticastRegisteredGroupSources (Core.Maybe Core.Text)
transitGatewayMulticastRegisteredGroupSources_groupIpAddress = Lens.lens (\TransitGatewayMulticastRegisteredGroupSources' {groupIpAddress} -> groupIpAddress) (\s@TransitGatewayMulticastRegisteredGroupSources' {} a -> s {groupIpAddress = a} :: TransitGatewayMulticastRegisteredGroupSources)

-- | The IDs of the network interfaces members registered with the transit
-- gateway multicast group.
transitGatewayMulticastRegisteredGroupSources_registeredNetworkInterfaceIds :: Lens.Lens' TransitGatewayMulticastRegisteredGroupSources (Core.Maybe [Core.Text])
transitGatewayMulticastRegisteredGroupSources_registeredNetworkInterfaceIds = Lens.lens (\TransitGatewayMulticastRegisteredGroupSources' {registeredNetworkInterfaceIds} -> registeredNetworkInterfaceIds) (\s@TransitGatewayMulticastRegisteredGroupSources' {} a -> s {registeredNetworkInterfaceIds = a} :: TransitGatewayMulticastRegisteredGroupSources) Core.. Lens.mapping Lens._Coerce

instance
  Core.FromXML
    TransitGatewayMulticastRegisteredGroupSources
  where
  parseXML x =
    TransitGatewayMulticastRegisteredGroupSources'
      Core.<$> (x Core..@? "transitGatewayMulticastDomainId")
        Core.<*> (x Core..@? "groupIpAddress")
        Core.<*> ( x Core..@? "registeredNetworkInterfaceIds"
                     Core..!@ Core.mempty
                     Core.>>= Core.may (Core.parseXMLList "item")
                 )

instance
  Core.Hashable
    TransitGatewayMulticastRegisteredGroupSources

instance
  Core.NFData
    TransitGatewayMulticastRegisteredGroupSources
