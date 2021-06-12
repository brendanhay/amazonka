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
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastDeregisteredGroupSources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulticastDeregisteredGroupSources where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes the deregistered transit gateway multicast group sources.
--
-- /See:/ 'newTransitGatewayMulticastDeregisteredGroupSources' smart constructor.
data TransitGatewayMulticastDeregisteredGroupSources = TransitGatewayMulticastDeregisteredGroupSources'
  { -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Core.Maybe Core.Text,
    -- | The network interface IDs of the non-registered members.
    deregisteredNetworkInterfaceIds :: Core.Maybe [Core.Text],
    -- | The IP address assigned to the transit gateway multicast group.
    groupIpAddress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransitGatewayMulticastDeregisteredGroupSources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayMulticastDomainId', 'transitGatewayMulticastDeregisteredGroupSources_transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- 'deregisteredNetworkInterfaceIds', 'transitGatewayMulticastDeregisteredGroupSources_deregisteredNetworkInterfaceIds' - The network interface IDs of the non-registered members.
--
-- 'groupIpAddress', 'transitGatewayMulticastDeregisteredGroupSources_groupIpAddress' - The IP address assigned to the transit gateway multicast group.
newTransitGatewayMulticastDeregisteredGroupSources ::
  TransitGatewayMulticastDeregisteredGroupSources
newTransitGatewayMulticastDeregisteredGroupSources =
  TransitGatewayMulticastDeregisteredGroupSources'
    { transitGatewayMulticastDomainId =
        Core.Nothing,
      deregisteredNetworkInterfaceIds =
        Core.Nothing,
      groupIpAddress =
        Core.Nothing
    }

-- | The ID of the transit gateway multicast domain.
transitGatewayMulticastDeregisteredGroupSources_transitGatewayMulticastDomainId :: Lens.Lens' TransitGatewayMulticastDeregisteredGroupSources (Core.Maybe Core.Text)
transitGatewayMulticastDeregisteredGroupSources_transitGatewayMulticastDomainId = Lens.lens (\TransitGatewayMulticastDeregisteredGroupSources' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@TransitGatewayMulticastDeregisteredGroupSources' {} a -> s {transitGatewayMulticastDomainId = a} :: TransitGatewayMulticastDeregisteredGroupSources)

-- | The network interface IDs of the non-registered members.
transitGatewayMulticastDeregisteredGroupSources_deregisteredNetworkInterfaceIds :: Lens.Lens' TransitGatewayMulticastDeregisteredGroupSources (Core.Maybe [Core.Text])
transitGatewayMulticastDeregisteredGroupSources_deregisteredNetworkInterfaceIds = Lens.lens (\TransitGatewayMulticastDeregisteredGroupSources' {deregisteredNetworkInterfaceIds} -> deregisteredNetworkInterfaceIds) (\s@TransitGatewayMulticastDeregisteredGroupSources' {} a -> s {deregisteredNetworkInterfaceIds = a} :: TransitGatewayMulticastDeregisteredGroupSources) Core.. Lens.mapping Lens._Coerce

-- | The IP address assigned to the transit gateway multicast group.
transitGatewayMulticastDeregisteredGroupSources_groupIpAddress :: Lens.Lens' TransitGatewayMulticastDeregisteredGroupSources (Core.Maybe Core.Text)
transitGatewayMulticastDeregisteredGroupSources_groupIpAddress = Lens.lens (\TransitGatewayMulticastDeregisteredGroupSources' {groupIpAddress} -> groupIpAddress) (\s@TransitGatewayMulticastDeregisteredGroupSources' {} a -> s {groupIpAddress = a} :: TransitGatewayMulticastDeregisteredGroupSources)

instance
  Core.FromXML
    TransitGatewayMulticastDeregisteredGroupSources
  where
  parseXML x =
    TransitGatewayMulticastDeregisteredGroupSources'
      Core.<$> (x Core..@? "transitGatewayMulticastDomainId")
        Core.<*> ( x Core..@? "deregisteredNetworkInterfaceIds"
                     Core..!@ Core.mempty
                     Core.>>= Core.may (Core.parseXMLList "item")
                 )
        Core.<*> (x Core..@? "groupIpAddress")

instance
  Core.Hashable
    TransitGatewayMulticastDeregisteredGroupSources

instance
  Core.NFData
    TransitGatewayMulticastDeregisteredGroupSources
