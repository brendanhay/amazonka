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
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastDeregisteredGroupSources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulticastDeregisteredGroupSources where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the deregistered transit gateway multicast group sources.
--
-- /See:/ 'newTransitGatewayMulticastDeregisteredGroupSources' smart constructor.
data TransitGatewayMulticastDeregisteredGroupSources = TransitGatewayMulticastDeregisteredGroupSources'
  { -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Prelude.Maybe Prelude.Text,
    -- | The network interface IDs of the non-registered members.
    deregisteredNetworkInterfaceIds :: Prelude.Maybe [Prelude.Text],
    -- | The IP address assigned to the transit gateway multicast group.
    groupIpAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      deregisteredNetworkInterfaceIds =
        Prelude.Nothing,
      groupIpAddress =
        Prelude.Nothing
    }

-- | The ID of the transit gateway multicast domain.
transitGatewayMulticastDeregisteredGroupSources_transitGatewayMulticastDomainId :: Lens.Lens' TransitGatewayMulticastDeregisteredGroupSources (Prelude.Maybe Prelude.Text)
transitGatewayMulticastDeregisteredGroupSources_transitGatewayMulticastDomainId = Lens.lens (\TransitGatewayMulticastDeregisteredGroupSources' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@TransitGatewayMulticastDeregisteredGroupSources' {} a -> s {transitGatewayMulticastDomainId = a} :: TransitGatewayMulticastDeregisteredGroupSources)

-- | The network interface IDs of the non-registered members.
transitGatewayMulticastDeregisteredGroupSources_deregisteredNetworkInterfaceIds :: Lens.Lens' TransitGatewayMulticastDeregisteredGroupSources (Prelude.Maybe [Prelude.Text])
transitGatewayMulticastDeregisteredGroupSources_deregisteredNetworkInterfaceIds = Lens.lens (\TransitGatewayMulticastDeregisteredGroupSources' {deregisteredNetworkInterfaceIds} -> deregisteredNetworkInterfaceIds) (\s@TransitGatewayMulticastDeregisteredGroupSources' {} a -> s {deregisteredNetworkInterfaceIds = a} :: TransitGatewayMulticastDeregisteredGroupSources) Prelude.. Lens.mapping Prelude._Coerce

-- | The IP address assigned to the transit gateway multicast group.
transitGatewayMulticastDeregisteredGroupSources_groupIpAddress :: Lens.Lens' TransitGatewayMulticastDeregisteredGroupSources (Prelude.Maybe Prelude.Text)
transitGatewayMulticastDeregisteredGroupSources_groupIpAddress = Lens.lens (\TransitGatewayMulticastDeregisteredGroupSources' {groupIpAddress} -> groupIpAddress) (\s@TransitGatewayMulticastDeregisteredGroupSources' {} a -> s {groupIpAddress = a} :: TransitGatewayMulticastDeregisteredGroupSources)

instance
  Prelude.FromXML
    TransitGatewayMulticastDeregisteredGroupSources
  where
  parseXML x =
    TransitGatewayMulticastDeregisteredGroupSources'
      Prelude.<$> (x Prelude..@? "transitGatewayMulticastDomainId")
        Prelude.<*> ( x Prelude..@? "deregisteredNetworkInterfaceIds"
                        Prelude..!@ Prelude.mempty
                        Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                    )
        Prelude.<*> (x Prelude..@? "groupIpAddress")

instance
  Prelude.Hashable
    TransitGatewayMulticastDeregisteredGroupSources

instance
  Prelude.NFData
    TransitGatewayMulticastDeregisteredGroupSources
