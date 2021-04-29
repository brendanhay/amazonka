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
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastRegisteredGroupSources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulticastRegisteredGroupSources where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the members registered with the transit gateway multicast
-- group.
--
-- /See:/ 'newTransitGatewayMulticastRegisteredGroupSources' smart constructor.
data TransitGatewayMulticastRegisteredGroupSources = TransitGatewayMulticastRegisteredGroupSources'
  { -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Prelude.Maybe Prelude.Text,
    -- | The IP address assigned to the transit gateway multicast group.
    groupIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the network interfaces members registered with the transit
    -- gateway multicast group.
    registeredNetworkInterfaceIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      groupIpAddress =
        Prelude.Nothing,
      registeredNetworkInterfaceIds =
        Prelude.Nothing
    }

-- | The ID of the transit gateway multicast domain.
transitGatewayMulticastRegisteredGroupSources_transitGatewayMulticastDomainId :: Lens.Lens' TransitGatewayMulticastRegisteredGroupSources (Prelude.Maybe Prelude.Text)
transitGatewayMulticastRegisteredGroupSources_transitGatewayMulticastDomainId = Lens.lens (\TransitGatewayMulticastRegisteredGroupSources' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@TransitGatewayMulticastRegisteredGroupSources' {} a -> s {transitGatewayMulticastDomainId = a} :: TransitGatewayMulticastRegisteredGroupSources)

-- | The IP address assigned to the transit gateway multicast group.
transitGatewayMulticastRegisteredGroupSources_groupIpAddress :: Lens.Lens' TransitGatewayMulticastRegisteredGroupSources (Prelude.Maybe Prelude.Text)
transitGatewayMulticastRegisteredGroupSources_groupIpAddress = Lens.lens (\TransitGatewayMulticastRegisteredGroupSources' {groupIpAddress} -> groupIpAddress) (\s@TransitGatewayMulticastRegisteredGroupSources' {} a -> s {groupIpAddress = a} :: TransitGatewayMulticastRegisteredGroupSources)

-- | The IDs of the network interfaces members registered with the transit
-- gateway multicast group.
transitGatewayMulticastRegisteredGroupSources_registeredNetworkInterfaceIds :: Lens.Lens' TransitGatewayMulticastRegisteredGroupSources (Prelude.Maybe [Prelude.Text])
transitGatewayMulticastRegisteredGroupSources_registeredNetworkInterfaceIds = Lens.lens (\TransitGatewayMulticastRegisteredGroupSources' {registeredNetworkInterfaceIds} -> registeredNetworkInterfaceIds) (\s@TransitGatewayMulticastRegisteredGroupSources' {} a -> s {registeredNetworkInterfaceIds = a} :: TransitGatewayMulticastRegisteredGroupSources) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.FromXML
    TransitGatewayMulticastRegisteredGroupSources
  where
  parseXML x =
    TransitGatewayMulticastRegisteredGroupSources'
      Prelude.<$> (x Prelude..@? "transitGatewayMulticastDomainId")
        Prelude.<*> (x Prelude..@? "groupIpAddress")
        Prelude.<*> ( x Prelude..@? "registeredNetworkInterfaceIds"
                        Prelude..!@ Prelude.mempty
                        Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                    )

instance
  Prelude.Hashable
    TransitGatewayMulticastRegisteredGroupSources

instance
  Prelude.NFData
    TransitGatewayMulticastRegisteredGroupSources
