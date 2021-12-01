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
-- Module      : Amazonka.EC2.Types.TransitGatewayMulticastRegisteredGroupSources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayMulticastRegisteredGroupSources where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the members registered with the transit gateway multicast
-- group.
--
-- /See:/ 'newTransitGatewayMulticastRegisteredGroupSources' smart constructor.
data TransitGatewayMulticastRegisteredGroupSources = TransitGatewayMulticastRegisteredGroupSources'
  { -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the network interfaces members registered with the transit
    -- gateway multicast group.
    registeredNetworkInterfaceIds :: Prelude.Maybe [Prelude.Text],
    -- | The IP address assigned to the transit gateway multicast group.
    groupIpAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'registeredNetworkInterfaceIds', 'transitGatewayMulticastRegisteredGroupSources_registeredNetworkInterfaceIds' - The IDs of the network interfaces members registered with the transit
-- gateway multicast group.
--
-- 'groupIpAddress', 'transitGatewayMulticastRegisteredGroupSources_groupIpAddress' - The IP address assigned to the transit gateway multicast group.
newTransitGatewayMulticastRegisteredGroupSources ::
  TransitGatewayMulticastRegisteredGroupSources
newTransitGatewayMulticastRegisteredGroupSources =
  TransitGatewayMulticastRegisteredGroupSources'
    { transitGatewayMulticastDomainId =
        Prelude.Nothing,
      registeredNetworkInterfaceIds =
        Prelude.Nothing,
      groupIpAddress =
        Prelude.Nothing
    }

-- | The ID of the transit gateway multicast domain.
transitGatewayMulticastRegisteredGroupSources_transitGatewayMulticastDomainId :: Lens.Lens' TransitGatewayMulticastRegisteredGroupSources (Prelude.Maybe Prelude.Text)
transitGatewayMulticastRegisteredGroupSources_transitGatewayMulticastDomainId = Lens.lens (\TransitGatewayMulticastRegisteredGroupSources' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@TransitGatewayMulticastRegisteredGroupSources' {} a -> s {transitGatewayMulticastDomainId = a} :: TransitGatewayMulticastRegisteredGroupSources)

-- | The IDs of the network interfaces members registered with the transit
-- gateway multicast group.
transitGatewayMulticastRegisteredGroupSources_registeredNetworkInterfaceIds :: Lens.Lens' TransitGatewayMulticastRegisteredGroupSources (Prelude.Maybe [Prelude.Text])
transitGatewayMulticastRegisteredGroupSources_registeredNetworkInterfaceIds = Lens.lens (\TransitGatewayMulticastRegisteredGroupSources' {registeredNetworkInterfaceIds} -> registeredNetworkInterfaceIds) (\s@TransitGatewayMulticastRegisteredGroupSources' {} a -> s {registeredNetworkInterfaceIds = a} :: TransitGatewayMulticastRegisteredGroupSources) Prelude.. Lens.mapping Lens.coerced

-- | The IP address assigned to the transit gateway multicast group.
transitGatewayMulticastRegisteredGroupSources_groupIpAddress :: Lens.Lens' TransitGatewayMulticastRegisteredGroupSources (Prelude.Maybe Prelude.Text)
transitGatewayMulticastRegisteredGroupSources_groupIpAddress = Lens.lens (\TransitGatewayMulticastRegisteredGroupSources' {groupIpAddress} -> groupIpAddress) (\s@TransitGatewayMulticastRegisteredGroupSources' {} a -> s {groupIpAddress = a} :: TransitGatewayMulticastRegisteredGroupSources)

instance
  Core.FromXML
    TransitGatewayMulticastRegisteredGroupSources
  where
  parseXML x =
    TransitGatewayMulticastRegisteredGroupSources'
      Prelude.<$> (x Core..@? "transitGatewayMulticastDomainId")
        Prelude.<*> ( x Core..@? "registeredNetworkInterfaceIds"
                        Core..!@ Prelude.mempty
                        Prelude.>>= Core.may (Core.parseXMLList "item")
                    )
        Prelude.<*> (x Core..@? "groupIpAddress")

instance
  Prelude.Hashable
    TransitGatewayMulticastRegisteredGroupSources
  where
  hashWithSalt
    salt'
    TransitGatewayMulticastRegisteredGroupSources' {..} =
      salt' `Prelude.hashWithSalt` groupIpAddress
        `Prelude.hashWithSalt` registeredNetworkInterfaceIds
        `Prelude.hashWithSalt` transitGatewayMulticastDomainId

instance
  Prelude.NFData
    TransitGatewayMulticastRegisteredGroupSources
  where
  rnf
    TransitGatewayMulticastRegisteredGroupSources' {..} =
      Prelude.rnf transitGatewayMulticastDomainId
        `Prelude.seq` Prelude.rnf groupIpAddress
        `Prelude.seq` Prelude.rnf registeredNetworkInterfaceIds
