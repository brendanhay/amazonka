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
-- Module      : Network.AWS.EC2.Types.LocalGatewayVirtualInterface
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LocalGatewayVirtualInterface where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Describes a local gateway virtual interface.
--
-- /See:/ 'newLocalGatewayVirtualInterface' smart constructor.
data LocalGatewayVirtualInterface = LocalGatewayVirtualInterface'
  { -- | The peer address.
    peerAddress :: Core.Maybe Core.Text,
    -- | The AWS account ID that owns the local gateway virtual interface.
    ownerId :: Core.Maybe Core.Text,
    -- | The ID of the virtual interface.
    localGatewayVirtualInterfaceId :: Core.Maybe Core.Text,
    -- | The peer BGP ASN.
    peerBgpAsn :: Core.Maybe Core.Int,
    -- | The local address.
    localAddress :: Core.Maybe Core.Text,
    -- | The ID of the local gateway.
    localGatewayId :: Core.Maybe Core.Text,
    -- | The Border Gateway Protocol (BGP) Autonomous System Number (ASN) of the
    -- local gateway.
    localBgpAsn :: Core.Maybe Core.Int,
    -- | The tags assigned to the virtual interface.
    tags :: Core.Maybe [Tag],
    -- | The ID of the VLAN.
    vlan :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LocalGatewayVirtualInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'peerAddress', 'localGatewayVirtualInterface_peerAddress' - The peer address.
--
-- 'ownerId', 'localGatewayVirtualInterface_ownerId' - The AWS account ID that owns the local gateway virtual interface.
--
-- 'localGatewayVirtualInterfaceId', 'localGatewayVirtualInterface_localGatewayVirtualInterfaceId' - The ID of the virtual interface.
--
-- 'peerBgpAsn', 'localGatewayVirtualInterface_peerBgpAsn' - The peer BGP ASN.
--
-- 'localAddress', 'localGatewayVirtualInterface_localAddress' - The local address.
--
-- 'localGatewayId', 'localGatewayVirtualInterface_localGatewayId' - The ID of the local gateway.
--
-- 'localBgpAsn', 'localGatewayVirtualInterface_localBgpAsn' - The Border Gateway Protocol (BGP) Autonomous System Number (ASN) of the
-- local gateway.
--
-- 'tags', 'localGatewayVirtualInterface_tags' - The tags assigned to the virtual interface.
--
-- 'vlan', 'localGatewayVirtualInterface_vlan' - The ID of the VLAN.
newLocalGatewayVirtualInterface ::
  LocalGatewayVirtualInterface
newLocalGatewayVirtualInterface =
  LocalGatewayVirtualInterface'
    { peerAddress =
        Core.Nothing,
      ownerId = Core.Nothing,
      localGatewayVirtualInterfaceId = Core.Nothing,
      peerBgpAsn = Core.Nothing,
      localAddress = Core.Nothing,
      localGatewayId = Core.Nothing,
      localBgpAsn = Core.Nothing,
      tags = Core.Nothing,
      vlan = Core.Nothing
    }

-- | The peer address.
localGatewayVirtualInterface_peerAddress :: Lens.Lens' LocalGatewayVirtualInterface (Core.Maybe Core.Text)
localGatewayVirtualInterface_peerAddress = Lens.lens (\LocalGatewayVirtualInterface' {peerAddress} -> peerAddress) (\s@LocalGatewayVirtualInterface' {} a -> s {peerAddress = a} :: LocalGatewayVirtualInterface)

-- | The AWS account ID that owns the local gateway virtual interface.
localGatewayVirtualInterface_ownerId :: Lens.Lens' LocalGatewayVirtualInterface (Core.Maybe Core.Text)
localGatewayVirtualInterface_ownerId = Lens.lens (\LocalGatewayVirtualInterface' {ownerId} -> ownerId) (\s@LocalGatewayVirtualInterface' {} a -> s {ownerId = a} :: LocalGatewayVirtualInterface)

-- | The ID of the virtual interface.
localGatewayVirtualInterface_localGatewayVirtualInterfaceId :: Lens.Lens' LocalGatewayVirtualInterface (Core.Maybe Core.Text)
localGatewayVirtualInterface_localGatewayVirtualInterfaceId = Lens.lens (\LocalGatewayVirtualInterface' {localGatewayVirtualInterfaceId} -> localGatewayVirtualInterfaceId) (\s@LocalGatewayVirtualInterface' {} a -> s {localGatewayVirtualInterfaceId = a} :: LocalGatewayVirtualInterface)

-- | The peer BGP ASN.
localGatewayVirtualInterface_peerBgpAsn :: Lens.Lens' LocalGatewayVirtualInterface (Core.Maybe Core.Int)
localGatewayVirtualInterface_peerBgpAsn = Lens.lens (\LocalGatewayVirtualInterface' {peerBgpAsn} -> peerBgpAsn) (\s@LocalGatewayVirtualInterface' {} a -> s {peerBgpAsn = a} :: LocalGatewayVirtualInterface)

-- | The local address.
localGatewayVirtualInterface_localAddress :: Lens.Lens' LocalGatewayVirtualInterface (Core.Maybe Core.Text)
localGatewayVirtualInterface_localAddress = Lens.lens (\LocalGatewayVirtualInterface' {localAddress} -> localAddress) (\s@LocalGatewayVirtualInterface' {} a -> s {localAddress = a} :: LocalGatewayVirtualInterface)

-- | The ID of the local gateway.
localGatewayVirtualInterface_localGatewayId :: Lens.Lens' LocalGatewayVirtualInterface (Core.Maybe Core.Text)
localGatewayVirtualInterface_localGatewayId = Lens.lens (\LocalGatewayVirtualInterface' {localGatewayId} -> localGatewayId) (\s@LocalGatewayVirtualInterface' {} a -> s {localGatewayId = a} :: LocalGatewayVirtualInterface)

-- | The Border Gateway Protocol (BGP) Autonomous System Number (ASN) of the
-- local gateway.
localGatewayVirtualInterface_localBgpAsn :: Lens.Lens' LocalGatewayVirtualInterface (Core.Maybe Core.Int)
localGatewayVirtualInterface_localBgpAsn = Lens.lens (\LocalGatewayVirtualInterface' {localBgpAsn} -> localBgpAsn) (\s@LocalGatewayVirtualInterface' {} a -> s {localBgpAsn = a} :: LocalGatewayVirtualInterface)

-- | The tags assigned to the virtual interface.
localGatewayVirtualInterface_tags :: Lens.Lens' LocalGatewayVirtualInterface (Core.Maybe [Tag])
localGatewayVirtualInterface_tags = Lens.lens (\LocalGatewayVirtualInterface' {tags} -> tags) (\s@LocalGatewayVirtualInterface' {} a -> s {tags = a} :: LocalGatewayVirtualInterface) Core.. Lens.mapping Lens._Coerce

-- | The ID of the VLAN.
localGatewayVirtualInterface_vlan :: Lens.Lens' LocalGatewayVirtualInterface (Core.Maybe Core.Int)
localGatewayVirtualInterface_vlan = Lens.lens (\LocalGatewayVirtualInterface' {vlan} -> vlan) (\s@LocalGatewayVirtualInterface' {} a -> s {vlan = a} :: LocalGatewayVirtualInterface)

instance Core.FromXML LocalGatewayVirtualInterface where
  parseXML x =
    LocalGatewayVirtualInterface'
      Core.<$> (x Core..@? "peerAddress")
      Core.<*> (x Core..@? "ownerId")
      Core.<*> (x Core..@? "localGatewayVirtualInterfaceId")
      Core.<*> (x Core..@? "peerBgpAsn")
      Core.<*> (x Core..@? "localAddress")
      Core.<*> (x Core..@? "localGatewayId")
      Core.<*> (x Core..@? "localBgpAsn")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "vlan")

instance Core.Hashable LocalGatewayVirtualInterface

instance Core.NFData LocalGatewayVirtualInterface
