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
import qualified Network.AWS.Prelude as Prelude

-- | Describes a local gateway virtual interface.
--
-- /See:/ 'newLocalGatewayVirtualInterface' smart constructor.
data LocalGatewayVirtualInterface = LocalGatewayVirtualInterface'
  { -- | The peer address.
    peerAddress :: Prelude.Maybe Prelude.Text,
    -- | The ID of the virtual interface.
    localGatewayVirtualInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The AWS account ID that owns the local gateway virtual interface.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The local address.
    localAddress :: Prelude.Maybe Prelude.Text,
    -- | The peer BGP ASN.
    peerBgpAsn :: Prelude.Maybe Prelude.Int,
    -- | The ID of the local gateway.
    localGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The Border Gateway Protocol (BGP) Autonomous System Number (ASN) of the
    -- local gateway.
    localBgpAsn :: Prelude.Maybe Prelude.Int,
    -- | The tags assigned to the virtual interface.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the VLAN.
    vlan :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'localGatewayVirtualInterfaceId', 'localGatewayVirtualInterface_localGatewayVirtualInterfaceId' - The ID of the virtual interface.
--
-- 'ownerId', 'localGatewayVirtualInterface_ownerId' - The AWS account ID that owns the local gateway virtual interface.
--
-- 'localAddress', 'localGatewayVirtualInterface_localAddress' - The local address.
--
-- 'peerBgpAsn', 'localGatewayVirtualInterface_peerBgpAsn' - The peer BGP ASN.
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
        Prelude.Nothing,
      localGatewayVirtualInterfaceId =
        Prelude.Nothing,
      ownerId = Prelude.Nothing,
      localAddress = Prelude.Nothing,
      peerBgpAsn = Prelude.Nothing,
      localGatewayId = Prelude.Nothing,
      localBgpAsn = Prelude.Nothing,
      tags = Prelude.Nothing,
      vlan = Prelude.Nothing
    }

-- | The peer address.
localGatewayVirtualInterface_peerAddress :: Lens.Lens' LocalGatewayVirtualInterface (Prelude.Maybe Prelude.Text)
localGatewayVirtualInterface_peerAddress = Lens.lens (\LocalGatewayVirtualInterface' {peerAddress} -> peerAddress) (\s@LocalGatewayVirtualInterface' {} a -> s {peerAddress = a} :: LocalGatewayVirtualInterface)

-- | The ID of the virtual interface.
localGatewayVirtualInterface_localGatewayVirtualInterfaceId :: Lens.Lens' LocalGatewayVirtualInterface (Prelude.Maybe Prelude.Text)
localGatewayVirtualInterface_localGatewayVirtualInterfaceId = Lens.lens (\LocalGatewayVirtualInterface' {localGatewayVirtualInterfaceId} -> localGatewayVirtualInterfaceId) (\s@LocalGatewayVirtualInterface' {} a -> s {localGatewayVirtualInterfaceId = a} :: LocalGatewayVirtualInterface)

-- | The AWS account ID that owns the local gateway virtual interface.
localGatewayVirtualInterface_ownerId :: Lens.Lens' LocalGatewayVirtualInterface (Prelude.Maybe Prelude.Text)
localGatewayVirtualInterface_ownerId = Lens.lens (\LocalGatewayVirtualInterface' {ownerId} -> ownerId) (\s@LocalGatewayVirtualInterface' {} a -> s {ownerId = a} :: LocalGatewayVirtualInterface)

-- | The local address.
localGatewayVirtualInterface_localAddress :: Lens.Lens' LocalGatewayVirtualInterface (Prelude.Maybe Prelude.Text)
localGatewayVirtualInterface_localAddress = Lens.lens (\LocalGatewayVirtualInterface' {localAddress} -> localAddress) (\s@LocalGatewayVirtualInterface' {} a -> s {localAddress = a} :: LocalGatewayVirtualInterface)

-- | The peer BGP ASN.
localGatewayVirtualInterface_peerBgpAsn :: Lens.Lens' LocalGatewayVirtualInterface (Prelude.Maybe Prelude.Int)
localGatewayVirtualInterface_peerBgpAsn = Lens.lens (\LocalGatewayVirtualInterface' {peerBgpAsn} -> peerBgpAsn) (\s@LocalGatewayVirtualInterface' {} a -> s {peerBgpAsn = a} :: LocalGatewayVirtualInterface)

-- | The ID of the local gateway.
localGatewayVirtualInterface_localGatewayId :: Lens.Lens' LocalGatewayVirtualInterface (Prelude.Maybe Prelude.Text)
localGatewayVirtualInterface_localGatewayId = Lens.lens (\LocalGatewayVirtualInterface' {localGatewayId} -> localGatewayId) (\s@LocalGatewayVirtualInterface' {} a -> s {localGatewayId = a} :: LocalGatewayVirtualInterface)

-- | The Border Gateway Protocol (BGP) Autonomous System Number (ASN) of the
-- local gateway.
localGatewayVirtualInterface_localBgpAsn :: Lens.Lens' LocalGatewayVirtualInterface (Prelude.Maybe Prelude.Int)
localGatewayVirtualInterface_localBgpAsn = Lens.lens (\LocalGatewayVirtualInterface' {localBgpAsn} -> localBgpAsn) (\s@LocalGatewayVirtualInterface' {} a -> s {localBgpAsn = a} :: LocalGatewayVirtualInterface)

-- | The tags assigned to the virtual interface.
localGatewayVirtualInterface_tags :: Lens.Lens' LocalGatewayVirtualInterface (Prelude.Maybe [Tag])
localGatewayVirtualInterface_tags = Lens.lens (\LocalGatewayVirtualInterface' {tags} -> tags) (\s@LocalGatewayVirtualInterface' {} a -> s {tags = a} :: LocalGatewayVirtualInterface) Prelude.. Lens.mapping Lens._Coerce

-- | The ID of the VLAN.
localGatewayVirtualInterface_vlan :: Lens.Lens' LocalGatewayVirtualInterface (Prelude.Maybe Prelude.Int)
localGatewayVirtualInterface_vlan = Lens.lens (\LocalGatewayVirtualInterface' {vlan} -> vlan) (\s@LocalGatewayVirtualInterface' {} a -> s {vlan = a} :: LocalGatewayVirtualInterface)

instance Core.FromXML LocalGatewayVirtualInterface where
  parseXML x =
    LocalGatewayVirtualInterface'
      Prelude.<$> (x Core..@? "peerAddress")
      Prelude.<*> (x Core..@? "localGatewayVirtualInterfaceId")
      Prelude.<*> (x Core..@? "ownerId")
      Prelude.<*> (x Core..@? "localAddress")
      Prelude.<*> (x Core..@? "peerBgpAsn")
      Prelude.<*> (x Core..@? "localGatewayId")
      Prelude.<*> (x Core..@? "localBgpAsn")
      Prelude.<*> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "vlan")

instance
  Prelude.Hashable
    LocalGatewayVirtualInterface

instance Prelude.NFData LocalGatewayVirtualInterface
