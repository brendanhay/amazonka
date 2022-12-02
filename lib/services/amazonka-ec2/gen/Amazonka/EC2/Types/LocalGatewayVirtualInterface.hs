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
-- Module      : Amazonka.EC2.Types.LocalGatewayVirtualInterface
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LocalGatewayVirtualInterface where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a local gateway virtual interface.
--
-- /See:/ 'newLocalGatewayVirtualInterface' smart constructor.
data LocalGatewayVirtualInterface = LocalGatewayVirtualInterface'
  { -- | The Border Gateway Protocol (BGP) Autonomous System Number (ASN) of the
    -- local gateway.
    localBgpAsn :: Prelude.Maybe Prelude.Int,
    -- | The tags assigned to the virtual interface.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the local gateway.
    localGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the local gateway
    -- virtual interface.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The peer BGP ASN.
    peerBgpAsn :: Prelude.Maybe Prelude.Int,
    -- | The ID of the VLAN.
    vlan :: Prelude.Maybe Prelude.Int,
    -- | The ID of the virtual interface.
    localGatewayVirtualInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The peer address.
    peerAddress :: Prelude.Maybe Prelude.Text,
    -- | The local address.
    localAddress :: Prelude.Maybe Prelude.Text
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
-- 'localBgpAsn', 'localGatewayVirtualInterface_localBgpAsn' - The Border Gateway Protocol (BGP) Autonomous System Number (ASN) of the
-- local gateway.
--
-- 'tags', 'localGatewayVirtualInterface_tags' - The tags assigned to the virtual interface.
--
-- 'localGatewayId', 'localGatewayVirtualInterface_localGatewayId' - The ID of the local gateway.
--
-- 'ownerId', 'localGatewayVirtualInterface_ownerId' - The ID of the Amazon Web Services account that owns the local gateway
-- virtual interface.
--
-- 'peerBgpAsn', 'localGatewayVirtualInterface_peerBgpAsn' - The peer BGP ASN.
--
-- 'vlan', 'localGatewayVirtualInterface_vlan' - The ID of the VLAN.
--
-- 'localGatewayVirtualInterfaceId', 'localGatewayVirtualInterface_localGatewayVirtualInterfaceId' - The ID of the virtual interface.
--
-- 'peerAddress', 'localGatewayVirtualInterface_peerAddress' - The peer address.
--
-- 'localAddress', 'localGatewayVirtualInterface_localAddress' - The local address.
newLocalGatewayVirtualInterface ::
  LocalGatewayVirtualInterface
newLocalGatewayVirtualInterface =
  LocalGatewayVirtualInterface'
    { localBgpAsn =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      localGatewayId = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      peerBgpAsn = Prelude.Nothing,
      vlan = Prelude.Nothing,
      localGatewayVirtualInterfaceId =
        Prelude.Nothing,
      peerAddress = Prelude.Nothing,
      localAddress = Prelude.Nothing
    }

-- | The Border Gateway Protocol (BGP) Autonomous System Number (ASN) of the
-- local gateway.
localGatewayVirtualInterface_localBgpAsn :: Lens.Lens' LocalGatewayVirtualInterface (Prelude.Maybe Prelude.Int)
localGatewayVirtualInterface_localBgpAsn = Lens.lens (\LocalGatewayVirtualInterface' {localBgpAsn} -> localBgpAsn) (\s@LocalGatewayVirtualInterface' {} a -> s {localBgpAsn = a} :: LocalGatewayVirtualInterface)

-- | The tags assigned to the virtual interface.
localGatewayVirtualInterface_tags :: Lens.Lens' LocalGatewayVirtualInterface (Prelude.Maybe [Tag])
localGatewayVirtualInterface_tags = Lens.lens (\LocalGatewayVirtualInterface' {tags} -> tags) (\s@LocalGatewayVirtualInterface' {} a -> s {tags = a} :: LocalGatewayVirtualInterface) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the local gateway.
localGatewayVirtualInterface_localGatewayId :: Lens.Lens' LocalGatewayVirtualInterface (Prelude.Maybe Prelude.Text)
localGatewayVirtualInterface_localGatewayId = Lens.lens (\LocalGatewayVirtualInterface' {localGatewayId} -> localGatewayId) (\s@LocalGatewayVirtualInterface' {} a -> s {localGatewayId = a} :: LocalGatewayVirtualInterface)

-- | The ID of the Amazon Web Services account that owns the local gateway
-- virtual interface.
localGatewayVirtualInterface_ownerId :: Lens.Lens' LocalGatewayVirtualInterface (Prelude.Maybe Prelude.Text)
localGatewayVirtualInterface_ownerId = Lens.lens (\LocalGatewayVirtualInterface' {ownerId} -> ownerId) (\s@LocalGatewayVirtualInterface' {} a -> s {ownerId = a} :: LocalGatewayVirtualInterface)

-- | The peer BGP ASN.
localGatewayVirtualInterface_peerBgpAsn :: Lens.Lens' LocalGatewayVirtualInterface (Prelude.Maybe Prelude.Int)
localGatewayVirtualInterface_peerBgpAsn = Lens.lens (\LocalGatewayVirtualInterface' {peerBgpAsn} -> peerBgpAsn) (\s@LocalGatewayVirtualInterface' {} a -> s {peerBgpAsn = a} :: LocalGatewayVirtualInterface)

-- | The ID of the VLAN.
localGatewayVirtualInterface_vlan :: Lens.Lens' LocalGatewayVirtualInterface (Prelude.Maybe Prelude.Int)
localGatewayVirtualInterface_vlan = Lens.lens (\LocalGatewayVirtualInterface' {vlan} -> vlan) (\s@LocalGatewayVirtualInterface' {} a -> s {vlan = a} :: LocalGatewayVirtualInterface)

-- | The ID of the virtual interface.
localGatewayVirtualInterface_localGatewayVirtualInterfaceId :: Lens.Lens' LocalGatewayVirtualInterface (Prelude.Maybe Prelude.Text)
localGatewayVirtualInterface_localGatewayVirtualInterfaceId = Lens.lens (\LocalGatewayVirtualInterface' {localGatewayVirtualInterfaceId} -> localGatewayVirtualInterfaceId) (\s@LocalGatewayVirtualInterface' {} a -> s {localGatewayVirtualInterfaceId = a} :: LocalGatewayVirtualInterface)

-- | The peer address.
localGatewayVirtualInterface_peerAddress :: Lens.Lens' LocalGatewayVirtualInterface (Prelude.Maybe Prelude.Text)
localGatewayVirtualInterface_peerAddress = Lens.lens (\LocalGatewayVirtualInterface' {peerAddress} -> peerAddress) (\s@LocalGatewayVirtualInterface' {} a -> s {peerAddress = a} :: LocalGatewayVirtualInterface)

-- | The local address.
localGatewayVirtualInterface_localAddress :: Lens.Lens' LocalGatewayVirtualInterface (Prelude.Maybe Prelude.Text)
localGatewayVirtualInterface_localAddress = Lens.lens (\LocalGatewayVirtualInterface' {localAddress} -> localAddress) (\s@LocalGatewayVirtualInterface' {} a -> s {localAddress = a} :: LocalGatewayVirtualInterface)

instance Data.FromXML LocalGatewayVirtualInterface where
  parseXML x =
    LocalGatewayVirtualInterface'
      Prelude.<$> (x Data..@? "localBgpAsn")
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "localGatewayId")
      Prelude.<*> (x Data..@? "ownerId")
      Prelude.<*> (x Data..@? "peerBgpAsn")
      Prelude.<*> (x Data..@? "vlan")
      Prelude.<*> (x Data..@? "localGatewayVirtualInterfaceId")
      Prelude.<*> (x Data..@? "peerAddress")
      Prelude.<*> (x Data..@? "localAddress")

instance
  Prelude.Hashable
    LocalGatewayVirtualInterface
  where
  hashWithSalt _salt LocalGatewayVirtualInterface' {..} =
    _salt `Prelude.hashWithSalt` localBgpAsn
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` localGatewayId
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` peerBgpAsn
      `Prelude.hashWithSalt` vlan
      `Prelude.hashWithSalt` localGatewayVirtualInterfaceId
      `Prelude.hashWithSalt` peerAddress
      `Prelude.hashWithSalt` localAddress

instance Prelude.NFData LocalGatewayVirtualInterface where
  rnf LocalGatewayVirtualInterface' {..} =
    Prelude.rnf localBgpAsn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf localGatewayId
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf peerBgpAsn
      `Prelude.seq` Prelude.rnf vlan
      `Prelude.seq` Prelude.rnf localGatewayVirtualInterfaceId
      `Prelude.seq` Prelude.rnf peerAddress
      `Prelude.seq` Prelude.rnf localAddress
