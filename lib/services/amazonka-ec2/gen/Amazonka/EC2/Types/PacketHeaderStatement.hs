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
-- Module      : Amazonka.EC2.Types.PacketHeaderStatement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PacketHeaderStatement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Protocol
import qualified Amazonka.Prelude as Prelude

-- | Describes a packet header statement.
--
-- /See:/ 'newPacketHeaderStatement' smart constructor.
data PacketHeaderStatement = PacketHeaderStatement'
  { -- | The destination ports.
    destinationPorts :: Prelude.Maybe [Prelude.Text],
    -- | The destination prefix lists.
    destinationPrefixLists :: Prelude.Maybe [Prelude.Text],
    -- | The protocols.
    protocols :: Prelude.Maybe [Protocol],
    -- | The destination addresses.
    destinationAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The source addresses.
    sourceAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The source ports.
    sourcePorts :: Prelude.Maybe [Prelude.Text],
    -- | The source prefix lists.
    sourcePrefixLists :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PacketHeaderStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationPorts', 'packetHeaderStatement_destinationPorts' - The destination ports.
--
-- 'destinationPrefixLists', 'packetHeaderStatement_destinationPrefixLists' - The destination prefix lists.
--
-- 'protocols', 'packetHeaderStatement_protocols' - The protocols.
--
-- 'destinationAddresses', 'packetHeaderStatement_destinationAddresses' - The destination addresses.
--
-- 'sourceAddresses', 'packetHeaderStatement_sourceAddresses' - The source addresses.
--
-- 'sourcePorts', 'packetHeaderStatement_sourcePorts' - The source ports.
--
-- 'sourcePrefixLists', 'packetHeaderStatement_sourcePrefixLists' - The source prefix lists.
newPacketHeaderStatement ::
  PacketHeaderStatement
newPacketHeaderStatement =
  PacketHeaderStatement'
    { destinationPorts =
        Prelude.Nothing,
      destinationPrefixLists = Prelude.Nothing,
      protocols = Prelude.Nothing,
      destinationAddresses = Prelude.Nothing,
      sourceAddresses = Prelude.Nothing,
      sourcePorts = Prelude.Nothing,
      sourcePrefixLists = Prelude.Nothing
    }

-- | The destination ports.
packetHeaderStatement_destinationPorts :: Lens.Lens' PacketHeaderStatement (Prelude.Maybe [Prelude.Text])
packetHeaderStatement_destinationPorts = Lens.lens (\PacketHeaderStatement' {destinationPorts} -> destinationPorts) (\s@PacketHeaderStatement' {} a -> s {destinationPorts = a} :: PacketHeaderStatement) Prelude.. Lens.mapping Lens.coerced

-- | The destination prefix lists.
packetHeaderStatement_destinationPrefixLists :: Lens.Lens' PacketHeaderStatement (Prelude.Maybe [Prelude.Text])
packetHeaderStatement_destinationPrefixLists = Lens.lens (\PacketHeaderStatement' {destinationPrefixLists} -> destinationPrefixLists) (\s@PacketHeaderStatement' {} a -> s {destinationPrefixLists = a} :: PacketHeaderStatement) Prelude.. Lens.mapping Lens.coerced

-- | The protocols.
packetHeaderStatement_protocols :: Lens.Lens' PacketHeaderStatement (Prelude.Maybe [Protocol])
packetHeaderStatement_protocols = Lens.lens (\PacketHeaderStatement' {protocols} -> protocols) (\s@PacketHeaderStatement' {} a -> s {protocols = a} :: PacketHeaderStatement) Prelude.. Lens.mapping Lens.coerced

-- | The destination addresses.
packetHeaderStatement_destinationAddresses :: Lens.Lens' PacketHeaderStatement (Prelude.Maybe [Prelude.Text])
packetHeaderStatement_destinationAddresses = Lens.lens (\PacketHeaderStatement' {destinationAddresses} -> destinationAddresses) (\s@PacketHeaderStatement' {} a -> s {destinationAddresses = a} :: PacketHeaderStatement) Prelude.. Lens.mapping Lens.coerced

-- | The source addresses.
packetHeaderStatement_sourceAddresses :: Lens.Lens' PacketHeaderStatement (Prelude.Maybe [Prelude.Text])
packetHeaderStatement_sourceAddresses = Lens.lens (\PacketHeaderStatement' {sourceAddresses} -> sourceAddresses) (\s@PacketHeaderStatement' {} a -> s {sourceAddresses = a} :: PacketHeaderStatement) Prelude.. Lens.mapping Lens.coerced

-- | The source ports.
packetHeaderStatement_sourcePorts :: Lens.Lens' PacketHeaderStatement (Prelude.Maybe [Prelude.Text])
packetHeaderStatement_sourcePorts = Lens.lens (\PacketHeaderStatement' {sourcePorts} -> sourcePorts) (\s@PacketHeaderStatement' {} a -> s {sourcePorts = a} :: PacketHeaderStatement) Prelude.. Lens.mapping Lens.coerced

-- | The source prefix lists.
packetHeaderStatement_sourcePrefixLists :: Lens.Lens' PacketHeaderStatement (Prelude.Maybe [Prelude.Text])
packetHeaderStatement_sourcePrefixLists = Lens.lens (\PacketHeaderStatement' {sourcePrefixLists} -> sourcePrefixLists) (\s@PacketHeaderStatement' {} a -> s {sourcePrefixLists = a} :: PacketHeaderStatement) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML PacketHeaderStatement where
  parseXML x =
    PacketHeaderStatement'
      Prelude.<$> ( x Data..@? "destinationPortSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "destinationPrefixListSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "protocolSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "destinationAddressSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "sourceAddressSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "sourcePortSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "sourcePrefixListSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance Prelude.Hashable PacketHeaderStatement where
  hashWithSalt _salt PacketHeaderStatement' {..} =
    _salt `Prelude.hashWithSalt` destinationPorts
      `Prelude.hashWithSalt` destinationPrefixLists
      `Prelude.hashWithSalt` protocols
      `Prelude.hashWithSalt` destinationAddresses
      `Prelude.hashWithSalt` sourceAddresses
      `Prelude.hashWithSalt` sourcePorts
      `Prelude.hashWithSalt` sourcePrefixLists

instance Prelude.NFData PacketHeaderStatement where
  rnf PacketHeaderStatement' {..} =
    Prelude.rnf destinationPorts
      `Prelude.seq` Prelude.rnf destinationPrefixLists
      `Prelude.seq` Prelude.rnf protocols
      `Prelude.seq` Prelude.rnf destinationAddresses
      `Prelude.seq` Prelude.rnf sourceAddresses
      `Prelude.seq` Prelude.rnf sourcePorts
      `Prelude.seq` Prelude.rnf sourcePrefixLists
