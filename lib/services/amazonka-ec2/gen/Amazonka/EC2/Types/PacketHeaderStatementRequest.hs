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
-- Module      : Amazonka.EC2.Types.PacketHeaderStatementRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PacketHeaderStatementRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Protocol
import qualified Amazonka.Prelude as Prelude

-- | Describes a packet header statement.
--
-- /See:/ 'newPacketHeaderStatementRequest' smart constructor.
data PacketHeaderStatementRequest = PacketHeaderStatementRequest'
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
-- Create a value of 'PacketHeaderStatementRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationPorts', 'packetHeaderStatementRequest_destinationPorts' - The destination ports.
--
-- 'destinationPrefixLists', 'packetHeaderStatementRequest_destinationPrefixLists' - The destination prefix lists.
--
-- 'protocols', 'packetHeaderStatementRequest_protocols' - The protocols.
--
-- 'destinationAddresses', 'packetHeaderStatementRequest_destinationAddresses' - The destination addresses.
--
-- 'sourceAddresses', 'packetHeaderStatementRequest_sourceAddresses' - The source addresses.
--
-- 'sourcePorts', 'packetHeaderStatementRequest_sourcePorts' - The source ports.
--
-- 'sourcePrefixLists', 'packetHeaderStatementRequest_sourcePrefixLists' - The source prefix lists.
newPacketHeaderStatementRequest ::
  PacketHeaderStatementRequest
newPacketHeaderStatementRequest =
  PacketHeaderStatementRequest'
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
packetHeaderStatementRequest_destinationPorts :: Lens.Lens' PacketHeaderStatementRequest (Prelude.Maybe [Prelude.Text])
packetHeaderStatementRequest_destinationPorts = Lens.lens (\PacketHeaderStatementRequest' {destinationPorts} -> destinationPorts) (\s@PacketHeaderStatementRequest' {} a -> s {destinationPorts = a} :: PacketHeaderStatementRequest) Prelude.. Lens.mapping Lens.coerced

-- | The destination prefix lists.
packetHeaderStatementRequest_destinationPrefixLists :: Lens.Lens' PacketHeaderStatementRequest (Prelude.Maybe [Prelude.Text])
packetHeaderStatementRequest_destinationPrefixLists = Lens.lens (\PacketHeaderStatementRequest' {destinationPrefixLists} -> destinationPrefixLists) (\s@PacketHeaderStatementRequest' {} a -> s {destinationPrefixLists = a} :: PacketHeaderStatementRequest) Prelude.. Lens.mapping Lens.coerced

-- | The protocols.
packetHeaderStatementRequest_protocols :: Lens.Lens' PacketHeaderStatementRequest (Prelude.Maybe [Protocol])
packetHeaderStatementRequest_protocols = Lens.lens (\PacketHeaderStatementRequest' {protocols} -> protocols) (\s@PacketHeaderStatementRequest' {} a -> s {protocols = a} :: PacketHeaderStatementRequest) Prelude.. Lens.mapping Lens.coerced

-- | The destination addresses.
packetHeaderStatementRequest_destinationAddresses :: Lens.Lens' PacketHeaderStatementRequest (Prelude.Maybe [Prelude.Text])
packetHeaderStatementRequest_destinationAddresses = Lens.lens (\PacketHeaderStatementRequest' {destinationAddresses} -> destinationAddresses) (\s@PacketHeaderStatementRequest' {} a -> s {destinationAddresses = a} :: PacketHeaderStatementRequest) Prelude.. Lens.mapping Lens.coerced

-- | The source addresses.
packetHeaderStatementRequest_sourceAddresses :: Lens.Lens' PacketHeaderStatementRequest (Prelude.Maybe [Prelude.Text])
packetHeaderStatementRequest_sourceAddresses = Lens.lens (\PacketHeaderStatementRequest' {sourceAddresses} -> sourceAddresses) (\s@PacketHeaderStatementRequest' {} a -> s {sourceAddresses = a} :: PacketHeaderStatementRequest) Prelude.. Lens.mapping Lens.coerced

-- | The source ports.
packetHeaderStatementRequest_sourcePorts :: Lens.Lens' PacketHeaderStatementRequest (Prelude.Maybe [Prelude.Text])
packetHeaderStatementRequest_sourcePorts = Lens.lens (\PacketHeaderStatementRequest' {sourcePorts} -> sourcePorts) (\s@PacketHeaderStatementRequest' {} a -> s {sourcePorts = a} :: PacketHeaderStatementRequest) Prelude.. Lens.mapping Lens.coerced

-- | The source prefix lists.
packetHeaderStatementRequest_sourcePrefixLists :: Lens.Lens' PacketHeaderStatementRequest (Prelude.Maybe [Prelude.Text])
packetHeaderStatementRequest_sourcePrefixLists = Lens.lens (\PacketHeaderStatementRequest' {sourcePrefixLists} -> sourcePrefixLists) (\s@PacketHeaderStatementRequest' {} a -> s {sourcePrefixLists = a} :: PacketHeaderStatementRequest) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    PacketHeaderStatementRequest
  where
  hashWithSalt _salt PacketHeaderStatementRequest' {..} =
    _salt `Prelude.hashWithSalt` destinationPorts
      `Prelude.hashWithSalt` destinationPrefixLists
      `Prelude.hashWithSalt` protocols
      `Prelude.hashWithSalt` destinationAddresses
      `Prelude.hashWithSalt` sourceAddresses
      `Prelude.hashWithSalt` sourcePorts
      `Prelude.hashWithSalt` sourcePrefixLists

instance Prelude.NFData PacketHeaderStatementRequest where
  rnf PacketHeaderStatementRequest' {..} =
    Prelude.rnf destinationPorts
      `Prelude.seq` Prelude.rnf destinationPrefixLists
      `Prelude.seq` Prelude.rnf protocols
      `Prelude.seq` Prelude.rnf destinationAddresses
      `Prelude.seq` Prelude.rnf sourceAddresses
      `Prelude.seq` Prelude.rnf sourcePorts
      `Prelude.seq` Prelude.rnf sourcePrefixLists

instance Data.ToQuery PacketHeaderStatementRequest where
  toQuery PacketHeaderStatementRequest' {..} =
    Prelude.mconcat
      [ Data.toQuery
          ( Data.toQueryList "DestinationPort"
              Prelude.<$> destinationPorts
          ),
        Data.toQuery
          ( Data.toQueryList "DestinationPrefixList"
              Prelude.<$> destinationPrefixLists
          ),
        Data.toQuery
          (Data.toQueryList "Protocol" Prelude.<$> protocols),
        Data.toQuery
          ( Data.toQueryList "DestinationAddress"
              Prelude.<$> destinationAddresses
          ),
        Data.toQuery
          ( Data.toQueryList "SourceAddress"
              Prelude.<$> sourceAddresses
          ),
        Data.toQuery
          ( Data.toQueryList "SourcePort"
              Prelude.<$> sourcePorts
          ),
        Data.toQuery
          ( Data.toQueryList "SourcePrefixList"
              Prelude.<$> sourcePrefixLists
          )
      ]
