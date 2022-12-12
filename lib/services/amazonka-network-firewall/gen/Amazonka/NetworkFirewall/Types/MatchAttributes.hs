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
-- Module      : Amazonka.NetworkFirewall.Types.MatchAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.MatchAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types.Address
import Amazonka.NetworkFirewall.Types.PortRange
import Amazonka.NetworkFirewall.Types.TCPFlagField
import qualified Amazonka.Prelude as Prelude

-- | Criteria for Network Firewall to use to inspect an individual packet in
-- stateless rule inspection. Each match attributes set can include one or
-- more items such as IP address, CIDR range, port number, protocol, and
-- TCP flags.
--
-- /See:/ 'newMatchAttributes' smart constructor.
data MatchAttributes = MatchAttributes'
  { -- | The destination ports to inspect for. If not specified, this matches
    -- with any destination port. This setting is only used for protocols 6
    -- (TCP) and 17 (UDP).
    --
    -- You can specify individual ports, for example @1994@ and you can specify
    -- port ranges, for example @1990:1994@.
    destinationPorts :: Prelude.Maybe [PortRange],
    -- | The destination IP addresses and address ranges to inspect for, in CIDR
    -- notation. If not specified, this matches with any destination address.
    destinations :: Prelude.Maybe [Address],
    -- | The protocols to inspect for, specified using each protocol\'s assigned
    -- internet protocol number (IANA). If not specified, this matches with any
    -- protocol.
    protocols :: Prelude.Maybe [Prelude.Natural],
    -- | The source ports to inspect for. If not specified, this matches with any
    -- source port. This setting is only used for protocols 6 (TCP) and 17
    -- (UDP).
    --
    -- You can specify individual ports, for example @1994@ and you can specify
    -- port ranges, for example @1990:1994@.
    sourcePorts :: Prelude.Maybe [PortRange],
    -- | The source IP addresses and address ranges to inspect for, in CIDR
    -- notation. If not specified, this matches with any source address.
    sources :: Prelude.Maybe [Address],
    -- | The TCP flags and masks to inspect for. If not specified, this matches
    -- with any settings. This setting is only used for protocol 6 (TCP).
    tCPFlags :: Prelude.Maybe [TCPFlagField]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MatchAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationPorts', 'matchAttributes_destinationPorts' - The destination ports to inspect for. If not specified, this matches
-- with any destination port. This setting is only used for protocols 6
-- (TCP) and 17 (UDP).
--
-- You can specify individual ports, for example @1994@ and you can specify
-- port ranges, for example @1990:1994@.
--
-- 'destinations', 'matchAttributes_destinations' - The destination IP addresses and address ranges to inspect for, in CIDR
-- notation. If not specified, this matches with any destination address.
--
-- 'protocols', 'matchAttributes_protocols' - The protocols to inspect for, specified using each protocol\'s assigned
-- internet protocol number (IANA). If not specified, this matches with any
-- protocol.
--
-- 'sourcePorts', 'matchAttributes_sourcePorts' - The source ports to inspect for. If not specified, this matches with any
-- source port. This setting is only used for protocols 6 (TCP) and 17
-- (UDP).
--
-- You can specify individual ports, for example @1994@ and you can specify
-- port ranges, for example @1990:1994@.
--
-- 'sources', 'matchAttributes_sources' - The source IP addresses and address ranges to inspect for, in CIDR
-- notation. If not specified, this matches with any source address.
--
-- 'tCPFlags', 'matchAttributes_tCPFlags' - The TCP flags and masks to inspect for. If not specified, this matches
-- with any settings. This setting is only used for protocol 6 (TCP).
newMatchAttributes ::
  MatchAttributes
newMatchAttributes =
  MatchAttributes'
    { destinationPorts =
        Prelude.Nothing,
      destinations = Prelude.Nothing,
      protocols = Prelude.Nothing,
      sourcePorts = Prelude.Nothing,
      sources = Prelude.Nothing,
      tCPFlags = Prelude.Nothing
    }

-- | The destination ports to inspect for. If not specified, this matches
-- with any destination port. This setting is only used for protocols 6
-- (TCP) and 17 (UDP).
--
-- You can specify individual ports, for example @1994@ and you can specify
-- port ranges, for example @1990:1994@.
matchAttributes_destinationPorts :: Lens.Lens' MatchAttributes (Prelude.Maybe [PortRange])
matchAttributes_destinationPorts = Lens.lens (\MatchAttributes' {destinationPorts} -> destinationPorts) (\s@MatchAttributes' {} a -> s {destinationPorts = a} :: MatchAttributes) Prelude.. Lens.mapping Lens.coerced

-- | The destination IP addresses and address ranges to inspect for, in CIDR
-- notation. If not specified, this matches with any destination address.
matchAttributes_destinations :: Lens.Lens' MatchAttributes (Prelude.Maybe [Address])
matchAttributes_destinations = Lens.lens (\MatchAttributes' {destinations} -> destinations) (\s@MatchAttributes' {} a -> s {destinations = a} :: MatchAttributes) Prelude.. Lens.mapping Lens.coerced

-- | The protocols to inspect for, specified using each protocol\'s assigned
-- internet protocol number (IANA). If not specified, this matches with any
-- protocol.
matchAttributes_protocols :: Lens.Lens' MatchAttributes (Prelude.Maybe [Prelude.Natural])
matchAttributes_protocols = Lens.lens (\MatchAttributes' {protocols} -> protocols) (\s@MatchAttributes' {} a -> s {protocols = a} :: MatchAttributes) Prelude.. Lens.mapping Lens.coerced

-- | The source ports to inspect for. If not specified, this matches with any
-- source port. This setting is only used for protocols 6 (TCP) and 17
-- (UDP).
--
-- You can specify individual ports, for example @1994@ and you can specify
-- port ranges, for example @1990:1994@.
matchAttributes_sourcePorts :: Lens.Lens' MatchAttributes (Prelude.Maybe [PortRange])
matchAttributes_sourcePorts = Lens.lens (\MatchAttributes' {sourcePorts} -> sourcePorts) (\s@MatchAttributes' {} a -> s {sourcePorts = a} :: MatchAttributes) Prelude.. Lens.mapping Lens.coerced

-- | The source IP addresses and address ranges to inspect for, in CIDR
-- notation. If not specified, this matches with any source address.
matchAttributes_sources :: Lens.Lens' MatchAttributes (Prelude.Maybe [Address])
matchAttributes_sources = Lens.lens (\MatchAttributes' {sources} -> sources) (\s@MatchAttributes' {} a -> s {sources = a} :: MatchAttributes) Prelude.. Lens.mapping Lens.coerced

-- | The TCP flags and masks to inspect for. If not specified, this matches
-- with any settings. This setting is only used for protocol 6 (TCP).
matchAttributes_tCPFlags :: Lens.Lens' MatchAttributes (Prelude.Maybe [TCPFlagField])
matchAttributes_tCPFlags = Lens.lens (\MatchAttributes' {tCPFlags} -> tCPFlags) (\s@MatchAttributes' {} a -> s {tCPFlags = a} :: MatchAttributes) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON MatchAttributes where
  parseJSON =
    Data.withObject
      "MatchAttributes"
      ( \x ->
          MatchAttributes'
            Prelude.<$> ( x Data..:? "DestinationPorts"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Destinations" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Protocols" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SourcePorts" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Sources" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "TCPFlags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable MatchAttributes where
  hashWithSalt _salt MatchAttributes' {..} =
    _salt `Prelude.hashWithSalt` destinationPorts
      `Prelude.hashWithSalt` destinations
      `Prelude.hashWithSalt` protocols
      `Prelude.hashWithSalt` sourcePorts
      `Prelude.hashWithSalt` sources
      `Prelude.hashWithSalt` tCPFlags

instance Prelude.NFData MatchAttributes where
  rnf MatchAttributes' {..} =
    Prelude.rnf destinationPorts
      `Prelude.seq` Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf protocols
      `Prelude.seq` Prelude.rnf sourcePorts
      `Prelude.seq` Prelude.rnf sources
      `Prelude.seq` Prelude.rnf tCPFlags

instance Data.ToJSON MatchAttributes where
  toJSON MatchAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DestinationPorts" Data..=)
              Prelude.<$> destinationPorts,
            ("Destinations" Data..=) Prelude.<$> destinations,
            ("Protocols" Data..=) Prelude.<$> protocols,
            ("SourcePorts" Data..=) Prelude.<$> sourcePorts,
            ("Sources" Data..=) Prelude.<$> sources,
            ("TCPFlags" Data..=) Prelude.<$> tCPFlags
          ]
      )
