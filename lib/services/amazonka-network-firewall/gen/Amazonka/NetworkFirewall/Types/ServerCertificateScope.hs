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
-- Module      : Amazonka.NetworkFirewall.Types.ServerCertificateScope
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.ServerCertificateScope where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types.Address
import Amazonka.NetworkFirewall.Types.PortRange
import qualified Amazonka.Prelude as Prelude

-- | Settings that define the Secure Sockets Layer\/Transport Layer Security
-- (SSL\/TLS) traffic that Network Firewall should decrypt for inspection
-- by the stateful rule engine.
--
-- /See:/ 'newServerCertificateScope' smart constructor.
data ServerCertificateScope = ServerCertificateScope'
  { -- | The destination ports to decrypt for inspection, in Transmission Control
    -- Protocol (TCP) format. If not specified, this matches with any
    -- destination port.
    --
    -- You can specify individual ports, for example @1994@, and you can
    -- specify port ranges, such as @1990:1994@.
    destinationPorts :: Prelude.Maybe [PortRange],
    -- | The destination IP addresses and address ranges to decrypt for
    -- inspection, in CIDR notation. If not specified, this matches with any
    -- destination address.
    destinations :: Prelude.Maybe [Address],
    -- | The protocols to decrypt for inspection, specified using each
    -- protocol\'s assigned internet protocol number (IANA). Network Firewall
    -- currently supports only TCP.
    protocols :: Prelude.Maybe [Prelude.Natural],
    -- | The source ports to decrypt for inspection, in Transmission Control
    -- Protocol (TCP) format. If not specified, this matches with any source
    -- port.
    --
    -- You can specify individual ports, for example @1994@, and you can
    -- specify port ranges, such as @1990:1994@.
    sourcePorts :: Prelude.Maybe [PortRange],
    -- | The source IP addresses and address ranges to decrypt for inspection, in
    -- CIDR notation. If not specified, this matches with any source address.
    sources :: Prelude.Maybe [Address]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerCertificateScope' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationPorts', 'serverCertificateScope_destinationPorts' - The destination ports to decrypt for inspection, in Transmission Control
-- Protocol (TCP) format. If not specified, this matches with any
-- destination port.
--
-- You can specify individual ports, for example @1994@, and you can
-- specify port ranges, such as @1990:1994@.
--
-- 'destinations', 'serverCertificateScope_destinations' - The destination IP addresses and address ranges to decrypt for
-- inspection, in CIDR notation. If not specified, this matches with any
-- destination address.
--
-- 'protocols', 'serverCertificateScope_protocols' - The protocols to decrypt for inspection, specified using each
-- protocol\'s assigned internet protocol number (IANA). Network Firewall
-- currently supports only TCP.
--
-- 'sourcePorts', 'serverCertificateScope_sourcePorts' - The source ports to decrypt for inspection, in Transmission Control
-- Protocol (TCP) format. If not specified, this matches with any source
-- port.
--
-- You can specify individual ports, for example @1994@, and you can
-- specify port ranges, such as @1990:1994@.
--
-- 'sources', 'serverCertificateScope_sources' - The source IP addresses and address ranges to decrypt for inspection, in
-- CIDR notation. If not specified, this matches with any source address.
newServerCertificateScope ::
  ServerCertificateScope
newServerCertificateScope =
  ServerCertificateScope'
    { destinationPorts =
        Prelude.Nothing,
      destinations = Prelude.Nothing,
      protocols = Prelude.Nothing,
      sourcePorts = Prelude.Nothing,
      sources = Prelude.Nothing
    }

-- | The destination ports to decrypt for inspection, in Transmission Control
-- Protocol (TCP) format. If not specified, this matches with any
-- destination port.
--
-- You can specify individual ports, for example @1994@, and you can
-- specify port ranges, such as @1990:1994@.
serverCertificateScope_destinationPorts :: Lens.Lens' ServerCertificateScope (Prelude.Maybe [PortRange])
serverCertificateScope_destinationPorts = Lens.lens (\ServerCertificateScope' {destinationPorts} -> destinationPorts) (\s@ServerCertificateScope' {} a -> s {destinationPorts = a} :: ServerCertificateScope) Prelude.. Lens.mapping Lens.coerced

-- | The destination IP addresses and address ranges to decrypt for
-- inspection, in CIDR notation. If not specified, this matches with any
-- destination address.
serverCertificateScope_destinations :: Lens.Lens' ServerCertificateScope (Prelude.Maybe [Address])
serverCertificateScope_destinations = Lens.lens (\ServerCertificateScope' {destinations} -> destinations) (\s@ServerCertificateScope' {} a -> s {destinations = a} :: ServerCertificateScope) Prelude.. Lens.mapping Lens.coerced

-- | The protocols to decrypt for inspection, specified using each
-- protocol\'s assigned internet protocol number (IANA). Network Firewall
-- currently supports only TCP.
serverCertificateScope_protocols :: Lens.Lens' ServerCertificateScope (Prelude.Maybe [Prelude.Natural])
serverCertificateScope_protocols = Lens.lens (\ServerCertificateScope' {protocols} -> protocols) (\s@ServerCertificateScope' {} a -> s {protocols = a} :: ServerCertificateScope) Prelude.. Lens.mapping Lens.coerced

-- | The source ports to decrypt for inspection, in Transmission Control
-- Protocol (TCP) format. If not specified, this matches with any source
-- port.
--
-- You can specify individual ports, for example @1994@, and you can
-- specify port ranges, such as @1990:1994@.
serverCertificateScope_sourcePorts :: Lens.Lens' ServerCertificateScope (Prelude.Maybe [PortRange])
serverCertificateScope_sourcePorts = Lens.lens (\ServerCertificateScope' {sourcePorts} -> sourcePorts) (\s@ServerCertificateScope' {} a -> s {sourcePorts = a} :: ServerCertificateScope) Prelude.. Lens.mapping Lens.coerced

-- | The source IP addresses and address ranges to decrypt for inspection, in
-- CIDR notation. If not specified, this matches with any source address.
serverCertificateScope_sources :: Lens.Lens' ServerCertificateScope (Prelude.Maybe [Address])
serverCertificateScope_sources = Lens.lens (\ServerCertificateScope' {sources} -> sources) (\s@ServerCertificateScope' {} a -> s {sources = a} :: ServerCertificateScope) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ServerCertificateScope where
  parseJSON =
    Data.withObject
      "ServerCertificateScope"
      ( \x ->
          ServerCertificateScope'
            Prelude.<$> ( x
                            Data..:? "DestinationPorts"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Destinations" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Protocols" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SourcePorts" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Sources" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ServerCertificateScope where
  hashWithSalt _salt ServerCertificateScope' {..} =
    _salt
      `Prelude.hashWithSalt` destinationPorts
      `Prelude.hashWithSalt` destinations
      `Prelude.hashWithSalt` protocols
      `Prelude.hashWithSalt` sourcePorts
      `Prelude.hashWithSalt` sources

instance Prelude.NFData ServerCertificateScope where
  rnf ServerCertificateScope' {..} =
    Prelude.rnf destinationPorts
      `Prelude.seq` Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf protocols
      `Prelude.seq` Prelude.rnf sourcePorts
      `Prelude.seq` Prelude.rnf sources

instance Data.ToJSON ServerCertificateScope where
  toJSON ServerCertificateScope' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DestinationPorts" Data..=)
              Prelude.<$> destinationPorts,
            ("Destinations" Data..=) Prelude.<$> destinations,
            ("Protocols" Data..=) Prelude.<$> protocols,
            ("SourcePorts" Data..=) Prelude.<$> sourcePorts,
            ("Sources" Data..=) Prelude.<$> sources
          ]
      )
