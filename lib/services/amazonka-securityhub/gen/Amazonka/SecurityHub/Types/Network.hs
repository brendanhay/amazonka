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
-- Module      : Amazonka.SecurityHub.Types.Network
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.Network where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.NetworkDirection
import Amazonka.SecurityHub.Types.PortRange

-- | The details of network-related information about a finding.
--
-- /See:/ 'newNetwork' smart constructor.
data Network = Network'
  { -- | The destination domain of network-related information about a finding.
    destinationDomain :: Prelude.Maybe Prelude.Text,
    -- | The source port of network-related information about a finding.
    sourcePort :: Prelude.Maybe Prelude.Int,
    -- | The range of open ports that is present on the network.
    openPortRange :: Prelude.Maybe PortRange,
    -- | The source media access control (MAC) address of network-related
    -- information about a finding.
    sourceMac :: Prelude.Maybe Prelude.Text,
    -- | The direction of network traffic associated with a finding.
    direction :: Prelude.Maybe NetworkDirection,
    -- | The destination IPv6 address of network-related information about a
    -- finding.
    destinationIpV6 :: Prelude.Maybe Prelude.Text,
    -- | The protocol of network-related information about a finding.
    protocol :: Prelude.Maybe Prelude.Text,
    -- | The source IPv6 address of network-related information about a finding.
    sourceIpV6 :: Prelude.Maybe Prelude.Text,
    -- | The destination IPv4 address of network-related information about a
    -- finding.
    destinationIpV4 :: Prelude.Maybe Prelude.Text,
    -- | The source domain of network-related information about a finding.
    sourceDomain :: Prelude.Maybe Prelude.Text,
    -- | The destination port of network-related information about a finding.
    destinationPort :: Prelude.Maybe Prelude.Int,
    -- | The source IPv4 address of network-related information about a finding.
    sourceIpV4 :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Network' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationDomain', 'network_destinationDomain' - The destination domain of network-related information about a finding.
--
-- 'sourcePort', 'network_sourcePort' - The source port of network-related information about a finding.
--
-- 'openPortRange', 'network_openPortRange' - The range of open ports that is present on the network.
--
-- 'sourceMac', 'network_sourceMac' - The source media access control (MAC) address of network-related
-- information about a finding.
--
-- 'direction', 'network_direction' - The direction of network traffic associated with a finding.
--
-- 'destinationIpV6', 'network_destinationIpV6' - The destination IPv6 address of network-related information about a
-- finding.
--
-- 'protocol', 'network_protocol' - The protocol of network-related information about a finding.
--
-- 'sourceIpV6', 'network_sourceIpV6' - The source IPv6 address of network-related information about a finding.
--
-- 'destinationIpV4', 'network_destinationIpV4' - The destination IPv4 address of network-related information about a
-- finding.
--
-- 'sourceDomain', 'network_sourceDomain' - The source domain of network-related information about a finding.
--
-- 'destinationPort', 'network_destinationPort' - The destination port of network-related information about a finding.
--
-- 'sourceIpV4', 'network_sourceIpV4' - The source IPv4 address of network-related information about a finding.
newNetwork ::
  Network
newNetwork =
  Network'
    { destinationDomain = Prelude.Nothing,
      sourcePort = Prelude.Nothing,
      openPortRange = Prelude.Nothing,
      sourceMac = Prelude.Nothing,
      direction = Prelude.Nothing,
      destinationIpV6 = Prelude.Nothing,
      protocol = Prelude.Nothing,
      sourceIpV6 = Prelude.Nothing,
      destinationIpV4 = Prelude.Nothing,
      sourceDomain = Prelude.Nothing,
      destinationPort = Prelude.Nothing,
      sourceIpV4 = Prelude.Nothing
    }

-- | The destination domain of network-related information about a finding.
network_destinationDomain :: Lens.Lens' Network (Prelude.Maybe Prelude.Text)
network_destinationDomain = Lens.lens (\Network' {destinationDomain} -> destinationDomain) (\s@Network' {} a -> s {destinationDomain = a} :: Network)

-- | The source port of network-related information about a finding.
network_sourcePort :: Lens.Lens' Network (Prelude.Maybe Prelude.Int)
network_sourcePort = Lens.lens (\Network' {sourcePort} -> sourcePort) (\s@Network' {} a -> s {sourcePort = a} :: Network)

-- | The range of open ports that is present on the network.
network_openPortRange :: Lens.Lens' Network (Prelude.Maybe PortRange)
network_openPortRange = Lens.lens (\Network' {openPortRange} -> openPortRange) (\s@Network' {} a -> s {openPortRange = a} :: Network)

-- | The source media access control (MAC) address of network-related
-- information about a finding.
network_sourceMac :: Lens.Lens' Network (Prelude.Maybe Prelude.Text)
network_sourceMac = Lens.lens (\Network' {sourceMac} -> sourceMac) (\s@Network' {} a -> s {sourceMac = a} :: Network)

-- | The direction of network traffic associated with a finding.
network_direction :: Lens.Lens' Network (Prelude.Maybe NetworkDirection)
network_direction = Lens.lens (\Network' {direction} -> direction) (\s@Network' {} a -> s {direction = a} :: Network)

-- | The destination IPv6 address of network-related information about a
-- finding.
network_destinationIpV6 :: Lens.Lens' Network (Prelude.Maybe Prelude.Text)
network_destinationIpV6 = Lens.lens (\Network' {destinationIpV6} -> destinationIpV6) (\s@Network' {} a -> s {destinationIpV6 = a} :: Network)

-- | The protocol of network-related information about a finding.
network_protocol :: Lens.Lens' Network (Prelude.Maybe Prelude.Text)
network_protocol = Lens.lens (\Network' {protocol} -> protocol) (\s@Network' {} a -> s {protocol = a} :: Network)

-- | The source IPv6 address of network-related information about a finding.
network_sourceIpV6 :: Lens.Lens' Network (Prelude.Maybe Prelude.Text)
network_sourceIpV6 = Lens.lens (\Network' {sourceIpV6} -> sourceIpV6) (\s@Network' {} a -> s {sourceIpV6 = a} :: Network)

-- | The destination IPv4 address of network-related information about a
-- finding.
network_destinationIpV4 :: Lens.Lens' Network (Prelude.Maybe Prelude.Text)
network_destinationIpV4 = Lens.lens (\Network' {destinationIpV4} -> destinationIpV4) (\s@Network' {} a -> s {destinationIpV4 = a} :: Network)

-- | The source domain of network-related information about a finding.
network_sourceDomain :: Lens.Lens' Network (Prelude.Maybe Prelude.Text)
network_sourceDomain = Lens.lens (\Network' {sourceDomain} -> sourceDomain) (\s@Network' {} a -> s {sourceDomain = a} :: Network)

-- | The destination port of network-related information about a finding.
network_destinationPort :: Lens.Lens' Network (Prelude.Maybe Prelude.Int)
network_destinationPort = Lens.lens (\Network' {destinationPort} -> destinationPort) (\s@Network' {} a -> s {destinationPort = a} :: Network)

-- | The source IPv4 address of network-related information about a finding.
network_sourceIpV4 :: Lens.Lens' Network (Prelude.Maybe Prelude.Text)
network_sourceIpV4 = Lens.lens (\Network' {sourceIpV4} -> sourceIpV4) (\s@Network' {} a -> s {sourceIpV4 = a} :: Network)

instance Core.FromJSON Network where
  parseJSON =
    Core.withObject
      "Network"
      ( \x ->
          Network'
            Prelude.<$> (x Core..:? "DestinationDomain")
            Prelude.<*> (x Core..:? "SourcePort")
            Prelude.<*> (x Core..:? "OpenPortRange")
            Prelude.<*> (x Core..:? "SourceMac")
            Prelude.<*> (x Core..:? "Direction")
            Prelude.<*> (x Core..:? "DestinationIpV6")
            Prelude.<*> (x Core..:? "Protocol")
            Prelude.<*> (x Core..:? "SourceIpV6")
            Prelude.<*> (x Core..:? "DestinationIpV4")
            Prelude.<*> (x Core..:? "SourceDomain")
            Prelude.<*> (x Core..:? "DestinationPort")
            Prelude.<*> (x Core..:? "SourceIpV4")
      )

instance Prelude.Hashable Network where
  hashWithSalt _salt Network' {..} =
    _salt `Prelude.hashWithSalt` destinationDomain
      `Prelude.hashWithSalt` sourcePort
      `Prelude.hashWithSalt` openPortRange
      `Prelude.hashWithSalt` sourceMac
      `Prelude.hashWithSalt` direction
      `Prelude.hashWithSalt` destinationIpV6
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` sourceIpV6
      `Prelude.hashWithSalt` destinationIpV4
      `Prelude.hashWithSalt` sourceDomain
      `Prelude.hashWithSalt` destinationPort
      `Prelude.hashWithSalt` sourceIpV4

instance Prelude.NFData Network where
  rnf Network' {..} =
    Prelude.rnf destinationDomain
      `Prelude.seq` Prelude.rnf sourcePort
      `Prelude.seq` Prelude.rnf openPortRange
      `Prelude.seq` Prelude.rnf sourceMac
      `Prelude.seq` Prelude.rnf direction
      `Prelude.seq` Prelude.rnf destinationIpV6
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf sourceIpV6
      `Prelude.seq` Prelude.rnf destinationIpV4
      `Prelude.seq` Prelude.rnf sourceDomain
      `Prelude.seq` Prelude.rnf destinationPort
      `Prelude.seq` Prelude.rnf sourceIpV4

instance Core.ToJSON Network where
  toJSON Network' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DestinationDomain" Core..=)
              Prelude.<$> destinationDomain,
            ("SourcePort" Core..=) Prelude.<$> sourcePort,
            ("OpenPortRange" Core..=) Prelude.<$> openPortRange,
            ("SourceMac" Core..=) Prelude.<$> sourceMac,
            ("Direction" Core..=) Prelude.<$> direction,
            ("DestinationIpV6" Core..=)
              Prelude.<$> destinationIpV6,
            ("Protocol" Core..=) Prelude.<$> protocol,
            ("SourceIpV6" Core..=) Prelude.<$> sourceIpV6,
            ("DestinationIpV4" Core..=)
              Prelude.<$> destinationIpV4,
            ("SourceDomain" Core..=) Prelude.<$> sourceDomain,
            ("DestinationPort" Core..=)
              Prelude.<$> destinationPort,
            ("SourceIpV4" Core..=) Prelude.<$> sourceIpV4
          ]
      )
