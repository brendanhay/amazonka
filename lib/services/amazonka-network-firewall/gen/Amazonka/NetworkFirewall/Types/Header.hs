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
-- Module      : Amazonka.NetworkFirewall.Types.Header
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.Header where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types.StatefulRuleDirection
import Amazonka.NetworkFirewall.Types.StatefulRuleProtocol
import qualified Amazonka.Prelude as Prelude

-- | The basic rule criteria for Network Firewall to use to inspect packet
-- headers in stateful traffic flow inspection. Traffic flows that match
-- the criteria are a match for the corresponding StatefulRule.
--
-- /See:/ 'newHeader' smart constructor.
data Header = Header'
  { -- | The protocol to inspect for. To specify all, you can use @IP@, because
    -- all traffic on Amazon Web Services and on the internet is IP.
    protocol :: StatefulRuleProtocol,
    -- | The source IP address or address range to inspect for, in CIDR notation.
    -- To match with any address, specify @ANY@.
    --
    -- Specify an IP address or a block of IP addresses in Classless
    -- Inter-Domain Routing (CIDR) notation. Network Firewall supports all
    -- address ranges for IPv4.
    --
    -- Examples:
    --
    -- -   To configure Network Firewall to inspect for the IP address
    --     192.0.2.44, specify @192.0.2.44\/32@.
    --
    -- -   To configure Network Firewall to inspect for IP addresses from
    --     192.0.2.0 to 192.0.2.255, specify @192.0.2.0\/24@.
    --
    -- For more information about CIDR notation, see the Wikipedia entry
    -- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing>.
    source :: Prelude.Text,
    -- | The source port to inspect for. You can specify an individual port, for
    -- example @1994@ and you can specify a port range, for example
    -- @1990:1994@. To match with any port, specify @ANY@.
    sourcePort :: Prelude.Text,
    -- | The direction of traffic flow to inspect. If set to @ANY@, the
    -- inspection matches bidirectional traffic, both from the source to the
    -- destination and from the destination to the source. If set to @FORWARD@,
    -- the inspection only matches traffic going from the source to the
    -- destination.
    direction :: StatefulRuleDirection,
    -- | The destination IP address or address range to inspect for, in CIDR
    -- notation. To match with any address, specify @ANY@.
    --
    -- Specify an IP address or a block of IP addresses in Classless
    -- Inter-Domain Routing (CIDR) notation. Network Firewall supports all
    -- address ranges for IPv4.
    --
    -- Examples:
    --
    -- -   To configure Network Firewall to inspect for the IP address
    --     192.0.2.44, specify @192.0.2.44\/32@.
    --
    -- -   To configure Network Firewall to inspect for IP addresses from
    --     192.0.2.0 to 192.0.2.255, specify @192.0.2.0\/24@.
    --
    -- For more information about CIDR notation, see the Wikipedia entry
    -- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing>.
    destination :: Prelude.Text,
    -- | The destination port to inspect for. You can specify an individual port,
    -- for example @1994@ and you can specify a port range, for example
    -- @1990:1994@. To match with any port, specify @ANY@.
    destinationPort :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Header' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protocol', 'header_protocol' - The protocol to inspect for. To specify all, you can use @IP@, because
-- all traffic on Amazon Web Services and on the internet is IP.
--
-- 'source', 'header_source' - The source IP address or address range to inspect for, in CIDR notation.
-- To match with any address, specify @ANY@.
--
-- Specify an IP address or a block of IP addresses in Classless
-- Inter-Domain Routing (CIDR) notation. Network Firewall supports all
-- address ranges for IPv4.
--
-- Examples:
--
-- -   To configure Network Firewall to inspect for the IP address
--     192.0.2.44, specify @192.0.2.44\/32@.
--
-- -   To configure Network Firewall to inspect for IP addresses from
--     192.0.2.0 to 192.0.2.255, specify @192.0.2.0\/24@.
--
-- For more information about CIDR notation, see the Wikipedia entry
-- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing>.
--
-- 'sourcePort', 'header_sourcePort' - The source port to inspect for. You can specify an individual port, for
-- example @1994@ and you can specify a port range, for example
-- @1990:1994@. To match with any port, specify @ANY@.
--
-- 'direction', 'header_direction' - The direction of traffic flow to inspect. If set to @ANY@, the
-- inspection matches bidirectional traffic, both from the source to the
-- destination and from the destination to the source. If set to @FORWARD@,
-- the inspection only matches traffic going from the source to the
-- destination.
--
-- 'destination', 'header_destination' - The destination IP address or address range to inspect for, in CIDR
-- notation. To match with any address, specify @ANY@.
--
-- Specify an IP address or a block of IP addresses in Classless
-- Inter-Domain Routing (CIDR) notation. Network Firewall supports all
-- address ranges for IPv4.
--
-- Examples:
--
-- -   To configure Network Firewall to inspect for the IP address
--     192.0.2.44, specify @192.0.2.44\/32@.
--
-- -   To configure Network Firewall to inspect for IP addresses from
--     192.0.2.0 to 192.0.2.255, specify @192.0.2.0\/24@.
--
-- For more information about CIDR notation, see the Wikipedia entry
-- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing>.
--
-- 'destinationPort', 'header_destinationPort' - The destination port to inspect for. You can specify an individual port,
-- for example @1994@ and you can specify a port range, for example
-- @1990:1994@. To match with any port, specify @ANY@.
newHeader ::
  -- | 'protocol'
  StatefulRuleProtocol ->
  -- | 'source'
  Prelude.Text ->
  -- | 'sourcePort'
  Prelude.Text ->
  -- | 'direction'
  StatefulRuleDirection ->
  -- | 'destination'
  Prelude.Text ->
  -- | 'destinationPort'
  Prelude.Text ->
  Header
newHeader
  pProtocol_
  pSource_
  pSourcePort_
  pDirection_
  pDestination_
  pDestinationPort_ =
    Header'
      { protocol = pProtocol_,
        source = pSource_,
        sourcePort = pSourcePort_,
        direction = pDirection_,
        destination = pDestination_,
        destinationPort = pDestinationPort_
      }

-- | The protocol to inspect for. To specify all, you can use @IP@, because
-- all traffic on Amazon Web Services and on the internet is IP.
header_protocol :: Lens.Lens' Header StatefulRuleProtocol
header_protocol = Lens.lens (\Header' {protocol} -> protocol) (\s@Header' {} a -> s {protocol = a} :: Header)

-- | The source IP address or address range to inspect for, in CIDR notation.
-- To match with any address, specify @ANY@.
--
-- Specify an IP address or a block of IP addresses in Classless
-- Inter-Domain Routing (CIDR) notation. Network Firewall supports all
-- address ranges for IPv4.
--
-- Examples:
--
-- -   To configure Network Firewall to inspect for the IP address
--     192.0.2.44, specify @192.0.2.44\/32@.
--
-- -   To configure Network Firewall to inspect for IP addresses from
--     192.0.2.0 to 192.0.2.255, specify @192.0.2.0\/24@.
--
-- For more information about CIDR notation, see the Wikipedia entry
-- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing>.
header_source :: Lens.Lens' Header Prelude.Text
header_source = Lens.lens (\Header' {source} -> source) (\s@Header' {} a -> s {source = a} :: Header)

-- | The source port to inspect for. You can specify an individual port, for
-- example @1994@ and you can specify a port range, for example
-- @1990:1994@. To match with any port, specify @ANY@.
header_sourcePort :: Lens.Lens' Header Prelude.Text
header_sourcePort = Lens.lens (\Header' {sourcePort} -> sourcePort) (\s@Header' {} a -> s {sourcePort = a} :: Header)

-- | The direction of traffic flow to inspect. If set to @ANY@, the
-- inspection matches bidirectional traffic, both from the source to the
-- destination and from the destination to the source. If set to @FORWARD@,
-- the inspection only matches traffic going from the source to the
-- destination.
header_direction :: Lens.Lens' Header StatefulRuleDirection
header_direction = Lens.lens (\Header' {direction} -> direction) (\s@Header' {} a -> s {direction = a} :: Header)

-- | The destination IP address or address range to inspect for, in CIDR
-- notation. To match with any address, specify @ANY@.
--
-- Specify an IP address or a block of IP addresses in Classless
-- Inter-Domain Routing (CIDR) notation. Network Firewall supports all
-- address ranges for IPv4.
--
-- Examples:
--
-- -   To configure Network Firewall to inspect for the IP address
--     192.0.2.44, specify @192.0.2.44\/32@.
--
-- -   To configure Network Firewall to inspect for IP addresses from
--     192.0.2.0 to 192.0.2.255, specify @192.0.2.0\/24@.
--
-- For more information about CIDR notation, see the Wikipedia entry
-- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing>.
header_destination :: Lens.Lens' Header Prelude.Text
header_destination = Lens.lens (\Header' {destination} -> destination) (\s@Header' {} a -> s {destination = a} :: Header)

-- | The destination port to inspect for. You can specify an individual port,
-- for example @1994@ and you can specify a port range, for example
-- @1990:1994@. To match with any port, specify @ANY@.
header_destinationPort :: Lens.Lens' Header Prelude.Text
header_destinationPort = Lens.lens (\Header' {destinationPort} -> destinationPort) (\s@Header' {} a -> s {destinationPort = a} :: Header)

instance Data.FromJSON Header where
  parseJSON =
    Data.withObject
      "Header"
      ( \x ->
          Header'
            Prelude.<$> (x Data..: "Protocol")
            Prelude.<*> (x Data..: "Source")
            Prelude.<*> (x Data..: "SourcePort")
            Prelude.<*> (x Data..: "Direction")
            Prelude.<*> (x Data..: "Destination")
            Prelude.<*> (x Data..: "DestinationPort")
      )

instance Prelude.Hashable Header where
  hashWithSalt _salt Header' {..} =
    _salt
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` sourcePort
      `Prelude.hashWithSalt` direction
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` destinationPort

instance Prelude.NFData Header where
  rnf Header' {..} =
    Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf sourcePort
      `Prelude.seq` Prelude.rnf direction
      `Prelude.seq` Prelude.rnf destination
      `Prelude.seq` Prelude.rnf destinationPort

instance Data.ToJSON Header where
  toJSON Header' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Protocol" Data..= protocol),
            Prelude.Just ("Source" Data..= source),
            Prelude.Just ("SourcePort" Data..= sourcePort),
            Prelude.Just ("Direction" Data..= direction),
            Prelude.Just ("Destination" Data..= destination),
            Prelude.Just
              ("DestinationPort" Data..= destinationPort)
          ]
      )
