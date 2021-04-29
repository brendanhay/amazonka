{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Lightsail.Types.PortInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.PortInfo where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.NetworkProtocol
import qualified Network.AWS.Prelude as Prelude

-- | Describes ports to open on an instance, the IP addresses allowed to
-- connect to the instance through the ports, and the protocol.
--
-- /See:/ 'newPortInfo' smart constructor.
data PortInfo = PortInfo'
  { -- | The first port in a range of open ports on an instance.
    --
    -- Allowed ports:
    --
    -- -   TCP and UDP - @0@ to @65535@
    --
    -- -   ICMP - The ICMP type for IPv4 addresses. For example, specify @8@ as
    --     the @fromPort@ (ICMP type), and @-1@ as the @toPort@ (ICMP code), to
    --     enable ICMP Ping. For more information, see
    --     <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Control_messages Control Messages>
    --     on /Wikipedia/.
    --
    -- -   ICMPv6 - The ICMP type for IPv6 addresses. For example, specify
    --     @128@ as the @fromPort@ (ICMPv6 type), and @0@ as @toPort@ (ICMPv6
    --     code). For more information, see
    --     <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol_for_IPv6 Internet Control Message Protocol for IPv6>.
    fromPort :: Prelude.Maybe Prelude.Int,
    -- | An alias that defines access for a preconfigured range of IP addresses.
    --
    -- The only alias currently supported is @lightsail-connect@, which allows
    -- IP addresses of the browser-based RDP\/SSH client in the Lightsail
    -- console to connect to your instance.
    cidrListAliases :: Prelude.Maybe [Prelude.Text],
    -- | The IPv6 address, or range of IPv6 addresses (in CIDR notation) that are
    -- allowed to connect to an instance through the ports, and the protocol.
    -- Only devices with an IPv6 address can connect to an instance through
    -- IPv6; otherwise, IPv4 should be used.
    --
    -- The @cidrs@ parameter lists the IPv4 addresses that are allowed to
    -- connect to an instance.
    --
    -- For more information about CIDR block notation, see
    -- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing#CIDR_notation Classless Inter-Domain Routing>
    -- on /Wikipedia/.
    ipv6Cidrs :: Prelude.Maybe [Prelude.Text],
    -- | The IPv4 address, or range of IPv4 addresses (in CIDR notation) that are
    -- allowed to connect to an instance through the ports, and the protocol.
    --
    -- The @ipv6Cidrs@ parameter lists the IPv6 addresses that are allowed to
    -- connect to an instance.
    --
    -- Examples:
    --
    -- -   To allow the IP address @192.0.2.44@, specify @192.0.2.44@ or
    --     @192.0.2.44\/32@.
    --
    -- -   To allow the IP addresses @192.0.2.0@ to @192.0.2.255@, specify
    --     @192.0.2.0\/24@.
    --
    -- For more information about CIDR block notation, see
    -- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing#CIDR_notation Classless Inter-Domain Routing>
    -- on /Wikipedia/.
    cidrs :: Prelude.Maybe [Prelude.Text],
    -- | The IP protocol name.
    --
    -- The name can be one of the following:
    --
    -- -   @tcp@ - Transmission Control Protocol (TCP) provides reliable,
    --     ordered, and error-checked delivery of streamed data between
    --     applications running on hosts communicating by an IP network. If you
    --     have an application that doesn\'t require reliable data stream
    --     service, use UDP instead.
    --
    -- -   @all@ - All transport layer protocol types. For more general
    --     information, see
    --     <https://en.wikipedia.org/wiki/Transport_layer Transport layer> on
    --     /Wikipedia/.
    --
    -- -   @udp@ - With User Datagram Protocol (UDP), computer applications can
    --     send messages (or datagrams) to other hosts on an Internet Protocol
    --     (IP) network. Prior communications are not required to set up
    --     transmission channels or data paths. Applications that don\'t
    --     require reliable data stream service can use UDP, which provides a
    --     connectionless datagram service that emphasizes reduced latency over
    --     reliability. If you do require reliable data stream service, use TCP
    --     instead.
    --
    -- -   @icmp@ - Internet Control Message Protocol (ICMP) is used to send
    --     error messages and operational information indicating success or
    --     failure when communicating with an instance. For example, an error
    --     is indicated when an instance could not be reached. When you specify
    --     @icmp@ as the @protocol@, you must specify the ICMP type using the
    --     @fromPort@ parameter, and ICMP code using the @toPort@ parameter.
    protocol :: Prelude.Maybe NetworkProtocol,
    -- | The last port in a range of open ports on an instance.
    --
    -- Allowed ports:
    --
    -- -   TCP and UDP - @0@ to @65535@
    --
    -- -   ICMP - The ICMP code for IPv4 addresses. For example, specify @8@ as
    --     the @fromPort@ (ICMP type), and @-1@ as the @toPort@ (ICMP code), to
    --     enable ICMP Ping. For more information, see
    --     <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Control_messages Control Messages>
    --     on /Wikipedia/.
    --
    -- -   ICMPv6 - The ICMP code for IPv6 addresses. For example, specify
    --     @128@ as the @fromPort@ (ICMPv6 type), and @0@ as @toPort@ (ICMPv6
    --     code). For more information, see
    --     <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol_for_IPv6 Internet Control Message Protocol for IPv6>.
    toPort :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PortInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromPort', 'portInfo_fromPort' - The first port in a range of open ports on an instance.
--
-- Allowed ports:
--
-- -   TCP and UDP - @0@ to @65535@
--
-- -   ICMP - The ICMP type for IPv4 addresses. For example, specify @8@ as
--     the @fromPort@ (ICMP type), and @-1@ as the @toPort@ (ICMP code), to
--     enable ICMP Ping. For more information, see
--     <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Control_messages Control Messages>
--     on /Wikipedia/.
--
-- -   ICMPv6 - The ICMP type for IPv6 addresses. For example, specify
--     @128@ as the @fromPort@ (ICMPv6 type), and @0@ as @toPort@ (ICMPv6
--     code). For more information, see
--     <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol_for_IPv6 Internet Control Message Protocol for IPv6>.
--
-- 'cidrListAliases', 'portInfo_cidrListAliases' - An alias that defines access for a preconfigured range of IP addresses.
--
-- The only alias currently supported is @lightsail-connect@, which allows
-- IP addresses of the browser-based RDP\/SSH client in the Lightsail
-- console to connect to your instance.
--
-- 'ipv6Cidrs', 'portInfo_ipv6Cidrs' - The IPv6 address, or range of IPv6 addresses (in CIDR notation) that are
-- allowed to connect to an instance through the ports, and the protocol.
-- Only devices with an IPv6 address can connect to an instance through
-- IPv6; otherwise, IPv4 should be used.
--
-- The @cidrs@ parameter lists the IPv4 addresses that are allowed to
-- connect to an instance.
--
-- For more information about CIDR block notation, see
-- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing#CIDR_notation Classless Inter-Domain Routing>
-- on /Wikipedia/.
--
-- 'cidrs', 'portInfo_cidrs' - The IPv4 address, or range of IPv4 addresses (in CIDR notation) that are
-- allowed to connect to an instance through the ports, and the protocol.
--
-- The @ipv6Cidrs@ parameter lists the IPv6 addresses that are allowed to
-- connect to an instance.
--
-- Examples:
--
-- -   To allow the IP address @192.0.2.44@, specify @192.0.2.44@ or
--     @192.0.2.44\/32@.
--
-- -   To allow the IP addresses @192.0.2.0@ to @192.0.2.255@, specify
--     @192.0.2.0\/24@.
--
-- For more information about CIDR block notation, see
-- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing#CIDR_notation Classless Inter-Domain Routing>
-- on /Wikipedia/.
--
-- 'protocol', 'portInfo_protocol' - The IP protocol name.
--
-- The name can be one of the following:
--
-- -   @tcp@ - Transmission Control Protocol (TCP) provides reliable,
--     ordered, and error-checked delivery of streamed data between
--     applications running on hosts communicating by an IP network. If you
--     have an application that doesn\'t require reliable data stream
--     service, use UDP instead.
--
-- -   @all@ - All transport layer protocol types. For more general
--     information, see
--     <https://en.wikipedia.org/wiki/Transport_layer Transport layer> on
--     /Wikipedia/.
--
-- -   @udp@ - With User Datagram Protocol (UDP), computer applications can
--     send messages (or datagrams) to other hosts on an Internet Protocol
--     (IP) network. Prior communications are not required to set up
--     transmission channels or data paths. Applications that don\'t
--     require reliable data stream service can use UDP, which provides a
--     connectionless datagram service that emphasizes reduced latency over
--     reliability. If you do require reliable data stream service, use TCP
--     instead.
--
-- -   @icmp@ - Internet Control Message Protocol (ICMP) is used to send
--     error messages and operational information indicating success or
--     failure when communicating with an instance. For example, an error
--     is indicated when an instance could not be reached. When you specify
--     @icmp@ as the @protocol@, you must specify the ICMP type using the
--     @fromPort@ parameter, and ICMP code using the @toPort@ parameter.
--
-- 'toPort', 'portInfo_toPort' - The last port in a range of open ports on an instance.
--
-- Allowed ports:
--
-- -   TCP and UDP - @0@ to @65535@
--
-- -   ICMP - The ICMP code for IPv4 addresses. For example, specify @8@ as
--     the @fromPort@ (ICMP type), and @-1@ as the @toPort@ (ICMP code), to
--     enable ICMP Ping. For more information, see
--     <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Control_messages Control Messages>
--     on /Wikipedia/.
--
-- -   ICMPv6 - The ICMP code for IPv6 addresses. For example, specify
--     @128@ as the @fromPort@ (ICMPv6 type), and @0@ as @toPort@ (ICMPv6
--     code). For more information, see
--     <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol_for_IPv6 Internet Control Message Protocol for IPv6>.
newPortInfo ::
  PortInfo
newPortInfo =
  PortInfo'
    { fromPort = Prelude.Nothing,
      cidrListAliases = Prelude.Nothing,
      ipv6Cidrs = Prelude.Nothing,
      cidrs = Prelude.Nothing,
      protocol = Prelude.Nothing,
      toPort = Prelude.Nothing
    }

-- | The first port in a range of open ports on an instance.
--
-- Allowed ports:
--
-- -   TCP and UDP - @0@ to @65535@
--
-- -   ICMP - The ICMP type for IPv4 addresses. For example, specify @8@ as
--     the @fromPort@ (ICMP type), and @-1@ as the @toPort@ (ICMP code), to
--     enable ICMP Ping. For more information, see
--     <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Control_messages Control Messages>
--     on /Wikipedia/.
--
-- -   ICMPv6 - The ICMP type for IPv6 addresses. For example, specify
--     @128@ as the @fromPort@ (ICMPv6 type), and @0@ as @toPort@ (ICMPv6
--     code). For more information, see
--     <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol_for_IPv6 Internet Control Message Protocol for IPv6>.
portInfo_fromPort :: Lens.Lens' PortInfo (Prelude.Maybe Prelude.Int)
portInfo_fromPort = Lens.lens (\PortInfo' {fromPort} -> fromPort) (\s@PortInfo' {} a -> s {fromPort = a} :: PortInfo)

-- | An alias that defines access for a preconfigured range of IP addresses.
--
-- The only alias currently supported is @lightsail-connect@, which allows
-- IP addresses of the browser-based RDP\/SSH client in the Lightsail
-- console to connect to your instance.
portInfo_cidrListAliases :: Lens.Lens' PortInfo (Prelude.Maybe [Prelude.Text])
portInfo_cidrListAliases = Lens.lens (\PortInfo' {cidrListAliases} -> cidrListAliases) (\s@PortInfo' {} a -> s {cidrListAliases = a} :: PortInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | The IPv6 address, or range of IPv6 addresses (in CIDR notation) that are
-- allowed to connect to an instance through the ports, and the protocol.
-- Only devices with an IPv6 address can connect to an instance through
-- IPv6; otherwise, IPv4 should be used.
--
-- The @cidrs@ parameter lists the IPv4 addresses that are allowed to
-- connect to an instance.
--
-- For more information about CIDR block notation, see
-- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing#CIDR_notation Classless Inter-Domain Routing>
-- on /Wikipedia/.
portInfo_ipv6Cidrs :: Lens.Lens' PortInfo (Prelude.Maybe [Prelude.Text])
portInfo_ipv6Cidrs = Lens.lens (\PortInfo' {ipv6Cidrs} -> ipv6Cidrs) (\s@PortInfo' {} a -> s {ipv6Cidrs = a} :: PortInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | The IPv4 address, or range of IPv4 addresses (in CIDR notation) that are
-- allowed to connect to an instance through the ports, and the protocol.
--
-- The @ipv6Cidrs@ parameter lists the IPv6 addresses that are allowed to
-- connect to an instance.
--
-- Examples:
--
-- -   To allow the IP address @192.0.2.44@, specify @192.0.2.44@ or
--     @192.0.2.44\/32@.
--
-- -   To allow the IP addresses @192.0.2.0@ to @192.0.2.255@, specify
--     @192.0.2.0\/24@.
--
-- For more information about CIDR block notation, see
-- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing#CIDR_notation Classless Inter-Domain Routing>
-- on /Wikipedia/.
portInfo_cidrs :: Lens.Lens' PortInfo (Prelude.Maybe [Prelude.Text])
portInfo_cidrs = Lens.lens (\PortInfo' {cidrs} -> cidrs) (\s@PortInfo' {} a -> s {cidrs = a} :: PortInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | The IP protocol name.
--
-- The name can be one of the following:
--
-- -   @tcp@ - Transmission Control Protocol (TCP) provides reliable,
--     ordered, and error-checked delivery of streamed data between
--     applications running on hosts communicating by an IP network. If you
--     have an application that doesn\'t require reliable data stream
--     service, use UDP instead.
--
-- -   @all@ - All transport layer protocol types. For more general
--     information, see
--     <https://en.wikipedia.org/wiki/Transport_layer Transport layer> on
--     /Wikipedia/.
--
-- -   @udp@ - With User Datagram Protocol (UDP), computer applications can
--     send messages (or datagrams) to other hosts on an Internet Protocol
--     (IP) network. Prior communications are not required to set up
--     transmission channels or data paths. Applications that don\'t
--     require reliable data stream service can use UDP, which provides a
--     connectionless datagram service that emphasizes reduced latency over
--     reliability. If you do require reliable data stream service, use TCP
--     instead.
--
-- -   @icmp@ - Internet Control Message Protocol (ICMP) is used to send
--     error messages and operational information indicating success or
--     failure when communicating with an instance. For example, an error
--     is indicated when an instance could not be reached. When you specify
--     @icmp@ as the @protocol@, you must specify the ICMP type using the
--     @fromPort@ parameter, and ICMP code using the @toPort@ parameter.
portInfo_protocol :: Lens.Lens' PortInfo (Prelude.Maybe NetworkProtocol)
portInfo_protocol = Lens.lens (\PortInfo' {protocol} -> protocol) (\s@PortInfo' {} a -> s {protocol = a} :: PortInfo)

-- | The last port in a range of open ports on an instance.
--
-- Allowed ports:
--
-- -   TCP and UDP - @0@ to @65535@
--
-- -   ICMP - The ICMP code for IPv4 addresses. For example, specify @8@ as
--     the @fromPort@ (ICMP type), and @-1@ as the @toPort@ (ICMP code), to
--     enable ICMP Ping. For more information, see
--     <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Control_messages Control Messages>
--     on /Wikipedia/.
--
-- -   ICMPv6 - The ICMP code for IPv6 addresses. For example, specify
--     @128@ as the @fromPort@ (ICMPv6 type), and @0@ as @toPort@ (ICMPv6
--     code). For more information, see
--     <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol_for_IPv6 Internet Control Message Protocol for IPv6>.
portInfo_toPort :: Lens.Lens' PortInfo (Prelude.Maybe Prelude.Int)
portInfo_toPort = Lens.lens (\PortInfo' {toPort} -> toPort) (\s@PortInfo' {} a -> s {toPort = a} :: PortInfo)

instance Prelude.Hashable PortInfo

instance Prelude.NFData PortInfo

instance Prelude.ToJSON PortInfo where
  toJSON PortInfo' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("fromPort" Prelude..=) Prelude.<$> fromPort,
            ("cidrListAliases" Prelude..=)
              Prelude.<$> cidrListAliases,
            ("ipv6Cidrs" Prelude..=) Prelude.<$> ipv6Cidrs,
            ("cidrs" Prelude..=) Prelude.<$> cidrs,
            ("protocol" Prelude..=) Prelude.<$> protocol,
            ("toPort" Prelude..=) Prelude.<$> toPort
          ]
      )
