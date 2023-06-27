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
-- Module      : Amazonka.Lightsail.Types.InstancePortInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.InstancePortInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.AccessDirection
import Amazonka.Lightsail.Types.NetworkProtocol
import Amazonka.Lightsail.Types.PortAccessType
import qualified Amazonka.Prelude as Prelude

-- | Describes information about ports for an Amazon Lightsail instance.
--
-- /See:/ 'newInstancePortInfo' smart constructor.
data InstancePortInfo = InstancePortInfo'
  { -- | The access direction (@inbound@ or @outbound@).
    --
    -- Lightsail currently supports only @inbound@ access direction.
    accessDirection :: Prelude.Maybe AccessDirection,
    -- | The location from which access is allowed. For example,
    -- @Anywhere (0.0.0.0\/0)@, or @Custom@ if a specific IP address or range
    -- of IP addresses is allowed.
    accessFrom :: Prelude.Maybe Prelude.Text,
    -- | The type of access (@Public@ or @Private@).
    accessType :: Prelude.Maybe PortAccessType,
    -- | An alias that defines access for a preconfigured range of IP addresses.
    --
    -- The only alias currently supported is @lightsail-connect@, which allows
    -- IP addresses of the browser-based RDP\/SSH client in the Lightsail
    -- console to connect to your instance.
    cidrListAliases :: Prelude.Maybe [Prelude.Text],
    -- | The IPv4 address, or range of IPv4 addresses (in CIDR notation) that are
    -- allowed to connect to an instance through the ports, and the protocol.
    --
    -- The @ipv6Cidrs@ parameter lists the IPv6 addresses that are allowed to
    -- connect to an instance.
    --
    -- For more information about CIDR block notation, see
    -- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing#CIDR_notation Classless Inter-Domain Routing>
    -- on /Wikipedia/.
    cidrs :: Prelude.Maybe [Prelude.Text],
    -- | The common name of the port information.
    commonName :: Prelude.Maybe Prelude.Text,
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
    fromPort :: Prelude.Maybe Prelude.Int,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstancePortInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessDirection', 'instancePortInfo_accessDirection' - The access direction (@inbound@ or @outbound@).
--
-- Lightsail currently supports only @inbound@ access direction.
--
-- 'accessFrom', 'instancePortInfo_accessFrom' - The location from which access is allowed. For example,
-- @Anywhere (0.0.0.0\/0)@, or @Custom@ if a specific IP address or range
-- of IP addresses is allowed.
--
-- 'accessType', 'instancePortInfo_accessType' - The type of access (@Public@ or @Private@).
--
-- 'cidrListAliases', 'instancePortInfo_cidrListAliases' - An alias that defines access for a preconfigured range of IP addresses.
--
-- The only alias currently supported is @lightsail-connect@, which allows
-- IP addresses of the browser-based RDP\/SSH client in the Lightsail
-- console to connect to your instance.
--
-- 'cidrs', 'instancePortInfo_cidrs' - The IPv4 address, or range of IPv4 addresses (in CIDR notation) that are
-- allowed to connect to an instance through the ports, and the protocol.
--
-- The @ipv6Cidrs@ parameter lists the IPv6 addresses that are allowed to
-- connect to an instance.
--
-- For more information about CIDR block notation, see
-- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing#CIDR_notation Classless Inter-Domain Routing>
-- on /Wikipedia/.
--
-- 'commonName', 'instancePortInfo_commonName' - The common name of the port information.
--
-- 'fromPort', 'instancePortInfo_fromPort' - The first port in a range of open ports on an instance.
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
-- 'ipv6Cidrs', 'instancePortInfo_ipv6Cidrs' - The IPv6 address, or range of IPv6 addresses (in CIDR notation) that are
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
-- 'protocol', 'instancePortInfo_protocol' - The IP protocol name.
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
-- 'toPort', 'instancePortInfo_toPort' - The last port in a range of open ports on an instance.
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
newInstancePortInfo ::
  InstancePortInfo
newInstancePortInfo =
  InstancePortInfo'
    { accessDirection =
        Prelude.Nothing,
      accessFrom = Prelude.Nothing,
      accessType = Prelude.Nothing,
      cidrListAliases = Prelude.Nothing,
      cidrs = Prelude.Nothing,
      commonName = Prelude.Nothing,
      fromPort = Prelude.Nothing,
      ipv6Cidrs = Prelude.Nothing,
      protocol = Prelude.Nothing,
      toPort = Prelude.Nothing
    }

-- | The access direction (@inbound@ or @outbound@).
--
-- Lightsail currently supports only @inbound@ access direction.
instancePortInfo_accessDirection :: Lens.Lens' InstancePortInfo (Prelude.Maybe AccessDirection)
instancePortInfo_accessDirection = Lens.lens (\InstancePortInfo' {accessDirection} -> accessDirection) (\s@InstancePortInfo' {} a -> s {accessDirection = a} :: InstancePortInfo)

-- | The location from which access is allowed. For example,
-- @Anywhere (0.0.0.0\/0)@, or @Custom@ if a specific IP address or range
-- of IP addresses is allowed.
instancePortInfo_accessFrom :: Lens.Lens' InstancePortInfo (Prelude.Maybe Prelude.Text)
instancePortInfo_accessFrom = Lens.lens (\InstancePortInfo' {accessFrom} -> accessFrom) (\s@InstancePortInfo' {} a -> s {accessFrom = a} :: InstancePortInfo)

-- | The type of access (@Public@ or @Private@).
instancePortInfo_accessType :: Lens.Lens' InstancePortInfo (Prelude.Maybe PortAccessType)
instancePortInfo_accessType = Lens.lens (\InstancePortInfo' {accessType} -> accessType) (\s@InstancePortInfo' {} a -> s {accessType = a} :: InstancePortInfo)

-- | An alias that defines access for a preconfigured range of IP addresses.
--
-- The only alias currently supported is @lightsail-connect@, which allows
-- IP addresses of the browser-based RDP\/SSH client in the Lightsail
-- console to connect to your instance.
instancePortInfo_cidrListAliases :: Lens.Lens' InstancePortInfo (Prelude.Maybe [Prelude.Text])
instancePortInfo_cidrListAliases = Lens.lens (\InstancePortInfo' {cidrListAliases} -> cidrListAliases) (\s@InstancePortInfo' {} a -> s {cidrListAliases = a} :: InstancePortInfo) Prelude.. Lens.mapping Lens.coerced

-- | The IPv4 address, or range of IPv4 addresses (in CIDR notation) that are
-- allowed to connect to an instance through the ports, and the protocol.
--
-- The @ipv6Cidrs@ parameter lists the IPv6 addresses that are allowed to
-- connect to an instance.
--
-- For more information about CIDR block notation, see
-- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing#CIDR_notation Classless Inter-Domain Routing>
-- on /Wikipedia/.
instancePortInfo_cidrs :: Lens.Lens' InstancePortInfo (Prelude.Maybe [Prelude.Text])
instancePortInfo_cidrs = Lens.lens (\InstancePortInfo' {cidrs} -> cidrs) (\s@InstancePortInfo' {} a -> s {cidrs = a} :: InstancePortInfo) Prelude.. Lens.mapping Lens.coerced

-- | The common name of the port information.
instancePortInfo_commonName :: Lens.Lens' InstancePortInfo (Prelude.Maybe Prelude.Text)
instancePortInfo_commonName = Lens.lens (\InstancePortInfo' {commonName} -> commonName) (\s@InstancePortInfo' {} a -> s {commonName = a} :: InstancePortInfo)

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
instancePortInfo_fromPort :: Lens.Lens' InstancePortInfo (Prelude.Maybe Prelude.Int)
instancePortInfo_fromPort = Lens.lens (\InstancePortInfo' {fromPort} -> fromPort) (\s@InstancePortInfo' {} a -> s {fromPort = a} :: InstancePortInfo)

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
instancePortInfo_ipv6Cidrs :: Lens.Lens' InstancePortInfo (Prelude.Maybe [Prelude.Text])
instancePortInfo_ipv6Cidrs = Lens.lens (\InstancePortInfo' {ipv6Cidrs} -> ipv6Cidrs) (\s@InstancePortInfo' {} a -> s {ipv6Cidrs = a} :: InstancePortInfo) Prelude.. Lens.mapping Lens.coerced

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
instancePortInfo_protocol :: Lens.Lens' InstancePortInfo (Prelude.Maybe NetworkProtocol)
instancePortInfo_protocol = Lens.lens (\InstancePortInfo' {protocol} -> protocol) (\s@InstancePortInfo' {} a -> s {protocol = a} :: InstancePortInfo)

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
instancePortInfo_toPort :: Lens.Lens' InstancePortInfo (Prelude.Maybe Prelude.Int)
instancePortInfo_toPort = Lens.lens (\InstancePortInfo' {toPort} -> toPort) (\s@InstancePortInfo' {} a -> s {toPort = a} :: InstancePortInfo)

instance Data.FromJSON InstancePortInfo where
  parseJSON =
    Data.withObject
      "InstancePortInfo"
      ( \x ->
          InstancePortInfo'
            Prelude.<$> (x Data..:? "accessDirection")
            Prelude.<*> (x Data..:? "accessFrom")
            Prelude.<*> (x Data..:? "accessType")
            Prelude.<*> ( x
                            Data..:? "cidrListAliases"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "cidrs" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "commonName")
            Prelude.<*> (x Data..:? "fromPort")
            Prelude.<*> (x Data..:? "ipv6Cidrs" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "protocol")
            Prelude.<*> (x Data..:? "toPort")
      )

instance Prelude.Hashable InstancePortInfo where
  hashWithSalt _salt InstancePortInfo' {..} =
    _salt
      `Prelude.hashWithSalt` accessDirection
      `Prelude.hashWithSalt` accessFrom
      `Prelude.hashWithSalt` accessType
      `Prelude.hashWithSalt` cidrListAliases
      `Prelude.hashWithSalt` cidrs
      `Prelude.hashWithSalt` commonName
      `Prelude.hashWithSalt` fromPort
      `Prelude.hashWithSalt` ipv6Cidrs
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` toPort

instance Prelude.NFData InstancePortInfo where
  rnf InstancePortInfo' {..} =
    Prelude.rnf accessDirection
      `Prelude.seq` Prelude.rnf accessFrom
      `Prelude.seq` Prelude.rnf accessType
      `Prelude.seq` Prelude.rnf cidrListAliases
      `Prelude.seq` Prelude.rnf cidrs
      `Prelude.seq` Prelude.rnf commonName
      `Prelude.seq` Prelude.rnf fromPort
      `Prelude.seq` Prelude.rnf ipv6Cidrs
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf toPort
