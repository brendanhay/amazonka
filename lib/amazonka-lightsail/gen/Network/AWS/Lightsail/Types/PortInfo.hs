-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.PortInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.PortInfo
  ( PortInfo (..),

    -- * Smart constructor
    mkPortInfo,

    -- * Lenses
    piFromPort,
    piCidrs,
    piProtocol,
    piCidrListAliases,
    piToPort,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.NetworkProtocol
import qualified Network.AWS.Prelude as Lude

-- | Describes ports to open on an instance, the IP addresses allowed to connect to the instance through the ports, and the protocol.
--
-- /See:/ 'mkPortInfo' smart constructor.
data PortInfo = PortInfo'
  { fromPort :: Lude.Maybe Lude.Int,
    cidrs :: Lude.Maybe [Lude.Text],
    protocol :: Lude.Maybe NetworkProtocol,
    cidrListAliases :: Lude.Maybe [Lude.Text],
    toPort :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PortInfo' with the minimum fields required to make a request.
--
-- * 'cidrListAliases' - An alias that defines access for a preconfigured range of IP addresses.
--
-- The only alias currently supported is @lightsail-connect@ , which allows IP addresses of the browser-based RDP/SSH client in the Lightsail console to connect to your instance.
-- * 'cidrs' - The IP address, or range of IP addresses in CIDR notation, that are allowed to connect to an instance through the ports, and the protocol. Lightsail supports IPv4 addresses.
--
-- Examples:
--
--     * To allow the IP address @192.0.2.44@ , specify @192.0.2.44@ or @192.0.2.44/32@ .
--
--
--     * To allow the IP addresses @192.0.2.0@ to @192.0.2.255@ , specify @192.0.2.0/24@ .
--
--
-- For more information about CIDR block notation, see <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing#CIDR_notation Classless Inter-Domain Routing> on /Wikipedia/ .
-- * 'fromPort' - The first port in a range of open ports on an instance.
--
-- Allowed ports:
--
--     * TCP and UDP - @0@ to @65535@
--
--
--     * ICMP - The ICMP type. For example, specify @8@ as the @fromPort@ (ICMP type), and @-1@ as the @toPort@ (ICMP code), to enable ICMP Ping. For more information, see <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Control_messages Control Messages> on /Wikipedia/ .
--
--
-- * 'protocol' - The IP protocol name.
--
-- The name can be one of the following:
--
--     * @tcp@ - Transmission Control Protocol (TCP) provides reliable, ordered, and error-checked delivery of streamed data between applications running on hosts communicating by an IP network. If you have an application that doesn't require reliable data stream service, use UDP instead.
--
--
--     * @all@ - All transport layer protocol types. For more general information, see <https://en.wikipedia.org/wiki/Transport_layer Transport layer> on /Wikipedia/ .
--
--
--     * @udp@ - With User Datagram Protocol (UDP), computer applications can send messages (or datagrams) to other hosts on an Internet Protocol (IP) network. Prior communications are not required to set up transmission channels or data paths. Applications that don't require reliable data stream service can use UDP, which provides a connectionless datagram service that emphasizes reduced latency over reliability. If you do require reliable data stream service, use TCP instead.
--
--
--     * @icmp@ - Internet Control Message Protocol (ICMP) is used to send error messages and operational information indicating success or failure when communicating with an instance. For example, an error is indicated when an instance could not be reached. When you specify @icmp@ as the @protocol@ , you must specify the ICMP type using the @fromPort@ parameter, and ICMP code using the @toPort@ parameter.
--
--
-- * 'toPort' - The last port in a range of open ports on an instance.
--
-- Allowed ports:
--
--     * TCP and UDP - @0@ to @65535@
--
--
--     * ICMP - The ICMP code. For example, specify @8@ as the @fromPort@ (ICMP type), and @-1@ as the @toPort@ (ICMP code), to enable ICMP Ping. For more information, see <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Control_messages Control Messages> on /Wikipedia/ .
mkPortInfo ::
  PortInfo
mkPortInfo =
  PortInfo'
    { fromPort = Lude.Nothing,
      cidrs = Lude.Nothing,
      protocol = Lude.Nothing,
      cidrListAliases = Lude.Nothing,
      toPort = Lude.Nothing
    }

-- | The first port in a range of open ports on an instance.
--
-- Allowed ports:
--
--     * TCP and UDP - @0@ to @65535@
--
--
--     * ICMP - The ICMP type. For example, specify @8@ as the @fromPort@ (ICMP type), and @-1@ as the @toPort@ (ICMP code), to enable ICMP Ping. For more information, see <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Control_messages Control Messages> on /Wikipedia/ .
--
--
--
-- /Note:/ Consider using 'fromPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piFromPort :: Lens.Lens' PortInfo (Lude.Maybe Lude.Int)
piFromPort = Lens.lens (fromPort :: PortInfo -> Lude.Maybe Lude.Int) (\s a -> s {fromPort = a} :: PortInfo)
{-# DEPRECATED piFromPort "Use generic-lens or generic-optics with 'fromPort' instead." #-}

-- | The IP address, or range of IP addresses in CIDR notation, that are allowed to connect to an instance through the ports, and the protocol. Lightsail supports IPv4 addresses.
--
-- Examples:
--
--     * To allow the IP address @192.0.2.44@ , specify @192.0.2.44@ or @192.0.2.44/32@ .
--
--
--     * To allow the IP addresses @192.0.2.0@ to @192.0.2.255@ , specify @192.0.2.0/24@ .
--
--
-- For more information about CIDR block notation, see <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing#CIDR_notation Classless Inter-Domain Routing> on /Wikipedia/ .
--
-- /Note:/ Consider using 'cidrs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piCidrs :: Lens.Lens' PortInfo (Lude.Maybe [Lude.Text])
piCidrs = Lens.lens (cidrs :: PortInfo -> Lude.Maybe [Lude.Text]) (\s a -> s {cidrs = a} :: PortInfo)
{-# DEPRECATED piCidrs "Use generic-lens or generic-optics with 'cidrs' instead." #-}

-- | The IP protocol name.
--
-- The name can be one of the following:
--
--     * @tcp@ - Transmission Control Protocol (TCP) provides reliable, ordered, and error-checked delivery of streamed data between applications running on hosts communicating by an IP network. If you have an application that doesn't require reliable data stream service, use UDP instead.
--
--
--     * @all@ - All transport layer protocol types. For more general information, see <https://en.wikipedia.org/wiki/Transport_layer Transport layer> on /Wikipedia/ .
--
--
--     * @udp@ - With User Datagram Protocol (UDP), computer applications can send messages (or datagrams) to other hosts on an Internet Protocol (IP) network. Prior communications are not required to set up transmission channels or data paths. Applications that don't require reliable data stream service can use UDP, which provides a connectionless datagram service that emphasizes reduced latency over reliability. If you do require reliable data stream service, use TCP instead.
--
--
--     * @icmp@ - Internet Control Message Protocol (ICMP) is used to send error messages and operational information indicating success or failure when communicating with an instance. For example, an error is indicated when an instance could not be reached. When you specify @icmp@ as the @protocol@ , you must specify the ICMP type using the @fromPort@ parameter, and ICMP code using the @toPort@ parameter.
--
--
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piProtocol :: Lens.Lens' PortInfo (Lude.Maybe NetworkProtocol)
piProtocol = Lens.lens (protocol :: PortInfo -> Lude.Maybe NetworkProtocol) (\s a -> s {protocol = a} :: PortInfo)
{-# DEPRECATED piProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | An alias that defines access for a preconfigured range of IP addresses.
--
-- The only alias currently supported is @lightsail-connect@ , which allows IP addresses of the browser-based RDP/SSH client in the Lightsail console to connect to your instance.
--
-- /Note:/ Consider using 'cidrListAliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piCidrListAliases :: Lens.Lens' PortInfo (Lude.Maybe [Lude.Text])
piCidrListAliases = Lens.lens (cidrListAliases :: PortInfo -> Lude.Maybe [Lude.Text]) (\s a -> s {cidrListAliases = a} :: PortInfo)
{-# DEPRECATED piCidrListAliases "Use generic-lens or generic-optics with 'cidrListAliases' instead." #-}

-- | The last port in a range of open ports on an instance.
--
-- Allowed ports:
--
--     * TCP and UDP - @0@ to @65535@
--
--
--     * ICMP - The ICMP code. For example, specify @8@ as the @fromPort@ (ICMP type), and @-1@ as the @toPort@ (ICMP code), to enable ICMP Ping. For more information, see <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Control_messages Control Messages> on /Wikipedia/ .
--
--
--
-- /Note:/ Consider using 'toPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piToPort :: Lens.Lens' PortInfo (Lude.Maybe Lude.Int)
piToPort = Lens.lens (toPort :: PortInfo -> Lude.Maybe Lude.Int) (\s a -> s {toPort = a} :: PortInfo)
{-# DEPRECATED piToPort "Use generic-lens or generic-optics with 'toPort' instead." #-}

instance Lude.ToJSON PortInfo where
  toJSON PortInfo' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("fromPort" Lude..=) Lude.<$> fromPort,
            ("cidrs" Lude..=) Lude.<$> cidrs,
            ("protocol" Lude..=) Lude.<$> protocol,
            ("cidrListAliases" Lude..=) Lude.<$> cidrListAliases,
            ("toPort" Lude..=) Lude.<$> toPort
          ]
      )
