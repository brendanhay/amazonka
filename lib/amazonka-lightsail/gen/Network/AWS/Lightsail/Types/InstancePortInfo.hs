{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstancePortInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstancePortInfo
  ( InstancePortInfo (..),

    -- * Smart constructor
    mkInstancePortInfo,

    -- * Lenses
    ipiFromPort,
    ipiCidrs,
    ipiCommonName,
    ipiProtocol,
    ipiCidrListAliases,
    ipiAccessDirection,
    ipiAccessType,
    ipiToPort,
    ipiAccessFrom,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.AccessDirection
import Network.AWS.Lightsail.Types.NetworkProtocol
import Network.AWS.Lightsail.Types.PortAccessType
import qualified Network.AWS.Prelude as Lude

-- | Describes information about ports for an Amazon Lightsail instance.
--
-- /See:/ 'mkInstancePortInfo' smart constructor.
data InstancePortInfo = InstancePortInfo'
  { fromPort ::
      Lude.Maybe Lude.Int,
    cidrs :: Lude.Maybe [Lude.Text],
    commonName :: Lude.Maybe Lude.Text,
    protocol :: Lude.Maybe NetworkProtocol,
    cidrListAliases :: Lude.Maybe [Lude.Text],
    accessDirection :: Lude.Maybe AccessDirection,
    accessType :: Lude.Maybe PortAccessType,
    toPort :: Lude.Maybe Lude.Int,
    accessFrom :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstancePortInfo' with the minimum fields required to make a request.
--
-- * 'accessDirection' - The access direction (@inbound@ or @outbound@ ).
-- * 'accessFrom' - The location from which access is allowed. For example, @Anywhere (0.0.0.0/0)@ , or @Custom@ if a specific IP address or range of IP addresses is allowed.
-- * 'accessType' - The type of access (@Public@ or @Private@ ).
-- * 'cidrListAliases' - An alias that defines access for a preconfigured range of IP addresses.
--
-- The only alias currently supported is @lightsail-connect@ , which allows IP addresses of the browser-based RDP/SSH client in the Lightsail console to connect to your instance.
-- * 'cidrs' - The IP address, or range of IP addresses in CIDR notation, that are allowed to connect to an instance through the ports, and the protocol. Lightsail supports IPv4 addresses.
--
-- For more information about CIDR block notation, see <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing#CIDR_notation Classless Inter-Domain Routing> on /Wikipedia/ .
-- * 'commonName' - The common name of the port information.
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
mkInstancePortInfo ::
  InstancePortInfo
mkInstancePortInfo =
  InstancePortInfo'
    { fromPort = Lude.Nothing,
      cidrs = Lude.Nothing,
      commonName = Lude.Nothing,
      protocol = Lude.Nothing,
      cidrListAliases = Lude.Nothing,
      accessDirection = Lude.Nothing,
      accessType = Lude.Nothing,
      toPort = Lude.Nothing,
      accessFrom = Lude.Nothing
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
ipiFromPort :: Lens.Lens' InstancePortInfo (Lude.Maybe Lude.Int)
ipiFromPort = Lens.lens (fromPort :: InstancePortInfo -> Lude.Maybe Lude.Int) (\s a -> s {fromPort = a} :: InstancePortInfo)
{-# DEPRECATED ipiFromPort "Use generic-lens or generic-optics with 'fromPort' instead." #-}

-- | The IP address, or range of IP addresses in CIDR notation, that are allowed to connect to an instance through the ports, and the protocol. Lightsail supports IPv4 addresses.
--
-- For more information about CIDR block notation, see <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing#CIDR_notation Classless Inter-Domain Routing> on /Wikipedia/ .
--
-- /Note:/ Consider using 'cidrs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipiCidrs :: Lens.Lens' InstancePortInfo (Lude.Maybe [Lude.Text])
ipiCidrs = Lens.lens (cidrs :: InstancePortInfo -> Lude.Maybe [Lude.Text]) (\s a -> s {cidrs = a} :: InstancePortInfo)
{-# DEPRECATED ipiCidrs "Use generic-lens or generic-optics with 'cidrs' instead." #-}

-- | The common name of the port information.
--
-- /Note:/ Consider using 'commonName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipiCommonName :: Lens.Lens' InstancePortInfo (Lude.Maybe Lude.Text)
ipiCommonName = Lens.lens (commonName :: InstancePortInfo -> Lude.Maybe Lude.Text) (\s a -> s {commonName = a} :: InstancePortInfo)
{-# DEPRECATED ipiCommonName "Use generic-lens or generic-optics with 'commonName' instead." #-}

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
ipiProtocol :: Lens.Lens' InstancePortInfo (Lude.Maybe NetworkProtocol)
ipiProtocol = Lens.lens (protocol :: InstancePortInfo -> Lude.Maybe NetworkProtocol) (\s a -> s {protocol = a} :: InstancePortInfo)
{-# DEPRECATED ipiProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | An alias that defines access for a preconfigured range of IP addresses.
--
-- The only alias currently supported is @lightsail-connect@ , which allows IP addresses of the browser-based RDP/SSH client in the Lightsail console to connect to your instance.
--
-- /Note:/ Consider using 'cidrListAliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipiCidrListAliases :: Lens.Lens' InstancePortInfo (Lude.Maybe [Lude.Text])
ipiCidrListAliases = Lens.lens (cidrListAliases :: InstancePortInfo -> Lude.Maybe [Lude.Text]) (\s a -> s {cidrListAliases = a} :: InstancePortInfo)
{-# DEPRECATED ipiCidrListAliases "Use generic-lens or generic-optics with 'cidrListAliases' instead." #-}

-- | The access direction (@inbound@ or @outbound@ ).
--
-- /Note:/ Consider using 'accessDirection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipiAccessDirection :: Lens.Lens' InstancePortInfo (Lude.Maybe AccessDirection)
ipiAccessDirection = Lens.lens (accessDirection :: InstancePortInfo -> Lude.Maybe AccessDirection) (\s a -> s {accessDirection = a} :: InstancePortInfo)
{-# DEPRECATED ipiAccessDirection "Use generic-lens or generic-optics with 'accessDirection' instead." #-}

-- | The type of access (@Public@ or @Private@ ).
--
-- /Note:/ Consider using 'accessType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipiAccessType :: Lens.Lens' InstancePortInfo (Lude.Maybe PortAccessType)
ipiAccessType = Lens.lens (accessType :: InstancePortInfo -> Lude.Maybe PortAccessType) (\s a -> s {accessType = a} :: InstancePortInfo)
{-# DEPRECATED ipiAccessType "Use generic-lens or generic-optics with 'accessType' instead." #-}

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
ipiToPort :: Lens.Lens' InstancePortInfo (Lude.Maybe Lude.Int)
ipiToPort = Lens.lens (toPort :: InstancePortInfo -> Lude.Maybe Lude.Int) (\s a -> s {toPort = a} :: InstancePortInfo)
{-# DEPRECATED ipiToPort "Use generic-lens or generic-optics with 'toPort' instead." #-}

-- | The location from which access is allowed. For example, @Anywhere (0.0.0.0/0)@ , or @Custom@ if a specific IP address or range of IP addresses is allowed.
--
-- /Note:/ Consider using 'accessFrom' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipiAccessFrom :: Lens.Lens' InstancePortInfo (Lude.Maybe Lude.Text)
ipiAccessFrom = Lens.lens (accessFrom :: InstancePortInfo -> Lude.Maybe Lude.Text) (\s a -> s {accessFrom = a} :: InstancePortInfo)
{-# DEPRECATED ipiAccessFrom "Use generic-lens or generic-optics with 'accessFrom' instead." #-}

instance Lude.FromJSON InstancePortInfo where
  parseJSON =
    Lude.withObject
      "InstancePortInfo"
      ( \x ->
          InstancePortInfo'
            Lude.<$> (x Lude..:? "fromPort")
            Lude.<*> (x Lude..:? "cidrs" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "commonName")
            Lude.<*> (x Lude..:? "protocol")
            Lude.<*> (x Lude..:? "cidrListAliases" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "accessDirection")
            Lude.<*> (x Lude..:? "accessType")
            Lude.<*> (x Lude..:? "toPort")
            Lude.<*> (x Lude..:? "accessFrom")
      )
