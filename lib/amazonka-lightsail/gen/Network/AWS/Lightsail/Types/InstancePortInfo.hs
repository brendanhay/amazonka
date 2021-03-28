{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstancePortInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.InstancePortInfo
  ( InstancePortInfo (..)
  -- * Smart constructor
  , mkInstancePortInfo
  -- * Lenses
  , ipiAccessDirection
  , ipiAccessFrom
  , ipiAccessType
  , ipiCidrListAliases
  , ipiCidrs
  , ipiCommonName
  , ipiFromPort
  , ipiProtocol
  , ipiToPort
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.AccessDirection as Types
import qualified Network.AWS.Lightsail.Types.NetworkProtocol as Types
import qualified Network.AWS.Lightsail.Types.PortAccessType as Types
import qualified Network.AWS.Prelude as Core

-- | Describes information about ports for an Amazon Lightsail instance.
--
-- /See:/ 'mkInstancePortInfo' smart constructor.
data InstancePortInfo = InstancePortInfo'
  { accessDirection :: Core.Maybe Types.AccessDirection
    -- ^ The access direction (@inbound@ or @outbound@ ).
  , accessFrom :: Core.Maybe Core.Text
    -- ^ The location from which access is allowed. For example, @Anywhere (0.0.0.0/0)@ , or @Custom@ if a specific IP address or range of IP addresses is allowed.
  , accessType :: Core.Maybe Types.PortAccessType
    -- ^ The type of access (@Public@ or @Private@ ).
  , cidrListAliases :: Core.Maybe [Core.Text]
    -- ^ An alias that defines access for a preconfigured range of IP addresses.
--
-- The only alias currently supported is @lightsail-connect@ , which allows IP addresses of the browser-based RDP/SSH client in the Lightsail console to connect to your instance.
  , cidrs :: Core.Maybe [Core.Text]
    -- ^ The IP address, or range of IP addresses in CIDR notation, that are allowed to connect to an instance through the ports, and the protocol. Lightsail supports IPv4 addresses.
--
-- For more information about CIDR block notation, see <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing#CIDR_notation Classless Inter-Domain Routing> on /Wikipedia/ .
  , commonName :: Core.Maybe Core.Text
    -- ^ The common name of the port information.
  , fromPort :: Core.Maybe Core.Int
    -- ^ The first port in a range of open ports on an instance.
--
-- Allowed ports:
--
--     * TCP and UDP - @0@ to @65535@ 
--
--
--     * ICMP - The ICMP type. For example, specify @8@ as the @fromPort@ (ICMP type), and @-1@ as the @toPort@ (ICMP code), to enable ICMP Ping. For more information, see <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Control_messages Control Messages> on /Wikipedia/ .
--
--
  , protocol :: Core.Maybe Types.NetworkProtocol
    -- ^ The IP protocol name.
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
  , toPort :: Core.Maybe Core.Int
    -- ^ The last port in a range of open ports on an instance.
--
-- Allowed ports:
--
--     * TCP and UDP - @0@ to @65535@ 
--
--
--     * ICMP - The ICMP code. For example, specify @8@ as the @fromPort@ (ICMP type), and @-1@ as the @toPort@ (ICMP code), to enable ICMP Ping. For more information, see <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Control_messages Control Messages> on /Wikipedia/ .
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstancePortInfo' value with any optional fields omitted.
mkInstancePortInfo
    :: InstancePortInfo
mkInstancePortInfo
  = InstancePortInfo'{accessDirection = Core.Nothing,
                      accessFrom = Core.Nothing, accessType = Core.Nothing,
                      cidrListAliases = Core.Nothing, cidrs = Core.Nothing,
                      commonName = Core.Nothing, fromPort = Core.Nothing,
                      protocol = Core.Nothing, toPort = Core.Nothing}

-- | The access direction (@inbound@ or @outbound@ ).
--
-- /Note:/ Consider using 'accessDirection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipiAccessDirection :: Lens.Lens' InstancePortInfo (Core.Maybe Types.AccessDirection)
ipiAccessDirection = Lens.field @"accessDirection"
{-# INLINEABLE ipiAccessDirection #-}
{-# DEPRECATED accessDirection "Use generic-lens or generic-optics with 'accessDirection' instead"  #-}

-- | The location from which access is allowed. For example, @Anywhere (0.0.0.0/0)@ , or @Custom@ if a specific IP address or range of IP addresses is allowed.
--
-- /Note:/ Consider using 'accessFrom' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipiAccessFrom :: Lens.Lens' InstancePortInfo (Core.Maybe Core.Text)
ipiAccessFrom = Lens.field @"accessFrom"
{-# INLINEABLE ipiAccessFrom #-}
{-# DEPRECATED accessFrom "Use generic-lens or generic-optics with 'accessFrom' instead"  #-}

-- | The type of access (@Public@ or @Private@ ).
--
-- /Note:/ Consider using 'accessType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipiAccessType :: Lens.Lens' InstancePortInfo (Core.Maybe Types.PortAccessType)
ipiAccessType = Lens.field @"accessType"
{-# INLINEABLE ipiAccessType #-}
{-# DEPRECATED accessType "Use generic-lens or generic-optics with 'accessType' instead"  #-}

-- | An alias that defines access for a preconfigured range of IP addresses.
--
-- The only alias currently supported is @lightsail-connect@ , which allows IP addresses of the browser-based RDP/SSH client in the Lightsail console to connect to your instance.
--
-- /Note:/ Consider using 'cidrListAliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipiCidrListAliases :: Lens.Lens' InstancePortInfo (Core.Maybe [Core.Text])
ipiCidrListAliases = Lens.field @"cidrListAliases"
{-# INLINEABLE ipiCidrListAliases #-}
{-# DEPRECATED cidrListAliases "Use generic-lens or generic-optics with 'cidrListAliases' instead"  #-}

-- | The IP address, or range of IP addresses in CIDR notation, that are allowed to connect to an instance through the ports, and the protocol. Lightsail supports IPv4 addresses.
--
-- For more information about CIDR block notation, see <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing#CIDR_notation Classless Inter-Domain Routing> on /Wikipedia/ .
--
-- /Note:/ Consider using 'cidrs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipiCidrs :: Lens.Lens' InstancePortInfo (Core.Maybe [Core.Text])
ipiCidrs = Lens.field @"cidrs"
{-# INLINEABLE ipiCidrs #-}
{-# DEPRECATED cidrs "Use generic-lens or generic-optics with 'cidrs' instead"  #-}

-- | The common name of the port information.
--
-- /Note:/ Consider using 'commonName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipiCommonName :: Lens.Lens' InstancePortInfo (Core.Maybe Core.Text)
ipiCommonName = Lens.field @"commonName"
{-# INLINEABLE ipiCommonName #-}
{-# DEPRECATED commonName "Use generic-lens or generic-optics with 'commonName' instead"  #-}

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
ipiFromPort :: Lens.Lens' InstancePortInfo (Core.Maybe Core.Int)
ipiFromPort = Lens.field @"fromPort"
{-# INLINEABLE ipiFromPort #-}
{-# DEPRECATED fromPort "Use generic-lens or generic-optics with 'fromPort' instead"  #-}

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
ipiProtocol :: Lens.Lens' InstancePortInfo (Core.Maybe Types.NetworkProtocol)
ipiProtocol = Lens.field @"protocol"
{-# INLINEABLE ipiProtocol #-}
{-# DEPRECATED protocol "Use generic-lens or generic-optics with 'protocol' instead"  #-}

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
ipiToPort :: Lens.Lens' InstancePortInfo (Core.Maybe Core.Int)
ipiToPort = Lens.field @"toPort"
{-# INLINEABLE ipiToPort #-}
{-# DEPRECATED toPort "Use generic-lens or generic-optics with 'toPort' instead"  #-}

instance Core.FromJSON InstancePortInfo where
        parseJSON
          = Core.withObject "InstancePortInfo" Core.$
              \ x ->
                InstancePortInfo' Core.<$>
                  (x Core..:? "accessDirection") Core.<*> x Core..:? "accessFrom"
                    Core.<*> x Core..:? "accessType"
                    Core.<*> x Core..:? "cidrListAliases"
                    Core.<*> x Core..:? "cidrs"
                    Core.<*> x Core..:? "commonName"
                    Core.<*> x Core..:? "fromPort"
                    Core.<*> x Core..:? "protocol"
                    Core.<*> x Core..:? "toPort"
