{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstancePortState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.InstancePortState
  ( InstancePortState (..)
  -- * Smart constructor
  , mkInstancePortState
  -- * Lenses
  , ipsCidrListAliases
  , ipsCidrs
  , ipsFromPort
  , ipsProtocol
  , ipsState
  , ipsToPort
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.NetworkProtocol as Types
import qualified Network.AWS.Lightsail.Types.PortState as Types
import qualified Network.AWS.Prelude as Core

-- | Describes open ports on an instance, the IP addresses allowed to connect to the instance through the ports, and the protocol.
--
-- /See:/ 'mkInstancePortState' smart constructor.
data InstancePortState = InstancePortState'
  { cidrListAliases :: Core.Maybe [Core.Text]
    -- ^ An alias that defines access for a preconfigured range of IP addresses.
--
-- The only alias currently supported is @lightsail-connect@ , which allows IP addresses of the browser-based RDP/SSH client in the Lightsail console to connect to your instance.
  , cidrs :: Core.Maybe [Core.Text]
    -- ^ The IP address, or range of IP addresses in CIDR notation, that are allowed to connect to an instance through the ports, and the protocol. Lightsail supports IPv4 addresses.
--
-- For more information about CIDR block notation, see <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing#CIDR_notation Classless Inter-Domain Routing> on /Wikipedia/ .
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
  , state :: Core.Maybe Types.PortState
    -- ^ Specifies whether the instance port is @open@ or @closed@ .
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

-- | Creates a 'InstancePortState' value with any optional fields omitted.
mkInstancePortState
    :: InstancePortState
mkInstancePortState
  = InstancePortState'{cidrListAliases = Core.Nothing,
                       cidrs = Core.Nothing, fromPort = Core.Nothing,
                       protocol = Core.Nothing, state = Core.Nothing,
                       toPort = Core.Nothing}

-- | An alias that defines access for a preconfigured range of IP addresses.
--
-- The only alias currently supported is @lightsail-connect@ , which allows IP addresses of the browser-based RDP/SSH client in the Lightsail console to connect to your instance.
--
-- /Note:/ Consider using 'cidrListAliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsCidrListAliases :: Lens.Lens' InstancePortState (Core.Maybe [Core.Text])
ipsCidrListAliases = Lens.field @"cidrListAliases"
{-# INLINEABLE ipsCidrListAliases #-}
{-# DEPRECATED cidrListAliases "Use generic-lens or generic-optics with 'cidrListAliases' instead"  #-}

-- | The IP address, or range of IP addresses in CIDR notation, that are allowed to connect to an instance through the ports, and the protocol. Lightsail supports IPv4 addresses.
--
-- For more information about CIDR block notation, see <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing#CIDR_notation Classless Inter-Domain Routing> on /Wikipedia/ .
--
-- /Note:/ Consider using 'cidrs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsCidrs :: Lens.Lens' InstancePortState (Core.Maybe [Core.Text])
ipsCidrs = Lens.field @"cidrs"
{-# INLINEABLE ipsCidrs #-}
{-# DEPRECATED cidrs "Use generic-lens or generic-optics with 'cidrs' instead"  #-}

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
ipsFromPort :: Lens.Lens' InstancePortState (Core.Maybe Core.Int)
ipsFromPort = Lens.field @"fromPort"
{-# INLINEABLE ipsFromPort #-}
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
ipsProtocol :: Lens.Lens' InstancePortState (Core.Maybe Types.NetworkProtocol)
ipsProtocol = Lens.field @"protocol"
{-# INLINEABLE ipsProtocol #-}
{-# DEPRECATED protocol "Use generic-lens or generic-optics with 'protocol' instead"  #-}

-- | Specifies whether the instance port is @open@ or @closed@ .
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsState :: Lens.Lens' InstancePortState (Core.Maybe Types.PortState)
ipsState = Lens.field @"state"
{-# INLINEABLE ipsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

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
ipsToPort :: Lens.Lens' InstancePortState (Core.Maybe Core.Int)
ipsToPort = Lens.field @"toPort"
{-# INLINEABLE ipsToPort #-}
{-# DEPRECATED toPort "Use generic-lens or generic-optics with 'toPort' instead"  #-}

instance Core.FromJSON InstancePortState where
        parseJSON
          = Core.withObject "InstancePortState" Core.$
              \ x ->
                InstancePortState' Core.<$>
                  (x Core..:? "cidrListAliases") Core.<*> x Core..:? "cidrs" Core.<*>
                    x Core..:? "fromPort"
                    Core.<*> x Core..:? "protocol"
                    Core.<*> x Core..:? "state"
                    Core.<*> x Core..:? "toPort"
