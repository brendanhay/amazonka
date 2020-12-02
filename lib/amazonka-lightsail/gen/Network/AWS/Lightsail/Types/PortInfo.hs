{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.PortInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.PortInfo where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.NetworkProtocol
import Network.AWS.Prelude

-- | Describes ports to open on an instance, the IP addresses allowed to connect to the instance through the ports, and the protocol.
--
--
--
-- /See:/ 'portInfo' smart constructor.
data PortInfo = PortInfo'
  { _piFromPort :: !(Maybe Int),
    _piCidrs :: !(Maybe [Text]),
    _piProtocol :: !(Maybe NetworkProtocol),
    _piCidrListAliases :: !(Maybe [Text]),
    _piToPort :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PortInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piFromPort' - The first port in a range of open ports on an instance. Allowed ports:     * TCP and UDP - @0@ to @65535@      * ICMP - The ICMP type. For example, specify @8@ as the @fromPort@ (ICMP type), and @-1@ as the @toPort@ (ICMP code), to enable ICMP Ping. For more information, see <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Control_messages Control Messages> on /Wikipedia/ .
--
-- * 'piCidrs' - The IP address, or range of IP addresses in CIDR notation, that are allowed to connect to an instance through the ports, and the protocol. Lightsail supports IPv4 addresses. Examples:     * To allow the IP address @192.0.2.44@ , specify @192.0.2.44@ or @192.0.2.44/32@ .      * To allow the IP addresses @192.0.2.0@ to @192.0.2.255@ , specify @192.0.2.0/24@ . For more information about CIDR block notation, see <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing#CIDR_notation Classless Inter-Domain Routing> on /Wikipedia/ .
--
-- * 'piProtocol' - The IP protocol name. The name can be one of the following:     * @tcp@ - Transmission Control Protocol (TCP) provides reliable, ordered, and error-checked delivery of streamed data between applications running on hosts communicating by an IP network. If you have an application that doesn't require reliable data stream service, use UDP instead.     * @all@ - All transport layer protocol types. For more general information, see <https://en.wikipedia.org/wiki/Transport_layer Transport layer> on /Wikipedia/ .     * @udp@ - With User Datagram Protocol (UDP), computer applications can send messages (or datagrams) to other hosts on an Internet Protocol (IP) network. Prior communications are not required to set up transmission channels or data paths. Applications that don't require reliable data stream service can use UDP, which provides a connectionless datagram service that emphasizes reduced latency over reliability. If you do require reliable data stream service, use TCP instead.     * @icmp@ - Internet Control Message Protocol (ICMP) is used to send error messages and operational information indicating success or failure when communicating with an instance. For example, an error is indicated when an instance could not be reached. When you specify @icmp@ as the @protocol@ , you must specify the ICMP type using the @fromPort@ parameter, and ICMP code using the @toPort@ parameter.
--
-- * 'piCidrListAliases' - An alias that defines access for a preconfigured range of IP addresses. The only alias currently supported is @lightsail-connect@ , which allows IP addresses of the browser-based RDP/SSH client in the Lightsail console to connect to your instance.
--
-- * 'piToPort' - The last port in a range of open ports on an instance. Allowed ports:     * TCP and UDP - @0@ to @65535@      * ICMP - The ICMP code. For example, specify @8@ as the @fromPort@ (ICMP type), and @-1@ as the @toPort@ (ICMP code), to enable ICMP Ping. For more information, see <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Control_messages Control Messages> on /Wikipedia/ .
portInfo ::
  PortInfo
portInfo =
  PortInfo'
    { _piFromPort = Nothing,
      _piCidrs = Nothing,
      _piProtocol = Nothing,
      _piCidrListAliases = Nothing,
      _piToPort = Nothing
    }

-- | The first port in a range of open ports on an instance. Allowed ports:     * TCP and UDP - @0@ to @65535@      * ICMP - The ICMP type. For example, specify @8@ as the @fromPort@ (ICMP type), and @-1@ as the @toPort@ (ICMP code), to enable ICMP Ping. For more information, see <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Control_messages Control Messages> on /Wikipedia/ .
piFromPort :: Lens' PortInfo (Maybe Int)
piFromPort = lens _piFromPort (\s a -> s {_piFromPort = a})

-- | The IP address, or range of IP addresses in CIDR notation, that are allowed to connect to an instance through the ports, and the protocol. Lightsail supports IPv4 addresses. Examples:     * To allow the IP address @192.0.2.44@ , specify @192.0.2.44@ or @192.0.2.44/32@ .      * To allow the IP addresses @192.0.2.0@ to @192.0.2.255@ , specify @192.0.2.0/24@ . For more information about CIDR block notation, see <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing#CIDR_notation Classless Inter-Domain Routing> on /Wikipedia/ .
piCidrs :: Lens' PortInfo [Text]
piCidrs = lens _piCidrs (\s a -> s {_piCidrs = a}) . _Default . _Coerce

-- | The IP protocol name. The name can be one of the following:     * @tcp@ - Transmission Control Protocol (TCP) provides reliable, ordered, and error-checked delivery of streamed data between applications running on hosts communicating by an IP network. If you have an application that doesn't require reliable data stream service, use UDP instead.     * @all@ - All transport layer protocol types. For more general information, see <https://en.wikipedia.org/wiki/Transport_layer Transport layer> on /Wikipedia/ .     * @udp@ - With User Datagram Protocol (UDP), computer applications can send messages (or datagrams) to other hosts on an Internet Protocol (IP) network. Prior communications are not required to set up transmission channels or data paths. Applications that don't require reliable data stream service can use UDP, which provides a connectionless datagram service that emphasizes reduced latency over reliability. If you do require reliable data stream service, use TCP instead.     * @icmp@ - Internet Control Message Protocol (ICMP) is used to send error messages and operational information indicating success or failure when communicating with an instance. For example, an error is indicated when an instance could not be reached. When you specify @icmp@ as the @protocol@ , you must specify the ICMP type using the @fromPort@ parameter, and ICMP code using the @toPort@ parameter.
piProtocol :: Lens' PortInfo (Maybe NetworkProtocol)
piProtocol = lens _piProtocol (\s a -> s {_piProtocol = a})

-- | An alias that defines access for a preconfigured range of IP addresses. The only alias currently supported is @lightsail-connect@ , which allows IP addresses of the browser-based RDP/SSH client in the Lightsail console to connect to your instance.
piCidrListAliases :: Lens' PortInfo [Text]
piCidrListAliases = lens _piCidrListAliases (\s a -> s {_piCidrListAliases = a}) . _Default . _Coerce

-- | The last port in a range of open ports on an instance. Allowed ports:     * TCP and UDP - @0@ to @65535@      * ICMP - The ICMP code. For example, specify @8@ as the @fromPort@ (ICMP type), and @-1@ as the @toPort@ (ICMP code), to enable ICMP Ping. For more information, see <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Control_messages Control Messages> on /Wikipedia/ .
piToPort :: Lens' PortInfo (Maybe Int)
piToPort = lens _piToPort (\s a -> s {_piToPort = a})

instance Hashable PortInfo

instance NFData PortInfo

instance ToJSON PortInfo where
  toJSON PortInfo' {..} =
    object
      ( catMaybes
          [ ("fromPort" .=) <$> _piFromPort,
            ("cidrs" .=) <$> _piCidrs,
            ("protocol" .=) <$> _piProtocol,
            ("cidrListAliases" .=) <$> _piCidrListAliases,
            ("toPort" .=) <$> _piToPort
          ]
      )
