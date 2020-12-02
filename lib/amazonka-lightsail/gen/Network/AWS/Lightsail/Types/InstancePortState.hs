{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstancePortState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstancePortState where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.NetworkProtocol
import Network.AWS.Lightsail.Types.PortState
import Network.AWS.Prelude

-- | Describes open ports on an instance, the IP addresses allowed to connect to the instance through the ports, and the protocol.
--
--
--
-- /See:/ 'instancePortState' smart constructor.
data InstancePortState = InstancePortState'
  { _ipsFromPort ::
      !(Maybe Int),
    _ipsCidrs :: !(Maybe [Text]),
    _ipsState :: !(Maybe PortState),
    _ipsProtocol :: !(Maybe NetworkProtocol),
    _ipsCidrListAliases :: !(Maybe [Text]),
    _ipsToPort :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstancePortState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipsFromPort' - The first port in a range of open ports on an instance. Allowed ports:     * TCP and UDP - @0@ to @65535@      * ICMP - The ICMP type. For example, specify @8@ as the @fromPort@ (ICMP type), and @-1@ as the @toPort@ (ICMP code), to enable ICMP Ping. For more information, see <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Control_messages Control Messages> on /Wikipedia/ .
--
-- * 'ipsCidrs' - The IP address, or range of IP addresses in CIDR notation, that are allowed to connect to an instance through the ports, and the protocol. Lightsail supports IPv4 addresses. For more information about CIDR block notation, see <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing#CIDR_notation Classless Inter-Domain Routing> on /Wikipedia/ .
--
-- * 'ipsState' - Specifies whether the instance port is @open@ or @closed@ .
--
-- * 'ipsProtocol' - The IP protocol name. The name can be one of the following:     * @tcp@ - Transmission Control Protocol (TCP) provides reliable, ordered, and error-checked delivery of streamed data between applications running on hosts communicating by an IP network. If you have an application that doesn't require reliable data stream service, use UDP instead.     * @all@ - All transport layer protocol types. For more general information, see <https://en.wikipedia.org/wiki/Transport_layer Transport layer> on /Wikipedia/ .     * @udp@ - With User Datagram Protocol (UDP), computer applications can send messages (or datagrams) to other hosts on an Internet Protocol (IP) network. Prior communications are not required to set up transmission channels or data paths. Applications that don't require reliable data stream service can use UDP, which provides a connectionless datagram service that emphasizes reduced latency over reliability. If you do require reliable data stream service, use TCP instead.     * @icmp@ - Internet Control Message Protocol (ICMP) is used to send error messages and operational information indicating success or failure when communicating with an instance. For example, an error is indicated when an instance could not be reached. When you specify @icmp@ as the @protocol@ , you must specify the ICMP type using the @fromPort@ parameter, and ICMP code using the @toPort@ parameter.
--
-- * 'ipsCidrListAliases' - An alias that defines access for a preconfigured range of IP addresses. The only alias currently supported is @lightsail-connect@ , which allows IP addresses of the browser-based RDP/SSH client in the Lightsail console to connect to your instance.
--
-- * 'ipsToPort' - The last port in a range of open ports on an instance. Allowed ports:     * TCP and UDP - @0@ to @65535@      * ICMP - The ICMP code. For example, specify @8@ as the @fromPort@ (ICMP type), and @-1@ as the @toPort@ (ICMP code), to enable ICMP Ping. For more information, see <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Control_messages Control Messages> on /Wikipedia/ .
instancePortState ::
  InstancePortState
instancePortState =
  InstancePortState'
    { _ipsFromPort = Nothing,
      _ipsCidrs = Nothing,
      _ipsState = Nothing,
      _ipsProtocol = Nothing,
      _ipsCidrListAliases = Nothing,
      _ipsToPort = Nothing
    }

-- | The first port in a range of open ports on an instance. Allowed ports:     * TCP and UDP - @0@ to @65535@      * ICMP - The ICMP type. For example, specify @8@ as the @fromPort@ (ICMP type), and @-1@ as the @toPort@ (ICMP code), to enable ICMP Ping. For more information, see <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Control_messages Control Messages> on /Wikipedia/ .
ipsFromPort :: Lens' InstancePortState (Maybe Int)
ipsFromPort = lens _ipsFromPort (\s a -> s {_ipsFromPort = a})

-- | The IP address, or range of IP addresses in CIDR notation, that are allowed to connect to an instance through the ports, and the protocol. Lightsail supports IPv4 addresses. For more information about CIDR block notation, see <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing#CIDR_notation Classless Inter-Domain Routing> on /Wikipedia/ .
ipsCidrs :: Lens' InstancePortState [Text]
ipsCidrs = lens _ipsCidrs (\s a -> s {_ipsCidrs = a}) . _Default . _Coerce

-- | Specifies whether the instance port is @open@ or @closed@ .
ipsState :: Lens' InstancePortState (Maybe PortState)
ipsState = lens _ipsState (\s a -> s {_ipsState = a})

-- | The IP protocol name. The name can be one of the following:     * @tcp@ - Transmission Control Protocol (TCP) provides reliable, ordered, and error-checked delivery of streamed data between applications running on hosts communicating by an IP network. If you have an application that doesn't require reliable data stream service, use UDP instead.     * @all@ - All transport layer protocol types. For more general information, see <https://en.wikipedia.org/wiki/Transport_layer Transport layer> on /Wikipedia/ .     * @udp@ - With User Datagram Protocol (UDP), computer applications can send messages (or datagrams) to other hosts on an Internet Protocol (IP) network. Prior communications are not required to set up transmission channels or data paths. Applications that don't require reliable data stream service can use UDP, which provides a connectionless datagram service that emphasizes reduced latency over reliability. If you do require reliable data stream service, use TCP instead.     * @icmp@ - Internet Control Message Protocol (ICMP) is used to send error messages and operational information indicating success or failure when communicating with an instance. For example, an error is indicated when an instance could not be reached. When you specify @icmp@ as the @protocol@ , you must specify the ICMP type using the @fromPort@ parameter, and ICMP code using the @toPort@ parameter.
ipsProtocol :: Lens' InstancePortState (Maybe NetworkProtocol)
ipsProtocol = lens _ipsProtocol (\s a -> s {_ipsProtocol = a})

-- | An alias that defines access for a preconfigured range of IP addresses. The only alias currently supported is @lightsail-connect@ , which allows IP addresses of the browser-based RDP/SSH client in the Lightsail console to connect to your instance.
ipsCidrListAliases :: Lens' InstancePortState [Text]
ipsCidrListAliases = lens _ipsCidrListAliases (\s a -> s {_ipsCidrListAliases = a}) . _Default . _Coerce

-- | The last port in a range of open ports on an instance. Allowed ports:     * TCP and UDP - @0@ to @65535@      * ICMP - The ICMP code. For example, specify @8@ as the @fromPort@ (ICMP type), and @-1@ as the @toPort@ (ICMP code), to enable ICMP Ping. For more information, see <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Control_messages Control Messages> on /Wikipedia/ .
ipsToPort :: Lens' InstancePortState (Maybe Int)
ipsToPort = lens _ipsToPort (\s a -> s {_ipsToPort = a})

instance FromJSON InstancePortState where
  parseJSON =
    withObject
      "InstancePortState"
      ( \x ->
          InstancePortState'
            <$> (x .:? "fromPort")
            <*> (x .:? "cidrs" .!= mempty)
            <*> (x .:? "state")
            <*> (x .:? "protocol")
            <*> (x .:? "cidrListAliases" .!= mempty)
            <*> (x .:? "toPort")
      )

instance Hashable InstancePortState

instance NFData InstancePortState
