{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstancePortInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstancePortInfo where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.AccessDirection
import Network.AWS.Lightsail.Types.NetworkProtocol
import Network.AWS.Lightsail.Types.PortAccessType
import Network.AWS.Prelude

-- | Describes information about ports for an Amazon Lightsail instance.
--
--
--
-- /See:/ 'instancePortInfo' smart constructor.
data InstancePortInfo = InstancePortInfo'
  { _ipiFromPort ::
      !(Maybe Int),
    _ipiCidrs :: !(Maybe [Text]),
    _ipiCommonName :: !(Maybe Text),
    _ipiProtocol :: !(Maybe NetworkProtocol),
    _ipiCidrListAliases :: !(Maybe [Text]),
    _ipiAccessDirection :: !(Maybe AccessDirection),
    _ipiAccessType :: !(Maybe PortAccessType),
    _ipiToPort :: !(Maybe Int),
    _ipiAccessFrom :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstancePortInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipiFromPort' - The first port in a range of open ports on an instance. Allowed ports:     * TCP and UDP - @0@ to @65535@      * ICMP - The ICMP type. For example, specify @8@ as the @fromPort@ (ICMP type), and @-1@ as the @toPort@ (ICMP code), to enable ICMP Ping. For more information, see <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Control_messages Control Messages> on /Wikipedia/ .
--
-- * 'ipiCidrs' - The IP address, or range of IP addresses in CIDR notation, that are allowed to connect to an instance through the ports, and the protocol. Lightsail supports IPv4 addresses. For more information about CIDR block notation, see <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing#CIDR_notation Classless Inter-Domain Routing> on /Wikipedia/ .
--
-- * 'ipiCommonName' - The common name of the port information.
--
-- * 'ipiProtocol' - The IP protocol name. The name can be one of the following:     * @tcp@ - Transmission Control Protocol (TCP) provides reliable, ordered, and error-checked delivery of streamed data between applications running on hosts communicating by an IP network. If you have an application that doesn't require reliable data stream service, use UDP instead.     * @all@ - All transport layer protocol types. For more general information, see <https://en.wikipedia.org/wiki/Transport_layer Transport layer> on /Wikipedia/ .     * @udp@ - With User Datagram Protocol (UDP), computer applications can send messages (or datagrams) to other hosts on an Internet Protocol (IP) network. Prior communications are not required to set up transmission channels or data paths. Applications that don't require reliable data stream service can use UDP, which provides a connectionless datagram service that emphasizes reduced latency over reliability. If you do require reliable data stream service, use TCP instead.     * @icmp@ - Internet Control Message Protocol (ICMP) is used to send error messages and operational information indicating success or failure when communicating with an instance. For example, an error is indicated when an instance could not be reached. When you specify @icmp@ as the @protocol@ , you must specify the ICMP type using the @fromPort@ parameter, and ICMP code using the @toPort@ parameter.
--
-- * 'ipiCidrListAliases' - An alias that defines access for a preconfigured range of IP addresses. The only alias currently supported is @lightsail-connect@ , which allows IP addresses of the browser-based RDP/SSH client in the Lightsail console to connect to your instance.
--
-- * 'ipiAccessDirection' - The access direction (@inbound@ or @outbound@ ).
--
-- * 'ipiAccessType' - The type of access (@Public@ or @Private@ ).
--
-- * 'ipiToPort' - The last port in a range of open ports on an instance. Allowed ports:     * TCP and UDP - @0@ to @65535@      * ICMP - The ICMP code. For example, specify @8@ as the @fromPort@ (ICMP type), and @-1@ as the @toPort@ (ICMP code), to enable ICMP Ping. For more information, see <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Control_messages Control Messages> on /Wikipedia/ .
--
-- * 'ipiAccessFrom' - The location from which access is allowed. For example, @Anywhere (0.0.0.0/0)@ , or @Custom@ if a specific IP address or range of IP addresses is allowed.
instancePortInfo ::
  InstancePortInfo
instancePortInfo =
  InstancePortInfo'
    { _ipiFromPort = Nothing,
      _ipiCidrs = Nothing,
      _ipiCommonName = Nothing,
      _ipiProtocol = Nothing,
      _ipiCidrListAliases = Nothing,
      _ipiAccessDirection = Nothing,
      _ipiAccessType = Nothing,
      _ipiToPort = Nothing,
      _ipiAccessFrom = Nothing
    }

-- | The first port in a range of open ports on an instance. Allowed ports:     * TCP and UDP - @0@ to @65535@      * ICMP - The ICMP type. For example, specify @8@ as the @fromPort@ (ICMP type), and @-1@ as the @toPort@ (ICMP code), to enable ICMP Ping. For more information, see <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Control_messages Control Messages> on /Wikipedia/ .
ipiFromPort :: Lens' InstancePortInfo (Maybe Int)
ipiFromPort = lens _ipiFromPort (\s a -> s {_ipiFromPort = a})

-- | The IP address, or range of IP addresses in CIDR notation, that are allowed to connect to an instance through the ports, and the protocol. Lightsail supports IPv4 addresses. For more information about CIDR block notation, see <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing#CIDR_notation Classless Inter-Domain Routing> on /Wikipedia/ .
ipiCidrs :: Lens' InstancePortInfo [Text]
ipiCidrs = lens _ipiCidrs (\s a -> s {_ipiCidrs = a}) . _Default . _Coerce

-- | The common name of the port information.
ipiCommonName :: Lens' InstancePortInfo (Maybe Text)
ipiCommonName = lens _ipiCommonName (\s a -> s {_ipiCommonName = a})

-- | The IP protocol name. The name can be one of the following:     * @tcp@ - Transmission Control Protocol (TCP) provides reliable, ordered, and error-checked delivery of streamed data between applications running on hosts communicating by an IP network. If you have an application that doesn't require reliable data stream service, use UDP instead.     * @all@ - All transport layer protocol types. For more general information, see <https://en.wikipedia.org/wiki/Transport_layer Transport layer> on /Wikipedia/ .     * @udp@ - With User Datagram Protocol (UDP), computer applications can send messages (or datagrams) to other hosts on an Internet Protocol (IP) network. Prior communications are not required to set up transmission channels or data paths. Applications that don't require reliable data stream service can use UDP, which provides a connectionless datagram service that emphasizes reduced latency over reliability. If you do require reliable data stream service, use TCP instead.     * @icmp@ - Internet Control Message Protocol (ICMP) is used to send error messages and operational information indicating success or failure when communicating with an instance. For example, an error is indicated when an instance could not be reached. When you specify @icmp@ as the @protocol@ , you must specify the ICMP type using the @fromPort@ parameter, and ICMP code using the @toPort@ parameter.
ipiProtocol :: Lens' InstancePortInfo (Maybe NetworkProtocol)
ipiProtocol = lens _ipiProtocol (\s a -> s {_ipiProtocol = a})

-- | An alias that defines access for a preconfigured range of IP addresses. The only alias currently supported is @lightsail-connect@ , which allows IP addresses of the browser-based RDP/SSH client in the Lightsail console to connect to your instance.
ipiCidrListAliases :: Lens' InstancePortInfo [Text]
ipiCidrListAliases = lens _ipiCidrListAliases (\s a -> s {_ipiCidrListAliases = a}) . _Default . _Coerce

-- | The access direction (@inbound@ or @outbound@ ).
ipiAccessDirection :: Lens' InstancePortInfo (Maybe AccessDirection)
ipiAccessDirection = lens _ipiAccessDirection (\s a -> s {_ipiAccessDirection = a})

-- | The type of access (@Public@ or @Private@ ).
ipiAccessType :: Lens' InstancePortInfo (Maybe PortAccessType)
ipiAccessType = lens _ipiAccessType (\s a -> s {_ipiAccessType = a})

-- | The last port in a range of open ports on an instance. Allowed ports:     * TCP and UDP - @0@ to @65535@      * ICMP - The ICMP code. For example, specify @8@ as the @fromPort@ (ICMP type), and @-1@ as the @toPort@ (ICMP code), to enable ICMP Ping. For more information, see <https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Control_messages Control Messages> on /Wikipedia/ .
ipiToPort :: Lens' InstancePortInfo (Maybe Int)
ipiToPort = lens _ipiToPort (\s a -> s {_ipiToPort = a})

-- | The location from which access is allowed. For example, @Anywhere (0.0.0.0/0)@ , or @Custom@ if a specific IP address or range of IP addresses is allowed.
ipiAccessFrom :: Lens' InstancePortInfo (Maybe Text)
ipiAccessFrom = lens _ipiAccessFrom (\s a -> s {_ipiAccessFrom = a})

instance FromJSON InstancePortInfo where
  parseJSON =
    withObject
      "InstancePortInfo"
      ( \x ->
          InstancePortInfo'
            <$> (x .:? "fromPort")
            <*> (x .:? "cidrs" .!= mempty)
            <*> (x .:? "commonName")
            <*> (x .:? "protocol")
            <*> (x .:? "cidrListAliases" .!= mempty)
            <*> (x .:? "accessDirection")
            <*> (x .:? "accessType")
            <*> (x .:? "toPort")
            <*> (x .:? "accessFrom")
      )

instance Hashable InstancePortInfo

instance NFData InstancePortInfo
