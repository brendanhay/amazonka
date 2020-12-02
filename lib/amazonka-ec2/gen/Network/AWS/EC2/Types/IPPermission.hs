{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IPPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IPPermission where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.IPRange
import Network.AWS.EC2.Types.IPv6Range
import Network.AWS.EC2.Types.PrefixListId
import Network.AWS.EC2.Types.UserIdGroupPair
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a set of permissions for a security group rule.
--
--
--
-- /See:/ 'ipPermission' smart constructor.
data IPPermission = IPPermission'
  { _ipFromPort :: !(Maybe Int),
    _ipUserIdGroupPairs :: !(Maybe [UserIdGroupPair]),
    _ipPrefixListIds :: !(Maybe [PrefixListId]),
    _ipToPort :: !(Maybe Int),
    _ipIPv6Ranges :: !(Maybe [IPv6Range]),
    _ipIPRanges :: !(Maybe [IPRange]),
    _ipIPProtocol :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IPPermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipFromPort' - The start of port range for the TCP and UDP protocols, or an ICMP/ICMPv6 type number. A value of @-1@ indicates all ICMP/ICMPv6 types. If you specify all ICMP/ICMPv6 types, you must specify all codes.
--
-- * 'ipUserIdGroupPairs' - The security group and AWS account ID pairs.
--
-- * 'ipPrefixListIds' - [VPC only] The prefix list IDs.
--
-- * 'ipToPort' - The end of port range for the TCP and UDP protocols, or an ICMP/ICMPv6 code. A value of @-1@ indicates all ICMP/ICMPv6 codes. If you specify all ICMP/ICMPv6 types, you must specify all codes.
--
-- * 'ipIPv6Ranges' - [VPC only] The IPv6 ranges.
--
-- * 'ipIPRanges' - The IPv4 ranges.
--
-- * 'ipIPProtocol' - The IP protocol name (@tcp@ , @udp@ , @icmp@ , @icmpv6@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> ). [VPC only] Use @-1@ to specify all protocols. When authorizing security group rules, specifying @-1@ or a protocol number other than @tcp@ , @udp@ , @icmp@ , or @icmpv6@ allows traffic on all ports, regardless of any port range you specify. For @tcp@ , @udp@ , and @icmp@ , you must specify a port range. For @icmpv6@ , the port range is optional; if you omit the port range, traffic for all types and codes is allowed.
ipPermission ::
  -- | 'ipIPProtocol'
  Text ->
  IPPermission
ipPermission pIPProtocol_ =
  IPPermission'
    { _ipFromPort = Nothing,
      _ipUserIdGroupPairs = Nothing,
      _ipPrefixListIds = Nothing,
      _ipToPort = Nothing,
      _ipIPv6Ranges = Nothing,
      _ipIPRanges = Nothing,
      _ipIPProtocol = pIPProtocol_
    }

-- | The start of port range for the TCP and UDP protocols, or an ICMP/ICMPv6 type number. A value of @-1@ indicates all ICMP/ICMPv6 types. If you specify all ICMP/ICMPv6 types, you must specify all codes.
ipFromPort :: Lens' IPPermission (Maybe Int)
ipFromPort = lens _ipFromPort (\s a -> s {_ipFromPort = a})

-- | The security group and AWS account ID pairs.
ipUserIdGroupPairs :: Lens' IPPermission [UserIdGroupPair]
ipUserIdGroupPairs = lens _ipUserIdGroupPairs (\s a -> s {_ipUserIdGroupPairs = a}) . _Default . _Coerce

-- | [VPC only] The prefix list IDs.
ipPrefixListIds :: Lens' IPPermission [PrefixListId]
ipPrefixListIds = lens _ipPrefixListIds (\s a -> s {_ipPrefixListIds = a}) . _Default . _Coerce

-- | The end of port range for the TCP and UDP protocols, or an ICMP/ICMPv6 code. A value of @-1@ indicates all ICMP/ICMPv6 codes. If you specify all ICMP/ICMPv6 types, you must specify all codes.
ipToPort :: Lens' IPPermission (Maybe Int)
ipToPort = lens _ipToPort (\s a -> s {_ipToPort = a})

-- | [VPC only] The IPv6 ranges.
ipIPv6Ranges :: Lens' IPPermission [IPv6Range]
ipIPv6Ranges = lens _ipIPv6Ranges (\s a -> s {_ipIPv6Ranges = a}) . _Default . _Coerce

-- | The IPv4 ranges.
ipIPRanges :: Lens' IPPermission [IPRange]
ipIPRanges = lens _ipIPRanges (\s a -> s {_ipIPRanges = a}) . _Default . _Coerce

-- | The IP protocol name (@tcp@ , @udp@ , @icmp@ , @icmpv6@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> ). [VPC only] Use @-1@ to specify all protocols. When authorizing security group rules, specifying @-1@ or a protocol number other than @tcp@ , @udp@ , @icmp@ , or @icmpv6@ allows traffic on all ports, regardless of any port range you specify. For @tcp@ , @udp@ , and @icmp@ , you must specify a port range. For @icmpv6@ , the port range is optional; if you omit the port range, traffic for all types and codes is allowed.
ipIPProtocol :: Lens' IPPermission Text
ipIPProtocol = lens _ipIPProtocol (\s a -> s {_ipIPProtocol = a})

instance FromXML IPPermission where
  parseXML x =
    IPPermission'
      <$> (x .@? "fromPort")
      <*> (x .@? "groups" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "prefixListIds" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "toPort")
      <*> (x .@? "ipv6Ranges" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "ipRanges" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@ "ipProtocol")

instance Hashable IPPermission

instance NFData IPPermission

instance ToQuery IPPermission where
  toQuery IPPermission' {..} =
    mconcat
      [ "FromPort" =: _ipFromPort,
        toQuery (toQueryList "Groups" <$> _ipUserIdGroupPairs),
        toQuery (toQueryList "PrefixListIds" <$> _ipPrefixListIds),
        "ToPort" =: _ipToPort,
        toQuery (toQueryList "Ipv6Ranges" <$> _ipIPv6Ranges),
        toQuery (toQueryList "IpRanges" <$> _ipIPRanges),
        "IpProtocol" =: _ipIPProtocol
      ]
