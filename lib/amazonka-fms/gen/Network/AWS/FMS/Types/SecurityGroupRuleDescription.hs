{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.SecurityGroupRuleDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.SecurityGroupRuleDescription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a set of permissions for a security group rule.
--
--
--
-- /See:/ 'securityGroupRuleDescription' smart constructor.
data SecurityGroupRuleDescription = SecurityGroupRuleDescription'
  { _sgrdFromPort ::
      !(Maybe Nat),
    _sgrdProtocol :: !(Maybe Text),
    _sgrdIPV4Range :: !(Maybe Text),
    _sgrdPrefixListId ::
      !(Maybe Text),
    _sgrdToPort :: !(Maybe Nat),
    _sgrdIPV6Range :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SecurityGroupRuleDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgrdFromPort' - The start of the port range for the TCP and UDP protocols, or an ICMP/ICMPv6 type number. A value of @-1@ indicates all ICMP/ICMPv6 types.
--
-- * 'sgrdProtocol' - The IP protocol name (@tcp@ , @udp@ , @icmp@ , @icmpv6@ ) or number.
--
-- * 'sgrdIPV4Range' - The IPv4 ranges for the security group rule.
--
-- * 'sgrdPrefixListId' - The ID of the prefix list for the security group rule.
--
-- * 'sgrdToPort' - The end of the port range for the TCP and UDP protocols, or an ICMP/ICMPv6 code. A value of @-1@ indicates all ICMP/ICMPv6 codes.
--
-- * 'sgrdIPV6Range' - The IPv6 ranges for the security group rule.
securityGroupRuleDescription ::
  SecurityGroupRuleDescription
securityGroupRuleDescription =
  SecurityGroupRuleDescription'
    { _sgrdFromPort = Nothing,
      _sgrdProtocol = Nothing,
      _sgrdIPV4Range = Nothing,
      _sgrdPrefixListId = Nothing,
      _sgrdToPort = Nothing,
      _sgrdIPV6Range = Nothing
    }

-- | The start of the port range for the TCP and UDP protocols, or an ICMP/ICMPv6 type number. A value of @-1@ indicates all ICMP/ICMPv6 types.
sgrdFromPort :: Lens' SecurityGroupRuleDescription (Maybe Natural)
sgrdFromPort = lens _sgrdFromPort (\s a -> s {_sgrdFromPort = a}) . mapping _Nat

-- | The IP protocol name (@tcp@ , @udp@ , @icmp@ , @icmpv6@ ) or number.
sgrdProtocol :: Lens' SecurityGroupRuleDescription (Maybe Text)
sgrdProtocol = lens _sgrdProtocol (\s a -> s {_sgrdProtocol = a})

-- | The IPv4 ranges for the security group rule.
sgrdIPV4Range :: Lens' SecurityGroupRuleDescription (Maybe Text)
sgrdIPV4Range = lens _sgrdIPV4Range (\s a -> s {_sgrdIPV4Range = a})

-- | The ID of the prefix list for the security group rule.
sgrdPrefixListId :: Lens' SecurityGroupRuleDescription (Maybe Text)
sgrdPrefixListId = lens _sgrdPrefixListId (\s a -> s {_sgrdPrefixListId = a})

-- | The end of the port range for the TCP and UDP protocols, or an ICMP/ICMPv6 code. A value of @-1@ indicates all ICMP/ICMPv6 codes.
sgrdToPort :: Lens' SecurityGroupRuleDescription (Maybe Natural)
sgrdToPort = lens _sgrdToPort (\s a -> s {_sgrdToPort = a}) . mapping _Nat

-- | The IPv6 ranges for the security group rule.
sgrdIPV6Range :: Lens' SecurityGroupRuleDescription (Maybe Text)
sgrdIPV6Range = lens _sgrdIPV6Range (\s a -> s {_sgrdIPV6Range = a})

instance FromJSON SecurityGroupRuleDescription where
  parseJSON =
    withObject
      "SecurityGroupRuleDescription"
      ( \x ->
          SecurityGroupRuleDescription'
            <$> (x .:? "FromPort")
            <*> (x .:? "Protocol")
            <*> (x .:? "IPV4Range")
            <*> (x .:? "PrefixListId")
            <*> (x .:? "ToPort")
            <*> (x .:? "IPV6Range")
      )

instance Hashable SecurityGroupRuleDescription

instance NFData SecurityGroupRuleDescription
