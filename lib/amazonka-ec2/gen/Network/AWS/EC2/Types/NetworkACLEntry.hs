{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkACLEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkACLEntry where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ICMPTypeCode
import Network.AWS.EC2.Types.PortRange
import Network.AWS.EC2.Types.RuleAction
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an entry in a network ACL.
--
--
--
-- /See:/ 'networkACLEntry' smart constructor.
data NetworkACLEntry = NetworkACLEntry'
  { _naeIPv6CidrBlock ::
      !(Maybe Text),
    _naeICMPTypeCode :: !(Maybe ICMPTypeCode),
    _naeRuleNumber :: !(Maybe Int),
    _naeRuleAction :: !(Maybe RuleAction),
    _naeProtocol :: !(Maybe Text),
    _naePortRange :: !(Maybe PortRange),
    _naeCidrBlock :: !(Maybe Text),
    _naeEgress :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NetworkACLEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'naeIPv6CidrBlock' - The IPv6 network range to allow or deny, in CIDR notation.
--
-- * 'naeICMPTypeCode' - ICMP protocol: The ICMP type and code.
--
-- * 'naeRuleNumber' - The rule number for the entry. ACL entries are processed in ascending order by rule number.
--
-- * 'naeRuleAction' - Indicates whether to allow or deny the traffic that matches the rule.
--
-- * 'naeProtocol' - The protocol number. A value of "-1" means all protocols.
--
-- * 'naePortRange' - TCP or UDP protocols: The range of ports the rule applies to.
--
-- * 'naeCidrBlock' - The IPv4 network range to allow or deny, in CIDR notation.
--
-- * 'naeEgress' - Indicates whether the rule is an egress rule (applied to traffic leaving the subnet).
networkACLEntry ::
  NetworkACLEntry
networkACLEntry =
  NetworkACLEntry'
    { _naeIPv6CidrBlock = Nothing,
      _naeICMPTypeCode = Nothing,
      _naeRuleNumber = Nothing,
      _naeRuleAction = Nothing,
      _naeProtocol = Nothing,
      _naePortRange = Nothing,
      _naeCidrBlock = Nothing,
      _naeEgress = Nothing
    }

-- | The IPv6 network range to allow or deny, in CIDR notation.
naeIPv6CidrBlock :: Lens' NetworkACLEntry (Maybe Text)
naeIPv6CidrBlock = lens _naeIPv6CidrBlock (\s a -> s {_naeIPv6CidrBlock = a})

-- | ICMP protocol: The ICMP type and code.
naeICMPTypeCode :: Lens' NetworkACLEntry (Maybe ICMPTypeCode)
naeICMPTypeCode = lens _naeICMPTypeCode (\s a -> s {_naeICMPTypeCode = a})

-- | The rule number for the entry. ACL entries are processed in ascending order by rule number.
naeRuleNumber :: Lens' NetworkACLEntry (Maybe Int)
naeRuleNumber = lens _naeRuleNumber (\s a -> s {_naeRuleNumber = a})

-- | Indicates whether to allow or deny the traffic that matches the rule.
naeRuleAction :: Lens' NetworkACLEntry (Maybe RuleAction)
naeRuleAction = lens _naeRuleAction (\s a -> s {_naeRuleAction = a})

-- | The protocol number. A value of "-1" means all protocols.
naeProtocol :: Lens' NetworkACLEntry (Maybe Text)
naeProtocol = lens _naeProtocol (\s a -> s {_naeProtocol = a})

-- | TCP or UDP protocols: The range of ports the rule applies to.
naePortRange :: Lens' NetworkACLEntry (Maybe PortRange)
naePortRange = lens _naePortRange (\s a -> s {_naePortRange = a})

-- | The IPv4 network range to allow or deny, in CIDR notation.
naeCidrBlock :: Lens' NetworkACLEntry (Maybe Text)
naeCidrBlock = lens _naeCidrBlock (\s a -> s {_naeCidrBlock = a})

-- | Indicates whether the rule is an egress rule (applied to traffic leaving the subnet).
naeEgress :: Lens' NetworkACLEntry (Maybe Bool)
naeEgress = lens _naeEgress (\s a -> s {_naeEgress = a})

instance FromXML NetworkACLEntry where
  parseXML x =
    NetworkACLEntry'
      <$> (x .@? "ipv6CidrBlock")
      <*> (x .@? "icmpTypeCode")
      <*> (x .@? "ruleNumber")
      <*> (x .@? "ruleAction")
      <*> (x .@? "protocol")
      <*> (x .@? "portRange")
      <*> (x .@? "cidrBlock")
      <*> (x .@? "egress")

instance Hashable NetworkACLEntry

instance NFData NetworkACLEntry
