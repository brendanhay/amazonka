{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateNetworkACLEntry
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an entry (a rule) in a network ACL with the specified rule number. Each network ACL has a set of numbered ingress rules and a separate set of numbered egress rules. When determining whether a packet should be allowed in or out of a subnet associated with the ACL, we process the entries in the ACL according to the rule numbers, in ascending order. Each network ACL has a set of ingress rules and a separate set of egress rules.
--
--
-- We recommend that you leave room between the rule numbers (for example, 100, 110, 120, ...), and not number them one right after the other (for example, 101, 102, 103, ...). This makes it easier to add a rule between existing ones without having to renumber the rules.
--
-- After you add an entry, you can't modify it; you must either replace it, or create an entry and delete the old one.
--
-- For more information about network ACLs, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_ACLs.html Network ACLs> in the /Amazon Virtual Private Cloud User Guide/ .
--
module Network.AWS.EC2.CreateNetworkACLEntry
    (
    -- * Creating a Request
      createNetworkACLEntry
    , CreateNetworkACLEntry
    -- * Request Lenses
    , cnaeIPv6CidrBlock
    , cnaeICMPTypeCode
    , cnaePortRange
    , cnaeCidrBlock
    , cnaeDryRun
    , cnaeEgress
    , cnaeNetworkACLId
    , cnaeProtocol
    , cnaeRuleAction
    , cnaeRuleNumber

    -- * Destructuring the Response
    , createNetworkACLEntryResponse
    , CreateNetworkACLEntryResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for CreateNetworkAclEntry.
--
--
--
-- /See:/ 'createNetworkACLEntry' smart constructor.
data CreateNetworkACLEntry = CreateNetworkACLEntry'
  { _cnaeIPv6CidrBlock :: !(Maybe Text)
  , _cnaeICMPTypeCode  :: !(Maybe ICMPTypeCode)
  , _cnaePortRange     :: !(Maybe PortRange)
  , _cnaeCidrBlock     :: !(Maybe Text)
  , _cnaeDryRun        :: !(Maybe Bool)
  , _cnaeEgress        :: !Bool
  , _cnaeNetworkACLId  :: !Text
  , _cnaeProtocol      :: !Text
  , _cnaeRuleAction    :: !RuleAction
  , _cnaeRuleNumber    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateNetworkACLEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnaeIPv6CidrBlock' - The IPv6 network range to allow or deny, in CIDR notation (for example @2001:db8:1234:1a00::/64@ ).
--
-- * 'cnaeICMPTypeCode' - ICMP protocol: The ICMP or ICMPv6 type and code. Required if specifying the ICMP protocol, or protocol 58 (ICMPv6) with an IPv6 CIDR block.
--
-- * 'cnaePortRange' - TCP or UDP protocols: The range of ports the rule applies to.
--
-- * 'cnaeCidrBlock' - The IPv4 network range to allow or deny, in CIDR notation (for example @172.16.0.0/24@ ).
--
-- * 'cnaeDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'cnaeEgress' - Indicates whether this is an egress rule (rule is applied to traffic leaving the subnet).
--
-- * 'cnaeNetworkACLId' - The ID of the network ACL.
--
-- * 'cnaeProtocol' - The protocol. A value of @-1@ or @all@ means all protocols. If you specify @all@ , @-1@ , or a protocol number other than @6@ (tcp), @17@ (udp), or @1@ (icmp), traffic on all ports is allowed, regardless of any ports or ICMP types or codes you specify. If you specify protocol @58@ (ICMPv6) and specify an IPv4 CIDR block, traffic for all ICMP types and codes allowed, regardless of any that you specify. If you specify protocol @58@ (ICMPv6) and specify an IPv6 CIDR block, you must specify an ICMP type and code.
--
-- * 'cnaeRuleAction' - Indicates whether to allow or deny the traffic that matches the rule.
--
-- * 'cnaeRuleNumber' - The rule number for the entry (for example, 100). ACL entries are processed in ascending order by rule number. Constraints: Positive integer from 1 to 32766. The range 32767 to 65535 is reserved for internal use.
createNetworkACLEntry
    :: Bool -- ^ 'cnaeEgress'
    -> Text -- ^ 'cnaeNetworkACLId'
    -> Text -- ^ 'cnaeProtocol'
    -> RuleAction -- ^ 'cnaeRuleAction'
    -> Int -- ^ 'cnaeRuleNumber'
    -> CreateNetworkACLEntry
createNetworkACLEntry pEgress_ pNetworkACLId_ pProtocol_ pRuleAction_ pRuleNumber_ =
  CreateNetworkACLEntry'
    { _cnaeIPv6CidrBlock = Nothing
    , _cnaeICMPTypeCode = Nothing
    , _cnaePortRange = Nothing
    , _cnaeCidrBlock = Nothing
    , _cnaeDryRun = Nothing
    , _cnaeEgress = pEgress_
    , _cnaeNetworkACLId = pNetworkACLId_
    , _cnaeProtocol = pProtocol_
    , _cnaeRuleAction = pRuleAction_
    , _cnaeRuleNumber = pRuleNumber_
    }


-- | The IPv6 network range to allow or deny, in CIDR notation (for example @2001:db8:1234:1a00::/64@ ).
cnaeIPv6CidrBlock :: Lens' CreateNetworkACLEntry (Maybe Text)
cnaeIPv6CidrBlock = lens _cnaeIPv6CidrBlock (\ s a -> s{_cnaeIPv6CidrBlock = a})

-- | ICMP protocol: The ICMP or ICMPv6 type and code. Required if specifying the ICMP protocol, or protocol 58 (ICMPv6) with an IPv6 CIDR block.
cnaeICMPTypeCode :: Lens' CreateNetworkACLEntry (Maybe ICMPTypeCode)
cnaeICMPTypeCode = lens _cnaeICMPTypeCode (\ s a -> s{_cnaeICMPTypeCode = a})

-- | TCP or UDP protocols: The range of ports the rule applies to.
cnaePortRange :: Lens' CreateNetworkACLEntry (Maybe PortRange)
cnaePortRange = lens _cnaePortRange (\ s a -> s{_cnaePortRange = a})

-- | The IPv4 network range to allow or deny, in CIDR notation (for example @172.16.0.0/24@ ).
cnaeCidrBlock :: Lens' CreateNetworkACLEntry (Maybe Text)
cnaeCidrBlock = lens _cnaeCidrBlock (\ s a -> s{_cnaeCidrBlock = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cnaeDryRun :: Lens' CreateNetworkACLEntry (Maybe Bool)
cnaeDryRun = lens _cnaeDryRun (\ s a -> s{_cnaeDryRun = a})

-- | Indicates whether this is an egress rule (rule is applied to traffic leaving the subnet).
cnaeEgress :: Lens' CreateNetworkACLEntry Bool
cnaeEgress = lens _cnaeEgress (\ s a -> s{_cnaeEgress = a})

-- | The ID of the network ACL.
cnaeNetworkACLId :: Lens' CreateNetworkACLEntry Text
cnaeNetworkACLId = lens _cnaeNetworkACLId (\ s a -> s{_cnaeNetworkACLId = a})

-- | The protocol. A value of @-1@ or @all@ means all protocols. If you specify @all@ , @-1@ , or a protocol number other than @6@ (tcp), @17@ (udp), or @1@ (icmp), traffic on all ports is allowed, regardless of any ports or ICMP types or codes you specify. If you specify protocol @58@ (ICMPv6) and specify an IPv4 CIDR block, traffic for all ICMP types and codes allowed, regardless of any that you specify. If you specify protocol @58@ (ICMPv6) and specify an IPv6 CIDR block, you must specify an ICMP type and code.
cnaeProtocol :: Lens' CreateNetworkACLEntry Text
cnaeProtocol = lens _cnaeProtocol (\ s a -> s{_cnaeProtocol = a})

-- | Indicates whether to allow or deny the traffic that matches the rule.
cnaeRuleAction :: Lens' CreateNetworkACLEntry RuleAction
cnaeRuleAction = lens _cnaeRuleAction (\ s a -> s{_cnaeRuleAction = a})

-- | The rule number for the entry (for example, 100). ACL entries are processed in ascending order by rule number. Constraints: Positive integer from 1 to 32766. The range 32767 to 65535 is reserved for internal use.
cnaeRuleNumber :: Lens' CreateNetworkACLEntry Int
cnaeRuleNumber = lens _cnaeRuleNumber (\ s a -> s{_cnaeRuleNumber = a})

instance AWSRequest CreateNetworkACLEntry where
        type Rs CreateNetworkACLEntry =
             CreateNetworkACLEntryResponse
        request = postQuery ec2
        response = receiveNull CreateNetworkACLEntryResponse'

instance Hashable CreateNetworkACLEntry where

instance NFData CreateNetworkACLEntry where

instance ToHeaders CreateNetworkACLEntry where
        toHeaders = const mempty

instance ToPath CreateNetworkACLEntry where
        toPath = const "/"

instance ToQuery CreateNetworkACLEntry where
        toQuery CreateNetworkACLEntry'{..}
          = mconcat
              ["Action" =: ("CreateNetworkAclEntry" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "Ipv6CidrBlock" =: _cnaeIPv6CidrBlock,
               "Icmp" =: _cnaeICMPTypeCode,
               "PortRange" =: _cnaePortRange,
               "CidrBlock" =: _cnaeCidrBlock,
               "DryRun" =: _cnaeDryRun, "Egress" =: _cnaeEgress,
               "NetworkAclId" =: _cnaeNetworkACLId,
               "Protocol" =: _cnaeProtocol,
               "RuleAction" =: _cnaeRuleAction,
               "RuleNumber" =: _cnaeRuleNumber]

-- | /See:/ 'createNetworkACLEntryResponse' smart constructor.
data CreateNetworkACLEntryResponse =
  CreateNetworkACLEntryResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateNetworkACLEntryResponse' with the minimum fields required to make a request.
--
createNetworkACLEntryResponse
    :: CreateNetworkACLEntryResponse
createNetworkACLEntryResponse = CreateNetworkACLEntryResponse'


instance NFData CreateNetworkACLEntryResponse where
