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
-- Module      : Network.AWS.EC2.ReplaceNetworkACLEntry
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces an entry (rule) in a network ACL. For more information about network ACLs, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_ACLs.html Network ACLs> in the /Amazon Virtual Private Cloud User Guide/ .
--
--
module Network.AWS.EC2.ReplaceNetworkACLEntry
    (
    -- * Creating a Request
      replaceNetworkACLEntry
    , ReplaceNetworkACLEntry
    -- * Request Lenses
    , rnaeIPv6CidrBlock
    , rnaeICMPTypeCode
    , rnaePortRange
    , rnaeCidrBlock
    , rnaeDryRun
    , rnaeEgress
    , rnaeNetworkACLId
    , rnaeProtocol
    , rnaeRuleAction
    , rnaeRuleNumber

    -- * Destructuring the Response
    , replaceNetworkACLEntryResponse
    , ReplaceNetworkACLEntryResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for ReplaceNetworkAclEntry.
--
--
--
-- /See:/ 'replaceNetworkACLEntry' smart constructor.
data ReplaceNetworkACLEntry = ReplaceNetworkACLEntry'
  { _rnaeIPv6CidrBlock :: !(Maybe Text)
  , _rnaeICMPTypeCode  :: !(Maybe ICMPTypeCode)
  , _rnaePortRange     :: !(Maybe PortRange)
  , _rnaeCidrBlock     :: !(Maybe Text)
  , _rnaeDryRun        :: !(Maybe Bool)
  , _rnaeEgress        :: !Bool
  , _rnaeNetworkACLId  :: !Text
  , _rnaeProtocol      :: !Text
  , _rnaeRuleAction    :: !RuleAction
  , _rnaeRuleNumber    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplaceNetworkACLEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rnaeIPv6CidrBlock' - The IPv6 network range to allow or deny, in CIDR notation (for example @2001:bd8:1234:1a00::/64@ ).
--
-- * 'rnaeICMPTypeCode' - ICMP protocol: The ICMP or ICMPv6 type and code. Required if specifying the ICMP (1) protocol, or protocol 58 (ICMPv6) with an IPv6 CIDR block.
--
-- * 'rnaePortRange' - TCP or UDP protocols: The range of ports the rule applies to. Required if specifying TCP (6) or UDP (17) for the protocol.
--
-- * 'rnaeCidrBlock' - The IPv4 network range to allow or deny, in CIDR notation (for example @172.16.0.0/24@ ).
--
-- * 'rnaeDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'rnaeEgress' - Indicates whether to replace the egress rule. Default: If no value is specified, we replace the ingress rule.
--
-- * 'rnaeNetworkACLId' - The ID of the ACL.
--
-- * 'rnaeProtocol' - The IP protocol. You can specify @all@ or @-1@ to mean all protocols. If you specify @all@ , @-1@ , or a protocol number other than @tcp@ , @udp@ , or @icmp@ , traffic on all ports is allowed, regardless of any ports or ICMP types or codes you specify. If you specify protocol @58@ (ICMPv6) and specify an IPv4 CIDR block, traffic for all ICMP types and codes allowed, regardless of any that you specify. If you specify protocol @58@ (ICMPv6) and specify an IPv6 CIDR block, you must specify an ICMP type and code.
--
-- * 'rnaeRuleAction' - Indicates whether to allow or deny the traffic that matches the rule.
--
-- * 'rnaeRuleNumber' - The rule number of the entry to replace.
replaceNetworkACLEntry
    :: Bool -- ^ 'rnaeEgress'
    -> Text -- ^ 'rnaeNetworkACLId'
    -> Text -- ^ 'rnaeProtocol'
    -> RuleAction -- ^ 'rnaeRuleAction'
    -> Int -- ^ 'rnaeRuleNumber'
    -> ReplaceNetworkACLEntry
replaceNetworkACLEntry pEgress_ pNetworkACLId_ pProtocol_ pRuleAction_ pRuleNumber_ =
  ReplaceNetworkACLEntry'
    { _rnaeIPv6CidrBlock = Nothing
    , _rnaeICMPTypeCode = Nothing
    , _rnaePortRange = Nothing
    , _rnaeCidrBlock = Nothing
    , _rnaeDryRun = Nothing
    , _rnaeEgress = pEgress_
    , _rnaeNetworkACLId = pNetworkACLId_
    , _rnaeProtocol = pProtocol_
    , _rnaeRuleAction = pRuleAction_
    , _rnaeRuleNumber = pRuleNumber_
    }


-- | The IPv6 network range to allow or deny, in CIDR notation (for example @2001:bd8:1234:1a00::/64@ ).
rnaeIPv6CidrBlock :: Lens' ReplaceNetworkACLEntry (Maybe Text)
rnaeIPv6CidrBlock = lens _rnaeIPv6CidrBlock (\ s a -> s{_rnaeIPv6CidrBlock = a})

-- | ICMP protocol: The ICMP or ICMPv6 type and code. Required if specifying the ICMP (1) protocol, or protocol 58 (ICMPv6) with an IPv6 CIDR block.
rnaeICMPTypeCode :: Lens' ReplaceNetworkACLEntry (Maybe ICMPTypeCode)
rnaeICMPTypeCode = lens _rnaeICMPTypeCode (\ s a -> s{_rnaeICMPTypeCode = a})

-- | TCP or UDP protocols: The range of ports the rule applies to. Required if specifying TCP (6) or UDP (17) for the protocol.
rnaePortRange :: Lens' ReplaceNetworkACLEntry (Maybe PortRange)
rnaePortRange = lens _rnaePortRange (\ s a -> s{_rnaePortRange = a})

-- | The IPv4 network range to allow or deny, in CIDR notation (for example @172.16.0.0/24@ ).
rnaeCidrBlock :: Lens' ReplaceNetworkACLEntry (Maybe Text)
rnaeCidrBlock = lens _rnaeCidrBlock (\ s a -> s{_rnaeCidrBlock = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
rnaeDryRun :: Lens' ReplaceNetworkACLEntry (Maybe Bool)
rnaeDryRun = lens _rnaeDryRun (\ s a -> s{_rnaeDryRun = a})

-- | Indicates whether to replace the egress rule. Default: If no value is specified, we replace the ingress rule.
rnaeEgress :: Lens' ReplaceNetworkACLEntry Bool
rnaeEgress = lens _rnaeEgress (\ s a -> s{_rnaeEgress = a})

-- | The ID of the ACL.
rnaeNetworkACLId :: Lens' ReplaceNetworkACLEntry Text
rnaeNetworkACLId = lens _rnaeNetworkACLId (\ s a -> s{_rnaeNetworkACLId = a})

-- | The IP protocol. You can specify @all@ or @-1@ to mean all protocols. If you specify @all@ , @-1@ , or a protocol number other than @tcp@ , @udp@ , or @icmp@ , traffic on all ports is allowed, regardless of any ports or ICMP types or codes you specify. If you specify protocol @58@ (ICMPv6) and specify an IPv4 CIDR block, traffic for all ICMP types and codes allowed, regardless of any that you specify. If you specify protocol @58@ (ICMPv6) and specify an IPv6 CIDR block, you must specify an ICMP type and code.
rnaeProtocol :: Lens' ReplaceNetworkACLEntry Text
rnaeProtocol = lens _rnaeProtocol (\ s a -> s{_rnaeProtocol = a})

-- | Indicates whether to allow or deny the traffic that matches the rule.
rnaeRuleAction :: Lens' ReplaceNetworkACLEntry RuleAction
rnaeRuleAction = lens _rnaeRuleAction (\ s a -> s{_rnaeRuleAction = a})

-- | The rule number of the entry to replace.
rnaeRuleNumber :: Lens' ReplaceNetworkACLEntry Int
rnaeRuleNumber = lens _rnaeRuleNumber (\ s a -> s{_rnaeRuleNumber = a})

instance AWSRequest ReplaceNetworkACLEntry where
        type Rs ReplaceNetworkACLEntry =
             ReplaceNetworkACLEntryResponse
        request = postQuery ec2
        response
          = receiveNull ReplaceNetworkACLEntryResponse'

instance Hashable ReplaceNetworkACLEntry where

instance NFData ReplaceNetworkACLEntry where

instance ToHeaders ReplaceNetworkACLEntry where
        toHeaders = const mempty

instance ToPath ReplaceNetworkACLEntry where
        toPath = const "/"

instance ToQuery ReplaceNetworkACLEntry where
        toQuery ReplaceNetworkACLEntry'{..}
          = mconcat
              ["Action" =:
                 ("ReplaceNetworkAclEntry" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "Ipv6CidrBlock" =: _rnaeIPv6CidrBlock,
               "Icmp" =: _rnaeICMPTypeCode,
               "PortRange" =: _rnaePortRange,
               "CidrBlock" =: _rnaeCidrBlock,
               "DryRun" =: _rnaeDryRun, "Egress" =: _rnaeEgress,
               "NetworkAclId" =: _rnaeNetworkACLId,
               "Protocol" =: _rnaeProtocol,
               "RuleAction" =: _rnaeRuleAction,
               "RuleNumber" =: _rnaeRuleNumber]

-- | /See:/ 'replaceNetworkACLEntryResponse' smart constructor.
data ReplaceNetworkACLEntryResponse =
  ReplaceNetworkACLEntryResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplaceNetworkACLEntryResponse' with the minimum fields required to make a request.
--
replaceNetworkACLEntryResponse
    :: ReplaceNetworkACLEntryResponse
replaceNetworkACLEntryResponse = ReplaceNetworkACLEntryResponse'


instance NFData ReplaceNetworkACLEntryResponse where
