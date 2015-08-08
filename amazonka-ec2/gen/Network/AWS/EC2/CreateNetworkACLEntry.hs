{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateNetworkACLEntry
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates an entry (a rule) in a network ACL with the specified rule
-- number. Each network ACL has a set of numbered ingress rules and a
-- separate set of numbered egress rules. When determining whether a packet
-- should be allowed in or out of a subnet associated with the ACL, we
-- process the entries in the ACL according to the rule numbers, in
-- ascending order. Each network ACL has a set of ingress rules and a
-- separate set of egress rules.
--
-- We recommend that you leave room between the rule numbers (for example,
-- 100, 110, 120, ...), and not number them one right after the other (for
-- example, 101, 102, 103, ...). This makes it easier to add a rule between
-- existing ones without having to renumber the rules.
--
-- After you add an entry, you can\'t modify it; you must either replace
-- it, or create an entry and delete the old one.
--
-- For more information about network ACLs, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_ACLs.html Network ACLs>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateNetworkACLEntry.html AWS API Reference> for CreateNetworkACLEntry.
module Network.AWS.EC2.CreateNetworkACLEntry
    (
    -- * Creating a Request
      CreateNetworkACLEntry
    , createNetworkACLEntry
    -- * Request Lenses
    , cnaeICMPTypeCode
    , cnaePortRange
    , cnaeDryRun
    , cnaeNetworkACLId
    , cnaeRuleNumber
    , cnaeProtocol
    , cnaeRuleAction
    , cnaeEgress
    , cnaeCIdRBlock

    -- * Destructuring the Response
    , CreateNetworkACLEntryResponse
    , createNetworkACLEntryResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createNetworkACLEntry' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cnaeICMPTypeCode'
--
-- * 'cnaePortRange'
--
-- * 'cnaeDryRun'
--
-- * 'cnaeNetworkACLId'
--
-- * 'cnaeRuleNumber'
--
-- * 'cnaeProtocol'
--
-- * 'cnaeRuleAction'
--
-- * 'cnaeEgress'
--
-- * 'cnaeCIdRBlock'
data CreateNetworkACLEntry = CreateNetworkACLEntry'
    { _cnaeICMPTypeCode :: !(Maybe ICMPTypeCode)
    , _cnaePortRange    :: !(Maybe PortRange)
    , _cnaeDryRun       :: !(Maybe Bool)
    , _cnaeNetworkACLId :: !Text
    , _cnaeRuleNumber   :: !Int
    , _cnaeProtocol     :: !Text
    , _cnaeRuleAction   :: !RuleAction
    , _cnaeEgress       :: !Bool
    , _cnaeCIdRBlock    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateNetworkACLEntry' smart constructor.
createNetworkACLEntry :: Text -> Int -> Text -> RuleAction -> Bool -> Text -> CreateNetworkACLEntry
createNetworkACLEntry pNetworkACLId_ pRuleNumber_ pProtocol_ pRuleAction_ pEgress_ pCIdRBlock_ =
    CreateNetworkACLEntry'
    { _cnaeICMPTypeCode = Nothing
    , _cnaePortRange = Nothing
    , _cnaeDryRun = Nothing
    , _cnaeNetworkACLId = pNetworkACLId_
    , _cnaeRuleNumber = pRuleNumber_
    , _cnaeProtocol = pProtocol_
    , _cnaeRuleAction = pRuleAction_
    , _cnaeEgress = pEgress_
    , _cnaeCIdRBlock = pCIdRBlock_
    }

-- | ICMP protocol: The ICMP type and code. Required if specifying ICMP for
-- the protocol.
cnaeICMPTypeCode :: Lens' CreateNetworkACLEntry (Maybe ICMPTypeCode)
cnaeICMPTypeCode = lens _cnaeICMPTypeCode (\ s a -> s{_cnaeICMPTypeCode = a});

-- | TCP or UDP protocols: The range of ports the rule applies to.
cnaePortRange :: Lens' CreateNetworkACLEntry (Maybe PortRange)
cnaePortRange = lens _cnaePortRange (\ s a -> s{_cnaePortRange = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cnaeDryRun :: Lens' CreateNetworkACLEntry (Maybe Bool)
cnaeDryRun = lens _cnaeDryRun (\ s a -> s{_cnaeDryRun = a});

-- | The ID of the network ACL.
cnaeNetworkACLId :: Lens' CreateNetworkACLEntry Text
cnaeNetworkACLId = lens _cnaeNetworkACLId (\ s a -> s{_cnaeNetworkACLId = a});

-- | The rule number for the entry (for example, 100). ACL entries are
-- processed in ascending order by rule number.
--
-- Constraints: Positive integer from 1 to 32766
cnaeRuleNumber :: Lens' CreateNetworkACLEntry Int
cnaeRuleNumber = lens _cnaeRuleNumber (\ s a -> s{_cnaeRuleNumber = a});

-- | The protocol. A value of -1 means all protocols.
cnaeProtocol :: Lens' CreateNetworkACLEntry Text
cnaeProtocol = lens _cnaeProtocol (\ s a -> s{_cnaeProtocol = a});

-- | Indicates whether to allow or deny the traffic that matches the rule.
cnaeRuleAction :: Lens' CreateNetworkACLEntry RuleAction
cnaeRuleAction = lens _cnaeRuleAction (\ s a -> s{_cnaeRuleAction = a});

-- | Indicates whether this is an egress rule (rule is applied to traffic
-- leaving the subnet).
cnaeEgress :: Lens' CreateNetworkACLEntry Bool
cnaeEgress = lens _cnaeEgress (\ s a -> s{_cnaeEgress = a});

-- | The network range to allow or deny, in CIDR notation (for example
-- @172.16.0.0\/24@).
cnaeCIdRBlock :: Lens' CreateNetworkACLEntry Text
cnaeCIdRBlock = lens _cnaeCIdRBlock (\ s a -> s{_cnaeCIdRBlock = a});

instance AWSRequest CreateNetworkACLEntry where
        type Sv CreateNetworkACLEntry = EC2
        type Rs CreateNetworkACLEntry =
             CreateNetworkACLEntryResponse
        request = post
        response = receiveNull CreateNetworkACLEntryResponse'

instance ToHeaders CreateNetworkACLEntry where
        toHeaders = const mempty

instance ToPath CreateNetworkACLEntry where
        toPath = const "/"

instance ToQuery CreateNetworkACLEntry where
        toQuery CreateNetworkACLEntry'{..}
          = mconcat
              ["Action" =: ("CreateNetworkAclEntry" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "Icmp" =: _cnaeICMPTypeCode,
               "PortRange" =: _cnaePortRange,
               "DryRun" =: _cnaeDryRun,
               "NetworkAclId" =: _cnaeNetworkACLId,
               "RuleNumber" =: _cnaeRuleNumber,
               "Protocol" =: _cnaeProtocol,
               "RuleAction" =: _cnaeRuleAction,
               "Egress" =: _cnaeEgress,
               "CidrBlock" =: _cnaeCIdRBlock]

-- | /See:/ 'createNetworkACLEntryResponse' smart constructor.
data CreateNetworkACLEntryResponse =
    CreateNetworkACLEntryResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateNetworkACLEntryResponse' smart constructor.
createNetworkACLEntryResponse :: CreateNetworkACLEntryResponse
createNetworkACLEntryResponse = CreateNetworkACLEntryResponse'
