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
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateNetworkACLEntry.html>
module Network.AWS.EC2.CreateNetworkACLEntry
    (
    -- * Request
      CreateNetworkACLEntry
    -- ** Request constructor
    , createNetworkACLEntry
    -- ** Request lenses
    , cnaerqICMPTypeCode
    , cnaerqPortRange
    , cnaerqDryRun
    , cnaerqNetworkACLId
    , cnaerqRuleNumber
    , cnaerqProtocol
    , cnaerqRuleAction
    , cnaerqEgress
    , cnaerqCIdRBlock

    -- * Response
    , CreateNetworkACLEntryResponse
    -- ** Response constructor
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
-- * 'cnaerqICMPTypeCode'
--
-- * 'cnaerqPortRange'
--
-- * 'cnaerqDryRun'
--
-- * 'cnaerqNetworkACLId'
--
-- * 'cnaerqRuleNumber'
--
-- * 'cnaerqProtocol'
--
-- * 'cnaerqRuleAction'
--
-- * 'cnaerqEgress'
--
-- * 'cnaerqCIdRBlock'
data CreateNetworkACLEntry = CreateNetworkACLEntry'
    { _cnaerqICMPTypeCode :: !(Maybe ICMPTypeCode)
    , _cnaerqPortRange    :: !(Maybe PortRange)
    , _cnaerqDryRun       :: !(Maybe Bool)
    , _cnaerqNetworkACLId :: !Text
    , _cnaerqRuleNumber   :: !Int
    , _cnaerqProtocol     :: !Text
    , _cnaerqRuleAction   :: !RuleAction
    , _cnaerqEgress       :: !Bool
    , _cnaerqCIdRBlock    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateNetworkACLEntry' smart constructor.
createNetworkACLEntry :: Text -> Int -> Text -> RuleAction -> Bool -> Text -> CreateNetworkACLEntry
createNetworkACLEntry pNetworkACLId_ pRuleNumber_ pProtocol_ pRuleAction_ pEgress_ pCIdRBlock_ =
    CreateNetworkACLEntry'
    { _cnaerqICMPTypeCode = Nothing
    , _cnaerqPortRange = Nothing
    , _cnaerqDryRun = Nothing
    , _cnaerqNetworkACLId = pNetworkACLId_
    , _cnaerqRuleNumber = pRuleNumber_
    , _cnaerqProtocol = pProtocol_
    , _cnaerqRuleAction = pRuleAction_
    , _cnaerqEgress = pEgress_
    , _cnaerqCIdRBlock = pCIdRBlock_
    }

-- | ICMP protocol: The ICMP type and code. Required if specifying ICMP for
-- the protocol.
cnaerqICMPTypeCode :: Lens' CreateNetworkACLEntry (Maybe ICMPTypeCode)
cnaerqICMPTypeCode = lens _cnaerqICMPTypeCode (\ s a -> s{_cnaerqICMPTypeCode = a});

-- | TCP or UDP protocols: The range of ports the rule applies to.
cnaerqPortRange :: Lens' CreateNetworkACLEntry (Maybe PortRange)
cnaerqPortRange = lens _cnaerqPortRange (\ s a -> s{_cnaerqPortRange = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cnaerqDryRun :: Lens' CreateNetworkACLEntry (Maybe Bool)
cnaerqDryRun = lens _cnaerqDryRun (\ s a -> s{_cnaerqDryRun = a});

-- | The ID of the network ACL.
cnaerqNetworkACLId :: Lens' CreateNetworkACLEntry Text
cnaerqNetworkACLId = lens _cnaerqNetworkACLId (\ s a -> s{_cnaerqNetworkACLId = a});

-- | The rule number for the entry (for example, 100). ACL entries are
-- processed in ascending order by rule number.
--
-- Constraints: Positive integer from 1 to 32766
cnaerqRuleNumber :: Lens' CreateNetworkACLEntry Int
cnaerqRuleNumber = lens _cnaerqRuleNumber (\ s a -> s{_cnaerqRuleNumber = a});

-- | The protocol. A value of -1 means all protocols.
cnaerqProtocol :: Lens' CreateNetworkACLEntry Text
cnaerqProtocol = lens _cnaerqProtocol (\ s a -> s{_cnaerqProtocol = a});

-- | Indicates whether to allow or deny the traffic that matches the rule.
cnaerqRuleAction :: Lens' CreateNetworkACLEntry RuleAction
cnaerqRuleAction = lens _cnaerqRuleAction (\ s a -> s{_cnaerqRuleAction = a});

-- | Indicates whether this is an egress rule (rule is applied to traffic
-- leaving the subnet).
cnaerqEgress :: Lens' CreateNetworkACLEntry Bool
cnaerqEgress = lens _cnaerqEgress (\ s a -> s{_cnaerqEgress = a});

-- | The network range to allow or deny, in CIDR notation (for example
-- @172.16.0.0\/24@).
cnaerqCIdRBlock :: Lens' CreateNetworkACLEntry Text
cnaerqCIdRBlock = lens _cnaerqCIdRBlock (\ s a -> s{_cnaerqCIdRBlock = a});

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
              ["Action" =: ("CreateNetworkACLEntry" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "Icmp" =: _cnaerqICMPTypeCode,
               "PortRange" =: _cnaerqPortRange,
               "DryRun" =: _cnaerqDryRun,
               "NetworkAclId" =: _cnaerqNetworkACLId,
               "RuleNumber" =: _cnaerqRuleNumber,
               "Protocol" =: _cnaerqProtocol,
               "RuleAction" =: _cnaerqRuleAction,
               "Egress" =: _cnaerqEgress,
               "CidrBlock" =: _cnaerqCIdRBlock]

-- | /See:/ 'createNetworkACLEntryResponse' smart constructor.
data CreateNetworkACLEntryResponse =
    CreateNetworkACLEntryResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateNetworkACLEntryResponse' smart constructor.
createNetworkACLEntryResponse :: CreateNetworkACLEntryResponse
createNetworkACLEntryResponse = CreateNetworkACLEntryResponse'
