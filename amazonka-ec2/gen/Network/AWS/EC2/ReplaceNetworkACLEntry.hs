{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ReplaceNetworkACLEntry
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Replaces an entry (rule) in a network ACL. For more information about
-- network ACLs, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_ACLs.html Network ACLs>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ReplaceNetworkACLEntry.html>
module Network.AWS.EC2.ReplaceNetworkACLEntry
    (
    -- * Request
      ReplaceNetworkACLEntry
    -- ** Request constructor
    , replaceNetworkACLEntry
    -- ** Request lenses
    , rnaerqICMPTypeCode
    , rnaerqPortRange
    , rnaerqDryRun
    , rnaerqNetworkACLId
    , rnaerqRuleNumber
    , rnaerqProtocol
    , rnaerqRuleAction
    , rnaerqEgress
    , rnaerqCIdRBlock

    -- * Response
    , ReplaceNetworkACLEntryResponse
    -- ** Response constructor
    , replaceNetworkACLEntryResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'replaceNetworkACLEntry' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rnaerqICMPTypeCode'
--
-- * 'rnaerqPortRange'
--
-- * 'rnaerqDryRun'
--
-- * 'rnaerqNetworkACLId'
--
-- * 'rnaerqRuleNumber'
--
-- * 'rnaerqProtocol'
--
-- * 'rnaerqRuleAction'
--
-- * 'rnaerqEgress'
--
-- * 'rnaerqCIdRBlock'
data ReplaceNetworkACLEntry = ReplaceNetworkACLEntry'
    { _rnaerqICMPTypeCode :: !(Maybe ICMPTypeCode)
    , _rnaerqPortRange    :: !(Maybe PortRange)
    , _rnaerqDryRun       :: !(Maybe Bool)
    , _rnaerqNetworkACLId :: !Text
    , _rnaerqRuleNumber   :: !Int
    , _rnaerqProtocol     :: !Text
    , _rnaerqRuleAction   :: !RuleAction
    , _rnaerqEgress       :: !Bool
    , _rnaerqCIdRBlock    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ReplaceNetworkACLEntry' smart constructor.
replaceNetworkACLEntry :: Text -> Int -> Text -> RuleAction -> Bool -> Text -> ReplaceNetworkACLEntry
replaceNetworkACLEntry pNetworkACLId_ pRuleNumber_ pProtocol_ pRuleAction_ pEgress_ pCIdRBlock_ =
    ReplaceNetworkACLEntry'
    { _rnaerqICMPTypeCode = Nothing
    , _rnaerqPortRange = Nothing
    , _rnaerqDryRun = Nothing
    , _rnaerqNetworkACLId = pNetworkACLId_
    , _rnaerqRuleNumber = pRuleNumber_
    , _rnaerqProtocol = pProtocol_
    , _rnaerqRuleAction = pRuleAction_
    , _rnaerqEgress = pEgress_
    , _rnaerqCIdRBlock = pCIdRBlock_
    }

-- | ICMP protocol: The ICMP type and code. Required if specifying 1 (ICMP)
-- for the protocol.
rnaerqICMPTypeCode :: Lens' ReplaceNetworkACLEntry (Maybe ICMPTypeCode)
rnaerqICMPTypeCode = lens _rnaerqICMPTypeCode (\ s a -> s{_rnaerqICMPTypeCode = a});

-- | TCP or UDP protocols: The range of ports the rule applies to. Required
-- if specifying 6 (TCP) or 17 (UDP) for the protocol.
rnaerqPortRange :: Lens' ReplaceNetworkACLEntry (Maybe PortRange)
rnaerqPortRange = lens _rnaerqPortRange (\ s a -> s{_rnaerqPortRange = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
rnaerqDryRun :: Lens' ReplaceNetworkACLEntry (Maybe Bool)
rnaerqDryRun = lens _rnaerqDryRun (\ s a -> s{_rnaerqDryRun = a});

-- | The ID of the ACL.
rnaerqNetworkACLId :: Lens' ReplaceNetworkACLEntry Text
rnaerqNetworkACLId = lens _rnaerqNetworkACLId (\ s a -> s{_rnaerqNetworkACLId = a});

-- | The rule number of the entry to replace.
rnaerqRuleNumber :: Lens' ReplaceNetworkACLEntry Int
rnaerqRuleNumber = lens _rnaerqRuleNumber (\ s a -> s{_rnaerqRuleNumber = a});

-- | The IP protocol. You can specify @all@ or @-1@ to mean all protocols.
rnaerqProtocol :: Lens' ReplaceNetworkACLEntry Text
rnaerqProtocol = lens _rnaerqProtocol (\ s a -> s{_rnaerqProtocol = a});

-- | Indicates whether to allow or deny the traffic that matches the rule.
rnaerqRuleAction :: Lens' ReplaceNetworkACLEntry RuleAction
rnaerqRuleAction = lens _rnaerqRuleAction (\ s a -> s{_rnaerqRuleAction = a});

-- | Indicates whether to replace the egress rule.
--
-- Default: If no value is specified, we replace the ingress rule.
rnaerqEgress :: Lens' ReplaceNetworkACLEntry Bool
rnaerqEgress = lens _rnaerqEgress (\ s a -> s{_rnaerqEgress = a});

-- | The network range to allow or deny, in CIDR notation.
rnaerqCIdRBlock :: Lens' ReplaceNetworkACLEntry Text
rnaerqCIdRBlock = lens _rnaerqCIdRBlock (\ s a -> s{_rnaerqCIdRBlock = a});

instance AWSRequest ReplaceNetworkACLEntry where
        type Sv ReplaceNetworkACLEntry = EC2
        type Rs ReplaceNetworkACLEntry =
             ReplaceNetworkACLEntryResponse
        request = post
        response
          = receiveNull ReplaceNetworkACLEntryResponse'

instance ToHeaders ReplaceNetworkACLEntry where
        toHeaders = const mempty

instance ToPath ReplaceNetworkACLEntry where
        toPath = const "/"

instance ToQuery ReplaceNetworkACLEntry where
        toQuery ReplaceNetworkACLEntry'{..}
          = mconcat
              ["Action" =:
                 ("ReplaceNetworkACLEntry" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "Icmp" =: _rnaerqICMPTypeCode,
               "PortRange" =: _rnaerqPortRange,
               "DryRun" =: _rnaerqDryRun,
               "NetworkAclId" =: _rnaerqNetworkACLId,
               "RuleNumber" =: _rnaerqRuleNumber,
               "Protocol" =: _rnaerqProtocol,
               "RuleAction" =: _rnaerqRuleAction,
               "Egress" =: _rnaerqEgress,
               "CidrBlock" =: _rnaerqCIdRBlock]

-- | /See:/ 'replaceNetworkACLEntryResponse' smart constructor.
data ReplaceNetworkACLEntryResponse =
    ReplaceNetworkACLEntryResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ReplaceNetworkACLEntryResponse' smart constructor.
replaceNetworkACLEntryResponse :: ReplaceNetworkACLEntryResponse
replaceNetworkACLEntryResponse = ReplaceNetworkACLEntryResponse'
