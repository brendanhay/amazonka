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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces an entry (rule) in a network ACL. For more information about
-- network ACLs, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_ACLs.html Network ACLs>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Network.AWS.EC2.ReplaceNetworkACLEntry
    (
    -- * Creating a Request
      replaceNetworkACLEntry
    , ReplaceNetworkACLEntry
    -- * Request Lenses
    , rnaeICMPTypeCode
    , rnaePortRange
    , rnaeDryRun
    , rnaeNetworkACLId
    , rnaeRuleNumber
    , rnaeProtocol
    , rnaeRuleAction
    , rnaeEgress
    , rnaeCIdRBlock

    -- * Destructuring the Response
    , replaceNetworkACLEntryResponse
    , ReplaceNetworkACLEntryResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for ReplaceNetworkAclEntry.
--
-- /See:/ 'replaceNetworkACLEntry' smart constructor.
data ReplaceNetworkACLEntry = ReplaceNetworkACLEntry'
    { _rnaeICMPTypeCode :: !(Maybe ICMPTypeCode)
    , _rnaePortRange    :: !(Maybe PortRange)
    , _rnaeDryRun       :: !(Maybe Bool)
    , _rnaeNetworkACLId :: !Text
    , _rnaeRuleNumber   :: !Int
    , _rnaeProtocol     :: !Text
    , _rnaeRuleAction   :: !RuleAction
    , _rnaeEgress       :: !Bool
    , _rnaeCIdRBlock    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReplaceNetworkACLEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rnaeICMPTypeCode'
--
-- * 'rnaePortRange'
--
-- * 'rnaeDryRun'
--
-- * 'rnaeNetworkACLId'
--
-- * 'rnaeRuleNumber'
--
-- * 'rnaeProtocol'
--
-- * 'rnaeRuleAction'
--
-- * 'rnaeEgress'
--
-- * 'rnaeCIdRBlock'
replaceNetworkACLEntry
    :: Text -- ^ 'rnaeNetworkACLId'
    -> Int -- ^ 'rnaeRuleNumber'
    -> Text -- ^ 'rnaeProtocol'
    -> RuleAction -- ^ 'rnaeRuleAction'
    -> Bool -- ^ 'rnaeEgress'
    -> Text -- ^ 'rnaeCIdRBlock'
    -> ReplaceNetworkACLEntry
replaceNetworkACLEntry pNetworkACLId_ pRuleNumber_ pProtocol_ pRuleAction_ pEgress_ pCIdRBlock_ =
    ReplaceNetworkACLEntry'
    { _rnaeICMPTypeCode = Nothing
    , _rnaePortRange = Nothing
    , _rnaeDryRun = Nothing
    , _rnaeNetworkACLId = pNetworkACLId_
    , _rnaeRuleNumber = pRuleNumber_
    , _rnaeProtocol = pProtocol_
    , _rnaeRuleAction = pRuleAction_
    , _rnaeEgress = pEgress_
    , _rnaeCIdRBlock = pCIdRBlock_
    }

-- | ICMP protocol: The ICMP type and code. Required if specifying 1 (ICMP)
-- for the protocol.
rnaeICMPTypeCode :: Lens' ReplaceNetworkACLEntry (Maybe ICMPTypeCode)
rnaeICMPTypeCode = lens _rnaeICMPTypeCode (\ s a -> s{_rnaeICMPTypeCode = a});

-- | TCP or UDP protocols: The range of ports the rule applies to. Required
-- if specifying 6 (TCP) or 17 (UDP) for the protocol.
rnaePortRange :: Lens' ReplaceNetworkACLEntry (Maybe PortRange)
rnaePortRange = lens _rnaePortRange (\ s a -> s{_rnaePortRange = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
rnaeDryRun :: Lens' ReplaceNetworkACLEntry (Maybe Bool)
rnaeDryRun = lens _rnaeDryRun (\ s a -> s{_rnaeDryRun = a});

-- | The ID of the ACL.
rnaeNetworkACLId :: Lens' ReplaceNetworkACLEntry Text
rnaeNetworkACLId = lens _rnaeNetworkACLId (\ s a -> s{_rnaeNetworkACLId = a});

-- | The rule number of the entry to replace.
rnaeRuleNumber :: Lens' ReplaceNetworkACLEntry Int
rnaeRuleNumber = lens _rnaeRuleNumber (\ s a -> s{_rnaeRuleNumber = a});

-- | The IP protocol. You can specify 'all' or '-1' to mean all protocols.
rnaeProtocol :: Lens' ReplaceNetworkACLEntry Text
rnaeProtocol = lens _rnaeProtocol (\ s a -> s{_rnaeProtocol = a});

-- | Indicates whether to allow or deny the traffic that matches the rule.
rnaeRuleAction :: Lens' ReplaceNetworkACLEntry RuleAction
rnaeRuleAction = lens _rnaeRuleAction (\ s a -> s{_rnaeRuleAction = a});

-- | Indicates whether to replace the egress rule.
--
-- Default: If no value is specified, we replace the ingress rule.
rnaeEgress :: Lens' ReplaceNetworkACLEntry Bool
rnaeEgress = lens _rnaeEgress (\ s a -> s{_rnaeEgress = a});

-- | The network range to allow or deny, in CIDR notation.
rnaeCIdRBlock :: Lens' ReplaceNetworkACLEntry Text
rnaeCIdRBlock = lens _rnaeCIdRBlock (\ s a -> s{_rnaeCIdRBlock = a});

instance AWSRequest ReplaceNetworkACLEntry where
        type Rs ReplaceNetworkACLEntry =
             ReplaceNetworkACLEntryResponse
        request = postQuery ec2
        response
          = receiveNull ReplaceNetworkACLEntryResponse'

instance Hashable ReplaceNetworkACLEntry

instance NFData ReplaceNetworkACLEntry

instance ToHeaders ReplaceNetworkACLEntry where
        toHeaders = const mempty

instance ToPath ReplaceNetworkACLEntry where
        toPath = const "/"

instance ToQuery ReplaceNetworkACLEntry where
        toQuery ReplaceNetworkACLEntry'{..}
          = mconcat
              ["Action" =:
                 ("ReplaceNetworkAclEntry" :: ByteString),
               "Version" =: ("2015-10-01" :: ByteString),
               "Icmp" =: _rnaeICMPTypeCode,
               "PortRange" =: _rnaePortRange,
               "DryRun" =: _rnaeDryRun,
               "NetworkAclId" =: _rnaeNetworkACLId,
               "RuleNumber" =: _rnaeRuleNumber,
               "Protocol" =: _rnaeProtocol,
               "RuleAction" =: _rnaeRuleAction,
               "Egress" =: _rnaeEgress,
               "CidrBlock" =: _rnaeCIdRBlock]

-- | /See:/ 'replaceNetworkACLEntryResponse' smart constructor.
data ReplaceNetworkACLEntryResponse =
    ReplaceNetworkACLEntryResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReplaceNetworkACLEntryResponse' with the minimum fields required to make a request.
--
replaceNetworkACLEntryResponse
    :: ReplaceNetworkACLEntryResponse
replaceNetworkACLEntryResponse = ReplaceNetworkACLEntryResponse'

instance NFData ReplaceNetworkACLEntryResponse
