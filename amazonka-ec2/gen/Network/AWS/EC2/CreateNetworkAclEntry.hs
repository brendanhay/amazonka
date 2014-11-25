{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateNetworkAclEntry
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an entry (a rule) in a network ACL with the specified rule number.
-- Each network ACL has a set of numbered ingress rules and a separate set of
-- numbered egress rules. When determining whether a packet should be allowed in
-- or out of a subnet associated with the ACL, we process the entries in the ACL
-- according to the rule numbers, in ascending order. Each network ACL has a set
-- of ingress rules and a separate set of egress rules.
--
-- We recommend that you leave room between the rule numbers (for example, 100,
-- 110, 120, ...), and not number them one right after the other (for example,
-- 101, 102, 103, ...). This makes it easier to add a rule between existing ones
-- without having to renumber the rules.
--
-- After you add an entry, you can't modify it; you must either replace it, or
-- create an entry and delete the old one.
--
-- For more information about network ACLs, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_ACLs.html Network ACLs> in the /AmazonVirtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateNetworkAclEntry.html>
module Network.AWS.EC2.CreateNetworkAclEntry
    (
    -- * Request
      CreateNetworkAclEntry
    -- ** Request constructor
    , createNetworkAclEntry
    -- ** Request lenses
    , cnaeCidrBlock
    , cnaeDryRun
    , cnaeEgress
    , cnaeIcmpTypeCode
    , cnaeNetworkAclId
    , cnaePortRange
    , cnaeProtocol
    , cnaeRuleAction
    , cnaeRuleNumber

    -- * Response
    , CreateNetworkAclEntryResponse
    -- ** Response constructor
    , createNetworkAclEntryResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data CreateNetworkAclEntry = CreateNetworkAclEntry
    { _cnaeCidrBlock    :: Text
    , _cnaeDryRun       :: Maybe Bool
    , _cnaeEgress       :: Bool
    , _cnaeIcmpTypeCode :: Maybe IcmpTypeCode
    , _cnaeNetworkAclId :: Text
    , _cnaePortRange    :: Maybe PortRange
    , _cnaeProtocol     :: Text
    , _cnaeRuleAction   :: RuleAction
    , _cnaeRuleNumber   :: Int
    } deriving (Eq, Show)

-- | 'CreateNetworkAclEntry' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cnaeCidrBlock' @::@ 'Text'
--
-- * 'cnaeDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'cnaeEgress' @::@ 'Bool'
--
-- * 'cnaeIcmpTypeCode' @::@ 'Maybe' 'IcmpTypeCode'
--
-- * 'cnaeNetworkAclId' @::@ 'Text'
--
-- * 'cnaePortRange' @::@ 'Maybe' 'PortRange'
--
-- * 'cnaeProtocol' @::@ 'Text'
--
-- * 'cnaeRuleAction' @::@ 'RuleAction'
--
-- * 'cnaeRuleNumber' @::@ 'Int'
--
createNetworkAclEntry :: Text -- ^ 'cnaeNetworkAclId'
                      -> Int -- ^ 'cnaeRuleNumber'
                      -> Text -- ^ 'cnaeProtocol'
                      -> RuleAction -- ^ 'cnaeRuleAction'
                      -> Bool -- ^ 'cnaeEgress'
                      -> Text -- ^ 'cnaeCidrBlock'
                      -> CreateNetworkAclEntry
createNetworkAclEntry p1 p2 p3 p4 p5 p6 = CreateNetworkAclEntry
    { _cnaeNetworkAclId = p1
    , _cnaeRuleNumber   = p2
    , _cnaeProtocol     = p3
    , _cnaeRuleAction   = p4
    , _cnaeEgress       = p5
    , _cnaeCidrBlock    = p6
    , _cnaeDryRun       = Nothing
    , _cnaeIcmpTypeCode = Nothing
    , _cnaePortRange    = Nothing
    }

-- | The network range to allow or deny, in CIDR notation (for example '172.16.0.0/24').
cnaeCidrBlock :: Lens' CreateNetworkAclEntry Text
cnaeCidrBlock = lens _cnaeCidrBlock (\s a -> s { _cnaeCidrBlock = a })

cnaeDryRun :: Lens' CreateNetworkAclEntry (Maybe Bool)
cnaeDryRun = lens _cnaeDryRun (\s a -> s { _cnaeDryRun = a })

-- | Indicates whether this is an egress rule (rule is applied to traffic leaving
-- the subnet).
cnaeEgress :: Lens' CreateNetworkAclEntry Bool
cnaeEgress = lens _cnaeEgress (\s a -> s { _cnaeEgress = a })

-- | ICMP protocol: The ICMP type and code. Required if specifying ICMP for the
-- protocol.
cnaeIcmpTypeCode :: Lens' CreateNetworkAclEntry (Maybe IcmpTypeCode)
cnaeIcmpTypeCode = lens _cnaeIcmpTypeCode (\s a -> s { _cnaeIcmpTypeCode = a })

-- | The ID of the network ACL.
cnaeNetworkAclId :: Lens' CreateNetworkAclEntry Text
cnaeNetworkAclId = lens _cnaeNetworkAclId (\s a -> s { _cnaeNetworkAclId = a })

-- | TCP or UDP protocols: The range of ports the rule applies to.
cnaePortRange :: Lens' CreateNetworkAclEntry (Maybe PortRange)
cnaePortRange = lens _cnaePortRange (\s a -> s { _cnaePortRange = a })

-- | The protocol. A value of -1 means all protocols.
cnaeProtocol :: Lens' CreateNetworkAclEntry Text
cnaeProtocol = lens _cnaeProtocol (\s a -> s { _cnaeProtocol = a })

-- | Indicates whether to allow or deny the traffic that matches the rule.
cnaeRuleAction :: Lens' CreateNetworkAclEntry RuleAction
cnaeRuleAction = lens _cnaeRuleAction (\s a -> s { _cnaeRuleAction = a })

-- | The rule number for the entry (for example, 100). ACL entries are processed
-- in ascending order by rule number.
--
-- Constraints: Positive integer from 1 to 32766
cnaeRuleNumber :: Lens' CreateNetworkAclEntry Int
cnaeRuleNumber = lens _cnaeRuleNumber (\s a -> s { _cnaeRuleNumber = a })

data CreateNetworkAclEntryResponse = CreateNetworkAclEntryResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'CreateNetworkAclEntryResponse' constructor.
createNetworkAclEntryResponse :: CreateNetworkAclEntryResponse
createNetworkAclEntryResponse = CreateNetworkAclEntryResponse

instance ToPath CreateNetworkAclEntry where
    toPath = const "/"

instance ToQuery CreateNetworkAclEntry where
    toQuery CreateNetworkAclEntry{..} = mconcat
        [ "cidrBlock"    =? _cnaeCidrBlock
        , "dryRun"       =? _cnaeDryRun
        , "egress"       =? _cnaeEgress
        , "Icmp"         =? _cnaeIcmpTypeCode
        , "networkAclId" =? _cnaeNetworkAclId
        , "portRange"    =? _cnaePortRange
        , "protocol"     =? _cnaeProtocol
        , "ruleAction"   =? _cnaeRuleAction
        , "ruleNumber"   =? _cnaeRuleNumber
        ]

instance ToHeaders CreateNetworkAclEntry

instance AWSRequest CreateNetworkAclEntry where
    type Sv CreateNetworkAclEntry = EC2
    type Rs CreateNetworkAclEntry = CreateNetworkAclEntryResponse

    request  = post "CreateNetworkAclEntry"
    response = nullResponse CreateNetworkAclEntryResponse
