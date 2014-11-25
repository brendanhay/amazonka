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

-- Module      : Network.AWS.EC2.ReplaceNetworkAclEntry
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Replaces an entry (rule) in a network ACL. For more information about network
-- ACLs, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_ACLs.html Network ACLs> in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ReplaceNetworkAclEntry.html>
module Network.AWS.EC2.ReplaceNetworkAclEntry
    (
    -- * Request
      ReplaceNetworkAclEntry
    -- ** Request constructor
    , replaceNetworkAclEntry
    -- ** Request lenses
    , rnaeCidrBlock
    , rnaeDryRun
    , rnaeEgress
    , rnaeIcmpTypeCode
    , rnaeNetworkAclId
    , rnaePortRange
    , rnaeProtocol
    , rnaeRuleAction
    , rnaeRuleNumber

    -- * Response
    , ReplaceNetworkAclEntryResponse
    -- ** Response constructor
    , replaceNetworkAclEntryResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data ReplaceNetworkAclEntry = ReplaceNetworkAclEntry
    { _rnaeCidrBlock    :: Text
    , _rnaeDryRun       :: Maybe Bool
    , _rnaeEgress       :: Bool
    , _rnaeIcmpTypeCode :: Maybe IcmpTypeCode
    , _rnaeNetworkAclId :: Text
    , _rnaePortRange    :: Maybe PortRange
    , _rnaeProtocol     :: Text
    , _rnaeRuleAction   :: RuleAction
    , _rnaeRuleNumber   :: Int
    } deriving (Eq, Show)

-- | 'ReplaceNetworkAclEntry' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rnaeCidrBlock' @::@ 'Text'
--
-- * 'rnaeDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'rnaeEgress' @::@ 'Bool'
--
-- * 'rnaeIcmpTypeCode' @::@ 'Maybe' 'IcmpTypeCode'
--
-- * 'rnaeNetworkAclId' @::@ 'Text'
--
-- * 'rnaePortRange' @::@ 'Maybe' 'PortRange'
--
-- * 'rnaeProtocol' @::@ 'Text'
--
-- * 'rnaeRuleAction' @::@ 'RuleAction'
--
-- * 'rnaeRuleNumber' @::@ 'Int'
--
replaceNetworkAclEntry :: Text -- ^ 'rnaeNetworkAclId'
                       -> Int -- ^ 'rnaeRuleNumber'
                       -> Text -- ^ 'rnaeProtocol'
                       -> RuleAction -- ^ 'rnaeRuleAction'
                       -> Bool -- ^ 'rnaeEgress'
                       -> Text -- ^ 'rnaeCidrBlock'
                       -> ReplaceNetworkAclEntry
replaceNetworkAclEntry p1 p2 p3 p4 p5 p6 = ReplaceNetworkAclEntry
    { _rnaeNetworkAclId = p1
    , _rnaeRuleNumber   = p2
    , _rnaeProtocol     = p3
    , _rnaeRuleAction   = p4
    , _rnaeEgress       = p5
    , _rnaeCidrBlock    = p6
    , _rnaeDryRun       = Nothing
    , _rnaeIcmpTypeCode = Nothing
    , _rnaePortRange    = Nothing
    }

-- | The network range to allow or deny, in CIDR notation.
--
rnaeCidrBlock :: Lens' ReplaceNetworkAclEntry Text
rnaeCidrBlock = lens _rnaeCidrBlock (\s a -> s { _rnaeCidrBlock = a })

rnaeDryRun :: Lens' ReplaceNetworkAclEntry (Maybe Bool)
rnaeDryRun = lens _rnaeDryRun (\s a -> s { _rnaeDryRun = a })

-- | Indicates whether to replace the egress rule.
--
-- Default: If no value is specified, we replace the ingress rule.
--
rnaeEgress :: Lens' ReplaceNetworkAclEntry Bool
rnaeEgress = lens _rnaeEgress (\s a -> s { _rnaeEgress = a })

-- | ICMP protocol: The ICMP type and code. Required if specifying 1 (ICMP) for
-- the protocol.
--
rnaeIcmpTypeCode :: Lens' ReplaceNetworkAclEntry (Maybe IcmpTypeCode)
rnaeIcmpTypeCode = lens _rnaeIcmpTypeCode (\s a -> s { _rnaeIcmpTypeCode = a })

-- | The ID of the ACL.
--
rnaeNetworkAclId :: Lens' ReplaceNetworkAclEntry Text
rnaeNetworkAclId = lens _rnaeNetworkAclId (\s a -> s { _rnaeNetworkAclId = a })

-- | TCP or UDP protocols: The range of ports the rule applies to. Required if
-- specifying 6 (TCP) or 17 (UDP) for the protocol.
--
rnaePortRange :: Lens' ReplaceNetworkAclEntry (Maybe PortRange)
rnaePortRange = lens _rnaePortRange (\s a -> s { _rnaePortRange = a })

-- | The IP protocol. You can specify 'all' or '-1' to mean all protocols.
--
rnaeProtocol :: Lens' ReplaceNetworkAclEntry Text
rnaeProtocol = lens _rnaeProtocol (\s a -> s { _rnaeProtocol = a })

-- | Indicates whether to allow or deny the traffic that matches the rule.
--
rnaeRuleAction :: Lens' ReplaceNetworkAclEntry RuleAction
rnaeRuleAction = lens _rnaeRuleAction (\s a -> s { _rnaeRuleAction = a })

-- | The rule number of the entry to replace.
--
rnaeRuleNumber :: Lens' ReplaceNetworkAclEntry Int
rnaeRuleNumber = lens _rnaeRuleNumber (\s a -> s { _rnaeRuleNumber = a })

data ReplaceNetworkAclEntryResponse = ReplaceNetworkAclEntryResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'ReplaceNetworkAclEntryResponse' constructor.
replaceNetworkAclEntryResponse :: ReplaceNetworkAclEntryResponse
replaceNetworkAclEntryResponse = ReplaceNetworkAclEntryResponse

instance ToPath ReplaceNetworkAclEntry where
    toPath = const "/"

instance ToQuery ReplaceNetworkAclEntry where
    toQuery ReplaceNetworkAclEntry{..} = mconcat
        [ "cidrBlock"    =? _rnaeCidrBlock
        , "dryRun"       =? _rnaeDryRun
        , "egress"       =? _rnaeEgress
        , "Icmp"         =? _rnaeIcmpTypeCode
        , "networkAclId" =? _rnaeNetworkAclId
        , "portRange"    =? _rnaePortRange
        , "protocol"     =? _rnaeProtocol
        , "ruleAction"   =? _rnaeRuleAction
        , "ruleNumber"   =? _rnaeRuleNumber
        ]

instance ToHeaders ReplaceNetworkAclEntry

instance AWSRequest ReplaceNetworkAclEntry where
    type Sv ReplaceNetworkAclEntry = EC2
    type Rs ReplaceNetworkAclEntry = ReplaceNetworkAclEntryResponse

    request  = post "ReplaceNetworkAclEntry"
    response = nullResponse ReplaceNetworkAclEntryResponse
