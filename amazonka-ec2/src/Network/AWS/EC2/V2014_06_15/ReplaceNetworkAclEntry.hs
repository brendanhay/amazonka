{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.EC2.V2014_06_15.ReplaceNetworkAclEntry
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Replaces an entry (rule) in a network ACL. For more information about
-- network ACLs, see Network ACLs in the Amazon Virtual Private Cloud User
-- Guide. Example This example replaces the egress entry numbered 110 in the
-- network ACL with ID acl-2cb85d45. The new rule denies egress traffic
-- destined for anywhere (0.0.0.0/0) on TCP port 139.
-- https://ec2.amazonaws.com/?Action=ReplaceNetworkAclEntry
-- &amp;NetworkAclId=acl-2cb85d45 &amp;RuleNumber=110 &amp;Protocol=tcp
-- &amp;RuleAction=deny &amp;Egress=true &amp;CidrBlock=0.0.0.0/0
-- &amp;PortRange.From=139 &amp;PortRange.To=139 &amp;AUTHPARAMS
-- &lt;ReplaceNetworkAclEntryResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/ReplaceNetworkAclEntryResponse&gt;.
module Network.AWS.EC2.V2014_06_15.ReplaceNetworkAclEntry where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ReplaceNetworkAclEntry' request.
replaceNetworkAclEntry :: Bool -- ^ '_rnaerEgress'
                       -> Integer -- ^ '_rnaerRuleNumber'
                       -> RuleAction -- ^ '_rnaerRuleAction'
                       -> Text -- ^ '_rnaerNetworkAclId'
                       -> Text -- ^ '_rnaerProtocol'
                       -> Text -- ^ '_rnaerCidrBlock'
                       -> ReplaceNetworkAclEntry
replaceNetworkAclEntry p1 p2 p3 p4 p5 p6 = ReplaceNetworkAclEntry
    { _rnaerEgress = p1
    , _rnaerRuleNumber = p2
    , _rnaerRuleAction = p3
    , _rnaerNetworkAclId = p4
    , _rnaerProtocol = p5
    , _rnaerCidrBlock = p6
    , _rnaerDryRun = Nothing
    , _rnaerIcmpTypeCode = Nothing
    , _rnaerPortRange = Nothing
    }

data ReplaceNetworkAclEntry = ReplaceNetworkAclEntry
    { _rnaerEgress :: Bool
      -- ^ Indicates whether to replace the egress rule. Default: If no
      -- value is specified, we replace the ingress rule.
    , _rnaerRuleNumber :: Integer
      -- ^ The rule number of the entry to replace.
    , _rnaerRuleAction :: RuleAction
      -- ^ Indicates whether to allow or deny the traffic that matches the
      -- rule.
    , _rnaerNetworkAclId :: Text
      -- ^ The ID of the ACL.
    , _rnaerProtocol :: Text
      -- ^ The IP protocol. You can specify all or -1 to mean all protocols.
    , _rnaerCidrBlock :: Text
      -- ^ The network range to allow or deny, in CIDR notation.
    , _rnaerDryRun :: Maybe Bool
      -- ^ 
    , _rnaerIcmpTypeCode :: Maybe IcmpTypeCode
      -- ^ ICMP protocol: The ICMP type and code.
    , _rnaerPortRange :: Maybe PortRange
      -- ^ TCP or UDP protocols: The range of ports the rule applies to.
    } deriving (Show, Generic)

makeLenses ''ReplaceNetworkAclEntry

instance ToQuery ReplaceNetworkAclEntry where
    toQuery = genericQuery def

data ReplaceNetworkAclEntryResponse = ReplaceNetworkAclEntryResponse
    deriving (Eq, Show, Generic)

makeLenses ''ReplaceNetworkAclEntryResponse

instance AWSRequest ReplaceNetworkAclEntry where
    type Sv ReplaceNetworkAclEntry = EC2
    type Rs ReplaceNetworkAclEntry = ReplaceNetworkAclEntryResponse

    request = post "ReplaceNetworkAclEntry"
    response _ = nullaryResponse ReplaceNetworkAclEntryResponse
