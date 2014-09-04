{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.EC2.V2014_06_15.ReplaceNetworkAclEntry
    (
    -- * Request
      ReplaceNetworkAclEntry
    -- ** Request constructor
    , mkReplaceNetworkAclEntryRequest
    -- ** Request lenses
    , rnaerNetworkAclId
    , rnaerRuleNumber
    , rnaerProtocol
    , rnaerRuleAction
    , rnaerEgress
    , rnaerCidrBlock
    , rnaerIcmpTypeCode
    , rnaerPortRange

    -- * Response
    , ReplaceNetworkAclEntryResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ReplaceNetworkAclEntry' request.
mkReplaceNetworkAclEntryRequest :: Text -- ^ 'rnaerNetworkAclId'
                                -> Integer -- ^ 'rnaerRuleNumber'
                                -> Text -- ^ 'rnaerProtocol'
                                -> RuleAction -- ^ 'rnaerRuleAction'
                                -> Bool -- ^ 'rnaerEgress'
                                -> Text -- ^ 'rnaerCidrBlock'
                                -> ReplaceNetworkAclEntry
mkReplaceNetworkAclEntryRequest p1 p2 p3 p4 p5 p6 = ReplaceNetworkAclEntry
    { _rnaerNetworkAclId = p1
    , _rnaerRuleNumber = p2
    , _rnaerProtocol = p3
    , _rnaerRuleAction = p4
    , _rnaerEgress = p5
    , _rnaerCidrBlock = p6
    , _rnaerIcmpTypeCode = Nothing
    , _rnaerPortRange = Nothing
    }
{-# INLINE mkReplaceNetworkAclEntryRequest #-}

data ReplaceNetworkAclEntry = ReplaceNetworkAclEntry
    { _rnaerNetworkAclId :: Text
      -- ^ The ID of the ACL.
    , _rnaerRuleNumber :: Integer
      -- ^ The rule number of the entry to replace.
    , _rnaerProtocol :: Text
      -- ^ The IP protocol. You can specify all or -1 to mean all protocols.
    , _rnaerRuleAction :: RuleAction
      -- ^ Indicates whether to allow or deny the traffic that matches the
      -- rule.
    , _rnaerEgress :: Bool
      -- ^ Indicates whether to replace the egress rule. Default: If no
      -- value is specified, we replace the ingress rule.
    , _rnaerCidrBlock :: Text
      -- ^ The network range to allow or deny, in CIDR notation.
    , _rnaerIcmpTypeCode :: Maybe IcmpTypeCode
      -- ^ ICMP protocol: The ICMP type and code.
    , _rnaerPortRange :: Maybe PortRange
      -- ^ TCP or UDP protocols: The range of ports the rule applies to.
    } deriving (Show, Generic)

-- | The ID of the ACL.
rnaerNetworkAclId :: Lens' ReplaceNetworkAclEntry (Text)
rnaerNetworkAclId = lens _rnaerNetworkAclId (\s a -> s { _rnaerNetworkAclId = a })
{-# INLINE rnaerNetworkAclId #-}

-- | The rule number of the entry to replace.
rnaerRuleNumber :: Lens' ReplaceNetworkAclEntry (Integer)
rnaerRuleNumber = lens _rnaerRuleNumber (\s a -> s { _rnaerRuleNumber = a })
{-# INLINE rnaerRuleNumber #-}

-- | The IP protocol. You can specify all or -1 to mean all protocols.
rnaerProtocol :: Lens' ReplaceNetworkAclEntry (Text)
rnaerProtocol = lens _rnaerProtocol (\s a -> s { _rnaerProtocol = a })
{-# INLINE rnaerProtocol #-}

-- | Indicates whether to allow or deny the traffic that matches the rule.
rnaerRuleAction :: Lens' ReplaceNetworkAclEntry (RuleAction)
rnaerRuleAction = lens _rnaerRuleAction (\s a -> s { _rnaerRuleAction = a })
{-# INLINE rnaerRuleAction #-}

-- | Indicates whether to replace the egress rule. Default: If no value is
-- specified, we replace the ingress rule.
rnaerEgress :: Lens' ReplaceNetworkAclEntry (Bool)
rnaerEgress = lens _rnaerEgress (\s a -> s { _rnaerEgress = a })
{-# INLINE rnaerEgress #-}

-- | The network range to allow or deny, in CIDR notation.
rnaerCidrBlock :: Lens' ReplaceNetworkAclEntry (Text)
rnaerCidrBlock = lens _rnaerCidrBlock (\s a -> s { _rnaerCidrBlock = a })
{-# INLINE rnaerCidrBlock #-}

-- | ICMP protocol: The ICMP type and code.
rnaerIcmpTypeCode :: Lens' ReplaceNetworkAclEntry (Maybe IcmpTypeCode)
rnaerIcmpTypeCode = lens _rnaerIcmpTypeCode (\s a -> s { _rnaerIcmpTypeCode = a })
{-# INLINE rnaerIcmpTypeCode #-}

-- | TCP or UDP protocols: The range of ports the rule applies to.
rnaerPortRange :: Lens' ReplaceNetworkAclEntry (Maybe PortRange)
rnaerPortRange = lens _rnaerPortRange (\s a -> s { _rnaerPortRange = a })
{-# INLINE rnaerPortRange #-}

instance ToQuery ReplaceNetworkAclEntry where
    toQuery = genericQuery def

data ReplaceNetworkAclEntryResponse = ReplaceNetworkAclEntryResponse
    deriving (Eq, Show, Generic)

instance AWSRequest ReplaceNetworkAclEntry where
    type Sv ReplaceNetworkAclEntry = EC2
    type Rs ReplaceNetworkAclEntry = ReplaceNetworkAclEntryResponse

    request = post "ReplaceNetworkAclEntry"
    response _ = nullaryResponse ReplaceNetworkAclEntryResponse
