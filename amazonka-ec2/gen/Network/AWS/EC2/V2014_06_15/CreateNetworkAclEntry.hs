{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.CreateNetworkAclEntry
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
-- numbered egress rules. When determining whether a packet should be allowed
-- in or out of a subnet associated with the ACL, we process the entries in
-- the ACL according to the rule numbers, in ascending order. Each network ACL
-- has a set of ingress rules and a separate set of egress rules. We recommend
-- that you leave room between the rule numbers (for example, 100, 110, 120,
-- ...), and not number them one right after the other (for example, 101, 102,
-- 103, ...). This makes it easier to add a rule between existing ones without
-- having to renumber the rules. After you add an entry, you can't modify it;
-- you must either replace it, or create an entry and delete the old one. For
-- more information about network ACLs, see Network ACLs in the Amazon Virtual
-- Private Cloud User Guide. Example This example creates an entry with rule
-- number 110 in the network ACL with the ID acl-2cb85d45. The rule allows
-- ingress traffic from anywhere (0.0.0.0/0) on UDP port 53 into any
-- associated subnet. https://ec2.amazonaws.com/?Action=CreateNetworkAclEntry
-- &amp;NetworkAclId=acl-2cb85d45 &amp;RuleNumber=110 &amp;Protocol=udp
-- &amp;RuleAction=allow &amp;Egress=false &amp;CidrBlock=0.0.0.0/0
-- &amp;PortRange.From=53 &amp;PortRange.To=53 &amp;AUTHPARAMS
-- &lt;CreateNetworkAclEntryResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/CreateNetworkAclEntryResponse&gt;.
module Network.AWS.EC2.V2014_06_15.CreateNetworkAclEntry
    (
    -- * Request
      CreateNetworkAclEntry
    -- ** Request constructor
    , mkCreateNetworkAclEntryRequest
    -- ** Request lenses
    , cnaerNetworkAclId
    , cnaerRuleNumber
    , cnaerProtocol
    , cnaerRuleAction
    , cnaerEgress
    , cnaerCidrBlock
    , cnaerIcmpTypeCode
    , cnaerPortRange

    -- * Response
    , CreateNetworkAclEntryResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateNetworkAclEntry' request.
mkCreateNetworkAclEntryRequest :: Text -- ^ 'cnaerNetworkAclId'
                               -> Integer -- ^ 'cnaerRuleNumber'
                               -> Text -- ^ 'cnaerProtocol'
                               -> RuleAction -- ^ 'cnaerRuleAction'
                               -> Bool -- ^ 'cnaerEgress'
                               -> Text -- ^ 'cnaerCidrBlock'
                               -> CreateNetworkAclEntry
mkCreateNetworkAclEntryRequest p1 p2 p3 p4 p5 p6 = CreateNetworkAclEntry
    { _cnaerNetworkAclId = p1
    , _cnaerRuleNumber = p2
    , _cnaerProtocol = p3
    , _cnaerRuleAction = p4
    , _cnaerEgress = p5
    , _cnaerCidrBlock = p6
    , _cnaerIcmpTypeCode = Nothing
    , _cnaerPortRange = Nothing
    }
{-# INLINE mkCreateNetworkAclEntryRequest #-}

data CreateNetworkAclEntry = CreateNetworkAclEntry
    { _cnaerNetworkAclId :: Text
      -- ^ The ID of the ACL.
    , _cnaerRuleNumber :: Integer
      -- ^ The rule number for the entry (for example, 100). ACL entries are
      -- processed in ascending order by rule number. Constraints:
      -- Positive integer from 1 to 32766.
    , _cnaerProtocol :: Text
      -- ^ The protocol. A value of -1 means all protocols.
    , _cnaerRuleAction :: RuleAction
      -- ^ Indicates whether to allow or deny the traffic that matches the
      -- rule.
    , _cnaerEgress :: Bool
      -- ^ Indicates whether this is an egress rule (rule is applied to
      -- traffic leaving the subnet).
    , _cnaerCidrBlock :: Text
      -- ^ The network range to allow or deny, in CIDR notation.
    , _cnaerIcmpTypeCode :: Maybe IcmpTypeCode
      -- ^ ICMP protocol: The ICMP type and code.
    , _cnaerPortRange :: Maybe PortRange
      -- ^ TCP or UDP protocols: The range of ports the rule applies to.
    } deriving (Show, Generic)

-- | The ID of the ACL.
cnaerNetworkAclId :: Lens' CreateNetworkAclEntry (Text)
cnaerNetworkAclId = lens _cnaerNetworkAclId (\s a -> s { _cnaerNetworkAclId = a })
{-# INLINE cnaerNetworkAclId #-}

-- | The rule number for the entry (for example, 100). ACL entries are processed
-- in ascending order by rule number. Constraints: Positive integer from 1 to
-- 32766.
cnaerRuleNumber :: Lens' CreateNetworkAclEntry (Integer)
cnaerRuleNumber = lens _cnaerRuleNumber (\s a -> s { _cnaerRuleNumber = a })
{-# INLINE cnaerRuleNumber #-}

-- | The protocol. A value of -1 means all protocols.
cnaerProtocol :: Lens' CreateNetworkAclEntry (Text)
cnaerProtocol = lens _cnaerProtocol (\s a -> s { _cnaerProtocol = a })
{-# INLINE cnaerProtocol #-}

-- | Indicates whether to allow or deny the traffic that matches the rule.
cnaerRuleAction :: Lens' CreateNetworkAclEntry (RuleAction)
cnaerRuleAction = lens _cnaerRuleAction (\s a -> s { _cnaerRuleAction = a })
{-# INLINE cnaerRuleAction #-}

-- | Indicates whether this is an egress rule (rule is applied to traffic
-- leaving the subnet).
cnaerEgress :: Lens' CreateNetworkAclEntry (Bool)
cnaerEgress = lens _cnaerEgress (\s a -> s { _cnaerEgress = a })
{-# INLINE cnaerEgress #-}

-- | The network range to allow or deny, in CIDR notation.
cnaerCidrBlock :: Lens' CreateNetworkAclEntry (Text)
cnaerCidrBlock = lens _cnaerCidrBlock (\s a -> s { _cnaerCidrBlock = a })
{-# INLINE cnaerCidrBlock #-}

-- | ICMP protocol: The ICMP type and code.
cnaerIcmpTypeCode :: Lens' CreateNetworkAclEntry (Maybe IcmpTypeCode)
cnaerIcmpTypeCode = lens _cnaerIcmpTypeCode (\s a -> s { _cnaerIcmpTypeCode = a })
{-# INLINE cnaerIcmpTypeCode #-}

-- | TCP or UDP protocols: The range of ports the rule applies to.
cnaerPortRange :: Lens' CreateNetworkAclEntry (Maybe PortRange)
cnaerPortRange = lens _cnaerPortRange (\s a -> s { _cnaerPortRange = a })
{-# INLINE cnaerPortRange #-}

instance ToQuery CreateNetworkAclEntry where
    toQuery = genericQuery def

data CreateNetworkAclEntryResponse = CreateNetworkAclEntryResponse
    deriving (Eq, Show, Generic)

instance AWSRequest CreateNetworkAclEntry where
    type Sv CreateNetworkAclEntry = EC2
    type Rs CreateNetworkAclEntry = CreateNetworkAclEntryResponse

    request = post "CreateNetworkAclEntry"
    response _ = nullaryResponse CreateNetworkAclEntryResponse
