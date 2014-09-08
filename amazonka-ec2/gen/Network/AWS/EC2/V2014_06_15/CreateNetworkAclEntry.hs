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
    , mkCreateNetworkAclEntry
    -- ** Request lenses
    , cnaeNetworkAclId
    , cnaeRuleNumber
    , cnaeProtocol
    , cnaeRuleAction
    , cnaeEgress
    , cnaeCidrBlock
    , cnaeIcmpTypeCode
    , cnaePortRange

    -- * Response
    , CreateNetworkAclEntryResponse
    -- ** Response constructor
    , mkCreateNetworkAclEntryResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data CreateNetworkAclEntry = CreateNetworkAclEntry
    { _cnaeNetworkAclId :: Text
    , _cnaeRuleNumber :: Integer
    , _cnaeProtocol :: Text
    , _cnaeRuleAction :: RuleAction
    , _cnaeEgress :: Bool
    , _cnaeCidrBlock :: Text
    , _cnaeIcmpTypeCode :: Maybe IcmpTypeCode
    , _cnaePortRange :: Maybe PortRange
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateNetworkAclEntry' request.
mkCreateNetworkAclEntry :: Text -- ^ 'cnaeNetworkAclId'
                        -> Integer -- ^ 'cnaeRuleNumber'
                        -> Text -- ^ 'cnaeProtocol'
                        -> RuleAction -- ^ 'cnaeRuleAction'
                        -> Bool -- ^ 'cnaeEgress'
                        -> Text -- ^ 'cnaeCidrBlock'
                        -> CreateNetworkAclEntry
mkCreateNetworkAclEntry p1 p2 p3 p4 p5 p6 = CreateNetworkAclEntry
    { _cnaeNetworkAclId = p1
    , _cnaeRuleNumber = p2
    , _cnaeProtocol = p3
    , _cnaeRuleAction = p4
    , _cnaeEgress = p5
    , _cnaeCidrBlock = p6
    , _cnaeIcmpTypeCode = Nothing
    , _cnaePortRange = Nothing
    }

-- | The ID of the ACL.
cnaeNetworkAclId :: Lens' CreateNetworkAclEntry Text
cnaeNetworkAclId =
    lens _cnaeNetworkAclId (\s a -> s { _cnaeNetworkAclId = a })

-- | The rule number for the entry (for example, 100). ACL entries are processed
-- in ascending order by rule number. Constraints: Positive integer from 1 to
-- 32766.
cnaeRuleNumber :: Lens' CreateNetworkAclEntry Integer
cnaeRuleNumber = lens _cnaeRuleNumber (\s a -> s { _cnaeRuleNumber = a })

-- | The protocol. A value of -1 means all protocols.
cnaeProtocol :: Lens' CreateNetworkAclEntry Text
cnaeProtocol = lens _cnaeProtocol (\s a -> s { _cnaeProtocol = a })

-- | Indicates whether to allow or deny the traffic that matches the rule.
cnaeRuleAction :: Lens' CreateNetworkAclEntry RuleAction
cnaeRuleAction = lens _cnaeRuleAction (\s a -> s { _cnaeRuleAction = a })

-- | Indicates whether this is an egress rule (rule is applied to traffic
-- leaving the subnet).
cnaeEgress :: Lens' CreateNetworkAclEntry Bool
cnaeEgress = lens _cnaeEgress (\s a -> s { _cnaeEgress = a })

-- | The network range to allow or deny, in CIDR notation.
cnaeCidrBlock :: Lens' CreateNetworkAclEntry Text
cnaeCidrBlock = lens _cnaeCidrBlock (\s a -> s { _cnaeCidrBlock = a })

-- | ICMP protocol: The ICMP type and code.
cnaeIcmpTypeCode :: Lens' CreateNetworkAclEntry (Maybe IcmpTypeCode)
cnaeIcmpTypeCode =
    lens _cnaeIcmpTypeCode (\s a -> s { _cnaeIcmpTypeCode = a })

-- | TCP or UDP protocols: The range of ports the rule applies to.
cnaePortRange :: Lens' CreateNetworkAclEntry (Maybe PortRange)
cnaePortRange = lens _cnaePortRange (\s a -> s { _cnaePortRange = a })

instance ToQuery CreateNetworkAclEntry where
    toQuery = genericQuery def

data CreateNetworkAclEntryResponse = CreateNetworkAclEntryResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateNetworkAclEntryResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkCreateNetworkAclEntryResponse :: CreateNetworkAclEntryResponse
mkCreateNetworkAclEntryResponse = CreateNetworkAclEntryResponse

instance AWSRequest CreateNetworkAclEntry where
    type Sv CreateNetworkAclEntry = EC2
    type Rs CreateNetworkAclEntry = CreateNetworkAclEntryResponse

    request = post "CreateNetworkAclEntry"
    response _ = nullaryResponse CreateNetworkAclEntryResponse
