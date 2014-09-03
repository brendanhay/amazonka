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
    , createNetworkAclEntry
    -- ** Request lenses
    , cnaerEgress
    , cnaerRuleNumber
    , cnaerRuleAction
    , cnaerNetworkAclId
    , cnaerProtocol
    , cnaerCidrBlock
    , cnaerIcmpTypeCode
    , cnaerPortRange

    -- * Response
    , CreateNetworkAclEntryResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateNetworkAclEntry' request.
createNetworkAclEntry :: Bool -- ^ 'cnaerEgress'
                      -> Integer -- ^ 'cnaerRuleNumber'
                      -> RuleAction -- ^ 'cnaerRuleAction'
                      -> Text -- ^ 'cnaerNetworkAclId'
                      -> Text -- ^ 'cnaerProtocol'
                      -> Text -- ^ 'cnaerCidrBlock'
                      -> CreateNetworkAclEntry
createNetworkAclEntry p1 p2 p3 p4 p5 p6 = CreateNetworkAclEntry
    { _cnaerEgress = p1
    , _cnaerRuleNumber = p2
    , _cnaerRuleAction = p3
    , _cnaerNetworkAclId = p4
    , _cnaerProtocol = p5
    , _cnaerCidrBlock = p6
    , _cnaerIcmpTypeCode = Nothing
    , _cnaerPortRange = Nothing
    }

data CreateNetworkAclEntry = CreateNetworkAclEntry
    { _cnaerEgress :: Bool
      -- ^ Indicates whether this is an egress rule (rule is applied to
      -- traffic leaving the subnet).
    , _cnaerRuleNumber :: Integer
      -- ^ The rule number for the entry (for example, 100). ACL entries are
      -- processed in ascending order by rule number. Constraints:
      -- Positive integer from 1 to 32766.
    , _cnaerRuleAction :: RuleAction
      -- ^ Indicates whether to allow or deny the traffic that matches the
      -- rule.
    , _cnaerNetworkAclId :: Text
      -- ^ The ID of the ACL.
    , _cnaerProtocol :: Text
      -- ^ The protocol. A value of -1 means all protocols.
    , _cnaerCidrBlock :: Text
      -- ^ The network range to allow or deny, in CIDR notation.
    , _cnaerIcmpTypeCode :: Maybe IcmpTypeCode
      -- ^ ICMP protocol: The ICMP type and code.
    , _cnaerPortRange :: Maybe PortRange
      -- ^ TCP or UDP protocols: The range of ports the rule applies to.
    } deriving (Show, Generic)

-- | Indicates whether this is an egress rule (rule is applied to traffic
-- leaving the subnet).
cnaerEgress
    :: Functor f
    => (Bool
    -> f (Bool))
    -> CreateNetworkAclEntry
    -> f CreateNetworkAclEntry
cnaerEgress f x =
    (\y -> x { _cnaerEgress = y })
       <$> f (_cnaerEgress x)
{-# INLINE cnaerEgress #-}

-- | The rule number for the entry (for example, 100). ACL entries are processed
-- in ascending order by rule number. Constraints: Positive integer from 1 to
-- 32766.
cnaerRuleNumber
    :: Functor f
    => (Integer
    -> f (Integer))
    -> CreateNetworkAclEntry
    -> f CreateNetworkAclEntry
cnaerRuleNumber f x =
    (\y -> x { _cnaerRuleNumber = y })
       <$> f (_cnaerRuleNumber x)
{-# INLINE cnaerRuleNumber #-}

-- | Indicates whether to allow or deny the traffic that matches the rule.
cnaerRuleAction
    :: Functor f
    => (RuleAction
    -> f (RuleAction))
    -> CreateNetworkAclEntry
    -> f CreateNetworkAclEntry
cnaerRuleAction f x =
    (\y -> x { _cnaerRuleAction = y })
       <$> f (_cnaerRuleAction x)
{-# INLINE cnaerRuleAction #-}

-- | The ID of the ACL.
cnaerNetworkAclId
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateNetworkAclEntry
    -> f CreateNetworkAclEntry
cnaerNetworkAclId f x =
    (\y -> x { _cnaerNetworkAclId = y })
       <$> f (_cnaerNetworkAclId x)
{-# INLINE cnaerNetworkAclId #-}

-- | The protocol. A value of -1 means all protocols.
cnaerProtocol
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateNetworkAclEntry
    -> f CreateNetworkAclEntry
cnaerProtocol f x =
    (\y -> x { _cnaerProtocol = y })
       <$> f (_cnaerProtocol x)
{-# INLINE cnaerProtocol #-}

-- | The network range to allow or deny, in CIDR notation.
cnaerCidrBlock
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateNetworkAclEntry
    -> f CreateNetworkAclEntry
cnaerCidrBlock f x =
    (\y -> x { _cnaerCidrBlock = y })
       <$> f (_cnaerCidrBlock x)
{-# INLINE cnaerCidrBlock #-}

-- | ICMP protocol: The ICMP type and code.
cnaerIcmpTypeCode
    :: Functor f
    => (Maybe IcmpTypeCode
    -> f (Maybe IcmpTypeCode))
    -> CreateNetworkAclEntry
    -> f CreateNetworkAclEntry
cnaerIcmpTypeCode f x =
    (\y -> x { _cnaerIcmpTypeCode = y })
       <$> f (_cnaerIcmpTypeCode x)
{-# INLINE cnaerIcmpTypeCode #-}

-- | TCP or UDP protocols: The range of ports the rule applies to.
cnaerPortRange
    :: Functor f
    => (Maybe PortRange
    -> f (Maybe PortRange))
    -> CreateNetworkAclEntry
    -> f CreateNetworkAclEntry
cnaerPortRange f x =
    (\y -> x { _cnaerPortRange = y })
       <$> f (_cnaerPortRange x)
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
