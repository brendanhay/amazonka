{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2
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
module Network.AWS.EC2
    (
    -- * Request
      ReplaceNetworkAclEntry
    -- ** Request constructor
    , mkReplaceNetworkAclEntry
    -- ** Request lenses
    , rnaeNetworkAclId
    , rnaeRuleNumber
    , rnaeProtocol
    , rnaeRuleAction
    , rnaeEgress
    , rnaeCidrBlock
    , rnaeIcmpTypeCode
    , rnaePortRange

    -- * Response
    , ReplaceNetworkAclEntryResponse
    -- ** Response constructor
    , mkReplaceNetworkAclEntryResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data ReplaceNetworkAclEntry = ReplaceNetworkAclEntry
    { _rnaeNetworkAclId :: !Text
    , _rnaeRuleNumber :: !Integer
    , _rnaeProtocol :: !Text
    , _rnaeRuleAction :: RuleAction
    , _rnaeEgress :: !Bool
    , _rnaeCidrBlock :: !Text
    , _rnaeIcmpTypeCode :: Maybe IcmpTypeCode
    , _rnaePortRange :: Maybe PortRange
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ReplaceNetworkAclEntry' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @NetworkAclId ::@ @Text@
--
-- * @RuleNumber ::@ @Integer@
--
-- * @Protocol ::@ @Text@
--
-- * @RuleAction ::@ @RuleAction@
--
-- * @Egress ::@ @Bool@
--
-- * @CidrBlock ::@ @Text@
--
-- * @IcmpTypeCode ::@ @Maybe IcmpTypeCode@
--
-- * @PortRange ::@ @Maybe PortRange@
--
mkReplaceNetworkAclEntry :: Text -- ^ 'rnaeNetworkAclId'
                         -> Integer -- ^ 'rnaeRuleNumber'
                         -> Text -- ^ 'rnaeProtocol'
                         -> RuleAction -- ^ 'rnaeRuleAction'
                         -> Bool -- ^ 'rnaeEgress'
                         -> Text -- ^ 'rnaeCidrBlock'
                         -> ReplaceNetworkAclEntry
mkReplaceNetworkAclEntry p1 p2 p3 p4 p5 p6 = ReplaceNetworkAclEntry
    { _rnaeNetworkAclId = p1
    , _rnaeRuleNumber = p2
    , _rnaeProtocol = p3
    , _rnaeRuleAction = p4
    , _rnaeEgress = p5
    , _rnaeCidrBlock = p6
    , _rnaeIcmpTypeCode = Nothing
    , _rnaePortRange = Nothing
    }

-- | The ID of the ACL.
rnaeNetworkAclId :: Lens' ReplaceNetworkAclEntry Text
rnaeNetworkAclId =
    lens _rnaeNetworkAclId (\s a -> s { _rnaeNetworkAclId = a })

-- | The rule number of the entry to replace.
rnaeRuleNumber :: Lens' ReplaceNetworkAclEntry Integer
rnaeRuleNumber = lens _rnaeRuleNumber (\s a -> s { _rnaeRuleNumber = a })

-- | The IP protocol. You can specify all or -1 to mean all protocols.
rnaeProtocol :: Lens' ReplaceNetworkAclEntry Text
rnaeProtocol = lens _rnaeProtocol (\s a -> s { _rnaeProtocol = a })

-- | Indicates whether to allow or deny the traffic that matches the rule.
rnaeRuleAction :: Lens' ReplaceNetworkAclEntry RuleAction
rnaeRuleAction = lens _rnaeRuleAction (\s a -> s { _rnaeRuleAction = a })

-- | Indicates whether to replace the egress rule. Default: If no value is
-- specified, we replace the ingress rule.
rnaeEgress :: Lens' ReplaceNetworkAclEntry Bool
rnaeEgress = lens _rnaeEgress (\s a -> s { _rnaeEgress = a })

-- | The network range to allow or deny, in CIDR notation.
rnaeCidrBlock :: Lens' ReplaceNetworkAclEntry Text
rnaeCidrBlock = lens _rnaeCidrBlock (\s a -> s { _rnaeCidrBlock = a })

-- | ICMP protocol: The ICMP type and code.
rnaeIcmpTypeCode :: Lens' ReplaceNetworkAclEntry (Maybe IcmpTypeCode)
rnaeIcmpTypeCode =
    lens _rnaeIcmpTypeCode (\s a -> s { _rnaeIcmpTypeCode = a })

-- | TCP or UDP protocols: The range of ports the rule applies to.
rnaePortRange :: Lens' ReplaceNetworkAclEntry (Maybe PortRange)
rnaePortRange = lens _rnaePortRange (\s a -> s { _rnaePortRange = a })

instance ToQuery ReplaceNetworkAclEntry where
    toQuery = genericQuery def

data ReplaceNetworkAclEntryResponse = ReplaceNetworkAclEntryResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ReplaceNetworkAclEntryResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkReplaceNetworkAclEntryResponse :: ReplaceNetworkAclEntryResponse
mkReplaceNetworkAclEntryResponse = ReplaceNetworkAclEntryResponse

instance AWSRequest ReplaceNetworkAclEntry where
    type Sv ReplaceNetworkAclEntry = EC2
    type Rs ReplaceNetworkAclEntry = ReplaceNetworkAclEntryResponse

    request = post "ReplaceNetworkAclEntry"
    response _ = nullaryResponse ReplaceNetworkAclEntryResponse
