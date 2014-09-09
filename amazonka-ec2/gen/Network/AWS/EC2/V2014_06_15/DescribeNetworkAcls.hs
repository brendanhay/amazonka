{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeNetworkAcls
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of your network ACLs. For more information about
-- network ACLs, see Network ACLs in the Amazon Virtual Private Cloud User
-- Guide. Example This example describes all your network ACLs.
-- https://ec2.amazonaws.com/?Action=DescribeNetworkAcls &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE acl-5566953c vpc-5266953b true 100 all
-- allow true 0.0.0.0/0 32767 all deny true 0.0.0.0/0 100 all allow false
-- 0.0.0.0/0 32767 all deny false 0.0.0.0/0 acl-5d659634 vpc-5266953b false
-- 110 6 allow true 0.0.0.0/0 49152 65535 32767 all deny true 0.0.0.0/0 110 6
-- allow false 0.0.0.0/0 80 80 120 6 allow false 0.0.0.0/0 443 443 32767 all
-- deny false 0.0.0.0/0 aclassoc-5c659635 acl-5d659634 subnet-ff669596
-- aclassoc-c26596ab acl-5d659634 subnet-f0669599.
module Network.AWS.EC2.V2014_06_15.DescribeNetworkAcls
    (
    -- * Request
      DescribeNetworkAcls
    -- ** Request constructor
    , mkDescribeNetworkAcls
    -- ** Request lenses
    , dna1NetworkAclIds
    , dna1Filters

    -- * Response
    , DescribeNetworkAclsResponse
    -- ** Response constructor
    , mkDescribeNetworkAclsResponse
    -- ** Response lenses
    , dnarNetworkAcls
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

data DescribeNetworkAcls = DescribeNetworkAcls
    { _dna1NetworkAclIds :: [Text]
    , _dna1Filters :: [Filter]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeNetworkAcls' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @NetworkAclIds ::@ @[Text]@
--
-- * @Filters ::@ @[Filter]@
--
mkDescribeNetworkAcls :: DescribeNetworkAcls
mkDescribeNetworkAcls = DescribeNetworkAcls
    { _dna1NetworkAclIds = mempty
    , _dna1Filters = mempty
    }

-- | One or more network ACL IDs. Default: Describes all your network ACLs.
dna1NetworkAclIds :: Lens' DescribeNetworkAcls [Text]
dna1NetworkAclIds =
    lens _dna1NetworkAclIds (\s a -> s { _dna1NetworkAclIds = a })

-- | One or more filters. association.association-id - The ID of an association
-- ID for the ACL. association.network-acl-id - The ID of the network ACL
-- involved in the association. association.subnet-id - The ID of the subnet
-- involved in the association. default - Indicates whether the ACL is the
-- default network ACL for the VPC. entry.cidr - The CIDR range specified in
-- the entry. entry.egress - Indicates whether the entry applies to egress
-- traffic. entry.icmp.code - The ICMP code specified in the entry, if any.
-- entry.icmp.type - The ICMP type specified in the entry, if any.
-- entry.port-range.from - The start of the port range specified in the entry.
-- entry.port-range.to - The end of the port range specified in the entry.
-- entry.protocol - The protocol specified in the entry (tcp | udp | icmp or a
-- protocol number). entry.rule-action - Allows or denies the matching traffic
-- (allow | deny). entry.rule-number - The number of an entry (in other words,
-- rule) in the ACL's set of entries. network-acl-id - The ID of the network
-- ACL. tag:key=value - The key/value combination of a tag assigned to the
-- resource. tag-key - The key of a tag assigned to the resource. This filter
-- is independent of the tag-value filter. For example, if you use both the
-- filter "tag-key=Purpose" and the filter "tag-value=X", you get any
-- resources assigned both the tag key Purpose (regardless of what the tag's
-- value is), and the tag value X (regardless of what the tag's key is). If
-- you want to list only resources where Purpose is X, see the tag:key=value
-- filter. tag-value - The value of a tag assigned to the resource. This
-- filter is independent of the tag-key filter. vpc-id - The ID of the VPC for
-- the network ACL.
dna1Filters :: Lens' DescribeNetworkAcls [Filter]
dna1Filters = lens _dna1Filters (\s a -> s { _dna1Filters = a })

instance ToQuery DescribeNetworkAcls where
    toQuery = genericQuery def

newtype DescribeNetworkAclsResponse = DescribeNetworkAclsResponse
    { _dnarNetworkAcls :: [NetworkAcl]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeNetworkAclsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @NetworkAcls ::@ @[NetworkAcl]@
--
mkDescribeNetworkAclsResponse :: DescribeNetworkAclsResponse
mkDescribeNetworkAclsResponse = DescribeNetworkAclsResponse
    { _dnarNetworkAcls = mempty
    }

-- | Information about one or more network ACLs.
dnarNetworkAcls :: Lens' DescribeNetworkAclsResponse [NetworkAcl]
dnarNetworkAcls = lens _dnarNetworkAcls (\s a -> s { _dnarNetworkAcls = a })

instance FromXML DescribeNetworkAclsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeNetworkAcls where
    type Sv DescribeNetworkAcls = EC2
    type Rs DescribeNetworkAcls = DescribeNetworkAclsResponse

    request = post "DescribeNetworkAcls"
    response _ = xmlResponse
