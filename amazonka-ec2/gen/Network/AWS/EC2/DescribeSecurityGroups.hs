{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeSecurityGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of your security groups. A security group is for use
-- with instances either in the EC2-Classic platform or in a specific VPC. For
-- more information, see Amazon EC2 Security Groups in the Amazon Elastic
-- Compute Cloud User Guide and Security Groups for Your VPC in the Amazon
-- Virtual Private Cloud User Guide. Example 1 This example returns
-- information about two security groups that are configured for the account.
-- https://ec2.amazonaws.com/?Action=DescribeSecurityGroups
-- &amp;GroupName.1=WebServers &amp;GroupName.2=RangedPortsBySource
-- &amp;AUTHPARAMS 59dbff89-35bd-4eac-99ed-be587EXAMPLE 123456789012
-- sg-1a2b3c4d WebServers Web Servers tcp 80 80 0.0.0.0/0 123456789012
-- sg-2a2b3c4d RangedPortsBySource Group A tcp 6000 7000 123456789012
-- sg-3a2b3c4d Group B Example 2 This example describes all security groups
-- that grant access over TCP specifically on port 22 from instances
-- associated with app_server_group or database_group.
-- https://ec2.amazonaws.com/?Action=DescribeSecurityGroups
-- &amp;Filter.1.Name=ip-permission.protocol &amp;Filter.1.Value.1=tcp
-- &amp;Filter.2.Name=ip-permission.from-port &amp;Filter.2.Value.1=22
-- &amp;Filter.3.Name=ip-permission.to-port &amp;Filter.3.Value.1=22
-- &amp;Filter.4.Name=ip-permission.group-name
-- &amp;Filter.4.Value.1=app_server_group &amp;Filter.4.Value.2=database_group
-- &amp;AUTHPARAMS.
module Network.AWS.EC2.DescribeSecurityGroups
    (
    -- * Request
      DescribeSecurityGroups
    -- ** Request constructor
    , describeSecurityGroups
    -- ** Request lenses
    , dsg1GroupName
    , dsg1GroupId
    , dsg1Filter

    -- * Response
    , DescribeSecurityGroupsResponse
    -- ** Response constructor
    , describeSecurityGroupsResponse
    -- ** Response lenses
    , dsgrItem
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data DescribeSecurityGroups = DescribeSecurityGroups
    { _dsg1GroupName :: [Text]
    , _dsg1GroupId :: [Text]
    , _dsg1Filter :: [Filter]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeSecurityGroups' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GroupName ::@ @[Text]@
--
-- * @GroupId ::@ @[Text]@
--
-- * @Filter ::@ @[Filter]@
--
describeSecurityGroups :: DescribeSecurityGroups
describeSecurityGroups = DescribeSecurityGroups
    { _dsg1GroupName = mempty
    , _dsg1GroupId = mempty
    , _dsg1Filter = mempty
    }

-- | [EC2-Classic, default VPC] One or more security group names. Default:
-- Describes all your security groups.
dsg1GroupName :: Lens' DescribeSecurityGroups [Text]
dsg1GroupName = lens _dsg1GroupName (\s a -> s { _dsg1GroupName = a })

-- | One or more security group IDs. Default: Describes all your security
-- groups.
dsg1GroupId :: Lens' DescribeSecurityGroups [Text]
dsg1GroupId = lens _dsg1GroupId (\s a -> s { _dsg1GroupId = a })

-- | One or more filters. description - The description of the security group.
-- group-id - The ID of the security group. group-name - The name of the
-- security group. ip-permission.cidr - A CIDR range that has been granted
-- permission. ip-permission.from-port - The start of port range for the TCP
-- and UDP protocols, or an ICMP type number. ip-permission.group-id - The ID
-- of a security group that has been granted permission.
-- ip-permission.group-name - The name of a security group that has been
-- granted permission. ip-permission.protocol - The IP protocol for the
-- permission (tcp | udp | icmp or a protocol number). ip-permission.to-port -
-- The end of port range for the TCP and UDP protocols, or an ICMP code.
-- ip-permission.user-id - The ID of an AWS account that has been granted
-- permission. owner-id - The AWS account ID of the owner of the security
-- group. tag-key - The key of a tag assigned to the security group. tag-value
-- - The value of a tag assigned to the security group. vpc-id - The ID of the
-- VPC specified when the security group was created.
dsg1Filter :: Lens' DescribeSecurityGroups [Filter]
dsg1Filter = lens _dsg1Filter (\s a -> s { _dsg1Filter = a })

instance ToQuery DescribeSecurityGroups where
    toQuery = genericQuery def

newtype DescribeSecurityGroupsResponse = DescribeSecurityGroupsResponse
    { _dsgrItem :: [SecurityGroup]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeSecurityGroupsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Item ::@ @[SecurityGroup]@
--
describeSecurityGroupsResponse :: DescribeSecurityGroupsResponse
describeSecurityGroupsResponse = DescribeSecurityGroupsResponse
    { _dsgrItem = mempty
    }

-- | Information about one or more security groups.
dsgrItem :: Lens' DescribeSecurityGroupsResponse [SecurityGroup]
dsgrItem = lens _dsgrItem (\s a -> s { _dsgrItem = a })

instance FromXML DescribeSecurityGroupsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeSecurityGroups where
    type Sv DescribeSecurityGroups = EC2
    type Rs DescribeSecurityGroups = DescribeSecurityGroupsResponse

    request = post "DescribeSecurityGroups"
    response _ = xmlResponse
