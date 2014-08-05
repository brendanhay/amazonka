{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeSecurityGroups
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
module Network.AWS.EC2.V2014_06_15.DescribeSecurityGroups where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeSecurityGroups' request.
describeSecurityGroups :: DescribeSecurityGroups
describeSecurityGroups = DescribeSecurityGroups
    { _dsgsDryRun = Nothing
    , _dsgsFilters = mempty
    , _dsgsGroupIds = mempty
    , _dsgsGroupNames = mempty
    }

data DescribeSecurityGroups = DescribeSecurityGroups
    { _dsgsDryRun :: Maybe Bool
      -- ^ 
    , _dsgsFilters :: [Filter]
      -- ^ One or more filters. description - The description of the
      -- security group. group-id - The ID of the security group.
      -- group-name - The name of the security group. ip-permission.cidr -
      -- A CIDR range that has been granted permission.
      -- ip-permission.from-port - The start of port range for the TCP and
      -- UDP protocols, or an ICMP type number. ip-permission.group-id -
      -- The ID of a security group that has been granted permission.
      -- ip-permission.group-name - The name of a security group that has
      -- been granted permission. ip-permission.protocol - The IP protocol
      -- for the permission (tcp | udp | icmp or a protocol number).
      -- ip-permission.to-port - The end of port range for the TCP and UDP
      -- protocols, or an ICMP code. ip-permission.user-id - The ID of an
      -- AWS account that has been granted permission. owner-id - The AWS
      -- account ID of the owner of the security group. tag-key - The key
      -- of a tag assigned to the security group. tag-value - The value of
      -- a tag assigned to the security group. vpc-id - The ID of the VPC
      -- specified when the security group was created.
    , _dsgsGroupIds :: [Text]
      -- ^ One or more security group IDs. Default: Describes all your
      -- security groups.
    , _dsgsGroupNames :: [Text]
      -- ^ [EC2-Classic, default VPC] One or more security group names.
      -- Default: Describes all your security groups.
    } deriving (Show, Generic)

makeLenses ''DescribeSecurityGroups

instance ToQuery DescribeSecurityGroups where
    toQuery = genericToQuery def

data DescribeSecurityGroupsResponse = DescribeSecurityGroupsResponse
    { _dsgtSecurityGroups :: [SecurityGroup]
      -- ^ Information about one or more security groups.
    } deriving (Show, Generic)

makeLenses ''DescribeSecurityGroupsResponse

instance AWSRequest DescribeSecurityGroups where
    type Sv DescribeSecurityGroups = EC2
    type Rs DescribeSecurityGroups = DescribeSecurityGroupsResponse

    request = post "DescribeSecurityGroups"
    response _ = cursorResponse $ \hs xml ->
        pure DescribeSecurityGroupsResponse
            <*> xml %| "SecurityGroupList"
