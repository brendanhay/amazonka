{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
-- Virtual Private Cloud User Guide.
module Network.AWS.EC2.DescribeSecurityGroups
    (
    -- * Request
      DescribeSecurityGroups
    -- ** Request constructor
    , describeSecurityGroups
    -- ** Request lenses
    , dsgDryRun
    , dsgFilters
    , dsgGroupIds
    , dsgGroupNames

    -- * Response
    , DescribeSecurityGroupsResult
    -- ** Response constructor
    , describeSecurityGroupsResult
    -- ** Response lenses
    , dsgrSecurityGroups
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DescribeSecurityGroups = DescribeSecurityGroups
    { _dsgDryRun     :: Maybe Bool
    , _dsgFilters    :: [Filter]
    , _dsgGroupIds   :: [Text]
    , _dsgGroupNames :: [Text]
    } deriving (Eq, Show, Generic)

-- | 'DescribeSecurityGroups' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsgDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dsgFilters' @::@ ['Filter']
--
-- * 'dsgGroupIds' @::@ ['Text']
--
-- * 'dsgGroupNames' @::@ ['Text']
--
describeSecurityGroups :: DescribeSecurityGroups
describeSecurityGroups = DescribeSecurityGroups
    { _dsgDryRun     = Nothing
    , _dsgGroupNames = mempty
    , _dsgGroupIds   = mempty
    , _dsgFilters    = mempty
    }

dsgDryRun :: Lens' DescribeSecurityGroups (Maybe Bool)
dsgDryRun = lens _dsgDryRun (\s a -> s { _dsgDryRun = a })

-- | One or more filters. description - The description of the security group.
-- group-id - The ID of the security group. group-name - The name of the
-- security group. ip-permission.cidr - A CIDR range that has been granted
-- permission. ip-permission.from-port - The start of port range for the TCP
-- and UDP protocols, or an ICMP type number. ip-permission.group-id - The
-- ID of a security group that has been granted permission.
-- ip-permission.group-name - The name of a security group that has been
-- granted permission. ip-permission.protocol - The IP protocol for the
-- permission (tcp | udp | icmp or a protocol number). ip-permission.to-port
-- - The end of port range for the TCP and UDP protocols, or an ICMP code.
-- ip-permission.user-id - The ID of an AWS account that has been granted
-- permission. owner-id - The AWS account ID of the owner of the security
-- group. tag-key - The key of a tag assigned to the security group.
-- tag-value - The value of a tag assigned to the security group. vpc-id -
-- The ID of the VPC specified when the security group was created.
dsgFilters :: Lens' DescribeSecurityGroups [Filter]
dsgFilters = lens _dsgFilters (\s a -> s { _dsgFilters = a })

-- | One or more security group IDs. Required for nondefault VPCs. Default:
-- Describes all your security groups.
dsgGroupIds :: Lens' DescribeSecurityGroups [Text]
dsgGroupIds = lens _dsgGroupIds (\s a -> s { _dsgGroupIds = a })

-- | [EC2-Classic, default VPC] One or more security group names. You can
-- specify either the security group name or the security group ID. Default:
-- Describes all your security groups.
dsgGroupNames :: Lens' DescribeSecurityGroups [Text]
dsgGroupNames = lens _dsgGroupNames (\s a -> s { _dsgGroupNames = a })

instance ToQuery DescribeSecurityGroups

instance ToPath DescribeSecurityGroups where
    toPath = const "/"

newtype DescribeSecurityGroupsResult = DescribeSecurityGroupsResult
    { _dsgrSecurityGroups :: [SecurityGroup]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance IsList DescribeSecurityGroupsResult where
    type Item DescribeSecurityGroupsResult = SecurityGroup

    fromList = DescribeSecurityGroupsResult . fromList
    toList   = toList . _dsgrSecurityGroups

-- | 'DescribeSecurityGroupsResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsgrSecurityGroups' @::@ ['SecurityGroup']
--
describeSecurityGroupsResult :: DescribeSecurityGroupsResult
describeSecurityGroupsResult = DescribeSecurityGroupsResult
    { _dsgrSecurityGroups = mempty
    }

-- | Information about one or more security groups.
dsgrSecurityGroups :: Lens' DescribeSecurityGroupsResult [SecurityGroup]
dsgrSecurityGroups =
    lens _dsgrSecurityGroups (\s a -> s { _dsgrSecurityGroups = a })

instance FromXML DescribeSecurityGroupsResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeSecurityGroupsResult"

instance AWSRequest DescribeSecurityGroups where
    type Sv DescribeSecurityGroups = EC2
    type Rs DescribeSecurityGroups = DescribeSecurityGroupsResult

    request  = post "DescribeSecurityGroups"
    response = xmlResponse $ \h x -> DescribeSecurityGroupsResult
        <$> x %| "securityGroupInfo"
