{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.DescribeSecurityGroups
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes one or more of your security groups.
--
-- A security group is for use with instances either in the EC2-Classic
-- platform or in a specific VPC. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html Amazon EC2 Security Groups>
-- in the /Amazon Elastic Compute Cloud User Guide/ and
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSecurityGroups.html>
module Network.AWS.EC2.DescribeSecurityGroups
    (
    -- * Request
      DescribeSecurityGroups
    -- ** Request constructor
    , describeSecurityGroups
    -- ** Request lenses
    , dsg1GroupNames
    , dsg1Filters
    , dsg1GroupIds
    , dsg1DryRun

    -- * Response
    , DescribeSecurityGroupsResponse
    -- ** Response constructor
    , describeSecurityGroupsResponse
    -- ** Response lenses
    , dsgrSecurityGroups
    , dsgrStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeSecurityGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsg1GroupNames'
--
-- * 'dsg1Filters'
--
-- * 'dsg1GroupIds'
--
-- * 'dsg1DryRun'
data DescribeSecurityGroups = DescribeSecurityGroups'
    { _dsg1GroupNames :: Maybe [Text]
    , _dsg1Filters    :: Maybe [Filter]
    , _dsg1GroupIds   :: Maybe [Text]
    , _dsg1DryRun     :: Maybe Bool
    } deriving (Eq,Read,Show)

-- | 'DescribeSecurityGroups' smart constructor.
describeSecurityGroups :: DescribeSecurityGroups
describeSecurityGroups =
    DescribeSecurityGroups'
    { _dsg1GroupNames = Nothing
    , _dsg1Filters = Nothing
    , _dsg1GroupIds = Nothing
    , _dsg1DryRun = Nothing
    }

-- | [EC2-Classic and default VPC only] One or more security group names. You
-- can specify either the security group name or the security group ID. For
-- security groups in a nondefault VPC, use the @group-name@ filter to
-- describe security groups by name.
--
-- Default: Describes all your security groups.
dsg1GroupNames :: Lens' DescribeSecurityGroups [Text]
dsg1GroupNames = lens _dsg1GroupNames (\ s a -> s{_dsg1GroupNames = a}) . _Default;

-- | One or more filters.
--
-- -   @description@ - The description of the security group.
--
-- -   @egress.ip-permission.prefix-list-id@ - The ID (prefix) of the AWS
--     service to which the security group allows access.
--
-- -   @group-id@ - The ID of the security group.
--
-- -   @group-name@ - The name of the security group.
--
-- -   @ip-permission.cidr@ - A CIDR range that has been granted
--     permission.
--
-- -   @ip-permission.from-port@ - The start of port range for the TCP and
--     UDP protocols, or an ICMP type number.
--
-- -   @ip-permission.group-id@ - The ID of a security group that has been
--     granted permission.
--
-- -   @ip-permission.group-name@ - The name of a security group that has
--     been granted permission.
--
-- -   @ip-permission.protocol@ - The IP protocol for the permission (@tcp@
--     | @udp@ | @icmp@ or a protocol number).
--
-- -   @ip-permission.to-port@ - The end of port range for the TCP and UDP
--     protocols, or an ICMP code.
--
-- -   @ip-permission.user-id@ - The ID of an AWS account that has been
--     granted permission.
--
-- -   @owner-id@ - The AWS account ID of the owner of the security group.
--
-- -   @tag-key@ - The key of a tag assigned to the security group.
--
-- -   @tag-value@ - The value of a tag assigned to the security group.
--
-- -   @vpc-id@ - The ID of the VPC specified when the security group was
--     created.
--
dsg1Filters :: Lens' DescribeSecurityGroups [Filter]
dsg1Filters = lens _dsg1Filters (\ s a -> s{_dsg1Filters = a}) . _Default;

-- | One or more security group IDs. Required for security groups in a
-- nondefault VPC.
--
-- Default: Describes all your security groups.
dsg1GroupIds :: Lens' DescribeSecurityGroups [Text]
dsg1GroupIds = lens _dsg1GroupIds (\ s a -> s{_dsg1GroupIds = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dsg1DryRun :: Lens' DescribeSecurityGroups (Maybe Bool)
dsg1DryRun = lens _dsg1DryRun (\ s a -> s{_dsg1DryRun = a});

instance AWSRequest DescribeSecurityGroups where
        type Sv DescribeSecurityGroups = EC2
        type Rs DescribeSecurityGroups =
             DescribeSecurityGroupsResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeSecurityGroupsResponse' <$>
                   (may (parseXMLList "item") x) <*>
                     (pure (fromEnum s)))

instance ToHeaders DescribeSecurityGroups where
        toHeaders = const mempty

instance ToPath DescribeSecurityGroups where
        toPath = const "/"

instance ToQuery DescribeSecurityGroups where
        toQuery DescribeSecurityGroups'{..}
          = mconcat
              ["Action" =:
                 ("DescribeSecurityGroups" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery
                 (toQueryList "GroupName" <$> _dsg1GroupNames),
               toQuery (toQueryList "Filter" <$> _dsg1Filters),
               toQuery (toQueryList "groupId" <$> _dsg1GroupIds),
               "DryRun" =: _dsg1DryRun]

-- | /See:/ 'describeSecurityGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsgrSecurityGroups'
--
-- * 'dsgrStatus'
data DescribeSecurityGroupsResponse = DescribeSecurityGroupsResponse'
    { _dsgrSecurityGroups :: Maybe [SecurityGroup]
    , _dsgrStatus         :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeSecurityGroupsResponse' smart constructor.
describeSecurityGroupsResponse :: Int -> DescribeSecurityGroupsResponse
describeSecurityGroupsResponse pStatus =
    DescribeSecurityGroupsResponse'
    { _dsgrSecurityGroups = Nothing
    , _dsgrStatus = pStatus
    }

-- | Information about one or more security groups.
dsgrSecurityGroups :: Lens' DescribeSecurityGroupsResponse [SecurityGroup]
dsgrSecurityGroups = lens _dsgrSecurityGroups (\ s a -> s{_dsgrSecurityGroups = a}) . _Default;

-- | FIXME: Undocumented member.
dsgrStatus :: Lens' DescribeSecurityGroupsResponse Int
dsgrStatus = lens _dsgrStatus (\ s a -> s{_dsgrStatus = a});
