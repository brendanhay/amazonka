{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeSecurityGroups
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your security groups.
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
    , dsgsrqGroupNames
    , dsgsrqFilters
    , dsgsrqGroupIds
    , dsgsrqDryRun

    -- * Response
    , DescribeSecurityGroupsResponse
    -- ** Response constructor
    , describeSecurityGroupsResponse
    -- ** Response lenses
    , dsgrsSecurityGroups
    , dsgrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeSecurityGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsgsrqGroupNames'
--
-- * 'dsgsrqFilters'
--
-- * 'dsgsrqGroupIds'
--
-- * 'dsgsrqDryRun'
data DescribeSecurityGroups = DescribeSecurityGroups'
    { _dsgsrqGroupNames :: !(Maybe [Text])
    , _dsgsrqFilters    :: !(Maybe [Filter])
    , _dsgsrqGroupIds   :: !(Maybe [Text])
    , _dsgsrqDryRun     :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSecurityGroups' smart constructor.
describeSecurityGroups :: DescribeSecurityGroups
describeSecurityGroups =
    DescribeSecurityGroups'
    { _dsgsrqGroupNames = Nothing
    , _dsgsrqFilters = Nothing
    , _dsgsrqGroupIds = Nothing
    , _dsgsrqDryRun = Nothing
    }

-- | [EC2-Classic and default VPC only] One or more security group names. You
-- can specify either the security group name or the security group ID. For
-- security groups in a nondefault VPC, use the @group-name@ filter to
-- describe security groups by name.
--
-- Default: Describes all your security groups.
dsgsrqGroupNames :: Lens' DescribeSecurityGroups [Text]
dsgsrqGroupNames = lens _dsgsrqGroupNames (\ s a -> s{_dsgsrqGroupNames = a}) . _Default;

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
dsgsrqFilters :: Lens' DescribeSecurityGroups [Filter]
dsgsrqFilters = lens _dsgsrqFilters (\ s a -> s{_dsgsrqFilters = a}) . _Default;

-- | One or more security group IDs. Required for security groups in a
-- nondefault VPC.
--
-- Default: Describes all your security groups.
dsgsrqGroupIds :: Lens' DescribeSecurityGroups [Text]
dsgsrqGroupIds = lens _dsgsrqGroupIds (\ s a -> s{_dsgsrqGroupIds = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dsgsrqDryRun :: Lens' DescribeSecurityGroups (Maybe Bool)
dsgsrqDryRun = lens _dsgsrqDryRun (\ s a -> s{_dsgsrqDryRun = a});

instance AWSRequest DescribeSecurityGroups where
        type Sv DescribeSecurityGroups = EC2
        type Rs DescribeSecurityGroups =
             DescribeSecurityGroupsResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeSecurityGroupsResponse' <$>
                   (x .@? "securityGroupInfo" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

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
                 (toQueryList "GroupName" <$> _dsgsrqGroupNames),
               toQuery (toQueryList "Filter" <$> _dsgsrqFilters),
               toQuery (toQueryList "groupId" <$> _dsgsrqGroupIds),
               "DryRun" =: _dsgsrqDryRun]

-- | /See:/ 'describeSecurityGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsgrsSecurityGroups'
--
-- * 'dsgrsStatus'
data DescribeSecurityGroupsResponse = DescribeSecurityGroupsResponse'
    { _dsgrsSecurityGroups :: !(Maybe [SecurityGroup])
    , _dsgrsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSecurityGroupsResponse' smart constructor.
describeSecurityGroupsResponse :: Int -> DescribeSecurityGroupsResponse
describeSecurityGroupsResponse pStatus =
    DescribeSecurityGroupsResponse'
    { _dsgrsSecurityGroups = Nothing
    , _dsgrsStatus = pStatus
    }

-- | Information about one or more security groups.
dsgrsSecurityGroups :: Lens' DescribeSecurityGroupsResponse [SecurityGroup]
dsgrsSecurityGroups = lens _dsgrsSecurityGroups (\ s a -> s{_dsgrsSecurityGroups = a}) . _Default;

-- | FIXME: Undocumented member.
dsgrsStatus :: Lens' DescribeSecurityGroupsResponse Int
dsgrsStatus = lens _dsgrsStatus (\ s a -> s{_dsgrsStatus = a});
