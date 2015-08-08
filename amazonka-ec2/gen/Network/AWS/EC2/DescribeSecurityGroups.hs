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
-- Stability   : auto-generated
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
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSecurityGroups.html AWS API Reference> for DescribeSecurityGroups.
module Network.AWS.EC2.DescribeSecurityGroups
    (
    -- * Creating a Request
      DescribeSecurityGroups
    , describeSecurityGroups
    -- * Request Lenses
    , dsgsGroupNames
    , dsgsFilters
    , dsgsGroupIds
    , dsgsDryRun

    -- * Destructuring the Response
    , DescribeSecurityGroupsResponse
    , describeSecurityGroupsResponse
    -- * Response Lenses
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
-- * 'dsgsGroupNames'
--
-- * 'dsgsFilters'
--
-- * 'dsgsGroupIds'
--
-- * 'dsgsDryRun'
data DescribeSecurityGroups = DescribeSecurityGroups'
    { _dsgsGroupNames :: !(Maybe [Text])
    , _dsgsFilters    :: !(Maybe [Filter])
    , _dsgsGroupIds   :: !(Maybe [Text])
    , _dsgsDryRun     :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSecurityGroups' smart constructor.
describeSecurityGroups :: DescribeSecurityGroups
describeSecurityGroups =
    DescribeSecurityGroups'
    { _dsgsGroupNames = Nothing
    , _dsgsFilters = Nothing
    , _dsgsGroupIds = Nothing
    , _dsgsDryRun = Nothing
    }

-- | [EC2-Classic and default VPC only] One or more security group names. You
-- can specify either the security group name or the security group ID. For
-- security groups in a nondefault VPC, use the @group-name@ filter to
-- describe security groups by name.
--
-- Default: Describes all your security groups.
dsgsGroupNames :: Lens' DescribeSecurityGroups [Text]
dsgsGroupNames = lens _dsgsGroupNames (\ s a -> s{_dsgsGroupNames = a}) . _Default . _Coerce;

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
dsgsFilters :: Lens' DescribeSecurityGroups [Filter]
dsgsFilters = lens _dsgsFilters (\ s a -> s{_dsgsFilters = a}) . _Default . _Coerce;

-- | One or more security group IDs. Required for security groups in a
-- nondefault VPC.
--
-- Default: Describes all your security groups.
dsgsGroupIds :: Lens' DescribeSecurityGroups [Text]
dsgsGroupIds = lens _dsgsGroupIds (\ s a -> s{_dsgsGroupIds = a}) . _Default . _Coerce;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dsgsDryRun :: Lens' DescribeSecurityGroups (Maybe Bool)
dsgsDryRun = lens _dsgsDryRun (\ s a -> s{_dsgsDryRun = a});

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
                 (toQueryList "GroupName" <$> _dsgsGroupNames),
               toQuery (toQueryList "Filter" <$> _dsgsFilters),
               toQuery (toQueryList "groupId" <$> _dsgsGroupIds),
               "DryRun" =: _dsgsDryRun]

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
describeSecurityGroupsResponse pStatus_ =
    DescribeSecurityGroupsResponse'
    { _dsgrsSecurityGroups = Nothing
    , _dsgrsStatus = pStatus_
    }

-- | Information about one or more security groups.
dsgrsSecurityGroups :: Lens' DescribeSecurityGroupsResponse [SecurityGroup]
dsgrsSecurityGroups = lens _dsgrsSecurityGroups (\ s a -> s{_dsgrsSecurityGroups = a}) . _Default . _Coerce;

-- | Undocumented member.
dsgrsStatus :: Lens' DescribeSecurityGroupsResponse Int
dsgrsStatus = lens _dsgrsStatus (\ s a -> s{_dsgrsStatus = a});
