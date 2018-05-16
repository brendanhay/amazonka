{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeSecurityGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your security groups.
--
--
-- A security group is for use with instances either in the EC2-Classic platform or in a specific VPC. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html Amazon EC2 Security Groups> in the /Amazon Elastic Compute Cloud User Guide/ and <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC> in the /Amazon Virtual Private Cloud User Guide/ .
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeSecurityGroups
    (
    -- * Creating a Request
      describeSecurityGroups
    , DescribeSecurityGroups
    -- * Request Lenses
    , dsgsFilters
    , dsgsGroupNames
    , dsgsGroupIds
    , dsgsNextToken
    , dsgsDryRun
    , dsgsMaxResults

    -- * Destructuring the Response
    , describeSecurityGroupsResponse
    , DescribeSecurityGroupsResponse
    -- * Response Lenses
    , dsgrsSecurityGroups
    , dsgrsNextToken
    , dsgrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeSecurityGroups.
--
--
--
-- /See:/ 'describeSecurityGroups' smart constructor.
data DescribeSecurityGroups = DescribeSecurityGroups'
  { _dsgsFilters    :: !(Maybe [Filter])
  , _dsgsGroupNames :: !(Maybe [Text])
  , _dsgsGroupIds   :: !(Maybe [Text])
  , _dsgsNextToken  :: !(Maybe Text)
  , _dsgsDryRun     :: !(Maybe Bool)
  , _dsgsMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSecurityGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsgsFilters' - One or more filters. If using multiple filters for rules, the results include security groups for which any combination of rules - not necessarily a single rule - match all filters.     * @description@ - The description of the security group.     * @egress.ip-permission.cidr@ - An IPv4 CIDR block for an outbound security group rule.     * @egress.ip-permission.from-port@ - For an outbound rule, the start of port range for the TCP and UDP protocols, or an ICMP type number.     * @egress.ip-permission.group-id@ - The ID of a security group that has been referenced in an outbound security group rule.     * @egress.ip-permission.group-name@ - The name of a security group that has been referenced in an outbound security group rule.     * @egress.ip-permission.ipv6-cidr@ - An IPv6 CIDR block for an outbound security group rule.     * @egress.ip-permission.prefix-list-id@ - The ID (prefix) of the AWS service to which a security group rule allows outbound access.     * @egress.ip-permission.protocol@ - The IP protocol for an outbound security group rule (@tcp@ | @udp@ | @icmp@ or a protocol number).     * @egress.ip-permission.to-port@ - For an outbound rule, the end of port range for the TCP and UDP protocols, or an ICMP code.     * @egress.ip-permission.user-id@ - The ID of an AWS account that has been referenced in an outbound security group rule.     * @group-id@ - The ID of the security group.      * @group-name@ - The name of the security group.     * @ip-permission.cidr@ - An IPv4 CIDR block for an inbound security group rule.     * @ip-permission.from-port@ - For an inbound rule, the start of port range for the TCP and UDP protocols, or an ICMP type number.     * @ip-permission.group-id@ - The ID of a security group that has been referenced in an inbound security group rule.     * @ip-permission.group-name@ - The name of a security group that has been referenced in an inbound security group rule.     * @ip-permission.ipv6-cidr@ - An IPv6 CIDR block for an inbound security group rule.     * @ip-permission.prefix-list-id@ - The ID (prefix) of the AWS service from which a security group rule allows inbound access.     * @ip-permission.protocol@ - The IP protocol for an inbound security group rule (@tcp@ | @udp@ | @icmp@ or a protocol number).     * @ip-permission.to-port@ - For an inbound rule, the end of port range for the TCP and UDP protocols, or an ICMP code.     * @ip-permission.user-id@ - The ID of an AWS account that has been referenced in an inbound security group rule.     * @owner-id@ - The AWS account ID of the owner of the security group.     * @tag-key@ - The key of a tag assigned to the security group.     * @tag-value@ - The value of a tag assigned to the security group.     * @vpc-id@ - The ID of the VPC specified when the security group was created.
--
-- * 'dsgsGroupNames' - [EC2-Classic and default VPC only] One or more security group names. You can specify either the security group name or the security group ID. For security groups in a nondefault VPC, use the @group-name@ filter to describe security groups by name. Default: Describes all your security groups.
--
-- * 'dsgsGroupIds' - One or more security group IDs. Required for security groups in a nondefault VPC. Default: Describes all your security groups.
--
-- * 'dsgsNextToken' - The token to request the next page of results.
--
-- * 'dsgsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dsgsMaxResults' - The maximum number of results to return in a single call. To retrieve the remaining results, make another request with the returned @NextToken@ value. This value can be between 5 and 1000. If this parameter is not specified, then all results are returned.
describeSecurityGroups
    :: DescribeSecurityGroups
describeSecurityGroups =
  DescribeSecurityGroups'
    { _dsgsFilters = Nothing
    , _dsgsGroupNames = Nothing
    , _dsgsGroupIds = Nothing
    , _dsgsNextToken = Nothing
    , _dsgsDryRun = Nothing
    , _dsgsMaxResults = Nothing
    }


-- | One or more filters. If using multiple filters for rules, the results include security groups for which any combination of rules - not necessarily a single rule - match all filters.     * @description@ - The description of the security group.     * @egress.ip-permission.cidr@ - An IPv4 CIDR block for an outbound security group rule.     * @egress.ip-permission.from-port@ - For an outbound rule, the start of port range for the TCP and UDP protocols, or an ICMP type number.     * @egress.ip-permission.group-id@ - The ID of a security group that has been referenced in an outbound security group rule.     * @egress.ip-permission.group-name@ - The name of a security group that has been referenced in an outbound security group rule.     * @egress.ip-permission.ipv6-cidr@ - An IPv6 CIDR block for an outbound security group rule.     * @egress.ip-permission.prefix-list-id@ - The ID (prefix) of the AWS service to which a security group rule allows outbound access.     * @egress.ip-permission.protocol@ - The IP protocol for an outbound security group rule (@tcp@ | @udp@ | @icmp@ or a protocol number).     * @egress.ip-permission.to-port@ - For an outbound rule, the end of port range for the TCP and UDP protocols, or an ICMP code.     * @egress.ip-permission.user-id@ - The ID of an AWS account that has been referenced in an outbound security group rule.     * @group-id@ - The ID of the security group.      * @group-name@ - The name of the security group.     * @ip-permission.cidr@ - An IPv4 CIDR block for an inbound security group rule.     * @ip-permission.from-port@ - For an inbound rule, the start of port range for the TCP and UDP protocols, or an ICMP type number.     * @ip-permission.group-id@ - The ID of a security group that has been referenced in an inbound security group rule.     * @ip-permission.group-name@ - The name of a security group that has been referenced in an inbound security group rule.     * @ip-permission.ipv6-cidr@ - An IPv6 CIDR block for an inbound security group rule.     * @ip-permission.prefix-list-id@ - The ID (prefix) of the AWS service from which a security group rule allows inbound access.     * @ip-permission.protocol@ - The IP protocol for an inbound security group rule (@tcp@ | @udp@ | @icmp@ or a protocol number).     * @ip-permission.to-port@ - For an inbound rule, the end of port range for the TCP and UDP protocols, or an ICMP code.     * @ip-permission.user-id@ - The ID of an AWS account that has been referenced in an inbound security group rule.     * @owner-id@ - The AWS account ID of the owner of the security group.     * @tag-key@ - The key of a tag assigned to the security group.     * @tag-value@ - The value of a tag assigned to the security group.     * @vpc-id@ - The ID of the VPC specified when the security group was created.
dsgsFilters :: Lens' DescribeSecurityGroups [Filter]
dsgsFilters = lens _dsgsFilters (\ s a -> s{_dsgsFilters = a}) . _Default . _Coerce

-- | [EC2-Classic and default VPC only] One or more security group names. You can specify either the security group name or the security group ID. For security groups in a nondefault VPC, use the @group-name@ filter to describe security groups by name. Default: Describes all your security groups.
dsgsGroupNames :: Lens' DescribeSecurityGroups [Text]
dsgsGroupNames = lens _dsgsGroupNames (\ s a -> s{_dsgsGroupNames = a}) . _Default . _Coerce

-- | One or more security group IDs. Required for security groups in a nondefault VPC. Default: Describes all your security groups.
dsgsGroupIds :: Lens' DescribeSecurityGroups [Text]
dsgsGroupIds = lens _dsgsGroupIds (\ s a -> s{_dsgsGroupIds = a}) . _Default . _Coerce

-- | The token to request the next page of results.
dsgsNextToken :: Lens' DescribeSecurityGroups (Maybe Text)
dsgsNextToken = lens _dsgsNextToken (\ s a -> s{_dsgsNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dsgsDryRun :: Lens' DescribeSecurityGroups (Maybe Bool)
dsgsDryRun = lens _dsgsDryRun (\ s a -> s{_dsgsDryRun = a})

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another request with the returned @NextToken@ value. This value can be between 5 and 1000. If this parameter is not specified, then all results are returned.
dsgsMaxResults :: Lens' DescribeSecurityGroups (Maybe Int)
dsgsMaxResults = lens _dsgsMaxResults (\ s a -> s{_dsgsMaxResults = a})

instance AWSPager DescribeSecurityGroups where
        page rq rs
          | stop (rs ^. dsgrsNextToken) = Nothing
          | stop (rs ^. dsgrsSecurityGroups) = Nothing
          | otherwise =
            Just $ rq & dsgsNextToken .~ rs ^. dsgrsNextToken

instance AWSRequest DescribeSecurityGroups where
        type Rs DescribeSecurityGroups =
             DescribeSecurityGroupsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeSecurityGroupsResponse' <$>
                   (x .@? "securityGroupInfo" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeSecurityGroups where

instance NFData DescribeSecurityGroups where

instance ToHeaders DescribeSecurityGroups where
        toHeaders = const mempty

instance ToPath DescribeSecurityGroups where
        toPath = const "/"

instance ToQuery DescribeSecurityGroups where
        toQuery DescribeSecurityGroups'{..}
          = mconcat
              ["Action" =:
                 ("DescribeSecurityGroups" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dsgsFilters),
               toQuery
                 (toQueryList "GroupName" <$> _dsgsGroupNames),
               toQuery (toQueryList "GroupId" <$> _dsgsGroupIds),
               "NextToken" =: _dsgsNextToken,
               "DryRun" =: _dsgsDryRun,
               "MaxResults" =: _dsgsMaxResults]

-- | Contains the output of DescribeSecurityGroups.
--
--
--
-- /See:/ 'describeSecurityGroupsResponse' smart constructor.
data DescribeSecurityGroupsResponse = DescribeSecurityGroupsResponse'
  { _dsgrsSecurityGroups :: !(Maybe [SecurityGroup])
  , _dsgrsNextToken      :: !(Maybe Text)
  , _dsgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSecurityGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsgrsSecurityGroups' - Information about one or more security groups.
--
-- * 'dsgrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dsgrsResponseStatus' - -- | The response status code.
describeSecurityGroupsResponse
    :: Int -- ^ 'dsgrsResponseStatus'
    -> DescribeSecurityGroupsResponse
describeSecurityGroupsResponse pResponseStatus_ =
  DescribeSecurityGroupsResponse'
    { _dsgrsSecurityGroups = Nothing
    , _dsgrsNextToken = Nothing
    , _dsgrsResponseStatus = pResponseStatus_
    }


-- | Information about one or more security groups.
dsgrsSecurityGroups :: Lens' DescribeSecurityGroupsResponse [SecurityGroup]
dsgrsSecurityGroups = lens _dsgrsSecurityGroups (\ s a -> s{_dsgrsSecurityGroups = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dsgrsNextToken :: Lens' DescribeSecurityGroupsResponse (Maybe Text)
dsgrsNextToken = lens _dsgrsNextToken (\ s a -> s{_dsgrsNextToken = a})

-- | -- | The response status code.
dsgrsResponseStatus :: Lens' DescribeSecurityGroupsResponse Int
dsgrsResponseStatus = lens _dsgrsResponseStatus (\ s a -> s{_dsgrsResponseStatus = a})

instance NFData DescribeSecurityGroupsResponse where
