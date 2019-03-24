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
-- Module      : Network.AWS.EC2.DescribeNetworkACLs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your network ACLs.
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_ACLs.html Network ACLs> in the /Amazon Virtual Private Cloud User Guide/ .
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeNetworkACLs
    (
    -- * Creating a Request
      describeNetworkACLs
    , DescribeNetworkACLs
    -- * Request Lenses
    , dnaclFilters
    , dnaclNextToken
    , dnaclNetworkACLIds
    , dnaclDryRun
    , dnaclMaxResults

    -- * Destructuring the Response
    , describeNetworkACLsResponse
    , DescribeNetworkACLsResponse
    -- * Response Lenses
    , dnarsNetworkACLs
    , dnarsNextToken
    , dnarsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeNetworkACLs' smart constructor.
data DescribeNetworkACLs = DescribeNetworkACLs'
  { _dnaclFilters       :: !(Maybe [Filter])
  , _dnaclNextToken     :: !(Maybe Text)
  , _dnaclNetworkACLIds :: !(Maybe [Text])
  , _dnaclDryRun        :: !(Maybe Bool)
  , _dnaclMaxResults    :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeNetworkACLs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnaclFilters' - One or more filters.     * @association.association-id@ - The ID of an association ID for the ACL.     * @association.network-acl-id@ - The ID of the network ACL involved in the association.     * @association.subnet-id@ - The ID of the subnet involved in the association.     * @default@ - Indicates whether the ACL is the default network ACL for the VPC.     * @entry.cidr@ - The IPv4 CIDR range specified in the entry.     * @entry.icmp.code@ - The ICMP code specified in the entry, if any.     * @entry.icmp.type@ - The ICMP type specified in the entry, if any.     * @entry.ipv6-cidr@ - The IPv6 CIDR range specified in the entry.     * @entry.port-range.from@ - The start of the port range specified in the entry.      * @entry.port-range.to@ - The end of the port range specified in the entry.      * @entry.protocol@ - The protocol specified in the entry (@tcp@ | @udp@ | @icmp@ or a protocol number).     * @entry.rule-action@ - Allows or denies the matching traffic (@allow@ | @deny@ ).     * @entry.rule-number@ - The number of an entry (in other words, rule) in the set of ACL entries.     * @network-acl-id@ - The ID of the network ACL.     * @owner-id@ - The ID of the AWS account that owns the network ACL.     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.     * @vpc-id@ - The ID of the VPC for the network ACL.
--
-- * 'dnaclNextToken' - The token for the next page of results.
--
-- * 'dnaclNetworkACLIds' - One or more network ACL IDs. Default: Describes all your network ACLs.
--
-- * 'dnaclDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dnaclMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
describeNetworkACLs
    :: DescribeNetworkACLs
describeNetworkACLs =
  DescribeNetworkACLs'
    { _dnaclFilters = Nothing
    , _dnaclNextToken = Nothing
    , _dnaclNetworkACLIds = Nothing
    , _dnaclDryRun = Nothing
    , _dnaclMaxResults = Nothing
    }


-- | One or more filters.     * @association.association-id@ - The ID of an association ID for the ACL.     * @association.network-acl-id@ - The ID of the network ACL involved in the association.     * @association.subnet-id@ - The ID of the subnet involved in the association.     * @default@ - Indicates whether the ACL is the default network ACL for the VPC.     * @entry.cidr@ - The IPv4 CIDR range specified in the entry.     * @entry.icmp.code@ - The ICMP code specified in the entry, if any.     * @entry.icmp.type@ - The ICMP type specified in the entry, if any.     * @entry.ipv6-cidr@ - The IPv6 CIDR range specified in the entry.     * @entry.port-range.from@ - The start of the port range specified in the entry.      * @entry.port-range.to@ - The end of the port range specified in the entry.      * @entry.protocol@ - The protocol specified in the entry (@tcp@ | @udp@ | @icmp@ or a protocol number).     * @entry.rule-action@ - Allows or denies the matching traffic (@allow@ | @deny@ ).     * @entry.rule-number@ - The number of an entry (in other words, rule) in the set of ACL entries.     * @network-acl-id@ - The ID of the network ACL.     * @owner-id@ - The ID of the AWS account that owns the network ACL.     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.     * @vpc-id@ - The ID of the VPC for the network ACL.
dnaclFilters :: Lens' DescribeNetworkACLs [Filter]
dnaclFilters = lens _dnaclFilters (\ s a -> s{_dnaclFilters = a}) . _Default . _Coerce

-- | The token for the next page of results.
dnaclNextToken :: Lens' DescribeNetworkACLs (Maybe Text)
dnaclNextToken = lens _dnaclNextToken (\ s a -> s{_dnaclNextToken = a})

-- | One or more network ACL IDs. Default: Describes all your network ACLs.
dnaclNetworkACLIds :: Lens' DescribeNetworkACLs [Text]
dnaclNetworkACLIds = lens _dnaclNetworkACLIds (\ s a -> s{_dnaclNetworkACLIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dnaclDryRun :: Lens' DescribeNetworkACLs (Maybe Bool)
dnaclDryRun = lens _dnaclDryRun (\ s a -> s{_dnaclDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
dnaclMaxResults :: Lens' DescribeNetworkACLs (Maybe Natural)
dnaclMaxResults = lens _dnaclMaxResults (\ s a -> s{_dnaclMaxResults = a}) . mapping _Nat

instance AWSPager DescribeNetworkACLs where
        page rq rs
          | stop (rs ^. dnarsNextToken) = Nothing
          | stop (rs ^. dnarsNetworkACLs) = Nothing
          | otherwise =
            Just $ rq & dnaclNextToken .~ rs ^. dnarsNextToken

instance AWSRequest DescribeNetworkACLs where
        type Rs DescribeNetworkACLs =
             DescribeNetworkACLsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeNetworkACLsResponse' <$>
                   (x .@? "networkAclSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeNetworkACLs where

instance NFData DescribeNetworkACLs where

instance ToHeaders DescribeNetworkACLs where
        toHeaders = const mempty

instance ToPath DescribeNetworkACLs where
        toPath = const "/"

instance ToQuery DescribeNetworkACLs where
        toQuery DescribeNetworkACLs'{..}
          = mconcat
              ["Action" =: ("DescribeNetworkAcls" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dnaclFilters),
               "NextToken" =: _dnaclNextToken,
               toQuery
                 (toQueryList "NetworkAclId" <$> _dnaclNetworkACLIds),
               "DryRun" =: _dnaclDryRun,
               "MaxResults" =: _dnaclMaxResults]

-- | /See:/ 'describeNetworkACLsResponse' smart constructor.
data DescribeNetworkACLsResponse = DescribeNetworkACLsResponse'
  { _dnarsNetworkACLs    :: !(Maybe [NetworkACL])
  , _dnarsNextToken      :: !(Maybe Text)
  , _dnarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeNetworkACLsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnarsNetworkACLs' - Information about one or more network ACLs.
--
-- * 'dnarsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dnarsResponseStatus' - -- | The response status code.
describeNetworkACLsResponse
    :: Int -- ^ 'dnarsResponseStatus'
    -> DescribeNetworkACLsResponse
describeNetworkACLsResponse pResponseStatus_ =
  DescribeNetworkACLsResponse'
    { _dnarsNetworkACLs = Nothing
    , _dnarsNextToken = Nothing
    , _dnarsResponseStatus = pResponseStatus_
    }


-- | Information about one or more network ACLs.
dnarsNetworkACLs :: Lens' DescribeNetworkACLsResponse [NetworkACL]
dnarsNetworkACLs = lens _dnarsNetworkACLs (\ s a -> s{_dnarsNetworkACLs = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dnarsNextToken :: Lens' DescribeNetworkACLsResponse (Maybe Text)
dnarsNextToken = lens _dnarsNextToken (\ s a -> s{_dnarsNextToken = a})

-- | -- | The response status code.
dnarsResponseStatus :: Lens' DescribeNetworkACLsResponse Int
dnarsResponseStatus = lens _dnarsResponseStatus (\ s a -> s{_dnarsResponseStatus = a})

instance NFData DescribeNetworkACLsResponse where
