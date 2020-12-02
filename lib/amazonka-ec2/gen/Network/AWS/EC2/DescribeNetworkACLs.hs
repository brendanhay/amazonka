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
-- For more information about network ACLs, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_ACLs.html Network ACLs> in the /Amazon Virtual Private Cloud User Guide/ .
--
module Network.AWS.EC2.DescribeNetworkACLs
    (
    -- * Creating a Request
      describeNetworkACLs
    , DescribeNetworkACLs
    -- * Request Lenses
    , dnaclFilters
    , dnaclNetworkACLIds
    , dnaclDryRun

    -- * Destructuring the Response
    , describeNetworkACLsResponse
    , DescribeNetworkACLsResponse
    -- * Response Lenses
    , dnarsNetworkACLs
    , dnarsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeNetworkAcls.
--
--
--
-- /See:/ 'describeNetworkACLs' smart constructor.
data DescribeNetworkACLs = DescribeNetworkACLs'
  { _dnaclFilters       :: !(Maybe [Filter])
  , _dnaclNetworkACLIds :: !(Maybe [Text])
  , _dnaclDryRun        :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeNetworkACLs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnaclFilters' - One or more filters.     * @association.association-id@ - The ID of an association ID for the ACL.     * @association.network-acl-id@ - The ID of the network ACL involved in the association.     * @association.subnet-id@ - The ID of the subnet involved in the association.     * @default@ - Indicates whether the ACL is the default network ACL for the VPC.     * @entry.cidr@ - The IPv4 CIDR range specified in the entry.     * @entry.egress@ - Indicates whether the entry applies to egress traffic.     * @entry.icmp.code@ - The ICMP code specified in the entry, if any.     * @entry.icmp.type@ - The ICMP type specified in the entry, if any.     * @entry.ipv6-cidr@ - The IPv6 CIDR range specified in the entry.     * @entry.port-range.from@ - The start of the port range specified in the entry.      * @entry.port-range.to@ - The end of the port range specified in the entry.      * @entry.protocol@ - The protocol specified in the entry (@tcp@ | @udp@ | @icmp@ or a protocol number).     * @entry.rule-action@ - Allows or denies the matching traffic (@allow@ | @deny@ ).     * @entry.rule-number@ - The number of an entry (in other words, rule) in the ACL's set of entries.     * @network-acl-id@ - The ID of the network ACL.     * @tag@ :/key/ =/value/ - The key/value combination of a tag assigned to the resource. Specify the key of the tag in the filter name and the value of the tag in the filter value. For example, for the tag Purpose=X, specify @tag:Purpose@ for the filter name and @X@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. This filter is independent of the @tag-value@ filter. For example, if you use both the filter "tag-key=Purpose" and the filter "tag-value=X", you get any resources assigned both the tag key Purpose (regardless of what the tag's value is), and the tag value X (regardless of what the tag's key is). If you want to list only resources where Purpose is X, see the @tag@ :/key/ =/value/ filter.     * @tag-value@ - The value of a tag assigned to the resource. This filter is independent of the @tag-key@ filter.     * @vpc-id@ - The ID of the VPC for the network ACL.
--
-- * 'dnaclNetworkACLIds' - One or more network ACL IDs. Default: Describes all your network ACLs.
--
-- * 'dnaclDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
describeNetworkACLs
    :: DescribeNetworkACLs
describeNetworkACLs =
  DescribeNetworkACLs'
    { _dnaclFilters = Nothing
    , _dnaclNetworkACLIds = Nothing
    , _dnaclDryRun = Nothing
    }


-- | One or more filters.     * @association.association-id@ - The ID of an association ID for the ACL.     * @association.network-acl-id@ - The ID of the network ACL involved in the association.     * @association.subnet-id@ - The ID of the subnet involved in the association.     * @default@ - Indicates whether the ACL is the default network ACL for the VPC.     * @entry.cidr@ - The IPv4 CIDR range specified in the entry.     * @entry.egress@ - Indicates whether the entry applies to egress traffic.     * @entry.icmp.code@ - The ICMP code specified in the entry, if any.     * @entry.icmp.type@ - The ICMP type specified in the entry, if any.     * @entry.ipv6-cidr@ - The IPv6 CIDR range specified in the entry.     * @entry.port-range.from@ - The start of the port range specified in the entry.      * @entry.port-range.to@ - The end of the port range specified in the entry.      * @entry.protocol@ - The protocol specified in the entry (@tcp@ | @udp@ | @icmp@ or a protocol number).     * @entry.rule-action@ - Allows or denies the matching traffic (@allow@ | @deny@ ).     * @entry.rule-number@ - The number of an entry (in other words, rule) in the ACL's set of entries.     * @network-acl-id@ - The ID of the network ACL.     * @tag@ :/key/ =/value/ - The key/value combination of a tag assigned to the resource. Specify the key of the tag in the filter name and the value of the tag in the filter value. For example, for the tag Purpose=X, specify @tag:Purpose@ for the filter name and @X@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. This filter is independent of the @tag-value@ filter. For example, if you use both the filter "tag-key=Purpose" and the filter "tag-value=X", you get any resources assigned both the tag key Purpose (regardless of what the tag's value is), and the tag value X (regardless of what the tag's key is). If you want to list only resources where Purpose is X, see the @tag@ :/key/ =/value/ filter.     * @tag-value@ - The value of a tag assigned to the resource. This filter is independent of the @tag-key@ filter.     * @vpc-id@ - The ID of the VPC for the network ACL.
dnaclFilters :: Lens' DescribeNetworkACLs [Filter]
dnaclFilters = lens _dnaclFilters (\ s a -> s{_dnaclFilters = a}) . _Default . _Coerce

-- | One or more network ACL IDs. Default: Describes all your network ACLs.
dnaclNetworkACLIds :: Lens' DescribeNetworkACLs [Text]
dnaclNetworkACLIds = lens _dnaclNetworkACLIds (\ s a -> s{_dnaclNetworkACLIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dnaclDryRun :: Lens' DescribeNetworkACLs (Maybe Bool)
dnaclDryRun = lens _dnaclDryRun (\ s a -> s{_dnaclDryRun = a})

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
               toQuery
                 (toQueryList "NetworkAclId" <$> _dnaclNetworkACLIds),
               "DryRun" =: _dnaclDryRun]

-- | Contains the output of DescribeNetworkAcls.
--
--
--
-- /See:/ 'describeNetworkACLsResponse' smart constructor.
data DescribeNetworkACLsResponse = DescribeNetworkACLsResponse'
  { _dnarsNetworkACLs    :: !(Maybe [NetworkACL])
  , _dnarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeNetworkACLsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnarsNetworkACLs' - Information about one or more network ACLs.
--
-- * 'dnarsResponseStatus' - -- | The response status code.
describeNetworkACLsResponse
    :: Int -- ^ 'dnarsResponseStatus'
    -> DescribeNetworkACLsResponse
describeNetworkACLsResponse pResponseStatus_ =
  DescribeNetworkACLsResponse'
    {_dnarsNetworkACLs = Nothing, _dnarsResponseStatus = pResponseStatus_}


-- | Information about one or more network ACLs.
dnarsNetworkACLs :: Lens' DescribeNetworkACLsResponse [NetworkACL]
dnarsNetworkACLs = lens _dnarsNetworkACLs (\ s a -> s{_dnarsNetworkACLs = a}) . _Default . _Coerce

-- | -- | The response status code.
dnarsResponseStatus :: Lens' DescribeNetworkACLsResponse Int
dnarsResponseStatus = lens _dnarsResponseStatus (\ s a -> s{_dnarsResponseStatus = a})

instance NFData DescribeNetworkACLsResponse where
