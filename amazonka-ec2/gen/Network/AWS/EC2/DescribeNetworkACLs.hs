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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your network ACLs.
--
-- For more information about network ACLs, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_ACLs.html Network ACLs>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeNetworkACLs.html AWS API Reference> for DescribeNetworkACLs.
module Network.AWS.EC2.DescribeNetworkACLs
    (
    -- * Creating a Request
      describeNetworkACLs
    , DescribeNetworkACLs
    -- * Request Lenses
    , dnaclFilters
    , dnaclDryRun
    , dnaclNetworkACLIds

    -- * Destructuring the Response
    , describeNetworkACLsResponse
    , DescribeNetworkACLsResponse
    -- * Response Lenses
    , dnarsNetworkACLs
    , dnarsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeNetworkACLs' smart constructor.
data DescribeNetworkACLs = DescribeNetworkACLs'
    { _dnaclFilters       :: !(Maybe [Filter])
    , _dnaclDryRun        :: !(Maybe Bool)
    , _dnaclNetworkACLIds :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeNetworkACLs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnaclFilters'
--
-- * 'dnaclDryRun'
--
-- * 'dnaclNetworkACLIds'
describeNetworkACLs
    :: DescribeNetworkACLs
describeNetworkACLs =
    DescribeNetworkACLs'
    { _dnaclFilters = Nothing
    , _dnaclDryRun = Nothing
    , _dnaclNetworkACLIds = Nothing
    }

-- | One or more filters.
--
-- -   'association.association-id' - The ID of an association ID for the
--     ACL.
--
-- -   'association.network-acl-id' - The ID of the network ACL involved in
--     the association.
--
-- -   'association.subnet-id' - The ID of the subnet involved in the
--     association.
--
-- -   'default' - Indicates whether the ACL is the default network ACL for
--     the VPC.
--
-- -   'entry.cidr' - The CIDR range specified in the entry.
--
-- -   'entry.egress' - Indicates whether the entry applies to egress
--     traffic.
--
-- -   'entry.icmp.code' - The ICMP code specified in the entry, if any.
--
-- -   'entry.icmp.type' - The ICMP type specified in the entry, if any.
--
-- -   'entry.port-range.from' - The start of the port range specified in
--     the entry.
--
-- -   'entry.port-range.to' - The end of the port range specified in the
--     entry.
--
-- -   'entry.protocol' - The protocol specified in the entry ('tcp' |
--     'udp' | 'icmp' or a protocol number).
--
-- -   'entry.rule-action' - Allows or denies the matching traffic ('allow'
--     | 'deny').
--
-- -   'entry.rule-number' - The number of an entry (in other words, rule)
--     in the ACL\'s set of entries.
--
-- -   'network-acl-id' - The ID of the network ACL.
--
-- -   'tag':/key/=/value/ - The key\/value combination of a tag assigned
--     to the resource.
--
-- -   'tag-key' - The key of a tag assigned to the resource. This filter
--     is independent of the 'tag-value' filter. For example, if you use
--     both the filter \"tag-key=Purpose\" and the filter \"tag-value=X\",
--     you get any resources assigned both the tag key Purpose (regardless
--     of what the tag\'s value is), and the tag value X (regardless of
--     what the tag\'s key is). If you want to list only resources where
--     Purpose is X, see the 'tag':/key/=/value/ filter.
--
-- -   'tag-value' - The value of a tag assigned to the resource. This
--     filter is independent of the 'tag-key' filter.
--
-- -   'vpc-id' - The ID of the VPC for the network ACL.
--
dnaclFilters :: Lens' DescribeNetworkACLs [Filter]
dnaclFilters = lens _dnaclFilters (\ s a -> s{_dnaclFilters = a}) . _Default . _Coerce;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
dnaclDryRun :: Lens' DescribeNetworkACLs (Maybe Bool)
dnaclDryRun = lens _dnaclDryRun (\ s a -> s{_dnaclDryRun = a});

-- | One or more network ACL IDs.
--
-- Default: Describes all your network ACLs.
dnaclNetworkACLIds :: Lens' DescribeNetworkACLs [Text]
dnaclNetworkACLIds = lens _dnaclNetworkACLIds (\ s a -> s{_dnaclNetworkACLIds = a}) . _Default . _Coerce;

instance AWSRequest DescribeNetworkACLs where
        type Rs DescribeNetworkACLs =
             DescribeNetworkACLsResponse
        request = postQuery eC2
        response
          = receiveXML
              (\ s h x ->
                 DescribeNetworkACLsResponse' <$>
                   (x .@? "networkAclSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeNetworkACLs where
        toHeaders = const mempty

instance ToPath DescribeNetworkACLs where
        toPath = const "/"

instance ToQuery DescribeNetworkACLs where
        toQuery DescribeNetworkACLs'{..}
          = mconcat
              ["Action" =: ("DescribeNetworkAcls" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dnaclFilters),
               "DryRun" =: _dnaclDryRun,
               toQuery
                 (toQueryList "NetworkAclId" <$> _dnaclNetworkACLIds)]

-- | /See:/ 'describeNetworkACLsResponse' smart constructor.
data DescribeNetworkACLsResponse = DescribeNetworkACLsResponse'
    { _dnarsNetworkACLs :: !(Maybe [NetworkACL])
    , _dnarsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeNetworkACLsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnarsNetworkACLs'
--
-- * 'dnarsStatus'
describeNetworkACLsResponse
    :: Int -- ^ 'dnarsStatus'
    -> DescribeNetworkACLsResponse
describeNetworkACLsResponse pStatus_ =
    DescribeNetworkACLsResponse'
    { _dnarsNetworkACLs = Nothing
    , _dnarsStatus = pStatus_
    }

-- | Information about one or more network ACLs.
dnarsNetworkACLs :: Lens' DescribeNetworkACLsResponse [NetworkACL]
dnarsNetworkACLs = lens _dnarsNetworkACLs (\ s a -> s{_dnarsNetworkACLs = a}) . _Default . _Coerce;

-- | The response status code.
dnarsStatus :: Lens' DescribeNetworkACLsResponse Int
dnarsStatus = lens _dnarsStatus (\ s a -> s{_dnarsStatus = a});
