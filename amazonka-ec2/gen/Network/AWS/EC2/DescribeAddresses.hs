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
-- Module      : Network.AWS.EC2.DescribeAddresses
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your Elastic IP addresses.
--
-- An Elastic IP address is for use in either the EC2-Classic platform or
-- in a VPC. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP Addresses>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeAddresses.html AWS API Reference> for DescribeAddresses.
module Network.AWS.EC2.DescribeAddresses
    (
    -- * Creating a Request
      describeAddresses
    , DescribeAddresses
    -- * Request Lenses
    , daPublicIPs
    , daFilters
    , daDryRun
    , daAllocationIds

    -- * Destructuring the Response
    , describeAddressesResponse
    , DescribeAddressesResponse
    -- * Response Lenses
    , darsAddresses
    , darsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeAddresses' smart constructor.
data DescribeAddresses = DescribeAddresses'
    { _daPublicIPs     :: !(Maybe [Text])
    , _daFilters       :: !(Maybe [Filter])
    , _daDryRun        :: !(Maybe Bool)
    , _daAllocationIds :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeAddresses' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daPublicIPs'
--
-- * 'daFilters'
--
-- * 'daDryRun'
--
-- * 'daAllocationIds'
describeAddresses
    :: DescribeAddresses
describeAddresses =
    DescribeAddresses'
    { _daPublicIPs = Nothing
    , _daFilters = Nothing
    , _daDryRun = Nothing
    , _daAllocationIds = Nothing
    }

-- | [EC2-Classic] One or more Elastic IP addresses.
--
-- Default: Describes all your Elastic IP addresses.
daPublicIPs :: Lens' DescribeAddresses [Text]
daPublicIPs = lens _daPublicIPs (\ s a -> s{_daPublicIPs = a}) . _Default . _Coerce;

-- | One or more filters. Filter names and values are case-sensitive.
--
-- -   'allocation-id' - [EC2-VPC] The allocation ID for the address.
--
-- -   'association-id' - [EC2-VPC] The association ID for the address.
--
-- -   'domain' - Indicates whether the address is for use in EC2-Classic
--     ('standard') or in a VPC ('vpc').
--
-- -   'instance-id' - The ID of the instance the address is associated
--     with, if any.
--
-- -   'network-interface-id' - [EC2-VPC] The ID of the network interface
--     that the address is associated with, if any.
--
-- -   'network-interface-owner-id' - The AWS account ID of the owner.
--
-- -   'private-ip-address' - [EC2-VPC] The private IP address associated
--     with the Elastic IP address.
--
-- -   'public-ip' - The Elastic IP address.
--
daFilters :: Lens' DescribeAddresses [Filter]
daFilters = lens _daFilters (\ s a -> s{_daFilters = a}) . _Default . _Coerce;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
daDryRun :: Lens' DescribeAddresses (Maybe Bool)
daDryRun = lens _daDryRun (\ s a -> s{_daDryRun = a});

-- | [EC2-VPC] One or more allocation IDs.
--
-- Default: Describes all your Elastic IP addresses.
daAllocationIds :: Lens' DescribeAddresses [Text]
daAllocationIds = lens _daAllocationIds (\ s a -> s{_daAllocationIds = a}) . _Default . _Coerce;

instance AWSRequest DescribeAddresses where
        type Rs DescribeAddresses = DescribeAddressesResponse
        request = postQuery eC2
        response
          = receiveXML
              (\ s h x ->
                 DescribeAddressesResponse' <$>
                   (x .@? "addressesSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeAddresses where
        toHeaders = const mempty

instance ToPath DescribeAddresses where
        toPath = const "/"

instance ToQuery DescribeAddresses where
        toQuery DescribeAddresses'{..}
          = mconcat
              ["Action" =: ("DescribeAddresses" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "PublicIp" <$> _daPublicIPs),
               toQuery (toQueryList "Filter" <$> _daFilters),
               "DryRun" =: _daDryRun,
               toQuery
                 (toQueryList "AllocationId" <$> _daAllocationIds)]

-- | /See:/ 'describeAddressesResponse' smart constructor.
data DescribeAddressesResponse = DescribeAddressesResponse'
    { _darsAddresses :: !(Maybe [Address])
    , _darsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeAddressesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsAddresses'
--
-- * 'darsStatus'
describeAddressesResponse
    :: Int -- ^ 'darsStatus'
    -> DescribeAddressesResponse
describeAddressesResponse pStatus_ =
    DescribeAddressesResponse'
    { _darsAddresses = Nothing
    , _darsStatus = pStatus_
    }

-- | Information about one or more Elastic IP addresses.
darsAddresses :: Lens' DescribeAddressesResponse [Address]
darsAddresses = lens _darsAddresses (\ s a -> s{_darsAddresses = a}) . _Default . _Coerce;

-- | The response status code.
darsStatus :: Lens' DescribeAddressesResponse Int
darsStatus = lens _darsStatus (\ s a -> s{_darsStatus = a});
