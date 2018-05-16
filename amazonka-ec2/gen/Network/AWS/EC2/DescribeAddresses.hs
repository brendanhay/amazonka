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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your Elastic IP addresses.
--
--
-- An Elastic IP address is for use in either the EC2-Classic platform or in a VPC. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP Addresses> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.DescribeAddresses
    (
    -- * Creating a Request
      describeAddresses
    , DescribeAddresses
    -- * Request Lenses
    , daFilters
    , daPublicIPs
    , daAllocationIds
    , daDryRun

    -- * Destructuring the Response
    , describeAddressesResponse
    , DescribeAddressesResponse
    -- * Response Lenses
    , darsAddresses
    , darsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeAddresses.
--
--
--
-- /See:/ 'describeAddresses' smart constructor.
data DescribeAddresses = DescribeAddresses'
  { _daFilters       :: !(Maybe [Filter])
  , _daPublicIPs     :: !(Maybe [Text])
  , _daAllocationIds :: !(Maybe [Text])
  , _daDryRun        :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAddresses' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daFilters' - One or more filters. Filter names and values are case-sensitive.     * @allocation-id@ - [EC2-VPC] The allocation ID for the address.     * @association-id@ - [EC2-VPC] The association ID for the address.     * @domain@ - Indicates whether the address is for use in EC2-Classic (@standard@ ) or in a VPC (@vpc@ ).     * @instance-id@ - The ID of the instance the address is associated with, if any.     * @network-interface-id@ - [EC2-VPC] The ID of the network interface that the address is associated with, if any.     * @network-interface-owner-id@ - The AWS account ID of the owner.     * @private-ip-address@ - [EC2-VPC] The private IP address associated with the Elastic IP address.     * @public-ip@ - The Elastic IP address.     * @tag@ :/key/ =/value/ - The key/value combination of a tag assigned to the resource. Specify the key of the tag in the filter name and the value of the tag in the filter value. For example, for the tag Purpose=X, specify @tag:Purpose@ for the filter name and @X@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. This filter is independent of the @tag-value@ filter. For example, if you use both the filter "tag-key=Purpose" and the filter "tag-value=X", you get any resources assigned both the tag key Purpose (regardless of what the tag's value is), and the tag value X (regardless of the tag's key). If you want to list only resources where Purpose is X, see the @tag@ :/key/ =/value/ filter.
--
-- * 'daPublicIPs' - [EC2-Classic] One or more Elastic IP addresses. Default: Describes all your Elastic IP addresses.
--
-- * 'daAllocationIds' - [EC2-VPC] One or more allocation IDs. Default: Describes all your Elastic IP addresses.
--
-- * 'daDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
describeAddresses
    :: DescribeAddresses
describeAddresses =
  DescribeAddresses'
    { _daFilters = Nothing
    , _daPublicIPs = Nothing
    , _daAllocationIds = Nothing
    , _daDryRun = Nothing
    }


-- | One or more filters. Filter names and values are case-sensitive.     * @allocation-id@ - [EC2-VPC] The allocation ID for the address.     * @association-id@ - [EC2-VPC] The association ID for the address.     * @domain@ - Indicates whether the address is for use in EC2-Classic (@standard@ ) or in a VPC (@vpc@ ).     * @instance-id@ - The ID of the instance the address is associated with, if any.     * @network-interface-id@ - [EC2-VPC] The ID of the network interface that the address is associated with, if any.     * @network-interface-owner-id@ - The AWS account ID of the owner.     * @private-ip-address@ - [EC2-VPC] The private IP address associated with the Elastic IP address.     * @public-ip@ - The Elastic IP address.     * @tag@ :/key/ =/value/ - The key/value combination of a tag assigned to the resource. Specify the key of the tag in the filter name and the value of the tag in the filter value. For example, for the tag Purpose=X, specify @tag:Purpose@ for the filter name and @X@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. This filter is independent of the @tag-value@ filter. For example, if you use both the filter "tag-key=Purpose" and the filter "tag-value=X", you get any resources assigned both the tag key Purpose (regardless of what the tag's value is), and the tag value X (regardless of the tag's key). If you want to list only resources where Purpose is X, see the @tag@ :/key/ =/value/ filter.
daFilters :: Lens' DescribeAddresses [Filter]
daFilters = lens _daFilters (\ s a -> s{_daFilters = a}) . _Default . _Coerce

-- | [EC2-Classic] One or more Elastic IP addresses. Default: Describes all your Elastic IP addresses.
daPublicIPs :: Lens' DescribeAddresses [Text]
daPublicIPs = lens _daPublicIPs (\ s a -> s{_daPublicIPs = a}) . _Default . _Coerce

-- | [EC2-VPC] One or more allocation IDs. Default: Describes all your Elastic IP addresses.
daAllocationIds :: Lens' DescribeAddresses [Text]
daAllocationIds = lens _daAllocationIds (\ s a -> s{_daAllocationIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
daDryRun :: Lens' DescribeAddresses (Maybe Bool)
daDryRun = lens _daDryRun (\ s a -> s{_daDryRun = a})

instance AWSRequest DescribeAddresses where
        type Rs DescribeAddresses = DescribeAddressesResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeAddressesResponse' <$>
                   (x .@? "addressesSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeAddresses where

instance NFData DescribeAddresses where

instance ToHeaders DescribeAddresses where
        toHeaders = const mempty

instance ToPath DescribeAddresses where
        toPath = const "/"

instance ToQuery DescribeAddresses where
        toQuery DescribeAddresses'{..}
          = mconcat
              ["Action" =: ("DescribeAddresses" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _daFilters),
               toQuery (toQueryList "PublicIp" <$> _daPublicIPs),
               toQuery
                 (toQueryList "AllocationId" <$> _daAllocationIds),
               "DryRun" =: _daDryRun]

-- | Contains the output of DescribeAddresses.
--
--
--
-- /See:/ 'describeAddressesResponse' smart constructor.
data DescribeAddressesResponse = DescribeAddressesResponse'
  { _darsAddresses      :: !(Maybe [Address])
  , _darsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAddressesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsAddresses' - Information about one or more Elastic IP addresses.
--
-- * 'darsResponseStatus' - -- | The response status code.
describeAddressesResponse
    :: Int -- ^ 'darsResponseStatus'
    -> DescribeAddressesResponse
describeAddressesResponse pResponseStatus_ =
  DescribeAddressesResponse'
    {_darsAddresses = Nothing, _darsResponseStatus = pResponseStatus_}


-- | Information about one or more Elastic IP addresses.
darsAddresses :: Lens' DescribeAddressesResponse [Address]
darsAddresses = lens _darsAddresses (\ s a -> s{_darsAddresses = a}) . _Default . _Coerce

-- | -- | The response status code.
darsResponseStatus :: Lens' DescribeAddressesResponse Int
darsResponseStatus = lens _darsResponseStatus (\ s a -> s{_darsResponseStatus = a})

instance NFData DescribeAddressesResponse where
