{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeAddresses
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your Elastic IP addresses.
--
-- An Elastic IP address is for use in either the EC2-Classic platform or
-- in a VPC. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP Addresses>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeAddresses.html>
module Network.AWS.EC2.DescribeAddresses
    (
    -- * Request
      DescribeAddresses
    -- ** Request constructor
    , describeAddresses
    -- ** Request lenses
    , daPublicIPs
    , daFilters
    , daDryRun
    , daAllocationIds

    -- * Response
    , DescribeAddressesResponse
    -- ** Response constructor
    , describeAddressesResponse
    -- ** Response lenses
    , darsAddresses
    , darsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeAddresses' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daPublicIPs'
--
-- * 'daFilters'
--
-- * 'daDryRun'
--
-- * 'daAllocationIds'
data DescribeAddresses = DescribeAddresses'
    { _daPublicIPs     :: !(Maybe [Text])
    , _daFilters       :: !(Maybe [Filter])
    , _daDryRun        :: !(Maybe Bool)
    , _daAllocationIds :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAddresses' smart constructor.
describeAddresses :: DescribeAddresses
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
daPublicIPs = lens _daPublicIPs (\ s a -> s{_daPublicIPs = a}) . _Default;

-- | One or more filters. Filter names and values are case-sensitive.
--
-- -   @allocation-id@ - [EC2-VPC] The allocation ID for the address.
--
-- -   @association-id@ - [EC2-VPC] The association ID for the address.
--
-- -   @domain@ - Indicates whether the address is for use in EC2-Classic
--     (@standard@) or in a VPC (@vpc@).
--
-- -   @instance-id@ - The ID of the instance the address is associated
--     with, if any.
--
-- -   @network-interface-id@ - [EC2-VPC] The ID of the network interface
--     that the address is associated with, if any.
--
-- -   @network-interface-owner-id@ - The AWS account ID of the owner.
--
-- -   @private-ip-address@ - [EC2-VPC] The private IP address associated
--     with the Elastic IP address.
--
-- -   @public-ip@ - The Elastic IP address.
--
daFilters :: Lens' DescribeAddresses [Filter]
daFilters = lens _daFilters (\ s a -> s{_daFilters = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
daDryRun :: Lens' DescribeAddresses (Maybe Bool)
daDryRun = lens _daDryRun (\ s a -> s{_daDryRun = a});

-- | [EC2-VPC] One or more allocation IDs.
--
-- Default: Describes all your Elastic IP addresses.
daAllocationIds :: Lens' DescribeAddresses [Text]
daAllocationIds = lens _daAllocationIds (\ s a -> s{_daAllocationIds = a}) . _Default;

instance AWSRequest DescribeAddresses where
        type Sv DescribeAddresses = EC2
        type Rs DescribeAddresses = DescribeAddressesResponse
        request = post "DescribeAddresses"
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'darsAddresses'
--
-- * 'darsStatus'
data DescribeAddressesResponse = DescribeAddressesResponse'
    { _darsAddresses :: !(Maybe [Address])
    , _darsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAddressesResponse' smart constructor.
describeAddressesResponse :: Int -> DescribeAddressesResponse
describeAddressesResponse pStatus_ =
    DescribeAddressesResponse'
    { _darsAddresses = Nothing
    , _darsStatus = pStatus_
    }

-- | Information about one or more Elastic IP addresses.
darsAddresses :: Lens' DescribeAddressesResponse [Address]
darsAddresses = lens _darsAddresses (\ s a -> s{_darsAddresses = a}) . _Default;

-- | FIXME: Undocumented member.
darsStatus :: Lens' DescribeAddressesResponse Int
darsStatus = lens _darsStatus (\ s a -> s{_darsStatus = a});
