{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.AssignPrivateIPAddresses
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

-- | Assigns one or more secondary private IP addresses to the specified
-- network interface. You can specify one or more specific secondary IP
-- addresses, or you can specify the number of secondary IP addresses to be
-- automatically assigned within the subnet\'s CIDR block range. The number
-- of secondary IP addresses that you can assign to an instance varies by
-- instance type. For information about instance types, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types>
-- in the /Amazon Elastic Compute Cloud User Guide/. For more information
-- about Elastic IP addresses, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP Addresses>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- AssignPrivateIpAddresses is available only in EC2-VPC.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AssignPrivateIPAddresses.html>
module Network.AWS.EC2.AssignPrivateIPAddresses
    (
    -- * Request
      AssignPrivateIPAddresses
    -- ** Request constructor
    , assignPrivateIPAddresses
    -- ** Request lenses
    , apiaPrivateIPAddresses
    , apiaAllowReassignment
    , apiaSecondaryPrivateIPAddressCount
    , apiaNetworkInterfaceId

    -- * Response
    , AssignPrivateIPAddressesResponse
    -- ** Response constructor
    , assignPrivateIPAddressesResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'assignPrivateIPAddresses' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'apiaPrivateIPAddresses'
--
-- * 'apiaAllowReassignment'
--
-- * 'apiaSecondaryPrivateIPAddressCount'
--
-- * 'apiaNetworkInterfaceId'
data AssignPrivateIPAddresses = AssignPrivateIPAddresses'
    { _apiaPrivateIPAddresses             :: Maybe [Text]
    , _apiaAllowReassignment              :: Maybe Bool
    , _apiaSecondaryPrivateIPAddressCount :: Maybe Int
    , _apiaNetworkInterfaceId             :: Text
    } deriving (Eq,Read,Show)

-- | 'AssignPrivateIPAddresses' smart constructor.
assignPrivateIPAddresses :: Text -> AssignPrivateIPAddresses
assignPrivateIPAddresses pNetworkInterfaceId =
    AssignPrivateIPAddresses'
    { _apiaPrivateIPAddresses = Nothing
    , _apiaAllowReassignment = Nothing
    , _apiaSecondaryPrivateIPAddressCount = Nothing
    , _apiaNetworkInterfaceId = pNetworkInterfaceId
    }

-- | One or more IP addresses to be assigned as a secondary private IP
-- address to the network interface. You can\'t specify this parameter when
-- also specifying a number of secondary IP addresses.
--
-- If you don\'t specify an IP address, Amazon EC2 automatically selects an
-- IP address within the subnet range.
apiaPrivateIPAddresses :: Lens' AssignPrivateIPAddresses [Text]
apiaPrivateIPAddresses = lens _apiaPrivateIPAddresses (\ s a -> s{_apiaPrivateIPAddresses = a}) . _Default;

-- | Indicates whether to allow an IP address that is already assigned to
-- another network interface or instance to be reassigned to the specified
-- network interface.
apiaAllowReassignment :: Lens' AssignPrivateIPAddresses (Maybe Bool)
apiaAllowReassignment = lens _apiaAllowReassignment (\ s a -> s{_apiaAllowReassignment = a});

-- | The number of secondary IP addresses to assign to the network interface.
-- You can\'t specify this parameter when also specifying private IP
-- addresses.
apiaSecondaryPrivateIPAddressCount :: Lens' AssignPrivateIPAddresses (Maybe Int)
apiaSecondaryPrivateIPAddressCount = lens _apiaSecondaryPrivateIPAddressCount (\ s a -> s{_apiaSecondaryPrivateIPAddressCount = a});

-- | The ID of the network interface.
apiaNetworkInterfaceId :: Lens' AssignPrivateIPAddresses Text
apiaNetworkInterfaceId = lens _apiaNetworkInterfaceId (\ s a -> s{_apiaNetworkInterfaceId = a});

instance AWSRequest AssignPrivateIPAddresses where
        type Sv AssignPrivateIPAddresses = EC2
        type Rs AssignPrivateIPAddresses =
             AssignPrivateIPAddressesResponse
        request = post
        response
          = receiveNull AssignPrivateIPAddressesResponse'

instance ToHeaders AssignPrivateIPAddresses where
        toHeaders = const mempty

instance ToPath AssignPrivateIPAddresses where
        toPath = const "/"

instance ToQuery AssignPrivateIPAddresses where
        toQuery AssignPrivateIPAddresses'{..}
          = mconcat
              ["Action" =:
                 ("AssignPrivateIPAddresses" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery
                 (toQueryList "PrivateIpAddress" <$>
                    _apiaPrivateIPAddresses),
               "AllowReassignment" =: _apiaAllowReassignment,
               "SecondaryPrivateIpAddressCount" =:
                 _apiaSecondaryPrivateIPAddressCount,
               "NetworkInterfaceId" =: _apiaNetworkInterfaceId]

-- | /See:/ 'assignPrivateIPAddressesResponse' smart constructor.
data AssignPrivateIPAddressesResponse =
    AssignPrivateIPAddressesResponse'
    deriving (Eq,Read,Show)

-- | 'AssignPrivateIPAddressesResponse' smart constructor.
assignPrivateIPAddressesResponse :: AssignPrivateIPAddressesResponse
assignPrivateIPAddressesResponse = AssignPrivateIPAddressesResponse'
