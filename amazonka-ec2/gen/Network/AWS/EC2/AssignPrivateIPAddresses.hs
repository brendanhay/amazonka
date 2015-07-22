{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AssignPrivateIPAddresses
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Assigns one or more secondary private IP addresses to the specified
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
    , apiarqPrivateIPAddresses
    , apiarqAllowReassignment
    , apiarqSecondaryPrivateIPAddressCount
    , apiarqNetworkInterfaceId

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
-- * 'apiarqPrivateIPAddresses'
--
-- * 'apiarqAllowReassignment'
--
-- * 'apiarqSecondaryPrivateIPAddressCount'
--
-- * 'apiarqNetworkInterfaceId'
data AssignPrivateIPAddresses = AssignPrivateIPAddresses'
    { _apiarqPrivateIPAddresses             :: !(Maybe [Text])
    , _apiarqAllowReassignment              :: !(Maybe Bool)
    , _apiarqSecondaryPrivateIPAddressCount :: !(Maybe Int)
    , _apiarqNetworkInterfaceId             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AssignPrivateIPAddresses' smart constructor.
assignPrivateIPAddresses :: Text -> AssignPrivateIPAddresses
assignPrivateIPAddresses pNetworkInterfaceId_ =
    AssignPrivateIPAddresses'
    { _apiarqPrivateIPAddresses = Nothing
    , _apiarqAllowReassignment = Nothing
    , _apiarqSecondaryPrivateIPAddressCount = Nothing
    , _apiarqNetworkInterfaceId = pNetworkInterfaceId_
    }

-- | One or more IP addresses to be assigned as a secondary private IP
-- address to the network interface. You can\'t specify this parameter when
-- also specifying a number of secondary IP addresses.
--
-- If you don\'t specify an IP address, Amazon EC2 automatically selects an
-- IP address within the subnet range.
apiarqPrivateIPAddresses :: Lens' AssignPrivateIPAddresses [Text]
apiarqPrivateIPAddresses = lens _apiarqPrivateIPAddresses (\ s a -> s{_apiarqPrivateIPAddresses = a}) . _Default;

-- | Indicates whether to allow an IP address that is already assigned to
-- another network interface or instance to be reassigned to the specified
-- network interface.
apiarqAllowReassignment :: Lens' AssignPrivateIPAddresses (Maybe Bool)
apiarqAllowReassignment = lens _apiarqAllowReassignment (\ s a -> s{_apiarqAllowReassignment = a});

-- | The number of secondary IP addresses to assign to the network interface.
-- You can\'t specify this parameter when also specifying private IP
-- addresses.
apiarqSecondaryPrivateIPAddressCount :: Lens' AssignPrivateIPAddresses (Maybe Int)
apiarqSecondaryPrivateIPAddressCount = lens _apiarqSecondaryPrivateIPAddressCount (\ s a -> s{_apiarqSecondaryPrivateIPAddressCount = a});

-- | The ID of the network interface.
apiarqNetworkInterfaceId :: Lens' AssignPrivateIPAddresses Text
apiarqNetworkInterfaceId = lens _apiarqNetworkInterfaceId (\ s a -> s{_apiarqNetworkInterfaceId = a});

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
                    _apiarqPrivateIPAddresses),
               "AllowReassignment" =: _apiarqAllowReassignment,
               "SecondaryPrivateIpAddressCount" =:
                 _apiarqSecondaryPrivateIPAddressCount,
               "NetworkInterfaceId" =: _apiarqNetworkInterfaceId]

-- | /See:/ 'assignPrivateIPAddressesResponse' smart constructor.
data AssignPrivateIPAddressesResponse =
    AssignPrivateIPAddressesResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AssignPrivateIPAddressesResponse' smart constructor.
assignPrivateIPAddressesResponse :: AssignPrivateIPAddressesResponse
assignPrivateIPAddressesResponse = AssignPrivateIPAddressesResponse'
