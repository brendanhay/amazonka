{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.AllocateAddress
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

-- | Acquires an Elastic IP address.
--
-- An Elastic IP address is for use either in the EC2-Classic platform or
-- in a VPC. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP Addresses>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AllocateAddress.html>
module Network.AWS.EC2.AllocateAddress
    (
    -- * Request
      AllocateAddress
    -- ** Request constructor
    , allocateAddress
    -- ** Request lenses
    , aaDomain
    , aaDryRun

    -- * Response
    , AllocateAddressResponse
    -- ** Response constructor
    , allocateAddressResponse
    -- ** Response lenses
    , aarAllocationId
    , aarDomain
    , aarPublicIP
    ) where

import Network.AWS.EC2.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'allocateAddress' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aaDomain'
--
-- * 'aaDryRun'
data AllocateAddress = AllocateAddress'{_aaDomain :: Maybe DomainType, _aaDryRun :: Maybe Bool} deriving (Eq, Read, Show)

-- | 'AllocateAddress' smart constructor.
allocateAddress :: AllocateAddress
allocateAddress = AllocateAddress'{_aaDomain = Nothing, _aaDryRun = Nothing};

-- | Set to @vpc@ to allocate the address for use with instances in a VPC.
--
-- Default: The address is for use with instances in EC2-Classic.
aaDomain :: Lens' AllocateAddress (Maybe DomainType)
aaDomain = lens _aaDomain (\ s a -> s{_aaDomain = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
aaDryRun :: Lens' AllocateAddress (Maybe Bool)
aaDryRun = lens _aaDryRun (\ s a -> s{_aaDryRun = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest AllocateAddress where
        type Sv AllocateAddress = EC2
        type Rs AllocateAddress = AllocateAddressResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 AllocateAddressResponse' <$>
                   (x .@? "allocationId") <*> (x .@? "domain") <*>
                     (x .@? "publicIp"))

instance ToHeaders AllocateAddress where
        toHeaders = const mempty

instance ToPath AllocateAddress where
        toPath = const "/"

instance ToQuery AllocateAddress where
        toQuery AllocateAddress'{..}
          = mconcat
              ["Action" =: ("AllocateAddress" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "Domain" =: _aaDomain, "DryRun" =: _aaDryRun]

-- | /See:/ 'allocateAddressResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aarAllocationId'
--
-- * 'aarDomain'
--
-- * 'aarPublicIP'
data AllocateAddressResponse = AllocateAddressResponse'{_aarAllocationId :: Maybe Text, _aarDomain :: Maybe DomainType, _aarPublicIP :: Maybe Text} deriving (Eq, Read, Show)

-- | 'AllocateAddressResponse' smart constructor.
allocateAddressResponse :: AllocateAddressResponse
allocateAddressResponse = AllocateAddressResponse'{_aarAllocationId = Nothing, _aarDomain = Nothing, _aarPublicIP = Nothing};

-- | [EC2-VPC] The ID that AWS assigns to represent the allocation of the
-- Elastic IP address for use with instances in a VPC.
aarAllocationId :: Lens' AllocateAddressResponse (Maybe Text)
aarAllocationId = lens _aarAllocationId (\ s a -> s{_aarAllocationId = a});

-- | Indicates whether this Elastic IP address is for use with instances in
-- EC2-Classic (@standard@) or instances in a VPC (@vpc@).
aarDomain :: Lens' AllocateAddressResponse (Maybe DomainType)
aarDomain = lens _aarDomain (\ s a -> s{_aarDomain = a});

-- | The Elastic IP address.
aarPublicIP :: Lens' AllocateAddressResponse (Maybe Text)
aarPublicIP = lens _aarPublicIP (\ s a -> s{_aarPublicIP = a});
