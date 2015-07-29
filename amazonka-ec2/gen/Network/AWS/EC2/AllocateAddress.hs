{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AllocateAddress
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Acquires an Elastic IP address.
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
    , aarsAllocationId
    , aarsDomain
    , aarsPublicIP
    , aarsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'allocateAddress' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aaDomain'
--
-- * 'aaDryRun'
data AllocateAddress = AllocateAddress'
    { _aaDomain :: !(Maybe DomainType)
    , _aaDryRun :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AllocateAddress' smart constructor.
allocateAddress :: AllocateAddress
allocateAddress =
    AllocateAddress'
    { _aaDomain = Nothing
    , _aaDryRun = Nothing
    }

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

instance AWSRequest AllocateAddress where
        type Sv AllocateAddress = EC2
        type Rs AllocateAddress = AllocateAddressResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 AllocateAddressResponse' <$>
                   (x .@? "allocationId") <*> (x .@? "domain") <*>
                     (x .@? "publicIp")
                     <*> (pure (fromEnum s)))

instance ToHeaders AllocateAddress where
        toHeaders = const mempty

instance ToPath AllocateAddress where
        toPath = const mempty

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
-- * 'aarsAllocationId'
--
-- * 'aarsDomain'
--
-- * 'aarsPublicIP'
--
-- * 'aarsStatus'
data AllocateAddressResponse = AllocateAddressResponse'
    { _aarsAllocationId :: !(Maybe Text)
    , _aarsDomain       :: !(Maybe DomainType)
    , _aarsPublicIP     :: !(Maybe Text)
    , _aarsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AllocateAddressResponse' smart constructor.
allocateAddressResponse :: Int -> AllocateAddressResponse
allocateAddressResponse pStatus_ =
    AllocateAddressResponse'
    { _aarsAllocationId = Nothing
    , _aarsDomain = Nothing
    , _aarsPublicIP = Nothing
    , _aarsStatus = pStatus_
    }

-- | [EC2-VPC] The ID that AWS assigns to represent the allocation of the
-- Elastic IP address for use with instances in a VPC.
aarsAllocationId :: Lens' AllocateAddressResponse (Maybe Text)
aarsAllocationId = lens _aarsAllocationId (\ s a -> s{_aarsAllocationId = a});

-- | Indicates whether this Elastic IP address is for use with instances in
-- EC2-Classic (@standard@) or instances in a VPC (@vpc@).
aarsDomain :: Lens' AllocateAddressResponse (Maybe DomainType)
aarsDomain = lens _aarsDomain (\ s a -> s{_aarsDomain = a});

-- | The Elastic IP address.
aarsPublicIP :: Lens' AllocateAddressResponse (Maybe Text)
aarsPublicIP = lens _aarsPublicIP (\ s a -> s{_aarsPublicIP = a});

-- | FIXME: Undocumented member.
aarsStatus :: Lens' AllocateAddressResponse Int
aarsStatus = lens _aarsStatus (\ s a -> s{_aarsStatus = a});
