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
-- Module      : Network.AWS.EC2.AllocateAddress
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Acquires an Elastic IP address.
--
-- An Elastic IP address is for use either in the EC2-Classic platform or
-- in a VPC. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP Addresses>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AllocateAddress.html AWS API Reference> for AllocateAddress.
module Network.AWS.EC2.AllocateAddress
    (
    -- * Creating a Request
      allocateAddress
    , AllocateAddress
    -- * Request Lenses
    , aaDomain
    , aaDryRun

    -- * Destructuring the Response
    , allocateAddressResponse
    , AllocateAddressResponse
    -- * Response Lenses
    , aarsAllocationId
    , aarsDomain
    , aarsPublicIP
    , aarsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'allocateAddress' smart constructor.
data AllocateAddress = AllocateAddress'
    { _aaDomain :: !(Maybe DomainType)
    , _aaDryRun :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AllocateAddress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaDomain'
--
-- * 'aaDryRun'
allocateAddress
    :: AllocateAddress
allocateAddress =
    AllocateAddress'
    { _aaDomain = Nothing
    , _aaDryRun = Nothing
    }

-- | Set to 'vpc' to allocate the address for use with instances in a VPC.
--
-- Default: The address is for use with instances in EC2-Classic.
aaDomain :: Lens' AllocateAddress (Maybe DomainType)
aaDomain = lens _aaDomain (\ s a -> s{_aaDomain = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
aaDryRun :: Lens' AllocateAddress (Maybe Bool)
aaDryRun = lens _aaDryRun (\ s a -> s{_aaDryRun = a});

instance AWSRequest AllocateAddress where
        type Rs AllocateAddress = AllocateAddressResponse
        request = postQuery eC2
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
        toPath = const "/"

instance ToQuery AllocateAddress where
        toQuery AllocateAddress'{..}
          = mconcat
              ["Action" =: ("AllocateAddress" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "Domain" =: _aaDomain, "DryRun" =: _aaDryRun]

-- | /See:/ 'allocateAddressResponse' smart constructor.
data AllocateAddressResponse = AllocateAddressResponse'
    { _aarsAllocationId :: !(Maybe Text)
    , _aarsDomain       :: !(Maybe DomainType)
    , _aarsPublicIP     :: !(Maybe Text)
    , _aarsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AllocateAddressResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aarsAllocationId'
--
-- * 'aarsDomain'
--
-- * 'aarsPublicIP'
--
-- * 'aarsStatus'
allocateAddressResponse
    :: Int -- ^ 'aarsStatus'
    -> AllocateAddressResponse
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
-- EC2-Classic ('standard') or instances in a VPC ('vpc').
aarsDomain :: Lens' AllocateAddressResponse (Maybe DomainType)
aarsDomain = lens _aarsDomain (\ s a -> s{_aarsDomain = a});

-- | The Elastic IP address.
aarsPublicIP :: Lens' AllocateAddressResponse (Maybe Text)
aarsPublicIP = lens _aarsPublicIP (\ s a -> s{_aarsPublicIP = a});

-- | The response status code.
aarsStatus :: Lens' AllocateAddressResponse Int
aarsStatus = lens _aarsStatus (\ s a -> s{_aarsStatus = a});
