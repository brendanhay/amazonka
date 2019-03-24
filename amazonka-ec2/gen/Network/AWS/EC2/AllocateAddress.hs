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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allocates an Elastic IP address to your AWS account. After you allocate the Elastic IP address you can associate it with an instance or network interface. After you release an Elastic IP address, it is released to the IP address pool and can be allocated to a different AWS account.
--
--
-- You can allocate an Elastic IP address from an address pool owned by AWS or from an address pool created from a public IPv4 address range that you have brought to AWS for use with your AWS resources using bring your own IP addresses (BYOIP). For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-byoip.html Bring Your Own IP Addresses (BYOIP)> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- [EC2-VPC] If you release an Elastic IP address, you might be able to recover it. You cannot recover an Elastic IP address that you released after it is allocated to another AWS account. You cannot recover an Elastic IP address for EC2-Classic. To attempt to recover an Elastic IP address that you released, specify it in this operation.
--
-- An Elastic IP address is for use either in the EC2-Classic platform or in a VPC. By default, you can allocate 5 Elastic IP addresses for EC2-Classic per region and 5 Elastic IP addresses for EC2-VPC per region.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP Addresses> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.AllocateAddress
    (
    -- * Creating a Request
      allocateAddress
    , AllocateAddress
    -- * Request Lenses
    , aaDomain
    , aaAddress
    , aaPublicIPv4Pool
    , aaDryRun

    -- * Destructuring the Response
    , allocateAddressResponse
    , AllocateAddressResponse
    -- * Response Lenses
    , aarsAllocationId
    , aarsDomain
    , aarsPublicIPv4Pool
    , aarsPublicIP
    , aarsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'allocateAddress' smart constructor.
data AllocateAddress = AllocateAddress'
  { _aaDomain         :: !(Maybe DomainType)
  , _aaAddress        :: !(Maybe Text)
  , _aaPublicIPv4Pool :: !(Maybe Text)
  , _aaDryRun         :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AllocateAddress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaDomain' - Set to @vpc@ to allocate the address for use with instances in a VPC. Default: The address is for use with instances in EC2-Classic.
--
-- * 'aaAddress' - [EC2-VPC] The Elastic IP address to recover or an IPv4 address from an address pool.
--
-- * 'aaPublicIPv4Pool' - The ID of an address pool that you own. Use this parameter to let Amazon EC2 select an address from the address pool. To specify a specific address from the address pool, use the @Address@ parameter instead.
--
-- * 'aaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
allocateAddress
    :: AllocateAddress
allocateAddress =
  AllocateAddress'
    { _aaDomain = Nothing
    , _aaAddress = Nothing
    , _aaPublicIPv4Pool = Nothing
    , _aaDryRun = Nothing
    }


-- | Set to @vpc@ to allocate the address for use with instances in a VPC. Default: The address is for use with instances in EC2-Classic.
aaDomain :: Lens' AllocateAddress (Maybe DomainType)
aaDomain = lens _aaDomain (\ s a -> s{_aaDomain = a})

-- | [EC2-VPC] The Elastic IP address to recover or an IPv4 address from an address pool.
aaAddress :: Lens' AllocateAddress (Maybe Text)
aaAddress = lens _aaAddress (\ s a -> s{_aaAddress = a})

-- | The ID of an address pool that you own. Use this parameter to let Amazon EC2 select an address from the address pool. To specify a specific address from the address pool, use the @Address@ parameter instead.
aaPublicIPv4Pool :: Lens' AllocateAddress (Maybe Text)
aaPublicIPv4Pool = lens _aaPublicIPv4Pool (\ s a -> s{_aaPublicIPv4Pool = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
aaDryRun :: Lens' AllocateAddress (Maybe Bool)
aaDryRun = lens _aaDryRun (\ s a -> s{_aaDryRun = a})

instance AWSRequest AllocateAddress where
        type Rs AllocateAddress = AllocateAddressResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 AllocateAddressResponse' <$>
                   (x .@? "allocationId") <*> (x .@? "domain") <*>
                     (x .@? "publicIpv4Pool")
                     <*> (x .@? "publicIp")
                     <*> (pure (fromEnum s)))

instance Hashable AllocateAddress where

instance NFData AllocateAddress where

instance ToHeaders AllocateAddress where
        toHeaders = const mempty

instance ToPath AllocateAddress where
        toPath = const "/"

instance ToQuery AllocateAddress where
        toQuery AllocateAddress'{..}
          = mconcat
              ["Action" =: ("AllocateAddress" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "Domain" =: _aaDomain, "Address" =: _aaAddress,
               "PublicIpv4Pool" =: _aaPublicIPv4Pool,
               "DryRun" =: _aaDryRun]

-- | /See:/ 'allocateAddressResponse' smart constructor.
data AllocateAddressResponse = AllocateAddressResponse'
  { _aarsAllocationId   :: !(Maybe Text)
  , _aarsDomain         :: !(Maybe DomainType)
  , _aarsPublicIPv4Pool :: !(Maybe Text)
  , _aarsPublicIP       :: !(Maybe Text)
  , _aarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AllocateAddressResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aarsAllocationId' - [EC2-VPC] The ID that AWS assigns to represent the allocation of the Elastic IP address for use with instances in a VPC.
--
-- * 'aarsDomain' - Indicates whether this Elastic IP address is for use with instances in EC2-Classic (@standard@ ) or instances in a VPC (@vpc@ ).
--
-- * 'aarsPublicIPv4Pool' - The ID of an address pool.
--
-- * 'aarsPublicIP' - The Elastic IP address.
--
-- * 'aarsResponseStatus' - -- | The response status code.
allocateAddressResponse
    :: Int -- ^ 'aarsResponseStatus'
    -> AllocateAddressResponse
allocateAddressResponse pResponseStatus_ =
  AllocateAddressResponse'
    { _aarsAllocationId = Nothing
    , _aarsDomain = Nothing
    , _aarsPublicIPv4Pool = Nothing
    , _aarsPublicIP = Nothing
    , _aarsResponseStatus = pResponseStatus_
    }


-- | [EC2-VPC] The ID that AWS assigns to represent the allocation of the Elastic IP address for use with instances in a VPC.
aarsAllocationId :: Lens' AllocateAddressResponse (Maybe Text)
aarsAllocationId = lens _aarsAllocationId (\ s a -> s{_aarsAllocationId = a})

-- | Indicates whether this Elastic IP address is for use with instances in EC2-Classic (@standard@ ) or instances in a VPC (@vpc@ ).
aarsDomain :: Lens' AllocateAddressResponse (Maybe DomainType)
aarsDomain = lens _aarsDomain (\ s a -> s{_aarsDomain = a})

-- | The ID of an address pool.
aarsPublicIPv4Pool :: Lens' AllocateAddressResponse (Maybe Text)
aarsPublicIPv4Pool = lens _aarsPublicIPv4Pool (\ s a -> s{_aarsPublicIPv4Pool = a})

-- | The Elastic IP address.
aarsPublicIP :: Lens' AllocateAddressResponse (Maybe Text)
aarsPublicIP = lens _aarsPublicIP (\ s a -> s{_aarsPublicIP = a})

-- | -- | The response status code.
aarsResponseStatus :: Lens' AllocateAddressResponse Int
aarsResponseStatus = lens _aarsResponseStatus (\ s a -> s{_aarsResponseStatus = a})

instance NFData AllocateAddressResponse where
