{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AllocateAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
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
-- An Elastic IP address is for use either in the EC2-Classic platform or in a VPC. By default, you can allocate 5 Elastic IP addresses for EC2-Classic per Region and 5 Elastic IP addresses for EC2-VPC per Region.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP Addresses> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- You can allocate a carrier IP address which is a public IP address from a telecommunication carrier, to a network interface which resides in a subnet in a Wavelength Zone (for example an EC2 instance).
module Network.AWS.EC2.AllocateAddress
  ( -- * Creating a Request
    allocateAddress,
    AllocateAddress,

    -- * Request Lenses
    aaNetworkBorderGroup,
    aaDomain,
    aaAddress,
    aaPublicIPv4Pool,
    aaCustomerOwnedIPv4Pool,
    aaDryRun,

    -- * Destructuring the Response
    allocateAddressResponse,
    AllocateAddressResponse,

    -- * Response Lenses
    aarsAllocationId,
    aarsCarrierIP,
    aarsNetworkBorderGroup,
    aarsDomain,
    aarsPublicIPv4Pool,
    aarsCustomerOwnedIPv4Pool,
    aarsCustomerOwnedIP,
    aarsPublicIP,
    aarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'allocateAddress' smart constructor.
data AllocateAddress = AllocateAddress'
  { _aaNetworkBorderGroup ::
      !(Maybe Text),
    _aaDomain :: !(Maybe DomainType),
    _aaAddress :: !(Maybe Text),
    _aaPublicIPv4Pool :: !(Maybe Text),
    _aaCustomerOwnedIPv4Pool :: !(Maybe Text),
    _aaDryRun :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AllocateAddress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaNetworkBorderGroup' - A unique set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses. Use this parameter to limit the IP address to this location. IP addresses cannot move between network border groups. Use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeAvailabilityZones.html DescribeAvailabilityZones> to view the network border groups.
--
-- * 'aaDomain' - Indicates whether the Elastic IP address is for use with instances in a VPC or instances in EC2-Classic. Default: If the Region supports EC2-Classic, the default is @standard@ . Otherwise, the default is @vpc@ .
--
-- * 'aaAddress' - [EC2-VPC] The Elastic IP address to recover or an IPv4 address from an address pool.
--
-- * 'aaPublicIPv4Pool' - The ID of an address pool that you own. Use this parameter to let Amazon EC2 select an address from the address pool. To specify a specific address from the address pool, use the @Address@ parameter instead.
--
-- * 'aaCustomerOwnedIPv4Pool' - The ID of a customer-owned address pool. Use this parameter to let Amazon EC2 select an address from the address pool. Alternatively, specify a specific address from the address pool.
--
-- * 'aaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
allocateAddress ::
  AllocateAddress
allocateAddress =
  AllocateAddress'
    { _aaNetworkBorderGroup = Nothing,
      _aaDomain = Nothing,
      _aaAddress = Nothing,
      _aaPublicIPv4Pool = Nothing,
      _aaCustomerOwnedIPv4Pool = Nothing,
      _aaDryRun = Nothing
    }

-- | A unique set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses. Use this parameter to limit the IP address to this location. IP addresses cannot move between network border groups. Use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeAvailabilityZones.html DescribeAvailabilityZones> to view the network border groups.
aaNetworkBorderGroup :: Lens' AllocateAddress (Maybe Text)
aaNetworkBorderGroup = lens _aaNetworkBorderGroup (\s a -> s {_aaNetworkBorderGroup = a})

-- | Indicates whether the Elastic IP address is for use with instances in a VPC or instances in EC2-Classic. Default: If the Region supports EC2-Classic, the default is @standard@ . Otherwise, the default is @vpc@ .
aaDomain :: Lens' AllocateAddress (Maybe DomainType)
aaDomain = lens _aaDomain (\s a -> s {_aaDomain = a})

-- | [EC2-VPC] The Elastic IP address to recover or an IPv4 address from an address pool.
aaAddress :: Lens' AllocateAddress (Maybe Text)
aaAddress = lens _aaAddress (\s a -> s {_aaAddress = a})

-- | The ID of an address pool that you own. Use this parameter to let Amazon EC2 select an address from the address pool. To specify a specific address from the address pool, use the @Address@ parameter instead.
aaPublicIPv4Pool :: Lens' AllocateAddress (Maybe Text)
aaPublicIPv4Pool = lens _aaPublicIPv4Pool (\s a -> s {_aaPublicIPv4Pool = a})

-- | The ID of a customer-owned address pool. Use this parameter to let Amazon EC2 select an address from the address pool. Alternatively, specify a specific address from the address pool.
aaCustomerOwnedIPv4Pool :: Lens' AllocateAddress (Maybe Text)
aaCustomerOwnedIPv4Pool = lens _aaCustomerOwnedIPv4Pool (\s a -> s {_aaCustomerOwnedIPv4Pool = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
aaDryRun :: Lens' AllocateAddress (Maybe Bool)
aaDryRun = lens _aaDryRun (\s a -> s {_aaDryRun = a})

instance AWSRequest AllocateAddress where
  type Rs AllocateAddress = AllocateAddressResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          AllocateAddressResponse'
            <$> (x .@? "allocationId")
            <*> (x .@? "carrierIp")
            <*> (x .@? "networkBorderGroup")
            <*> (x .@? "domain")
            <*> (x .@? "publicIpv4Pool")
            <*> (x .@? "customerOwnedIpv4Pool")
            <*> (x .@? "customerOwnedIp")
            <*> (x .@? "publicIp")
            <*> (pure (fromEnum s))
      )

instance Hashable AllocateAddress

instance NFData AllocateAddress

instance ToHeaders AllocateAddress where
  toHeaders = const mempty

instance ToPath AllocateAddress where
  toPath = const "/"

instance ToQuery AllocateAddress where
  toQuery AllocateAddress' {..} =
    mconcat
      [ "Action" =: ("AllocateAddress" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "NetworkBorderGroup" =: _aaNetworkBorderGroup,
        "Domain" =: _aaDomain,
        "Address" =: _aaAddress,
        "PublicIpv4Pool" =: _aaPublicIPv4Pool,
        "CustomerOwnedIpv4Pool" =: _aaCustomerOwnedIPv4Pool,
        "DryRun" =: _aaDryRun
      ]

-- | /See:/ 'allocateAddressResponse' smart constructor.
data AllocateAddressResponse = AllocateAddressResponse'
  { _aarsAllocationId ::
      !(Maybe Text),
    _aarsCarrierIP :: !(Maybe Text),
    _aarsNetworkBorderGroup :: !(Maybe Text),
    _aarsDomain :: !(Maybe DomainType),
    _aarsPublicIPv4Pool :: !(Maybe Text),
    _aarsCustomerOwnedIPv4Pool :: !(Maybe Text),
    _aarsCustomerOwnedIP :: !(Maybe Text),
    _aarsPublicIP :: !(Maybe Text),
    _aarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AllocateAddressResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aarsAllocationId' - [EC2-VPC] The ID that AWS assigns to represent the allocation of the Elastic IP address for use with instances in a VPC.
--
-- * 'aarsCarrierIP' - The carrier IP address. This option is only available for network interfaces which reside in a subnet in a Wavelength Zone (for example an EC2 instance).
--
-- * 'aarsNetworkBorderGroup' - The set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses.
--
-- * 'aarsDomain' - Indicates whether the Elastic IP address is for use with instances in a VPC (@vpc@ ) or instances in EC2-Classic (@standard@ ).
--
-- * 'aarsPublicIPv4Pool' - The ID of an address pool.
--
-- * 'aarsCustomerOwnedIPv4Pool' - The ID of the customer-owned address pool.
--
-- * 'aarsCustomerOwnedIP' - The customer-owned IP address.
--
-- * 'aarsPublicIP' - The Elastic IP address.
--
-- * 'aarsResponseStatus' - -- | The response status code.
allocateAddressResponse ::
  -- | 'aarsResponseStatus'
  Int ->
  AllocateAddressResponse
allocateAddressResponse pResponseStatus_ =
  AllocateAddressResponse'
    { _aarsAllocationId = Nothing,
      _aarsCarrierIP = Nothing,
      _aarsNetworkBorderGroup = Nothing,
      _aarsDomain = Nothing,
      _aarsPublicIPv4Pool = Nothing,
      _aarsCustomerOwnedIPv4Pool = Nothing,
      _aarsCustomerOwnedIP = Nothing,
      _aarsPublicIP = Nothing,
      _aarsResponseStatus = pResponseStatus_
    }

-- | [EC2-VPC] The ID that AWS assigns to represent the allocation of the Elastic IP address for use with instances in a VPC.
aarsAllocationId :: Lens' AllocateAddressResponse (Maybe Text)
aarsAllocationId = lens _aarsAllocationId (\s a -> s {_aarsAllocationId = a})

-- | The carrier IP address. This option is only available for network interfaces which reside in a subnet in a Wavelength Zone (for example an EC2 instance).
aarsCarrierIP :: Lens' AllocateAddressResponse (Maybe Text)
aarsCarrierIP = lens _aarsCarrierIP (\s a -> s {_aarsCarrierIP = a})

-- | The set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses.
aarsNetworkBorderGroup :: Lens' AllocateAddressResponse (Maybe Text)
aarsNetworkBorderGroup = lens _aarsNetworkBorderGroup (\s a -> s {_aarsNetworkBorderGroup = a})

-- | Indicates whether the Elastic IP address is for use with instances in a VPC (@vpc@ ) or instances in EC2-Classic (@standard@ ).
aarsDomain :: Lens' AllocateAddressResponse (Maybe DomainType)
aarsDomain = lens _aarsDomain (\s a -> s {_aarsDomain = a})

-- | The ID of an address pool.
aarsPublicIPv4Pool :: Lens' AllocateAddressResponse (Maybe Text)
aarsPublicIPv4Pool = lens _aarsPublicIPv4Pool (\s a -> s {_aarsPublicIPv4Pool = a})

-- | The ID of the customer-owned address pool.
aarsCustomerOwnedIPv4Pool :: Lens' AllocateAddressResponse (Maybe Text)
aarsCustomerOwnedIPv4Pool = lens _aarsCustomerOwnedIPv4Pool (\s a -> s {_aarsCustomerOwnedIPv4Pool = a})

-- | The customer-owned IP address.
aarsCustomerOwnedIP :: Lens' AllocateAddressResponse (Maybe Text)
aarsCustomerOwnedIP = lens _aarsCustomerOwnedIP (\s a -> s {_aarsCustomerOwnedIP = a})

-- | The Elastic IP address.
aarsPublicIP :: Lens' AllocateAddressResponse (Maybe Text)
aarsPublicIP = lens _aarsPublicIP (\s a -> s {_aarsPublicIP = a})

-- | -- | The response status code.
aarsResponseStatus :: Lens' AllocateAddressResponse Int
aarsResponseStatus = lens _aarsResponseStatus (\s a -> s {_aarsResponseStatus = a})

instance NFData AllocateAddressResponse
