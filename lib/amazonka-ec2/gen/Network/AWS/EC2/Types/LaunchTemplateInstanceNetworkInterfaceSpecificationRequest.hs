{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecificationRequest where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceIPv6AddressRequest
import Network.AWS.EC2.Types.PrivateIPAddressSpecification
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The parameters for a network interface.
--
--
--
-- /See:/ 'launchTemplateInstanceNetworkInterfaceSpecificationRequest' smart constructor.
data LaunchTemplateInstanceNetworkInterfaceSpecificationRequest = LaunchTemplateInstanceNetworkInterfaceSpecificationRequest'
  { _ltinisrGroups ::
      !( Maybe
           [Text]
       ),
    _ltinisrPrivateIPAddresses ::
      !( Maybe
           [PrivateIPAddressSpecification]
       ),
    _ltinisrDeleteOnTermination ::
      !( Maybe
           Bool
       ),
    _ltinisrAssociateCarrierIPAddress ::
      !( Maybe
           Bool
       ),
    _ltinisrAssociatePublicIPAddress ::
      !( Maybe
           Bool
       ),
    _ltinisrInterfaceType ::
      !( Maybe
           Text
       ),
    _ltinisrNetworkInterfaceId ::
      !( Maybe
           Text
       ),
    _ltinisrSubnetId ::
      !( Maybe
           Text
       ),
    _ltinisrIPv6AddressCount ::
      !( Maybe
           Int
       ),
    _ltinisrNetworkCardIndex ::
      !( Maybe
           Int
       ),
    _ltinisrPrivateIPAddress ::
      !( Maybe
           Text
       ),
    _ltinisrSecondaryPrivateIPAddressCount ::
      !( Maybe
           Int
       ),
    _ltinisrDescription ::
      !( Maybe
           Text
       ),
    _ltinisrDeviceIndex ::
      !( Maybe
           Int
       ),
    _ltinisrIPv6Addresses ::
      !( Maybe
           [InstanceIPv6AddressRequest]
       )
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltinisrGroups' - The IDs of one or more security groups.
--
-- * 'ltinisrPrivateIPAddresses' - One or more private IPv4 addresses.
--
-- * 'ltinisrDeleteOnTermination' - Indicates whether the network interface is deleted when the instance is terminated.
--
-- * 'ltinisrAssociateCarrierIPAddress' - Associates a Carrier IP address with eth0 for a new network interface. Use this option when you launch an instance in a Wavelength Zone and want to associate a Carrier IP address with the network interface. For more information about Carrier IP addresses, see <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses> in the /AWS Wavelength Developer Guide/ .
--
-- * 'ltinisrAssociatePublicIPAddress' - Associates a public IPv4 address with eth0 for a new network interface.
--
-- * 'ltinisrInterfaceType' - The type of network interface. To create an Elastic Fabric Adapter (EFA), specify @efa@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter> in the /Amazon Elastic Compute Cloud User Guide/ . If you are not creating an EFA, specify @interface@ or omit this parameter. Valid values: @interface@ | @efa@
--
-- * 'ltinisrNetworkInterfaceId' - The ID of the network interface.
--
-- * 'ltinisrSubnetId' - The ID of the subnet for the network interface.
--
-- * 'ltinisrIPv6AddressCount' - The number of IPv6 addresses to assign to a network interface. Amazon EC2 automatically selects the IPv6 addresses from the subnet range. You can't use this option if specifying specific IPv6 addresses.
--
-- * 'ltinisrNetworkCardIndex' - The index of the network card. Some instance types support multiple network cards. The primary network interface must be assigned to network card index 0. The default is network card index 0.
--
-- * 'ltinisrPrivateIPAddress' - The primary private IPv4 address of the network interface.
--
-- * 'ltinisrSecondaryPrivateIPAddressCount' - The number of secondary private IPv4 addresses to assign to a network interface.
--
-- * 'ltinisrDescription' - A description for the network interface.
--
-- * 'ltinisrDeviceIndex' - The device index for the network interface attachment.
--
-- * 'ltinisrIPv6Addresses' - One or more specific IPv6 addresses from the IPv6 CIDR block range of your subnet. You can't use this option if you're specifying a number of IPv6 addresses.
launchTemplateInstanceNetworkInterfaceSpecificationRequest ::
  LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
launchTemplateInstanceNetworkInterfaceSpecificationRequest =
  LaunchTemplateInstanceNetworkInterfaceSpecificationRequest'
    { _ltinisrGroups =
        Nothing,
      _ltinisrPrivateIPAddresses =
        Nothing,
      _ltinisrDeleteOnTermination =
        Nothing,
      _ltinisrAssociateCarrierIPAddress =
        Nothing,
      _ltinisrAssociatePublicIPAddress =
        Nothing,
      _ltinisrInterfaceType = Nothing,
      _ltinisrNetworkInterfaceId =
        Nothing,
      _ltinisrSubnetId = Nothing,
      _ltinisrIPv6AddressCount = Nothing,
      _ltinisrNetworkCardIndex = Nothing,
      _ltinisrPrivateIPAddress = Nothing,
      _ltinisrSecondaryPrivateIPAddressCount =
        Nothing,
      _ltinisrDescription = Nothing,
      _ltinisrDeviceIndex = Nothing,
      _ltinisrIPv6Addresses = Nothing
    }

-- | The IDs of one or more security groups.
ltinisrGroups :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest [Text]
ltinisrGroups = lens _ltinisrGroups (\s a -> s {_ltinisrGroups = a}) . _Default . _Coerce

-- | One or more private IPv4 addresses.
ltinisrPrivateIPAddresses :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest [PrivateIPAddressSpecification]
ltinisrPrivateIPAddresses = lens _ltinisrPrivateIPAddresses (\s a -> s {_ltinisrPrivateIPAddresses = a}) . _Default . _Coerce

-- | Indicates whether the network interface is deleted when the instance is terminated.
ltinisrDeleteOnTermination :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Maybe Bool)
ltinisrDeleteOnTermination = lens _ltinisrDeleteOnTermination (\s a -> s {_ltinisrDeleteOnTermination = a})

-- | Associates a Carrier IP address with eth0 for a new network interface. Use this option when you launch an instance in a Wavelength Zone and want to associate a Carrier IP address with the network interface. For more information about Carrier IP addresses, see <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses> in the /AWS Wavelength Developer Guide/ .
ltinisrAssociateCarrierIPAddress :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Maybe Bool)
ltinisrAssociateCarrierIPAddress = lens _ltinisrAssociateCarrierIPAddress (\s a -> s {_ltinisrAssociateCarrierIPAddress = a})

-- | Associates a public IPv4 address with eth0 for a new network interface.
ltinisrAssociatePublicIPAddress :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Maybe Bool)
ltinisrAssociatePublicIPAddress = lens _ltinisrAssociatePublicIPAddress (\s a -> s {_ltinisrAssociatePublicIPAddress = a})

-- | The type of network interface. To create an Elastic Fabric Adapter (EFA), specify @efa@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter> in the /Amazon Elastic Compute Cloud User Guide/ . If you are not creating an EFA, specify @interface@ or omit this parameter. Valid values: @interface@ | @efa@
ltinisrInterfaceType :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Maybe Text)
ltinisrInterfaceType = lens _ltinisrInterfaceType (\s a -> s {_ltinisrInterfaceType = a})

-- | The ID of the network interface.
ltinisrNetworkInterfaceId :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Maybe Text)
ltinisrNetworkInterfaceId = lens _ltinisrNetworkInterfaceId (\s a -> s {_ltinisrNetworkInterfaceId = a})

-- | The ID of the subnet for the network interface.
ltinisrSubnetId :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Maybe Text)
ltinisrSubnetId = lens _ltinisrSubnetId (\s a -> s {_ltinisrSubnetId = a})

-- | The number of IPv6 addresses to assign to a network interface. Amazon EC2 automatically selects the IPv6 addresses from the subnet range. You can't use this option if specifying specific IPv6 addresses.
ltinisrIPv6AddressCount :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Maybe Int)
ltinisrIPv6AddressCount = lens _ltinisrIPv6AddressCount (\s a -> s {_ltinisrIPv6AddressCount = a})

-- | The index of the network card. Some instance types support multiple network cards. The primary network interface must be assigned to network card index 0. The default is network card index 0.
ltinisrNetworkCardIndex :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Maybe Int)
ltinisrNetworkCardIndex = lens _ltinisrNetworkCardIndex (\s a -> s {_ltinisrNetworkCardIndex = a})

-- | The primary private IPv4 address of the network interface.
ltinisrPrivateIPAddress :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Maybe Text)
ltinisrPrivateIPAddress = lens _ltinisrPrivateIPAddress (\s a -> s {_ltinisrPrivateIPAddress = a})

-- | The number of secondary private IPv4 addresses to assign to a network interface.
ltinisrSecondaryPrivateIPAddressCount :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Maybe Int)
ltinisrSecondaryPrivateIPAddressCount = lens _ltinisrSecondaryPrivateIPAddressCount (\s a -> s {_ltinisrSecondaryPrivateIPAddressCount = a})

-- | A description for the network interface.
ltinisrDescription :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Maybe Text)
ltinisrDescription = lens _ltinisrDescription (\s a -> s {_ltinisrDescription = a})

-- | The device index for the network interface attachment.
ltinisrDeviceIndex :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Maybe Int)
ltinisrDeviceIndex = lens _ltinisrDeviceIndex (\s a -> s {_ltinisrDeviceIndex = a})

-- | One or more specific IPv6 addresses from the IPv6 CIDR block range of your subnet. You can't use this option if you're specifying a number of IPv6 addresses.
ltinisrIPv6Addresses :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest [InstanceIPv6AddressRequest]
ltinisrIPv6Addresses = lens _ltinisrIPv6Addresses (\s a -> s {_ltinisrIPv6Addresses = a}) . _Default . _Coerce

instance
  Hashable
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest

instance
  NFData
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest

instance
  ToQuery
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
  where
  toQuery
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {..} =
      mconcat
        [ toQuery (toQueryList "SecurityGroupId" <$> _ltinisrGroups),
          toQuery
            (toQueryList "PrivateIpAddresses" <$> _ltinisrPrivateIPAddresses),
          "DeleteOnTermination" =: _ltinisrDeleteOnTermination,
          "AssociateCarrierIpAddress" =: _ltinisrAssociateCarrierIPAddress,
          "AssociatePublicIpAddress" =: _ltinisrAssociatePublicIPAddress,
          "InterfaceType" =: _ltinisrInterfaceType,
          "NetworkInterfaceId" =: _ltinisrNetworkInterfaceId,
          "SubnetId" =: _ltinisrSubnetId,
          "Ipv6AddressCount" =: _ltinisrIPv6AddressCount,
          "NetworkCardIndex" =: _ltinisrNetworkCardIndex,
          "PrivateIpAddress" =: _ltinisrPrivateIPAddress,
          "SecondaryPrivateIpAddressCount"
            =: _ltinisrSecondaryPrivateIPAddressCount,
          "Description" =: _ltinisrDescription,
          "DeviceIndex" =: _ltinisrDeviceIndex,
          toQuery (toQueryList "Ipv6Addresses" <$> _ltinisrIPv6Addresses)
        ]
