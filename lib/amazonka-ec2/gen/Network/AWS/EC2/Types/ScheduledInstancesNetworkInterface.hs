{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesNetworkInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstancesNetworkInterface where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ScheduledInstancesIPv6Address
import Network.AWS.EC2.Types.ScheduledInstancesPrivateIPAddressConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a network interface for a Scheduled Instance.
--
--
--
-- /See:/ 'scheduledInstancesNetworkInterface' smart constructor.
data ScheduledInstancesNetworkInterface = ScheduledInstancesNetworkInterface'
  { _siniGroups ::
      !(Maybe [Text]),
    _siniDeleteOnTermination ::
      !(Maybe Bool),
    _siniAssociatePublicIPAddress ::
      !(Maybe Bool),
    _siniPrivateIPAddressConfigs ::
      !( Maybe
           [ScheduledInstancesPrivateIPAddressConfig]
       ),
    _siniNetworkInterfaceId ::
      !(Maybe Text),
    _siniSubnetId ::
      !(Maybe Text),
    _siniIPv6AddressCount ::
      !(Maybe Int),
    _siniPrivateIPAddress ::
      !(Maybe Text),
    _siniSecondaryPrivateIPAddressCount ::
      !(Maybe Int),
    _siniDescription ::
      !(Maybe Text),
    _siniDeviceIndex ::
      !(Maybe Int),
    _siniIPv6Addresses ::
      !( Maybe
           [ScheduledInstancesIPv6Address]
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScheduledInstancesNetworkInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siniGroups' - The IDs of the security groups.
--
-- * 'siniDeleteOnTermination' - Indicates whether to delete the interface when the instance is terminated.
--
-- * 'siniAssociatePublicIPAddress' - Indicates whether to assign a public IPv4 address to instances launched in a VPC. The public IPv4 address can only be assigned to a network interface for eth0, and can only be assigned to a new network interface, not an existing one. You cannot specify more than one network interface in the request. If launching into a default subnet, the default value is @true@ .
--
-- * 'siniPrivateIPAddressConfigs' - The private IPv4 addresses.
--
-- * 'siniNetworkInterfaceId' - The ID of the network interface.
--
-- * 'siniSubnetId' - The ID of the subnet.
--
-- * 'siniIPv6AddressCount' - The number of IPv6 addresses to assign to the network interface. The IPv6 addresses are automatically selected from the subnet range.
--
-- * 'siniPrivateIPAddress' - The IPv4 address of the network interface within the subnet.
--
-- * 'siniSecondaryPrivateIPAddressCount' - The number of secondary private IPv4 addresses.
--
-- * 'siniDescription' - The description.
--
-- * 'siniDeviceIndex' - The index of the device for the network interface attachment.
--
-- * 'siniIPv6Addresses' - The specific IPv6 addresses from the subnet range.
scheduledInstancesNetworkInterface ::
  ScheduledInstancesNetworkInterface
scheduledInstancesNetworkInterface =
  ScheduledInstancesNetworkInterface'
    { _siniGroups = Nothing,
      _siniDeleteOnTermination = Nothing,
      _siniAssociatePublicIPAddress = Nothing,
      _siniPrivateIPAddressConfigs = Nothing,
      _siniNetworkInterfaceId = Nothing,
      _siniSubnetId = Nothing,
      _siniIPv6AddressCount = Nothing,
      _siniPrivateIPAddress = Nothing,
      _siniSecondaryPrivateIPAddressCount = Nothing,
      _siniDescription = Nothing,
      _siniDeviceIndex = Nothing,
      _siniIPv6Addresses = Nothing
    }

-- | The IDs of the security groups.
siniGroups :: Lens' ScheduledInstancesNetworkInterface [Text]
siniGroups = lens _siniGroups (\s a -> s {_siniGroups = a}) . _Default . _Coerce

-- | Indicates whether to delete the interface when the instance is terminated.
siniDeleteOnTermination :: Lens' ScheduledInstancesNetworkInterface (Maybe Bool)
siniDeleteOnTermination = lens _siniDeleteOnTermination (\s a -> s {_siniDeleteOnTermination = a})

-- | Indicates whether to assign a public IPv4 address to instances launched in a VPC. The public IPv4 address can only be assigned to a network interface for eth0, and can only be assigned to a new network interface, not an existing one. You cannot specify more than one network interface in the request. If launching into a default subnet, the default value is @true@ .
siniAssociatePublicIPAddress :: Lens' ScheduledInstancesNetworkInterface (Maybe Bool)
siniAssociatePublicIPAddress = lens _siniAssociatePublicIPAddress (\s a -> s {_siniAssociatePublicIPAddress = a})

-- | The private IPv4 addresses.
siniPrivateIPAddressConfigs :: Lens' ScheduledInstancesNetworkInterface [ScheduledInstancesPrivateIPAddressConfig]
siniPrivateIPAddressConfigs = lens _siniPrivateIPAddressConfigs (\s a -> s {_siniPrivateIPAddressConfigs = a}) . _Default . _Coerce

-- | The ID of the network interface.
siniNetworkInterfaceId :: Lens' ScheduledInstancesNetworkInterface (Maybe Text)
siniNetworkInterfaceId = lens _siniNetworkInterfaceId (\s a -> s {_siniNetworkInterfaceId = a})

-- | The ID of the subnet.
siniSubnetId :: Lens' ScheduledInstancesNetworkInterface (Maybe Text)
siniSubnetId = lens _siniSubnetId (\s a -> s {_siniSubnetId = a})

-- | The number of IPv6 addresses to assign to the network interface. The IPv6 addresses are automatically selected from the subnet range.
siniIPv6AddressCount :: Lens' ScheduledInstancesNetworkInterface (Maybe Int)
siniIPv6AddressCount = lens _siniIPv6AddressCount (\s a -> s {_siniIPv6AddressCount = a})

-- | The IPv4 address of the network interface within the subnet.
siniPrivateIPAddress :: Lens' ScheduledInstancesNetworkInterface (Maybe Text)
siniPrivateIPAddress = lens _siniPrivateIPAddress (\s a -> s {_siniPrivateIPAddress = a})

-- | The number of secondary private IPv4 addresses.
siniSecondaryPrivateIPAddressCount :: Lens' ScheduledInstancesNetworkInterface (Maybe Int)
siniSecondaryPrivateIPAddressCount = lens _siniSecondaryPrivateIPAddressCount (\s a -> s {_siniSecondaryPrivateIPAddressCount = a})

-- | The description.
siniDescription :: Lens' ScheduledInstancesNetworkInterface (Maybe Text)
siniDescription = lens _siniDescription (\s a -> s {_siniDescription = a})

-- | The index of the device for the network interface attachment.
siniDeviceIndex :: Lens' ScheduledInstancesNetworkInterface (Maybe Int)
siniDeviceIndex = lens _siniDeviceIndex (\s a -> s {_siniDeviceIndex = a})

-- | The specific IPv6 addresses from the subnet range.
siniIPv6Addresses :: Lens' ScheduledInstancesNetworkInterface [ScheduledInstancesIPv6Address]
siniIPv6Addresses = lens _siniIPv6Addresses (\s a -> s {_siniIPv6Addresses = a}) . _Default . _Coerce

instance Hashable ScheduledInstancesNetworkInterface

instance NFData ScheduledInstancesNetworkInterface

instance ToQuery ScheduledInstancesNetworkInterface where
  toQuery ScheduledInstancesNetworkInterface' {..} =
    mconcat
      [ toQuery (toQueryList "Group" <$> _siniGroups),
        "DeleteOnTermination" =: _siniDeleteOnTermination,
        "AssociatePublicIpAddress" =: _siniAssociatePublicIPAddress,
        toQuery
          ( toQueryList "PrivateIpAddressConfig"
              <$> _siniPrivateIPAddressConfigs
          ),
        "NetworkInterfaceId" =: _siniNetworkInterfaceId,
        "SubnetId" =: _siniSubnetId,
        "Ipv6AddressCount" =: _siniIPv6AddressCount,
        "PrivateIpAddress" =: _siniPrivateIPAddress,
        "SecondaryPrivateIpAddressCount"
          =: _siniSecondaryPrivateIPAddressCount,
        "Description" =: _siniDescription,
        "DeviceIndex" =: _siniDeviceIndex,
        toQuery (toQueryList "Ipv6Address" <$> _siniIPv6Addresses)
      ]
