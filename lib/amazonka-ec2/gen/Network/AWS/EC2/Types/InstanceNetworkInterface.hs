{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceNetworkInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceNetworkInterface where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.GroupIdentifier
import Network.AWS.EC2.Types.InstanceIPv6Address
import Network.AWS.EC2.Types.InstanceNetworkInterfaceAssociation
import Network.AWS.EC2.Types.InstanceNetworkInterfaceAttachment
import Network.AWS.EC2.Types.InstancePrivateIPAddress
import Network.AWS.EC2.Types.NetworkInterfaceStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a network interface.
--
--
--
-- /See:/ 'instanceNetworkInterface' smart constructor.
data InstanceNetworkInterface = InstanceNetworkInterface'
  { _iniGroups ::
      !(Maybe [GroupIdentifier]),
    _iniStatus ::
      !(Maybe NetworkInterfaceStatus),
    _iniPrivateIPAddresses ::
      !(Maybe [InstancePrivateIPAddress]),
    _iniSourceDestCheck :: !(Maybe Bool),
    _iniInterfaceType :: !(Maybe Text),
    _iniVPCId :: !(Maybe Text),
    _iniNetworkInterfaceId :: !(Maybe Text),
    _iniSubnetId :: !(Maybe Text),
    _iniMACAddress :: !(Maybe Text),
    _iniAttachment ::
      !( Maybe
           InstanceNetworkInterfaceAttachment
       ),
    _iniOwnerId :: !(Maybe Text),
    _iniPrivateIPAddress :: !(Maybe Text),
    _iniPrivateDNSName :: !(Maybe Text),
    _iniDescription :: !(Maybe Text),
    _iniAssociation ::
      !( Maybe
           InstanceNetworkInterfaceAssociation
       ),
    _iniIPv6Addresses ::
      !(Maybe [InstanceIPv6Address])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceNetworkInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iniGroups' - One or more security groups.
--
-- * 'iniStatus' - The status of the network interface.
--
-- * 'iniPrivateIPAddresses' - One or more private IPv4 addresses associated with the network interface.
--
-- * 'iniSourceDestCheck' - Indicates whether to validate network traffic to or from this network interface.
--
-- * 'iniInterfaceType' - Describes the type of network interface. Valid values: @interface@ | @efa@
--
-- * 'iniVPCId' - The ID of the VPC.
--
-- * 'iniNetworkInterfaceId' - The ID of the network interface.
--
-- * 'iniSubnetId' - The ID of the subnet.
--
-- * 'iniMACAddress' - The MAC address.
--
-- * 'iniAttachment' - The network interface attachment.
--
-- * 'iniOwnerId' - The ID of the AWS account that created the network interface.
--
-- * 'iniPrivateIPAddress' - The IPv4 address of the network interface within the subnet.
--
-- * 'iniPrivateDNSName' - The private DNS name.
--
-- * 'iniDescription' - The description.
--
-- * 'iniAssociation' - The association information for an Elastic IPv4 associated with the network interface.
--
-- * 'iniIPv6Addresses' - One or more IPv6 addresses associated with the network interface.
instanceNetworkInterface ::
  InstanceNetworkInterface
instanceNetworkInterface =
  InstanceNetworkInterface'
    { _iniGroups = Nothing,
      _iniStatus = Nothing,
      _iniPrivateIPAddresses = Nothing,
      _iniSourceDestCheck = Nothing,
      _iniInterfaceType = Nothing,
      _iniVPCId = Nothing,
      _iniNetworkInterfaceId = Nothing,
      _iniSubnetId = Nothing,
      _iniMACAddress = Nothing,
      _iniAttachment = Nothing,
      _iniOwnerId = Nothing,
      _iniPrivateIPAddress = Nothing,
      _iniPrivateDNSName = Nothing,
      _iniDescription = Nothing,
      _iniAssociation = Nothing,
      _iniIPv6Addresses = Nothing
    }

-- | One or more security groups.
iniGroups :: Lens' InstanceNetworkInterface [GroupIdentifier]
iniGroups = lens _iniGroups (\s a -> s {_iniGroups = a}) . _Default . _Coerce

-- | The status of the network interface.
iniStatus :: Lens' InstanceNetworkInterface (Maybe NetworkInterfaceStatus)
iniStatus = lens _iniStatus (\s a -> s {_iniStatus = a})

-- | One or more private IPv4 addresses associated with the network interface.
iniPrivateIPAddresses :: Lens' InstanceNetworkInterface [InstancePrivateIPAddress]
iniPrivateIPAddresses = lens _iniPrivateIPAddresses (\s a -> s {_iniPrivateIPAddresses = a}) . _Default . _Coerce

-- | Indicates whether to validate network traffic to or from this network interface.
iniSourceDestCheck :: Lens' InstanceNetworkInterface (Maybe Bool)
iniSourceDestCheck = lens _iniSourceDestCheck (\s a -> s {_iniSourceDestCheck = a})

-- | Describes the type of network interface. Valid values: @interface@ | @efa@
iniInterfaceType :: Lens' InstanceNetworkInterface (Maybe Text)
iniInterfaceType = lens _iniInterfaceType (\s a -> s {_iniInterfaceType = a})

-- | The ID of the VPC.
iniVPCId :: Lens' InstanceNetworkInterface (Maybe Text)
iniVPCId = lens _iniVPCId (\s a -> s {_iniVPCId = a})

-- | The ID of the network interface.
iniNetworkInterfaceId :: Lens' InstanceNetworkInterface (Maybe Text)
iniNetworkInterfaceId = lens _iniNetworkInterfaceId (\s a -> s {_iniNetworkInterfaceId = a})

-- | The ID of the subnet.
iniSubnetId :: Lens' InstanceNetworkInterface (Maybe Text)
iniSubnetId = lens _iniSubnetId (\s a -> s {_iniSubnetId = a})

-- | The MAC address.
iniMACAddress :: Lens' InstanceNetworkInterface (Maybe Text)
iniMACAddress = lens _iniMACAddress (\s a -> s {_iniMACAddress = a})

-- | The network interface attachment.
iniAttachment :: Lens' InstanceNetworkInterface (Maybe InstanceNetworkInterfaceAttachment)
iniAttachment = lens _iniAttachment (\s a -> s {_iniAttachment = a})

-- | The ID of the AWS account that created the network interface.
iniOwnerId :: Lens' InstanceNetworkInterface (Maybe Text)
iniOwnerId = lens _iniOwnerId (\s a -> s {_iniOwnerId = a})

-- | The IPv4 address of the network interface within the subnet.
iniPrivateIPAddress :: Lens' InstanceNetworkInterface (Maybe Text)
iniPrivateIPAddress = lens _iniPrivateIPAddress (\s a -> s {_iniPrivateIPAddress = a})

-- | The private DNS name.
iniPrivateDNSName :: Lens' InstanceNetworkInterface (Maybe Text)
iniPrivateDNSName = lens _iniPrivateDNSName (\s a -> s {_iniPrivateDNSName = a})

-- | The description.
iniDescription :: Lens' InstanceNetworkInterface (Maybe Text)
iniDescription = lens _iniDescription (\s a -> s {_iniDescription = a})

-- | The association information for an Elastic IPv4 associated with the network interface.
iniAssociation :: Lens' InstanceNetworkInterface (Maybe InstanceNetworkInterfaceAssociation)
iniAssociation = lens _iniAssociation (\s a -> s {_iniAssociation = a})

-- | One or more IPv6 addresses associated with the network interface.
iniIPv6Addresses :: Lens' InstanceNetworkInterface [InstanceIPv6Address]
iniIPv6Addresses = lens _iniIPv6Addresses (\s a -> s {_iniIPv6Addresses = a}) . _Default . _Coerce

instance FromXML InstanceNetworkInterface where
  parseXML x =
    InstanceNetworkInterface'
      <$> (x .@? "groupSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "status")
      <*> ( x .@? "privateIpAddressesSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "sourceDestCheck")
      <*> (x .@? "interfaceType")
      <*> (x .@? "vpcId")
      <*> (x .@? "networkInterfaceId")
      <*> (x .@? "subnetId")
      <*> (x .@? "macAddress")
      <*> (x .@? "attachment")
      <*> (x .@? "ownerId")
      <*> (x .@? "privateIpAddress")
      <*> (x .@? "privateDnsName")
      <*> (x .@? "description")
      <*> (x .@? "association")
      <*> (x .@? "ipv6AddressesSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable InstanceNetworkInterface

instance NFData InstanceNetworkInterface
