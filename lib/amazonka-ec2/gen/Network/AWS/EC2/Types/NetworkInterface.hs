{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterface where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.GroupIdentifier
import Network.AWS.EC2.Types.NetworkInterfaceAssociation
import Network.AWS.EC2.Types.NetworkInterfaceAttachment
import Network.AWS.EC2.Types.NetworkInterfaceIPv6Address
import Network.AWS.EC2.Types.NetworkInterfacePrivateIPAddress
import Network.AWS.EC2.Types.NetworkInterfaceStatus
import Network.AWS.EC2.Types.NetworkInterfaceType
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a network interface.
--
--
--
-- /See:/ 'networkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { _niGroups ::
      !(Maybe [GroupIdentifier]),
    _niStatus :: !(Maybe NetworkInterfaceStatus),
    _niPrivateIPAddresses ::
      !(Maybe [NetworkInterfacePrivateIPAddress]),
    _niSourceDestCheck :: !(Maybe Bool),
    _niInterfaceType :: !(Maybe NetworkInterfaceType),
    _niVPCId :: !(Maybe Text),
    _niTagSet :: !(Maybe [Tag]),
    _niRequesterManaged :: !(Maybe Bool),
    _niOutpostARN :: !(Maybe Text),
    _niNetworkInterfaceId :: !(Maybe Text),
    _niSubnetId :: !(Maybe Text),
    _niMACAddress :: !(Maybe Text),
    _niAttachment :: !(Maybe NetworkInterfaceAttachment),
    _niOwnerId :: !(Maybe Text),
    _niAvailabilityZone :: !(Maybe Text),
    _niPrivateIPAddress :: !(Maybe Text),
    _niPrivateDNSName :: !(Maybe Text),
    _niRequesterId :: !(Maybe Text),
    _niDescription :: !(Maybe Text),
    _niAssociation :: !(Maybe NetworkInterfaceAssociation),
    _niIPv6Addresses ::
      !(Maybe [NetworkInterfaceIPv6Address])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NetworkInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'niGroups' - Any security groups for the network interface.
--
-- * 'niStatus' - The status of the network interface.
--
-- * 'niPrivateIPAddresses' - The private IPv4 addresses associated with the network interface.
--
-- * 'niSourceDestCheck' - Indicates whether traffic to or from the instance is validated.
--
-- * 'niInterfaceType' - The type of network interface.
--
-- * 'niVPCId' - The ID of the VPC.
--
-- * 'niTagSet' - Any tags assigned to the network interface.
--
-- * 'niRequesterManaged' - Indicates whether the network interface is being managed by AWS.
--
-- * 'niOutpostARN' - The Amazon Resource Name (ARN) of the Outpost.
--
-- * 'niNetworkInterfaceId' - The ID of the network interface.
--
-- * 'niSubnetId' - The ID of the subnet.
--
-- * 'niMACAddress' - The MAC address.
--
-- * 'niAttachment' - The network interface attachment.
--
-- * 'niOwnerId' - The AWS account ID of the owner of the network interface.
--
-- * 'niAvailabilityZone' - The Availability Zone.
--
-- * 'niPrivateIPAddress' - The IPv4 address of the network interface within the subnet.
--
-- * 'niPrivateDNSName' - The private DNS name.
--
-- * 'niRequesterId' - The ID of the entity that launched the instance on your behalf (for example, AWS Management Console or Auto Scaling).
--
-- * 'niDescription' - A description.
--
-- * 'niAssociation' - The association information for an Elastic IP address (IPv4) associated with the network interface.
--
-- * 'niIPv6Addresses' - The IPv6 addresses associated with the network interface.
networkInterface ::
  NetworkInterface
networkInterface =
  NetworkInterface'
    { _niGroups = Nothing,
      _niStatus = Nothing,
      _niPrivateIPAddresses = Nothing,
      _niSourceDestCheck = Nothing,
      _niInterfaceType = Nothing,
      _niVPCId = Nothing,
      _niTagSet = Nothing,
      _niRequesterManaged = Nothing,
      _niOutpostARN = Nothing,
      _niNetworkInterfaceId = Nothing,
      _niSubnetId = Nothing,
      _niMACAddress = Nothing,
      _niAttachment = Nothing,
      _niOwnerId = Nothing,
      _niAvailabilityZone = Nothing,
      _niPrivateIPAddress = Nothing,
      _niPrivateDNSName = Nothing,
      _niRequesterId = Nothing,
      _niDescription = Nothing,
      _niAssociation = Nothing,
      _niIPv6Addresses = Nothing
    }

-- | Any security groups for the network interface.
niGroups :: Lens' NetworkInterface [GroupIdentifier]
niGroups = lens _niGroups (\s a -> s {_niGroups = a}) . _Default . _Coerce

-- | The status of the network interface.
niStatus :: Lens' NetworkInterface (Maybe NetworkInterfaceStatus)
niStatus = lens _niStatus (\s a -> s {_niStatus = a})

-- | The private IPv4 addresses associated with the network interface.
niPrivateIPAddresses :: Lens' NetworkInterface [NetworkInterfacePrivateIPAddress]
niPrivateIPAddresses = lens _niPrivateIPAddresses (\s a -> s {_niPrivateIPAddresses = a}) . _Default . _Coerce

-- | Indicates whether traffic to or from the instance is validated.
niSourceDestCheck :: Lens' NetworkInterface (Maybe Bool)
niSourceDestCheck = lens _niSourceDestCheck (\s a -> s {_niSourceDestCheck = a})

-- | The type of network interface.
niInterfaceType :: Lens' NetworkInterface (Maybe NetworkInterfaceType)
niInterfaceType = lens _niInterfaceType (\s a -> s {_niInterfaceType = a})

-- | The ID of the VPC.
niVPCId :: Lens' NetworkInterface (Maybe Text)
niVPCId = lens _niVPCId (\s a -> s {_niVPCId = a})

-- | Any tags assigned to the network interface.
niTagSet :: Lens' NetworkInterface [Tag]
niTagSet = lens _niTagSet (\s a -> s {_niTagSet = a}) . _Default . _Coerce

-- | Indicates whether the network interface is being managed by AWS.
niRequesterManaged :: Lens' NetworkInterface (Maybe Bool)
niRequesterManaged = lens _niRequesterManaged (\s a -> s {_niRequesterManaged = a})

-- | The Amazon Resource Name (ARN) of the Outpost.
niOutpostARN :: Lens' NetworkInterface (Maybe Text)
niOutpostARN = lens _niOutpostARN (\s a -> s {_niOutpostARN = a})

-- | The ID of the network interface.
niNetworkInterfaceId :: Lens' NetworkInterface (Maybe Text)
niNetworkInterfaceId = lens _niNetworkInterfaceId (\s a -> s {_niNetworkInterfaceId = a})

-- | The ID of the subnet.
niSubnetId :: Lens' NetworkInterface (Maybe Text)
niSubnetId = lens _niSubnetId (\s a -> s {_niSubnetId = a})

-- | The MAC address.
niMACAddress :: Lens' NetworkInterface (Maybe Text)
niMACAddress = lens _niMACAddress (\s a -> s {_niMACAddress = a})

-- | The network interface attachment.
niAttachment :: Lens' NetworkInterface (Maybe NetworkInterfaceAttachment)
niAttachment = lens _niAttachment (\s a -> s {_niAttachment = a})

-- | The AWS account ID of the owner of the network interface.
niOwnerId :: Lens' NetworkInterface (Maybe Text)
niOwnerId = lens _niOwnerId (\s a -> s {_niOwnerId = a})

-- | The Availability Zone.
niAvailabilityZone :: Lens' NetworkInterface (Maybe Text)
niAvailabilityZone = lens _niAvailabilityZone (\s a -> s {_niAvailabilityZone = a})

-- | The IPv4 address of the network interface within the subnet.
niPrivateIPAddress :: Lens' NetworkInterface (Maybe Text)
niPrivateIPAddress = lens _niPrivateIPAddress (\s a -> s {_niPrivateIPAddress = a})

-- | The private DNS name.
niPrivateDNSName :: Lens' NetworkInterface (Maybe Text)
niPrivateDNSName = lens _niPrivateDNSName (\s a -> s {_niPrivateDNSName = a})

-- | The ID of the entity that launched the instance on your behalf (for example, AWS Management Console or Auto Scaling).
niRequesterId :: Lens' NetworkInterface (Maybe Text)
niRequesterId = lens _niRequesterId (\s a -> s {_niRequesterId = a})

-- | A description.
niDescription :: Lens' NetworkInterface (Maybe Text)
niDescription = lens _niDescription (\s a -> s {_niDescription = a})

-- | The association information for an Elastic IP address (IPv4) associated with the network interface.
niAssociation :: Lens' NetworkInterface (Maybe NetworkInterfaceAssociation)
niAssociation = lens _niAssociation (\s a -> s {_niAssociation = a})

-- | The IPv6 addresses associated with the network interface.
niIPv6Addresses :: Lens' NetworkInterface [NetworkInterfaceIPv6Address]
niIPv6Addresses = lens _niIPv6Addresses (\s a -> s {_niIPv6Addresses = a}) . _Default . _Coerce

instance FromXML NetworkInterface where
  parseXML x =
    NetworkInterface'
      <$> (x .@? "groupSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "status")
      <*> ( x .@? "privateIpAddressesSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "sourceDestCheck")
      <*> (x .@? "interfaceType")
      <*> (x .@? "vpcId")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "requesterManaged")
      <*> (x .@? "outpostArn")
      <*> (x .@? "networkInterfaceId")
      <*> (x .@? "subnetId")
      <*> (x .@? "macAddress")
      <*> (x .@? "attachment")
      <*> (x .@? "ownerId")
      <*> (x .@? "availabilityZone")
      <*> (x .@? "privateIpAddress")
      <*> (x .@? "privateDnsName")
      <*> (x .@? "requesterId")
      <*> (x .@? "description")
      <*> (x .@? "association")
      <*> (x .@? "ipv6AddressesSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable NetworkInterface

instance NFData NetworkInterface
