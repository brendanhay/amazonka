{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Subnet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Subnet where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.SubnetIPv6CidrBlockAssociation
import Network.AWS.EC2.Types.SubnetState
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a subnet.
--
--
--
-- /See:/ 'subnet' smart constructor.
data Subnet = Subnet'
  { _subIPv6CidrBlockAssociationSet ::
      !(Maybe [SubnetIPv6CidrBlockAssociation]),
    _subAvailabilityZoneId :: !(Maybe Text),
    _subOutpostARN :: !(Maybe Text),
    _subAssignIPv6AddressOnCreation :: !(Maybe Bool),
    _subSubnetARN :: !(Maybe Text),
    _subOwnerId :: !(Maybe Text),
    _subCustomerOwnedIPv4Pool :: !(Maybe Text),
    _subMapCustomerOwnedIPOnLaunch :: !(Maybe Bool),
    _subMapPublicIPOnLaunch :: !(Maybe Bool),
    _subDefaultForAz :: !(Maybe Bool),
    _subTags :: !(Maybe [Tag]),
    _subAvailabilityZone :: !Text,
    _subAvailableIPAddressCount :: !Int,
    _subCidrBlock :: !Text,
    _subState :: !SubnetState,
    _subSubnetId :: !Text,
    _subVPCId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Subnet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'subIPv6CidrBlockAssociationSet' - Information about the IPv6 CIDR blocks associated with the subnet.
--
-- * 'subAvailabilityZoneId' - The AZ ID of the subnet.
--
-- * 'subOutpostARN' - The Amazon Resource Name (ARN) of the Outpost.
--
-- * 'subAssignIPv6AddressOnCreation' - Indicates whether a network interface created in this subnet (including a network interface created by 'RunInstances' ) receives an IPv6 address.
--
-- * 'subSubnetARN' - The Amazon Resource Name (ARN) of the subnet.
--
-- * 'subOwnerId' - The ID of the AWS account that owns the subnet.
--
-- * 'subCustomerOwnedIPv4Pool' - The customer-owned IPv4 address pool associated with the subnet.
--
-- * 'subMapCustomerOwnedIPOnLaunch' - Indicates whether a network interface created in this subnet (including a network interface created by 'RunInstances' ) receives a customer-owned IPv4 address.
--
-- * 'subMapPublicIPOnLaunch' - Indicates whether instances launched in this subnet receive a public IPv4 address.
--
-- * 'subDefaultForAz' - Indicates whether this is the default subnet for the Availability Zone.
--
-- * 'subTags' - Any tags assigned to the subnet.
--
-- * 'subAvailabilityZone' - The Availability Zone of the subnet.
--
-- * 'subAvailableIPAddressCount' - The number of unused private IPv4 addresses in the subnet. The IPv4 addresses for any stopped instances are considered unavailable.
--
-- * 'subCidrBlock' - The IPv4 CIDR block assigned to the subnet.
--
-- * 'subState' - The current state of the subnet.
--
-- * 'subSubnetId' - The ID of the subnet.
--
-- * 'subVPCId' - The ID of the VPC the subnet is in.
subnet ::
  -- | 'subAvailabilityZone'
  Text ->
  -- | 'subAvailableIPAddressCount'
  Int ->
  -- | 'subCidrBlock'
  Text ->
  -- | 'subState'
  SubnetState ->
  -- | 'subSubnetId'
  Text ->
  -- | 'subVPCId'
  Text ->
  Subnet
subnet
  pAvailabilityZone_
  pAvailableIPAddressCount_
  pCidrBlock_
  pState_
  pSubnetId_
  pVPCId_ =
    Subnet'
      { _subIPv6CidrBlockAssociationSet = Nothing,
        _subAvailabilityZoneId = Nothing,
        _subOutpostARN = Nothing,
        _subAssignIPv6AddressOnCreation = Nothing,
        _subSubnetARN = Nothing,
        _subOwnerId = Nothing,
        _subCustomerOwnedIPv4Pool = Nothing,
        _subMapCustomerOwnedIPOnLaunch = Nothing,
        _subMapPublicIPOnLaunch = Nothing,
        _subDefaultForAz = Nothing,
        _subTags = Nothing,
        _subAvailabilityZone = pAvailabilityZone_,
        _subAvailableIPAddressCount = pAvailableIPAddressCount_,
        _subCidrBlock = pCidrBlock_,
        _subState = pState_,
        _subSubnetId = pSubnetId_,
        _subVPCId = pVPCId_
      }

-- | Information about the IPv6 CIDR blocks associated with the subnet.
subIPv6CidrBlockAssociationSet :: Lens' Subnet [SubnetIPv6CidrBlockAssociation]
subIPv6CidrBlockAssociationSet = lens _subIPv6CidrBlockAssociationSet (\s a -> s {_subIPv6CidrBlockAssociationSet = a}) . _Default . _Coerce

-- | The AZ ID of the subnet.
subAvailabilityZoneId :: Lens' Subnet (Maybe Text)
subAvailabilityZoneId = lens _subAvailabilityZoneId (\s a -> s {_subAvailabilityZoneId = a})

-- | The Amazon Resource Name (ARN) of the Outpost.
subOutpostARN :: Lens' Subnet (Maybe Text)
subOutpostARN = lens _subOutpostARN (\s a -> s {_subOutpostARN = a})

-- | Indicates whether a network interface created in this subnet (including a network interface created by 'RunInstances' ) receives an IPv6 address.
subAssignIPv6AddressOnCreation :: Lens' Subnet (Maybe Bool)
subAssignIPv6AddressOnCreation = lens _subAssignIPv6AddressOnCreation (\s a -> s {_subAssignIPv6AddressOnCreation = a})

-- | The Amazon Resource Name (ARN) of the subnet.
subSubnetARN :: Lens' Subnet (Maybe Text)
subSubnetARN = lens _subSubnetARN (\s a -> s {_subSubnetARN = a})

-- | The ID of the AWS account that owns the subnet.
subOwnerId :: Lens' Subnet (Maybe Text)
subOwnerId = lens _subOwnerId (\s a -> s {_subOwnerId = a})

-- | The customer-owned IPv4 address pool associated with the subnet.
subCustomerOwnedIPv4Pool :: Lens' Subnet (Maybe Text)
subCustomerOwnedIPv4Pool = lens _subCustomerOwnedIPv4Pool (\s a -> s {_subCustomerOwnedIPv4Pool = a})

-- | Indicates whether a network interface created in this subnet (including a network interface created by 'RunInstances' ) receives a customer-owned IPv4 address.
subMapCustomerOwnedIPOnLaunch :: Lens' Subnet (Maybe Bool)
subMapCustomerOwnedIPOnLaunch = lens _subMapCustomerOwnedIPOnLaunch (\s a -> s {_subMapCustomerOwnedIPOnLaunch = a})

-- | Indicates whether instances launched in this subnet receive a public IPv4 address.
subMapPublicIPOnLaunch :: Lens' Subnet (Maybe Bool)
subMapPublicIPOnLaunch = lens _subMapPublicIPOnLaunch (\s a -> s {_subMapPublicIPOnLaunch = a})

-- | Indicates whether this is the default subnet for the Availability Zone.
subDefaultForAz :: Lens' Subnet (Maybe Bool)
subDefaultForAz = lens _subDefaultForAz (\s a -> s {_subDefaultForAz = a})

-- | Any tags assigned to the subnet.
subTags :: Lens' Subnet [Tag]
subTags = lens _subTags (\s a -> s {_subTags = a}) . _Default . _Coerce

-- | The Availability Zone of the subnet.
subAvailabilityZone :: Lens' Subnet Text
subAvailabilityZone = lens _subAvailabilityZone (\s a -> s {_subAvailabilityZone = a})

-- | The number of unused private IPv4 addresses in the subnet. The IPv4 addresses for any stopped instances are considered unavailable.
subAvailableIPAddressCount :: Lens' Subnet Int
subAvailableIPAddressCount = lens _subAvailableIPAddressCount (\s a -> s {_subAvailableIPAddressCount = a})

-- | The IPv4 CIDR block assigned to the subnet.
subCidrBlock :: Lens' Subnet Text
subCidrBlock = lens _subCidrBlock (\s a -> s {_subCidrBlock = a})

-- | The current state of the subnet.
subState :: Lens' Subnet SubnetState
subState = lens _subState (\s a -> s {_subState = a})

-- | The ID of the subnet.
subSubnetId :: Lens' Subnet Text
subSubnetId = lens _subSubnetId (\s a -> s {_subSubnetId = a})

-- | The ID of the VPC the subnet is in.
subVPCId :: Lens' Subnet Text
subVPCId = lens _subVPCId (\s a -> s {_subVPCId = a})

instance FromXML Subnet where
  parseXML x =
    Subnet'
      <$> ( x .@? "ipv6CidrBlockAssociationSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "availabilityZoneId")
      <*> (x .@? "outpostArn")
      <*> (x .@? "assignIpv6AddressOnCreation")
      <*> (x .@? "subnetArn")
      <*> (x .@? "ownerId")
      <*> (x .@? "customerOwnedIpv4Pool")
      <*> (x .@? "mapCustomerOwnedIpOnLaunch")
      <*> (x .@? "mapPublicIpOnLaunch")
      <*> (x .@? "defaultForAz")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@ "availabilityZone")
      <*> (x .@ "availableIpAddressCount")
      <*> (x .@ "cidrBlock")
      <*> (x .@ "state")
      <*> (x .@ "subnetId")
      <*> (x .@ "vpcId")

instance Hashable Subnet

instance NFData Subnet
