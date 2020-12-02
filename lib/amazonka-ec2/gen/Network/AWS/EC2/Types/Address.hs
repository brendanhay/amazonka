{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Address
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Address where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DomainType
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an Elastic IP address, or a carrier IP address.
--
--
--
-- /See:/ 'address' smart constructor.
data Address = Address'
  { _aAssociationId :: !(Maybe Text),
    _aInstanceId :: !(Maybe Text),
    _aNetworkInterfaceOwnerId :: !(Maybe Text),
    _aAllocationId :: !(Maybe Text),
    _aCarrierIP :: !(Maybe Text),
    _aNetworkBorderGroup :: !(Maybe Text),
    _aDomain :: !(Maybe DomainType),
    _aNetworkInterfaceId :: !(Maybe Text),
    _aPublicIPv4Pool :: !(Maybe Text),
    _aCustomerOwnedIPv4Pool :: !(Maybe Text),
    _aCustomerOwnedIP :: !(Maybe Text),
    _aPrivateIPAddress :: !(Maybe Text),
    _aPublicIP :: !(Maybe Text),
    _aTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Address' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aAssociationId' - The ID representing the association of the address with an instance in a VPC.
--
-- * 'aInstanceId' - The ID of the instance that the address is associated with (if any).
--
-- * 'aNetworkInterfaceOwnerId' - The ID of the AWS account that owns the network interface.
--
-- * 'aAllocationId' - The ID representing the allocation of the address for use with EC2-VPC.
--
-- * 'aCarrierIP' - The carrier IP address associated. This option is only available for network interfaces which reside in a subnet in a Wavelength Zone (for example an EC2 instance).
--
-- * 'aNetworkBorderGroup' - The name of the unique set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses.
--
-- * 'aDomain' - Indicates whether this Elastic IP address is for use with instances in EC2-Classic (@standard@ ) or instances in a VPC (@vpc@ ).
--
-- * 'aNetworkInterfaceId' - The ID of the network interface.
--
-- * 'aPublicIPv4Pool' - The ID of an address pool.
--
-- * 'aCustomerOwnedIPv4Pool' - The ID of the customer-owned address pool.
--
-- * 'aCustomerOwnedIP' - The customer-owned IP address.
--
-- * 'aPrivateIPAddress' - The private IP address associated with the Elastic IP address.
--
-- * 'aPublicIP' - The Elastic IP address.
--
-- * 'aTags' - Any tags assigned to the Elastic IP address.
address ::
  Address
address =
  Address'
    { _aAssociationId = Nothing,
      _aInstanceId = Nothing,
      _aNetworkInterfaceOwnerId = Nothing,
      _aAllocationId = Nothing,
      _aCarrierIP = Nothing,
      _aNetworkBorderGroup = Nothing,
      _aDomain = Nothing,
      _aNetworkInterfaceId = Nothing,
      _aPublicIPv4Pool = Nothing,
      _aCustomerOwnedIPv4Pool = Nothing,
      _aCustomerOwnedIP = Nothing,
      _aPrivateIPAddress = Nothing,
      _aPublicIP = Nothing,
      _aTags = Nothing
    }

-- | The ID representing the association of the address with an instance in a VPC.
aAssociationId :: Lens' Address (Maybe Text)
aAssociationId = lens _aAssociationId (\s a -> s {_aAssociationId = a})

-- | The ID of the instance that the address is associated with (if any).
aInstanceId :: Lens' Address (Maybe Text)
aInstanceId = lens _aInstanceId (\s a -> s {_aInstanceId = a})

-- | The ID of the AWS account that owns the network interface.
aNetworkInterfaceOwnerId :: Lens' Address (Maybe Text)
aNetworkInterfaceOwnerId = lens _aNetworkInterfaceOwnerId (\s a -> s {_aNetworkInterfaceOwnerId = a})

-- | The ID representing the allocation of the address for use with EC2-VPC.
aAllocationId :: Lens' Address (Maybe Text)
aAllocationId = lens _aAllocationId (\s a -> s {_aAllocationId = a})

-- | The carrier IP address associated. This option is only available for network interfaces which reside in a subnet in a Wavelength Zone (for example an EC2 instance).
aCarrierIP :: Lens' Address (Maybe Text)
aCarrierIP = lens _aCarrierIP (\s a -> s {_aCarrierIP = a})

-- | The name of the unique set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses.
aNetworkBorderGroup :: Lens' Address (Maybe Text)
aNetworkBorderGroup = lens _aNetworkBorderGroup (\s a -> s {_aNetworkBorderGroup = a})

-- | Indicates whether this Elastic IP address is for use with instances in EC2-Classic (@standard@ ) or instances in a VPC (@vpc@ ).
aDomain :: Lens' Address (Maybe DomainType)
aDomain = lens _aDomain (\s a -> s {_aDomain = a})

-- | The ID of the network interface.
aNetworkInterfaceId :: Lens' Address (Maybe Text)
aNetworkInterfaceId = lens _aNetworkInterfaceId (\s a -> s {_aNetworkInterfaceId = a})

-- | The ID of an address pool.
aPublicIPv4Pool :: Lens' Address (Maybe Text)
aPublicIPv4Pool = lens _aPublicIPv4Pool (\s a -> s {_aPublicIPv4Pool = a})

-- | The ID of the customer-owned address pool.
aCustomerOwnedIPv4Pool :: Lens' Address (Maybe Text)
aCustomerOwnedIPv4Pool = lens _aCustomerOwnedIPv4Pool (\s a -> s {_aCustomerOwnedIPv4Pool = a})

-- | The customer-owned IP address.
aCustomerOwnedIP :: Lens' Address (Maybe Text)
aCustomerOwnedIP = lens _aCustomerOwnedIP (\s a -> s {_aCustomerOwnedIP = a})

-- | The private IP address associated with the Elastic IP address.
aPrivateIPAddress :: Lens' Address (Maybe Text)
aPrivateIPAddress = lens _aPrivateIPAddress (\s a -> s {_aPrivateIPAddress = a})

-- | The Elastic IP address.
aPublicIP :: Lens' Address (Maybe Text)
aPublicIP = lens _aPublicIP (\s a -> s {_aPublicIP = a})

-- | Any tags assigned to the Elastic IP address.
aTags :: Lens' Address [Tag]
aTags = lens _aTags (\s a -> s {_aTags = a}) . _Default . _Coerce

instance FromXML Address where
  parseXML x =
    Address'
      <$> (x .@? "associationId")
      <*> (x .@? "instanceId")
      <*> (x .@? "networkInterfaceOwnerId")
      <*> (x .@? "allocationId")
      <*> (x .@? "carrierIp")
      <*> (x .@? "networkBorderGroup")
      <*> (x .@? "domain")
      <*> (x .@? "networkInterfaceId")
      <*> (x .@? "publicIpv4Pool")
      <*> (x .@? "customerOwnedIpv4Pool")
      <*> (x .@? "customerOwnedIp")
      <*> (x .@? "privateIpAddress")
      <*> (x .@? "publicIp")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable Address

instance NFData Address
