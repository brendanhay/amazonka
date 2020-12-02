{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfaceAssociation where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes association information for an Elastic IP address (IPv4 only), or a Carrier IP address (for a network interface which resides in a subnet in a Wavelength Zone).
--
--
--
-- /See:/ 'networkInterfaceAssociation' smart constructor.
data NetworkInterfaceAssociation = NetworkInterfaceAssociation'
  { _niaAssociationId ::
      !(Maybe Text),
    _niaPublicDNSName :: !(Maybe Text),
    _niaAllocationId :: !(Maybe Text),
    _niaCarrierIP :: !(Maybe Text),
    _niaIPOwnerId :: !(Maybe Text),
    _niaCustomerOwnedIP ::
      !(Maybe Text),
    _niaPublicIP :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NetworkInterfaceAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'niaAssociationId' - The association ID.
--
-- * 'niaPublicDNSName' - The public DNS name.
--
-- * 'niaAllocationId' - The allocation ID.
--
-- * 'niaCarrierIP' - The carrier IP address associated with the network interface. This option is only available when the network interface is in a subnet which is associated with a Wavelength Zone.
--
-- * 'niaIPOwnerId' - The ID of the Elastic IP address owner.
--
-- * 'niaCustomerOwnedIP' - The customer-owned IP address associated with the network interface.
--
-- * 'niaPublicIP' - The address of the Elastic IP address bound to the network interface.
networkInterfaceAssociation ::
  NetworkInterfaceAssociation
networkInterfaceAssociation =
  NetworkInterfaceAssociation'
    { _niaAssociationId = Nothing,
      _niaPublicDNSName = Nothing,
      _niaAllocationId = Nothing,
      _niaCarrierIP = Nothing,
      _niaIPOwnerId = Nothing,
      _niaCustomerOwnedIP = Nothing,
      _niaPublicIP = Nothing
    }

-- | The association ID.
niaAssociationId :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaAssociationId = lens _niaAssociationId (\s a -> s {_niaAssociationId = a})

-- | The public DNS name.
niaPublicDNSName :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaPublicDNSName = lens _niaPublicDNSName (\s a -> s {_niaPublicDNSName = a})

-- | The allocation ID.
niaAllocationId :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaAllocationId = lens _niaAllocationId (\s a -> s {_niaAllocationId = a})

-- | The carrier IP address associated with the network interface. This option is only available when the network interface is in a subnet which is associated with a Wavelength Zone.
niaCarrierIP :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaCarrierIP = lens _niaCarrierIP (\s a -> s {_niaCarrierIP = a})

-- | The ID of the Elastic IP address owner.
niaIPOwnerId :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaIPOwnerId = lens _niaIPOwnerId (\s a -> s {_niaIPOwnerId = a})

-- | The customer-owned IP address associated with the network interface.
niaCustomerOwnedIP :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaCustomerOwnedIP = lens _niaCustomerOwnedIP (\s a -> s {_niaCustomerOwnedIP = a})

-- | The address of the Elastic IP address bound to the network interface.
niaPublicIP :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaPublicIP = lens _niaPublicIP (\s a -> s {_niaPublicIP = a})

instance FromXML NetworkInterfaceAssociation where
  parseXML x =
    NetworkInterfaceAssociation'
      <$> (x .@? "associationId")
      <*> (x .@? "publicDnsName")
      <*> (x .@? "allocationId")
      <*> (x .@? "carrierIp")
      <*> (x .@? "ipOwnerId")
      <*> (x .@? "customerOwnedIp")
      <*> (x .@? "publicIp")

instance Hashable NetworkInterfaceAssociation

instance NFData NetworkInterfaceAssociation
