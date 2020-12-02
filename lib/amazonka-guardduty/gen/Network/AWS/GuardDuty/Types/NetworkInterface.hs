{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.NetworkInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.NetworkInterface where

import Network.AWS.GuardDuty.Types.PrivateIPAddressDetails
import Network.AWS.GuardDuty.Types.SecurityGroup
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the elastic network interface of the EC2 instance.
--
--
--
-- /See:/ 'networkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { _niPrivateIPAddresses ::
      !(Maybe [PrivateIPAddressDetails]),
    _niPublicDNSName :: !(Maybe Text),
    _niSecurityGroups :: !(Maybe [SecurityGroup]),
    _niVPCId :: !(Maybe Text),
    _niNetworkInterfaceId :: !(Maybe Text),
    _niSubnetId :: !(Maybe Text),
    _niPrivateIPAddress :: !(Maybe Text),
    _niPublicIP :: !(Maybe Text),
    _niPrivateDNSName :: !(Maybe Text),
    _niIPv6Addresses :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NetworkInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'niPrivateIPAddresses' - Other private IP address information of the EC2 instance.
--
-- * 'niPublicDNSName' - The public DNS name of the EC2 instance.
--
-- * 'niSecurityGroups' - The security groups associated with the EC2 instance.
--
-- * 'niVPCId' - The VPC ID of the EC2 instance.
--
-- * 'niNetworkInterfaceId' - The ID of the network interface.
--
-- * 'niSubnetId' - The subnet ID of the EC2 instance.
--
-- * 'niPrivateIPAddress' - The private IP address of the EC2 instance.
--
-- * 'niPublicIP' - The public IP address of the EC2 instance.
--
-- * 'niPrivateDNSName' - The private DNS name of the EC2 instance.
--
-- * 'niIPv6Addresses' - A list of IPv6 addresses for the EC2 instance.
networkInterface ::
  NetworkInterface
networkInterface =
  NetworkInterface'
    { _niPrivateIPAddresses = Nothing,
      _niPublicDNSName = Nothing,
      _niSecurityGroups = Nothing,
      _niVPCId = Nothing,
      _niNetworkInterfaceId = Nothing,
      _niSubnetId = Nothing,
      _niPrivateIPAddress = Nothing,
      _niPublicIP = Nothing,
      _niPrivateDNSName = Nothing,
      _niIPv6Addresses = Nothing
    }

-- | Other private IP address information of the EC2 instance.
niPrivateIPAddresses :: Lens' NetworkInterface [PrivateIPAddressDetails]
niPrivateIPAddresses = lens _niPrivateIPAddresses (\s a -> s {_niPrivateIPAddresses = a}) . _Default . _Coerce

-- | The public DNS name of the EC2 instance.
niPublicDNSName :: Lens' NetworkInterface (Maybe Text)
niPublicDNSName = lens _niPublicDNSName (\s a -> s {_niPublicDNSName = a})

-- | The security groups associated with the EC2 instance.
niSecurityGroups :: Lens' NetworkInterface [SecurityGroup]
niSecurityGroups = lens _niSecurityGroups (\s a -> s {_niSecurityGroups = a}) . _Default . _Coerce

-- | The VPC ID of the EC2 instance.
niVPCId :: Lens' NetworkInterface (Maybe Text)
niVPCId = lens _niVPCId (\s a -> s {_niVPCId = a})

-- | The ID of the network interface.
niNetworkInterfaceId :: Lens' NetworkInterface (Maybe Text)
niNetworkInterfaceId = lens _niNetworkInterfaceId (\s a -> s {_niNetworkInterfaceId = a})

-- | The subnet ID of the EC2 instance.
niSubnetId :: Lens' NetworkInterface (Maybe Text)
niSubnetId = lens _niSubnetId (\s a -> s {_niSubnetId = a})

-- | The private IP address of the EC2 instance.
niPrivateIPAddress :: Lens' NetworkInterface (Maybe Text)
niPrivateIPAddress = lens _niPrivateIPAddress (\s a -> s {_niPrivateIPAddress = a})

-- | The public IP address of the EC2 instance.
niPublicIP :: Lens' NetworkInterface (Maybe Text)
niPublicIP = lens _niPublicIP (\s a -> s {_niPublicIP = a})

-- | The private DNS name of the EC2 instance.
niPrivateDNSName :: Lens' NetworkInterface (Maybe Text)
niPrivateDNSName = lens _niPrivateDNSName (\s a -> s {_niPrivateDNSName = a})

-- | A list of IPv6 addresses for the EC2 instance.
niIPv6Addresses :: Lens' NetworkInterface [Text]
niIPv6Addresses = lens _niIPv6Addresses (\s a -> s {_niIPv6Addresses = a}) . _Default . _Coerce

instance FromJSON NetworkInterface where
  parseJSON =
    withObject
      "NetworkInterface"
      ( \x ->
          NetworkInterface'
            <$> (x .:? "privateIpAddresses" .!= mempty)
            <*> (x .:? "publicDnsName")
            <*> (x .:? "securityGroups" .!= mempty)
            <*> (x .:? "vpcId")
            <*> (x .:? "networkInterfaceId")
            <*> (x .:? "subnetId")
            <*> (x .:? "privateIpAddress")
            <*> (x .:? "publicIp")
            <*> (x .:? "privateDnsName")
            <*> (x .:? "ipv6Addresses" .!= mempty)
      )

instance Hashable NetworkInterface

instance NFData NetworkInterface
