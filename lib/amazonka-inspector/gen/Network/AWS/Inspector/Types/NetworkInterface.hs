{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.NetworkInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.NetworkInterface where

import Network.AWS.Inspector.Types.PrivateIP
import Network.AWS.Inspector.Types.SecurityGroup
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the network interfaces interacting with an EC2 instance. This data type is used as one of the elements of the 'AssetAttributes' data type.
--
--
--
-- /See:/ 'networkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { _niPrivateIPAddresses ::
      !(Maybe [PrivateIP]),
    _niPublicDNSName :: !(Maybe Text),
    _niSecurityGroups :: !(Maybe [SecurityGroup]),
    _niVpcId :: !(Maybe Text),
    _niSubnetId :: !(Maybe Text),
    _niNetworkInterfaceId :: !(Maybe Text),
    _niPrivateIPAddress :: !(Maybe Text),
    _niPublicIP :: !(Maybe Text),
    _niPrivateDNSName :: !(Maybe Text),
    _niIpv6Addresses :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NetworkInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'niPrivateIPAddresses' - A list of the private IP addresses associated with the network interface. Includes the privateDnsName and privateIpAddress.
--
-- * 'niPublicDNSName' - The name of a public DNS associated with the network interface.
--
-- * 'niSecurityGroups' - A list of the security groups associated with the network interface. Includes the groupId and groupName.
--
-- * 'niVpcId' - The ID of a VPC associated with the network interface.
--
-- * 'niSubnetId' - The ID of a subnet associated with the network interface.
--
-- * 'niNetworkInterfaceId' - The ID of the network interface.
--
-- * 'niPrivateIPAddress' - The private IP address associated with the network interface.
--
-- * 'niPublicIP' - The public IP address from which the network interface is reachable.
--
-- * 'niPrivateDNSName' - The name of a private DNS associated with the network interface.
--
-- * 'niIpv6Addresses' - The IP addresses associated with the network interface.
networkInterface ::
  NetworkInterface
networkInterface =
  NetworkInterface'
    { _niPrivateIPAddresses = Nothing,
      _niPublicDNSName = Nothing,
      _niSecurityGroups = Nothing,
      _niVpcId = Nothing,
      _niSubnetId = Nothing,
      _niNetworkInterfaceId = Nothing,
      _niPrivateIPAddress = Nothing,
      _niPublicIP = Nothing,
      _niPrivateDNSName = Nothing,
      _niIpv6Addresses = Nothing
    }

-- | A list of the private IP addresses associated with the network interface. Includes the privateDnsName and privateIpAddress.
niPrivateIPAddresses :: Lens' NetworkInterface [PrivateIP]
niPrivateIPAddresses = lens _niPrivateIPAddresses (\s a -> s {_niPrivateIPAddresses = a}) . _Default . _Coerce

-- | The name of a public DNS associated with the network interface.
niPublicDNSName :: Lens' NetworkInterface (Maybe Text)
niPublicDNSName = lens _niPublicDNSName (\s a -> s {_niPublicDNSName = a})

-- | A list of the security groups associated with the network interface. Includes the groupId and groupName.
niSecurityGroups :: Lens' NetworkInterface [SecurityGroup]
niSecurityGroups = lens _niSecurityGroups (\s a -> s {_niSecurityGroups = a}) . _Default . _Coerce

-- | The ID of a VPC associated with the network interface.
niVpcId :: Lens' NetworkInterface (Maybe Text)
niVpcId = lens _niVpcId (\s a -> s {_niVpcId = a})

-- | The ID of a subnet associated with the network interface.
niSubnetId :: Lens' NetworkInterface (Maybe Text)
niSubnetId = lens _niSubnetId (\s a -> s {_niSubnetId = a})

-- | The ID of the network interface.
niNetworkInterfaceId :: Lens' NetworkInterface (Maybe Text)
niNetworkInterfaceId = lens _niNetworkInterfaceId (\s a -> s {_niNetworkInterfaceId = a})

-- | The private IP address associated with the network interface.
niPrivateIPAddress :: Lens' NetworkInterface (Maybe Text)
niPrivateIPAddress = lens _niPrivateIPAddress (\s a -> s {_niPrivateIPAddress = a})

-- | The public IP address from which the network interface is reachable.
niPublicIP :: Lens' NetworkInterface (Maybe Text)
niPublicIP = lens _niPublicIP (\s a -> s {_niPublicIP = a})

-- | The name of a private DNS associated with the network interface.
niPrivateDNSName :: Lens' NetworkInterface (Maybe Text)
niPrivateDNSName = lens _niPrivateDNSName (\s a -> s {_niPrivateDNSName = a})

-- | The IP addresses associated with the network interface.
niIpv6Addresses :: Lens' NetworkInterface [Text]
niIpv6Addresses = lens _niIpv6Addresses (\s a -> s {_niIpv6Addresses = a}) . _Default . _Coerce

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
            <*> (x .:? "subnetId")
            <*> (x .:? "networkInterfaceId")
            <*> (x .:? "privateIpAddress")
            <*> (x .:? "publicIp")
            <*> (x .:? "privateDnsName")
            <*> (x .:? "ipv6Addresses" .!= mempty)
      )

instance Hashable NetworkInterface

instance NFData NetworkInterface
