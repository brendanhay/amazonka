{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NatGatewayAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NatGatewayAddress where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the IP addresses and network interface associated with a NAT gateway.
--
--
--
-- /See:/ 'natGatewayAddress' smart constructor.
data NatGatewayAddress = NatGatewayAddress'
  { _ngaPrivateIP ::
      !(Maybe Text),
    _ngaAllocationId :: !(Maybe Text),
    _ngaNetworkInterfaceId :: !(Maybe Text),
    _ngaPublicIP :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NatGatewayAddress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ngaPrivateIP' - The private IP address associated with the Elastic IP address.
--
-- * 'ngaAllocationId' - The allocation ID of the Elastic IP address that's associated with the NAT gateway.
--
-- * 'ngaNetworkInterfaceId' - The ID of the network interface associated with the NAT gateway.
--
-- * 'ngaPublicIP' - The Elastic IP address associated with the NAT gateway.
natGatewayAddress ::
  NatGatewayAddress
natGatewayAddress =
  NatGatewayAddress'
    { _ngaPrivateIP = Nothing,
      _ngaAllocationId = Nothing,
      _ngaNetworkInterfaceId = Nothing,
      _ngaPublicIP = Nothing
    }

-- | The private IP address associated with the Elastic IP address.
ngaPrivateIP :: Lens' NatGatewayAddress (Maybe Text)
ngaPrivateIP = lens _ngaPrivateIP (\s a -> s {_ngaPrivateIP = a})

-- | The allocation ID of the Elastic IP address that's associated with the NAT gateway.
ngaAllocationId :: Lens' NatGatewayAddress (Maybe Text)
ngaAllocationId = lens _ngaAllocationId (\s a -> s {_ngaAllocationId = a})

-- | The ID of the network interface associated with the NAT gateway.
ngaNetworkInterfaceId :: Lens' NatGatewayAddress (Maybe Text)
ngaNetworkInterfaceId = lens _ngaNetworkInterfaceId (\s a -> s {_ngaNetworkInterfaceId = a})

-- | The Elastic IP address associated with the NAT gateway.
ngaPublicIP :: Lens' NatGatewayAddress (Maybe Text)
ngaPublicIP = lens _ngaPublicIP (\s a -> s {_ngaPublicIP = a})

instance FromXML NatGatewayAddress where
  parseXML x =
    NatGatewayAddress'
      <$> (x .@? "privateIp")
      <*> (x .@? "allocationId")
      <*> (x .@? "networkInterfaceId")
      <*> (x .@? "publicIp")

instance Hashable NatGatewayAddress

instance NFData NatGatewayAddress
