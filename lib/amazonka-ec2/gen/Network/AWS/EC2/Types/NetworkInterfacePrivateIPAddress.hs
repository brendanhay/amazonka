{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfacePrivateIPAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfacePrivateIPAddress where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.NetworkInterfaceAssociation
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the private IPv4 address of a network interface.
--
--
--
-- /See:/ 'networkInterfacePrivateIPAddress' smart constructor.
data NetworkInterfacePrivateIPAddress = NetworkInterfacePrivateIPAddress'
  { _nipiaPrimary ::
      !(Maybe Bool),
    _nipiaPrivateIPAddress ::
      !(Maybe Text),
    _nipiaPrivateDNSName ::
      !(Maybe Text),
    _nipiaAssociation ::
      !( Maybe
           NetworkInterfaceAssociation
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NetworkInterfacePrivateIPAddress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nipiaPrimary' - Indicates whether this IPv4 address is the primary private IPv4 address of the network interface.
--
-- * 'nipiaPrivateIPAddress' - The private IPv4 address.
--
-- * 'nipiaPrivateDNSName' - The private DNS name.
--
-- * 'nipiaAssociation' - The association information for an Elastic IP address (IPv4) associated with the network interface.
networkInterfacePrivateIPAddress ::
  NetworkInterfacePrivateIPAddress
networkInterfacePrivateIPAddress =
  NetworkInterfacePrivateIPAddress'
    { _nipiaPrimary = Nothing,
      _nipiaPrivateIPAddress = Nothing,
      _nipiaPrivateDNSName = Nothing,
      _nipiaAssociation = Nothing
    }

-- | Indicates whether this IPv4 address is the primary private IPv4 address of the network interface.
nipiaPrimary :: Lens' NetworkInterfacePrivateIPAddress (Maybe Bool)
nipiaPrimary = lens _nipiaPrimary (\s a -> s {_nipiaPrimary = a})

-- | The private IPv4 address.
nipiaPrivateIPAddress :: Lens' NetworkInterfacePrivateIPAddress (Maybe Text)
nipiaPrivateIPAddress = lens _nipiaPrivateIPAddress (\s a -> s {_nipiaPrivateIPAddress = a})

-- | The private DNS name.
nipiaPrivateDNSName :: Lens' NetworkInterfacePrivateIPAddress (Maybe Text)
nipiaPrivateDNSName = lens _nipiaPrivateDNSName (\s a -> s {_nipiaPrivateDNSName = a})

-- | The association information for an Elastic IP address (IPv4) associated with the network interface.
nipiaAssociation :: Lens' NetworkInterfacePrivateIPAddress (Maybe NetworkInterfaceAssociation)
nipiaAssociation = lens _nipiaAssociation (\s a -> s {_nipiaAssociation = a})

instance FromXML NetworkInterfacePrivateIPAddress where
  parseXML x =
    NetworkInterfacePrivateIPAddress'
      <$> (x .@? "primary")
      <*> (x .@? "privateIpAddress")
      <*> (x .@? "privateDnsName")
      <*> (x .@? "association")

instance Hashable NetworkInterfacePrivateIPAddress

instance NFData NetworkInterfacePrivateIPAddress
