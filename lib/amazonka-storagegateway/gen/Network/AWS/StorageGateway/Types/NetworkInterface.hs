{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.NetworkInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.NetworkInterface where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a gateway's network interface.
--
--
--
-- /See:/ 'networkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { _niIPv6Address ::
      !(Maybe Text),
    _niMACAddress :: !(Maybe Text),
    _niIPv4Address :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NetworkInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'niIPv6Address' - The Internet Protocol version 6 (IPv6) address of the interface. /Currently not supported/ .
--
-- * 'niMACAddress' - The Media Access Control (MAC) address of the interface.
--
-- * 'niIPv4Address' - The Internet Protocol version 4 (IPv4) address of the interface.
networkInterface ::
  NetworkInterface
networkInterface =
  NetworkInterface'
    { _niIPv6Address = Nothing,
      _niMACAddress = Nothing,
      _niIPv4Address = Nothing
    }

-- | The Internet Protocol version 6 (IPv6) address of the interface. /Currently not supported/ .
niIPv6Address :: Lens' NetworkInterface (Maybe Text)
niIPv6Address = lens _niIPv6Address (\s a -> s {_niIPv6Address = a})

-- | The Media Access Control (MAC) address of the interface.
niMACAddress :: Lens' NetworkInterface (Maybe Text)
niMACAddress = lens _niMACAddress (\s a -> s {_niMACAddress = a})

-- | The Internet Protocol version 4 (IPv4) address of the interface.
niIPv4Address :: Lens' NetworkInterface (Maybe Text)
niIPv4Address = lens _niIPv4Address (\s a -> s {_niIPv4Address = a})

instance FromJSON NetworkInterface where
  parseJSON =
    withObject
      "NetworkInterface"
      ( \x ->
          NetworkInterface'
            <$> (x .:? "Ipv6Address")
            <*> (x .:? "MacAddress")
            <*> (x .:? "Ipv4Address")
      )

instance Hashable NetworkInterface

instance NFData NetworkInterface
