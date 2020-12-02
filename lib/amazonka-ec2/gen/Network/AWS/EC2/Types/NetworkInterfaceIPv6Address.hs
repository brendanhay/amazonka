{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceIPv6Address
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfaceIPv6Address where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an IPv6 address associated with a network interface.
--
--
--
-- /See:/ 'networkInterfaceIPv6Address' smart constructor.
newtype NetworkInterfaceIPv6Address = NetworkInterfaceIPv6Address'
  { _niiaIPv6Address ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NetworkInterfaceIPv6Address' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'niiaIPv6Address' - The IPv6 address.
networkInterfaceIPv6Address ::
  NetworkInterfaceIPv6Address
networkInterfaceIPv6Address =
  NetworkInterfaceIPv6Address' {_niiaIPv6Address = Nothing}

-- | The IPv6 address.
niiaIPv6Address :: Lens' NetworkInterfaceIPv6Address (Maybe Text)
niiaIPv6Address = lens _niiaIPv6Address (\s a -> s {_niiaIPv6Address = a})

instance FromXML NetworkInterfaceIPv6Address where
  parseXML x = NetworkInterfaceIPv6Address' <$> (x .@? "ipv6Address")

instance Hashable NetworkInterfaceIPv6Address

instance NFData NetworkInterfaceIPv6Address
