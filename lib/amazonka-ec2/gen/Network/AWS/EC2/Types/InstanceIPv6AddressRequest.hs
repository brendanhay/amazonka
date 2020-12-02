{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceIPv6AddressRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceIPv6AddressRequest where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an IPv6 address.
--
--
--
-- /See:/ 'instanceIPv6AddressRequest' smart constructor.
newtype InstanceIPv6AddressRequest = InstanceIPv6AddressRequest'
  { _iiarIPv6Address ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceIPv6AddressRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiarIPv6Address' - The IPv6 address.
instanceIPv6AddressRequest ::
  InstanceIPv6AddressRequest
instanceIPv6AddressRequest =
  InstanceIPv6AddressRequest' {_iiarIPv6Address = Nothing}

-- | The IPv6 address.
iiarIPv6Address :: Lens' InstanceIPv6AddressRequest (Maybe Text)
iiarIPv6Address = lens _iiarIPv6Address (\s a -> s {_iiarIPv6Address = a})

instance Hashable InstanceIPv6AddressRequest

instance NFData InstanceIPv6AddressRequest

instance ToQuery InstanceIPv6AddressRequest where
  toQuery InstanceIPv6AddressRequest' {..} =
    mconcat ["Ipv6Address" =: _iiarIPv6Address]
