{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceIPv6Address
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceIPv6Address where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an IPv6 address.
--
--
--
-- /See:/ 'instanceIPv6Address' smart constructor.
newtype InstanceIPv6Address = InstanceIPv6Address'
  { _iiaIPv6Address ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceIPv6Address' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiaIPv6Address' - The IPv6 address.
instanceIPv6Address ::
  InstanceIPv6Address
instanceIPv6Address =
  InstanceIPv6Address' {_iiaIPv6Address = Nothing}

-- | The IPv6 address.
iiaIPv6Address :: Lens' InstanceIPv6Address (Maybe Text)
iiaIPv6Address = lens _iiaIPv6Address (\s a -> s {_iiaIPv6Address = a})

instance FromXML InstanceIPv6Address where
  parseXML x = InstanceIPv6Address' <$> (x .@? "ipv6Address")

instance Hashable InstanceIPv6Address

instance NFData InstanceIPv6Address

instance ToQuery InstanceIPv6Address where
  toQuery InstanceIPv6Address' {..} =
    mconcat ["Ipv6Address" =: _iiaIPv6Address]
