{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesIPv6Address
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstancesIPv6Address where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an IPv6 address.
--
--
--
-- /See:/ 'scheduledInstancesIPv6Address' smart constructor.
newtype ScheduledInstancesIPv6Address = ScheduledInstancesIPv6Address'
  { _siiaIPv6Address ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScheduledInstancesIPv6Address' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siiaIPv6Address' - The IPv6 address.
scheduledInstancesIPv6Address ::
  ScheduledInstancesIPv6Address
scheduledInstancesIPv6Address =
  ScheduledInstancesIPv6Address' {_siiaIPv6Address = Nothing}

-- | The IPv6 address.
siiaIPv6Address :: Lens' ScheduledInstancesIPv6Address (Maybe Text)
siiaIPv6Address = lens _siiaIPv6Address (\s a -> s {_siiaIPv6Address = a})

instance Hashable ScheduledInstancesIPv6Address

instance NFData ScheduledInstancesIPv6Address

instance ToQuery ScheduledInstancesIPv6Address where
  toQuery ScheduledInstancesIPv6Address' {..} =
    mconcat ["Ipv6Address" =: _siiaIPv6Address]
