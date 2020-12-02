{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesPrivateIPAddressConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstancesPrivateIPAddressConfig where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a private IPv4 address for a Scheduled Instance.
--
--
--
-- /See:/ 'scheduledInstancesPrivateIPAddressConfig' smart constructor.
data ScheduledInstancesPrivateIPAddressConfig = ScheduledInstancesPrivateIPAddressConfig'
  { _sipiacPrimary ::
      !( Maybe
           Bool
       ),
    _sipiacPrivateIPAddress ::
      !( Maybe
           Text
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScheduledInstancesPrivateIPAddressConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sipiacPrimary' - Indicates whether this is a primary IPv4 address. Otherwise, this is a secondary IPv4 address.
--
-- * 'sipiacPrivateIPAddress' - The IPv4 address.
scheduledInstancesPrivateIPAddressConfig ::
  ScheduledInstancesPrivateIPAddressConfig
scheduledInstancesPrivateIPAddressConfig =
  ScheduledInstancesPrivateIPAddressConfig'
    { _sipiacPrimary =
        Nothing,
      _sipiacPrivateIPAddress = Nothing
    }

-- | Indicates whether this is a primary IPv4 address. Otherwise, this is a secondary IPv4 address.
sipiacPrimary :: Lens' ScheduledInstancesPrivateIPAddressConfig (Maybe Bool)
sipiacPrimary = lens _sipiacPrimary (\s a -> s {_sipiacPrimary = a})

-- | The IPv4 address.
sipiacPrivateIPAddress :: Lens' ScheduledInstancesPrivateIPAddressConfig (Maybe Text)
sipiacPrivateIPAddress = lens _sipiacPrivateIPAddress (\s a -> s {_sipiacPrivateIPAddress = a})

instance Hashable ScheduledInstancesPrivateIPAddressConfig

instance NFData ScheduledInstancesPrivateIPAddressConfig

instance ToQuery ScheduledInstancesPrivateIPAddressConfig where
  toQuery ScheduledInstancesPrivateIPAddressConfig' {..} =
    mconcat
      [ "Primary" =: _sipiacPrimary,
        "PrivateIpAddress" =: _sipiacPrivateIPAddress
      ]
