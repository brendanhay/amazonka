{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.WirelessConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.WirelessConnection where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configures the wireless connection on an AWS Snowcone device.
--
--
--
-- /See:/ 'wirelessConnection' smart constructor.
newtype WirelessConnection = WirelessConnection'
  { _wcIsWifiEnabled ::
      Maybe Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WirelessConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wcIsWifiEnabled' - Enables the Wi-Fi adapter on an AWS Snowcone device.
wirelessConnection ::
  WirelessConnection
wirelessConnection =
  WirelessConnection' {_wcIsWifiEnabled = Nothing}

-- | Enables the Wi-Fi adapter on an AWS Snowcone device.
wcIsWifiEnabled :: Lens' WirelessConnection (Maybe Bool)
wcIsWifiEnabled = lens _wcIsWifiEnabled (\s a -> s {_wcIsWifiEnabled = a})

instance FromJSON WirelessConnection where
  parseJSON =
    withObject
      "WirelessConnection"
      (\x -> WirelessConnection' <$> (x .:? "IsWifiEnabled"))

instance Hashable WirelessConnection

instance NFData WirelessConnection

instance ToJSON WirelessConnection where
  toJSON WirelessConnection' {..} =
    object (catMaybes [("IsWifiEnabled" .=) <$> _wcIsWifiEnabled])
