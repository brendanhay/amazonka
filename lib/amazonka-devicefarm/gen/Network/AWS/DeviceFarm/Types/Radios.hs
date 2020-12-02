{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Radios
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Radios where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the set of radios and their states on a device. Examples of radios include Wi-Fi, GPS, Bluetooth, and NFC.
--
--
--
-- /See:/ 'radios' smart constructor.
data Radios = Radios'
  { _rNfc :: !(Maybe Bool),
    _rGps :: !(Maybe Bool),
    _rBluetooth :: !(Maybe Bool),
    _rWifi :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Radios' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rNfc' - True if NFC is enabled at the beginning of the test. Otherwise, false.
--
-- * 'rGps' - True if GPS is enabled at the beginning of the test. Otherwise, false.
--
-- * 'rBluetooth' - True if Bluetooth is enabled at the beginning of the test. Otherwise, false.
--
-- * 'rWifi' - True if Wi-Fi is enabled at the beginning of the test. Otherwise, false.
radios ::
  Radios
radios =
  Radios'
    { _rNfc = Nothing,
      _rGps = Nothing,
      _rBluetooth = Nothing,
      _rWifi = Nothing
    }

-- | True if NFC is enabled at the beginning of the test. Otherwise, false.
rNfc :: Lens' Radios (Maybe Bool)
rNfc = lens _rNfc (\s a -> s {_rNfc = a})

-- | True if GPS is enabled at the beginning of the test. Otherwise, false.
rGps :: Lens' Radios (Maybe Bool)
rGps = lens _rGps (\s a -> s {_rGps = a})

-- | True if Bluetooth is enabled at the beginning of the test. Otherwise, false.
rBluetooth :: Lens' Radios (Maybe Bool)
rBluetooth = lens _rBluetooth (\s a -> s {_rBluetooth = a})

-- | True if Wi-Fi is enabled at the beginning of the test. Otherwise, false.
rWifi :: Lens' Radios (Maybe Bool)
rWifi = lens _rWifi (\s a -> s {_rWifi = a})

instance FromJSON Radios where
  parseJSON =
    withObject
      "Radios"
      ( \x ->
          Radios'
            <$> (x .:? "nfc")
            <*> (x .:? "gps")
            <*> (x .:? "bluetooth")
            <*> (x .:? "wifi")
      )

instance Hashable Radios

instance NFData Radios

instance ToJSON Radios where
  toJSON Radios' {..} =
    object
      ( catMaybes
          [ ("nfc" .=) <$> _rNfc,
            ("gps" .=) <$> _rGps,
            ("bluetooth" .=) <$> _rBluetooth,
            ("wifi" .=) <$> _rWifi
          ]
      )
