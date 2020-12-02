{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDeviceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Settings for an input device.
--
-- /See:/ 'inputDeviceSettings' smart constructor.
newtype InputDeviceSettings = InputDeviceSettings'
  { _idssId ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputDeviceSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idssId' - The unique ID for the device.
inputDeviceSettings ::
  InputDeviceSettings
inputDeviceSettings = InputDeviceSettings' {_idssId = Nothing}

-- | The unique ID for the device.
idssId :: Lens' InputDeviceSettings (Maybe Text)
idssId = lens _idssId (\s a -> s {_idssId = a})

instance FromJSON InputDeviceSettings where
  parseJSON =
    withObject
      "InputDeviceSettings"
      (\x -> InputDeviceSettings' <$> (x .:? "id"))

instance Hashable InputDeviceSettings

instance NFData InputDeviceSettings

instance ToJSON InputDeviceSettings where
  toJSON InputDeviceSettings' {..} =
    object (catMaybes [("id" .=) <$> _idssId])
