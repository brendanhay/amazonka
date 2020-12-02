{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDeviceRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Settings for an input device.
--
-- /See:/ 'inputDeviceRequest' smart constructor.
newtype InputDeviceRequest = InputDeviceRequest'
  { _idrId ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputDeviceRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idrId' - The unique ID for the device.
inputDeviceRequest ::
  InputDeviceRequest
inputDeviceRequest = InputDeviceRequest' {_idrId = Nothing}

-- | The unique ID for the device.
idrId :: Lens' InputDeviceRequest (Maybe Text)
idrId = lens _idrId (\s a -> s {_idrId = a})

instance Hashable InputDeviceRequest

instance NFData InputDeviceRequest

instance ToJSON InputDeviceRequest where
  toJSON InputDeviceRequest' {..} =
    object (catMaybes [("id" .=) <$> _idrId])
