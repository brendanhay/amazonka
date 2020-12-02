{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.DevicePoolCompatibilityResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.DevicePoolCompatibilityResult where

import Network.AWS.DeviceFarm.Types.Device
import Network.AWS.DeviceFarm.Types.IncompatibilityMessage
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a device pool compatibility result.
--
--
--
-- /See:/ 'devicePoolCompatibilityResult' smart constructor.
data DevicePoolCompatibilityResult = DevicePoolCompatibilityResult'
  { _dpcrDevice ::
      !(Maybe Device),
    _dpcrCompatible ::
      !(Maybe Bool),
    _dpcrIncompatibilityMessages ::
      !( Maybe
           [IncompatibilityMessage]
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DevicePoolCompatibilityResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpcrDevice' - The device (phone or tablet) to return information about.
--
-- * 'dpcrCompatible' - Whether the result was compatible with the device pool.
--
-- * 'dpcrIncompatibilityMessages' - Information about the compatibility.
devicePoolCompatibilityResult ::
  DevicePoolCompatibilityResult
devicePoolCompatibilityResult =
  DevicePoolCompatibilityResult'
    { _dpcrDevice = Nothing,
      _dpcrCompatible = Nothing,
      _dpcrIncompatibilityMessages = Nothing
    }

-- | The device (phone or tablet) to return information about.
dpcrDevice :: Lens' DevicePoolCompatibilityResult (Maybe Device)
dpcrDevice = lens _dpcrDevice (\s a -> s {_dpcrDevice = a})

-- | Whether the result was compatible with the device pool.
dpcrCompatible :: Lens' DevicePoolCompatibilityResult (Maybe Bool)
dpcrCompatible = lens _dpcrCompatible (\s a -> s {_dpcrCompatible = a})

-- | Information about the compatibility.
dpcrIncompatibilityMessages :: Lens' DevicePoolCompatibilityResult [IncompatibilityMessage]
dpcrIncompatibilityMessages = lens _dpcrIncompatibilityMessages (\s a -> s {_dpcrIncompatibilityMessages = a}) . _Default . _Coerce

instance FromJSON DevicePoolCompatibilityResult where
  parseJSON =
    withObject
      "DevicePoolCompatibilityResult"
      ( \x ->
          DevicePoolCompatibilityResult'
            <$> (x .:? "device")
            <*> (x .:? "compatible")
            <*> (x .:? "incompatibilityMessages" .!= mempty)
      )

instance Hashable DevicePoolCompatibilityResult

instance NFData DevicePoolCompatibilityResult
