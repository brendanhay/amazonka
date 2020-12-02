{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.DeviceEvent where

import Network.AWS.AlexaBusiness.Types.DeviceEventType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The list of device events.
--
--
--
-- /See:/ 'deviceEvent' smart constructor.
data DeviceEvent = DeviceEvent'
  { _deValue :: !(Maybe Text),
    _deType :: !(Maybe DeviceEventType),
    _deTimestamp :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeviceEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deValue' - The value of the event.
--
-- * 'deType' - The type of device event.
--
-- * 'deTimestamp' - The time (in epoch) when the event occurred.
deviceEvent ::
  DeviceEvent
deviceEvent =
  DeviceEvent'
    { _deValue = Nothing,
      _deType = Nothing,
      _deTimestamp = Nothing
    }

-- | The value of the event.
deValue :: Lens' DeviceEvent (Maybe Text)
deValue = lens _deValue (\s a -> s {_deValue = a})

-- | The type of device event.
deType :: Lens' DeviceEvent (Maybe DeviceEventType)
deType = lens _deType (\s a -> s {_deType = a})

-- | The time (in epoch) when the event occurred.
deTimestamp :: Lens' DeviceEvent (Maybe UTCTime)
deTimestamp = lens _deTimestamp (\s a -> s {_deTimestamp = a}) . mapping _Time

instance FromJSON DeviceEvent where
  parseJSON =
    withObject
      "DeviceEvent"
      ( \x ->
          DeviceEvent'
            <$> (x .:? "Value") <*> (x .:? "Type") <*> (x .:? "Timestamp")
      )

instance Hashable DeviceEvent

instance NFData DeviceEvent
