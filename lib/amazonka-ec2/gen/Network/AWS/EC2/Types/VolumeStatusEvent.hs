{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeStatusEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeStatusEvent where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a volume status event.
--
--
--
-- /See:/ 'volumeStatusEvent' smart constructor.
data VolumeStatusEvent = VolumeStatusEvent'
  { _vseInstanceId ::
      !(Maybe Text),
    _vseNotBefore :: !(Maybe ISO8601),
    _vseEventType :: !(Maybe Text),
    _vseDescription :: !(Maybe Text),
    _vseNotAfter :: !(Maybe ISO8601),
    _vseEventId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VolumeStatusEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vseInstanceId' - The ID of the instance associated with the event.
--
-- * 'vseNotBefore' - The earliest start time of the event.
--
-- * 'vseEventType' - The type of this event.
--
-- * 'vseDescription' - A description of the event.
--
-- * 'vseNotAfter' - The latest end time of the event.
--
-- * 'vseEventId' - The ID of this event.
volumeStatusEvent ::
  VolumeStatusEvent
volumeStatusEvent =
  VolumeStatusEvent'
    { _vseInstanceId = Nothing,
      _vseNotBefore = Nothing,
      _vseEventType = Nothing,
      _vseDescription = Nothing,
      _vseNotAfter = Nothing,
      _vseEventId = Nothing
    }

-- | The ID of the instance associated with the event.
vseInstanceId :: Lens' VolumeStatusEvent (Maybe Text)
vseInstanceId = lens _vseInstanceId (\s a -> s {_vseInstanceId = a})

-- | The earliest start time of the event.
vseNotBefore :: Lens' VolumeStatusEvent (Maybe UTCTime)
vseNotBefore = lens _vseNotBefore (\s a -> s {_vseNotBefore = a}) . mapping _Time

-- | The type of this event.
vseEventType :: Lens' VolumeStatusEvent (Maybe Text)
vseEventType = lens _vseEventType (\s a -> s {_vseEventType = a})

-- | A description of the event.
vseDescription :: Lens' VolumeStatusEvent (Maybe Text)
vseDescription = lens _vseDescription (\s a -> s {_vseDescription = a})

-- | The latest end time of the event.
vseNotAfter :: Lens' VolumeStatusEvent (Maybe UTCTime)
vseNotAfter = lens _vseNotAfter (\s a -> s {_vseNotAfter = a}) . mapping _Time

-- | The ID of this event.
vseEventId :: Lens' VolumeStatusEvent (Maybe Text)
vseEventId = lens _vseEventId (\s a -> s {_vseEventId = a})

instance FromXML VolumeStatusEvent where
  parseXML x =
    VolumeStatusEvent'
      <$> (x .@? "instanceId")
      <*> (x .@? "notBefore")
      <*> (x .@? "eventType")
      <*> (x .@? "description")
      <*> (x .@? "notAfter")
      <*> (x .@? "eventId")

instance Hashable VolumeStatusEvent

instance NFData VolumeStatusEvent
