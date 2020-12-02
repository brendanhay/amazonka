{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceStatusEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceStatusEvent where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.EventCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a scheduled event for an instance.
--
--
--
-- /See:/ 'instanceStatusEvent' smart constructor.
data InstanceStatusEvent = InstanceStatusEvent'
  { _iseNotBefore ::
      !(Maybe ISO8601),
    _iseCode :: !(Maybe EventCode),
    _iseInstanceEventId :: !(Maybe Text),
    _iseDescription :: !(Maybe Text),
    _iseNotBeforeDeadline :: !(Maybe ISO8601),
    _iseNotAfter :: !(Maybe ISO8601)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceStatusEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iseNotBefore' - The earliest scheduled start time for the event.
--
-- * 'iseCode' - The event code.
--
-- * 'iseInstanceEventId' - The ID of the event.
--
-- * 'iseDescription' - A description of the event. After a scheduled event is completed, it can still be described for up to a week. If the event has been completed, this description starts with the following text: [Completed].
--
-- * 'iseNotBeforeDeadline' - The deadline for starting the event.
--
-- * 'iseNotAfter' - The latest scheduled end time for the event.
instanceStatusEvent ::
  InstanceStatusEvent
instanceStatusEvent =
  InstanceStatusEvent'
    { _iseNotBefore = Nothing,
      _iseCode = Nothing,
      _iseInstanceEventId = Nothing,
      _iseDescription = Nothing,
      _iseNotBeforeDeadline = Nothing,
      _iseNotAfter = Nothing
    }

-- | The earliest scheduled start time for the event.
iseNotBefore :: Lens' InstanceStatusEvent (Maybe UTCTime)
iseNotBefore = lens _iseNotBefore (\s a -> s {_iseNotBefore = a}) . mapping _Time

-- | The event code.
iseCode :: Lens' InstanceStatusEvent (Maybe EventCode)
iseCode = lens _iseCode (\s a -> s {_iseCode = a})

-- | The ID of the event.
iseInstanceEventId :: Lens' InstanceStatusEvent (Maybe Text)
iseInstanceEventId = lens _iseInstanceEventId (\s a -> s {_iseInstanceEventId = a})

-- | A description of the event. After a scheduled event is completed, it can still be described for up to a week. If the event has been completed, this description starts with the following text: [Completed].
iseDescription :: Lens' InstanceStatusEvent (Maybe Text)
iseDescription = lens _iseDescription (\s a -> s {_iseDescription = a})

-- | The deadline for starting the event.
iseNotBeforeDeadline :: Lens' InstanceStatusEvent (Maybe UTCTime)
iseNotBeforeDeadline = lens _iseNotBeforeDeadline (\s a -> s {_iseNotBeforeDeadline = a}) . mapping _Time

-- | The latest scheduled end time for the event.
iseNotAfter :: Lens' InstanceStatusEvent (Maybe UTCTime)
iseNotAfter = lens _iseNotAfter (\s a -> s {_iseNotAfter = a}) . mapping _Time

instance FromXML InstanceStatusEvent where
  parseXML x =
    InstanceStatusEvent'
      <$> (x .@? "notBefore")
      <*> (x .@? "code")
      <*> (x .@? "instanceEventId")
      <*> (x .@? "description")
      <*> (x .@? "notBeforeDeadline")
      <*> (x .@? "notAfter")

instance Hashable InstanceStatusEvent

instance NFData InstanceStatusEvent
