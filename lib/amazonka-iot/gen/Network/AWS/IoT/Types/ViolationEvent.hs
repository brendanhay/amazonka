{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ViolationEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ViolationEvent where

import Network.AWS.IoT.Types.Behavior
import Network.AWS.IoT.Types.MetricValue
import Network.AWS.IoT.Types.ViolationEventType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a Device Defender security profile behavior violation.
--
--
--
-- /See:/ 'violationEvent' smart constructor.
data ViolationEvent = ViolationEvent'
  { _veViolationEventType ::
      !(Maybe ViolationEventType),
    _veViolationId :: !(Maybe Text),
    _veBehavior :: !(Maybe Behavior),
    _veMetricValue :: !(Maybe MetricValue),
    _veSecurityProfileName :: !(Maybe Text),
    _veViolationEventTime :: !(Maybe POSIX),
    _veThingName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ViolationEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'veViolationEventType' - The type of violation event.
--
-- * 'veViolationId' - The ID of the violation event.
--
-- * 'veBehavior' - The behavior which was violated.
--
-- * 'veMetricValue' - The value of the metric (the measurement).
--
-- * 'veSecurityProfileName' - The name of the security profile whose behavior was violated.
--
-- * 'veViolationEventTime' - The time the violation event occurred.
--
-- * 'veThingName' - The name of the thing responsible for the violation event.
violationEvent ::
  ViolationEvent
violationEvent =
  ViolationEvent'
    { _veViolationEventType = Nothing,
      _veViolationId = Nothing,
      _veBehavior = Nothing,
      _veMetricValue = Nothing,
      _veSecurityProfileName = Nothing,
      _veViolationEventTime = Nothing,
      _veThingName = Nothing
    }

-- | The type of violation event.
veViolationEventType :: Lens' ViolationEvent (Maybe ViolationEventType)
veViolationEventType = lens _veViolationEventType (\s a -> s {_veViolationEventType = a})

-- | The ID of the violation event.
veViolationId :: Lens' ViolationEvent (Maybe Text)
veViolationId = lens _veViolationId (\s a -> s {_veViolationId = a})

-- | The behavior which was violated.
veBehavior :: Lens' ViolationEvent (Maybe Behavior)
veBehavior = lens _veBehavior (\s a -> s {_veBehavior = a})

-- | The value of the metric (the measurement).
veMetricValue :: Lens' ViolationEvent (Maybe MetricValue)
veMetricValue = lens _veMetricValue (\s a -> s {_veMetricValue = a})

-- | The name of the security profile whose behavior was violated.
veSecurityProfileName :: Lens' ViolationEvent (Maybe Text)
veSecurityProfileName = lens _veSecurityProfileName (\s a -> s {_veSecurityProfileName = a})

-- | The time the violation event occurred.
veViolationEventTime :: Lens' ViolationEvent (Maybe UTCTime)
veViolationEventTime = lens _veViolationEventTime (\s a -> s {_veViolationEventTime = a}) . mapping _Time

-- | The name of the thing responsible for the violation event.
veThingName :: Lens' ViolationEvent (Maybe Text)
veThingName = lens _veThingName (\s a -> s {_veThingName = a})

instance FromJSON ViolationEvent where
  parseJSON =
    withObject
      "ViolationEvent"
      ( \x ->
          ViolationEvent'
            <$> (x .:? "violationEventType")
            <*> (x .:? "violationId")
            <*> (x .:? "behavior")
            <*> (x .:? "metricValue")
            <*> (x .:? "securityProfileName")
            <*> (x .:? "violationEventTime")
            <*> (x .:? "thingName")
      )

instance Hashable ViolationEvent

instance NFData ViolationEvent
