{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.TimerStartedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.TimerStartedEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the details of the @TimerStarted@ event.
--
--
--
-- /See:/ 'timerStartedEventAttributes' smart constructor.
data TimerStartedEventAttributes = TimerStartedEventAttributes'
  { _tseaControl ::
      !(Maybe Text),
    _tseaTimerId :: !Text,
    _tseaStartToFireTimeout :: !Text,
    _tseaDecisionTaskCompletedEventId ::
      !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TimerStartedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tseaControl' - Data attached to the event that can be used by the decider in subsequent workflow tasks.
--
-- * 'tseaTimerId' - The unique ID of the timer that was started.
--
-- * 'tseaStartToFireTimeout' - The duration of time after which the timer fires. The duration is specified in seconds, an integer greater than or equal to @0@ .
--
-- * 'tseaDecisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @StartTimer@ decision for this activity task. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
timerStartedEventAttributes ::
  -- | 'tseaTimerId'
  Text ->
  -- | 'tseaStartToFireTimeout'
  Text ->
  -- | 'tseaDecisionTaskCompletedEventId'
  Integer ->
  TimerStartedEventAttributes
timerStartedEventAttributes
  pTimerId_
  pStartToFireTimeout_
  pDecisionTaskCompletedEventId_ =
    TimerStartedEventAttributes'
      { _tseaControl = Nothing,
        _tseaTimerId = pTimerId_,
        _tseaStartToFireTimeout = pStartToFireTimeout_,
        _tseaDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId_
      }

-- | Data attached to the event that can be used by the decider in subsequent workflow tasks.
tseaControl :: Lens' TimerStartedEventAttributes (Maybe Text)
tseaControl = lens _tseaControl (\s a -> s {_tseaControl = a})

-- | The unique ID of the timer that was started.
tseaTimerId :: Lens' TimerStartedEventAttributes Text
tseaTimerId = lens _tseaTimerId (\s a -> s {_tseaTimerId = a})

-- | The duration of time after which the timer fires. The duration is specified in seconds, an integer greater than or equal to @0@ .
tseaStartToFireTimeout :: Lens' TimerStartedEventAttributes Text
tseaStartToFireTimeout = lens _tseaStartToFireTimeout (\s a -> s {_tseaStartToFireTimeout = a})

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @StartTimer@ decision for this activity task. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
tseaDecisionTaskCompletedEventId :: Lens' TimerStartedEventAttributes Integer
tseaDecisionTaskCompletedEventId = lens _tseaDecisionTaskCompletedEventId (\s a -> s {_tseaDecisionTaskCompletedEventId = a})

instance FromJSON TimerStartedEventAttributes where
  parseJSON =
    withObject
      "TimerStartedEventAttributes"
      ( \x ->
          TimerStartedEventAttributes'
            <$> (x .:? "control")
            <*> (x .: "timerId")
            <*> (x .: "startToFireTimeout")
            <*> (x .: "decisionTaskCompletedEventId")
      )

instance Hashable TimerStartedEventAttributes

instance NFData TimerStartedEventAttributes
