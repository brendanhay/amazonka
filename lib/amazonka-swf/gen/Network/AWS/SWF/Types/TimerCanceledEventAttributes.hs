{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.TimerCanceledEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.TimerCanceledEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the details of the @TimerCanceled@ event.
--
--
--
-- /See:/ 'timerCanceledEventAttributes' smart constructor.
data TimerCanceledEventAttributes = TimerCanceledEventAttributes'
  { _tceaTimerId ::
      !Text,
    _tceaStartedEventId :: !Integer,
    _tceaDecisionTaskCompletedEventId ::
      !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TimerCanceledEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tceaTimerId' - The unique ID of the timer that was canceled.
--
-- * 'tceaStartedEventId' - The ID of the @TimerStarted@ event that was recorded when this timer was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- * 'tceaDecisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @CancelTimer@ decision to cancel this timer. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
timerCanceledEventAttributes ::
  -- | 'tceaTimerId'
  Text ->
  -- | 'tceaStartedEventId'
  Integer ->
  -- | 'tceaDecisionTaskCompletedEventId'
  Integer ->
  TimerCanceledEventAttributes
timerCanceledEventAttributes
  pTimerId_
  pStartedEventId_
  pDecisionTaskCompletedEventId_ =
    TimerCanceledEventAttributes'
      { _tceaTimerId = pTimerId_,
        _tceaStartedEventId = pStartedEventId_,
        _tceaDecisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | The unique ID of the timer that was canceled.
tceaTimerId :: Lens' TimerCanceledEventAttributes Text
tceaTimerId = lens _tceaTimerId (\s a -> s {_tceaTimerId = a})

-- | The ID of the @TimerStarted@ event that was recorded when this timer was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
tceaStartedEventId :: Lens' TimerCanceledEventAttributes Integer
tceaStartedEventId = lens _tceaStartedEventId (\s a -> s {_tceaStartedEventId = a})

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @CancelTimer@ decision to cancel this timer. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
tceaDecisionTaskCompletedEventId :: Lens' TimerCanceledEventAttributes Integer
tceaDecisionTaskCompletedEventId = lens _tceaDecisionTaskCompletedEventId (\s a -> s {_tceaDecisionTaskCompletedEventId = a})

instance FromJSON TimerCanceledEventAttributes where
  parseJSON =
    withObject
      "TimerCanceledEventAttributes"
      ( \x ->
          TimerCanceledEventAttributes'
            <$> (x .: "timerId")
            <*> (x .: "startedEventId")
            <*> (x .: "decisionTaskCompletedEventId")
      )

instance Hashable TimerCanceledEventAttributes

instance NFData TimerCanceledEventAttributes
