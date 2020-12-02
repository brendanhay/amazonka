{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.TimerFiredEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.TimerFiredEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the details of the @TimerFired@ event.
--
--
--
-- /See:/ 'timerFiredEventAttributes' smart constructor.
data TimerFiredEventAttributes = TimerFiredEventAttributes'
  { _tfeaTimerId ::
      !Text,
    _tfeaStartedEventId :: !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TimerFiredEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tfeaTimerId' - The unique ID of the timer that fired.
--
-- * 'tfeaStartedEventId' - The ID of the @TimerStarted@ event that was recorded when this timer was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
timerFiredEventAttributes ::
  -- | 'tfeaTimerId'
  Text ->
  -- | 'tfeaStartedEventId'
  Integer ->
  TimerFiredEventAttributes
timerFiredEventAttributes pTimerId_ pStartedEventId_ =
  TimerFiredEventAttributes'
    { _tfeaTimerId = pTimerId_,
      _tfeaStartedEventId = pStartedEventId_
    }

-- | The unique ID of the timer that fired.
tfeaTimerId :: Lens' TimerFiredEventAttributes Text
tfeaTimerId = lens _tfeaTimerId (\s a -> s {_tfeaTimerId = a})

-- | The ID of the @TimerStarted@ event that was recorded when this timer was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
tfeaStartedEventId :: Lens' TimerFiredEventAttributes Integer
tfeaStartedEventId = lens _tfeaStartedEventId (\s a -> s {_tfeaStartedEventId = a})

instance FromJSON TimerFiredEventAttributes where
  parseJSON =
    withObject
      "TimerFiredEventAttributes"
      ( \x ->
          TimerFiredEventAttributes'
            <$> (x .: "timerId") <*> (x .: "startedEventId")
      )

instance Hashable TimerFiredEventAttributes

instance NFData TimerFiredEventAttributes
