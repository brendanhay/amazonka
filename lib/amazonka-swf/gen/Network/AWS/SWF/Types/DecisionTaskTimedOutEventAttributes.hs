{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.DecisionTaskTimedOutEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.DecisionTaskTimedOutEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.DecisionTaskTimeoutType

-- | Provides the details of the @DecisionTaskTimedOut@ event.
--
--
--
-- /See:/ 'decisionTaskTimedOutEventAttributes' smart constructor.
data DecisionTaskTimedOutEventAttributes = DecisionTaskTimedOutEventAttributes'
  { _dttoeaTimeoutType ::
      !DecisionTaskTimeoutType,
    _dttoeaScheduledEventId ::
      !Integer,
    _dttoeaStartedEventId ::
      !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DecisionTaskTimedOutEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dttoeaTimeoutType' - The type of timeout that expired before the decision task could be completed.
--
-- * 'dttoeaScheduledEventId' - The ID of the @DecisionTaskScheduled@ event that was recorded when this decision task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- * 'dttoeaStartedEventId' - The ID of the @DecisionTaskStarted@ event recorded when this decision task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
decisionTaskTimedOutEventAttributes ::
  -- | 'dttoeaTimeoutType'
  DecisionTaskTimeoutType ->
  -- | 'dttoeaScheduledEventId'
  Integer ->
  -- | 'dttoeaStartedEventId'
  Integer ->
  DecisionTaskTimedOutEventAttributes
decisionTaskTimedOutEventAttributes
  pTimeoutType_
  pScheduledEventId_
  pStartedEventId_ =
    DecisionTaskTimedOutEventAttributes'
      { _dttoeaTimeoutType =
          pTimeoutType_,
        _dttoeaScheduledEventId = pScheduledEventId_,
        _dttoeaStartedEventId = pStartedEventId_
      }

-- | The type of timeout that expired before the decision task could be completed.
dttoeaTimeoutType :: Lens' DecisionTaskTimedOutEventAttributes DecisionTaskTimeoutType
dttoeaTimeoutType = lens _dttoeaTimeoutType (\s a -> s {_dttoeaTimeoutType = a})

-- | The ID of the @DecisionTaskScheduled@ event that was recorded when this decision task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
dttoeaScheduledEventId :: Lens' DecisionTaskTimedOutEventAttributes Integer
dttoeaScheduledEventId = lens _dttoeaScheduledEventId (\s a -> s {_dttoeaScheduledEventId = a})

-- | The ID of the @DecisionTaskStarted@ event recorded when this decision task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
dttoeaStartedEventId :: Lens' DecisionTaskTimedOutEventAttributes Integer
dttoeaStartedEventId = lens _dttoeaStartedEventId (\s a -> s {_dttoeaStartedEventId = a})

instance FromJSON DecisionTaskTimedOutEventAttributes where
  parseJSON =
    withObject
      "DecisionTaskTimedOutEventAttributes"
      ( \x ->
          DecisionTaskTimedOutEventAttributes'
            <$> (x .: "timeoutType")
            <*> (x .: "scheduledEventId")
            <*> (x .: "startedEventId")
      )

instance Hashable DecisionTaskTimedOutEventAttributes

instance NFData DecisionTaskTimedOutEventAttributes
