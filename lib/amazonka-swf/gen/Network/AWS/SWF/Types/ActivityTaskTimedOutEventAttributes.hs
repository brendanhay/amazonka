{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ActivityTaskTimedOutEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityTaskTimedOutEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.ActivityTaskTimeoutType

-- | Provides the details of the @ActivityTaskTimedOut@ event.
--
--
--
-- /See:/ 'activityTaskTimedOutEventAttributes' smart constructor.
data ActivityTaskTimedOutEventAttributes = ActivityTaskTimedOutEventAttributes'
  { _attoeaDetails ::
      !(Maybe Text),
    _attoeaTimeoutType ::
      !ActivityTaskTimeoutType,
    _attoeaScheduledEventId ::
      !Integer,
    _attoeaStartedEventId ::
      !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActivityTaskTimedOutEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'attoeaDetails' - Contains the content of the @details@ parameter for the last call made by the activity to @RecordActivityTaskHeartbeat@ .
--
-- * 'attoeaTimeoutType' - The type of the timeout that caused this event.
--
-- * 'attoeaScheduledEventId' - The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- * 'attoeaStartedEventId' - The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
activityTaskTimedOutEventAttributes ::
  -- | 'attoeaTimeoutType'
  ActivityTaskTimeoutType ->
  -- | 'attoeaScheduledEventId'
  Integer ->
  -- | 'attoeaStartedEventId'
  Integer ->
  ActivityTaskTimedOutEventAttributes
activityTaskTimedOutEventAttributes
  pTimeoutType_
  pScheduledEventId_
  pStartedEventId_ =
    ActivityTaskTimedOutEventAttributes'
      { _attoeaDetails = Nothing,
        _attoeaTimeoutType = pTimeoutType_,
        _attoeaScheduledEventId = pScheduledEventId_,
        _attoeaStartedEventId = pStartedEventId_
      }

-- | Contains the content of the @details@ parameter for the last call made by the activity to @RecordActivityTaskHeartbeat@ .
attoeaDetails :: Lens' ActivityTaskTimedOutEventAttributes (Maybe Text)
attoeaDetails = lens _attoeaDetails (\s a -> s {_attoeaDetails = a})

-- | The type of the timeout that caused this event.
attoeaTimeoutType :: Lens' ActivityTaskTimedOutEventAttributes ActivityTaskTimeoutType
attoeaTimeoutType = lens _attoeaTimeoutType (\s a -> s {_attoeaTimeoutType = a})

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
attoeaScheduledEventId :: Lens' ActivityTaskTimedOutEventAttributes Integer
attoeaScheduledEventId = lens _attoeaScheduledEventId (\s a -> s {_attoeaScheduledEventId = a})

-- | The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
attoeaStartedEventId :: Lens' ActivityTaskTimedOutEventAttributes Integer
attoeaStartedEventId = lens _attoeaStartedEventId (\s a -> s {_attoeaStartedEventId = a})

instance FromJSON ActivityTaskTimedOutEventAttributes where
  parseJSON =
    withObject
      "ActivityTaskTimedOutEventAttributes"
      ( \x ->
          ActivityTaskTimedOutEventAttributes'
            <$> (x .:? "details")
            <*> (x .: "timeoutType")
            <*> (x .: "scheduledEventId")
            <*> (x .: "startedEventId")
      )

instance Hashable ActivityTaskTimedOutEventAttributes

instance NFData ActivityTaskTimedOutEventAttributes
