{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ActivityTaskCompletedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityTaskCompletedEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the details of the @ActivityTaskCompleted@ event.
--
--
--
-- /See:/ 'activityTaskCompletedEventAttributes' smart constructor.
data ActivityTaskCompletedEventAttributes = ActivityTaskCompletedEventAttributes'
  { _atceaResult ::
      !(Maybe Text),
    _atceaScheduledEventId ::
      !Integer,
    _atceaStartedEventId ::
      !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActivityTaskCompletedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atceaResult' - The results of the activity task.
--
-- * 'atceaScheduledEventId' - The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- * 'atceaStartedEventId' - The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
activityTaskCompletedEventAttributes ::
  -- | 'atceaScheduledEventId'
  Integer ->
  -- | 'atceaStartedEventId'
  Integer ->
  ActivityTaskCompletedEventAttributes
activityTaskCompletedEventAttributes
  pScheduledEventId_
  pStartedEventId_ =
    ActivityTaskCompletedEventAttributes'
      { _atceaResult = Nothing,
        _atceaScheduledEventId = pScheduledEventId_,
        _atceaStartedEventId = pStartedEventId_
      }

-- | The results of the activity task.
atceaResult :: Lens' ActivityTaskCompletedEventAttributes (Maybe Text)
atceaResult = lens _atceaResult (\s a -> s {_atceaResult = a})

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
atceaScheduledEventId :: Lens' ActivityTaskCompletedEventAttributes Integer
atceaScheduledEventId = lens _atceaScheduledEventId (\s a -> s {_atceaScheduledEventId = a})

-- | The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
atceaStartedEventId :: Lens' ActivityTaskCompletedEventAttributes Integer
atceaStartedEventId = lens _atceaStartedEventId (\s a -> s {_atceaStartedEventId = a})

instance FromJSON ActivityTaskCompletedEventAttributes where
  parseJSON =
    withObject
      "ActivityTaskCompletedEventAttributes"
      ( \x ->
          ActivityTaskCompletedEventAttributes'
            <$> (x .:? "result")
            <*> (x .: "scheduledEventId")
            <*> (x .: "startedEventId")
      )

instance Hashable ActivityTaskCompletedEventAttributes

instance NFData ActivityTaskCompletedEventAttributes
