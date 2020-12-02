{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ActivityTaskCanceledEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityTaskCanceledEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the details of the @ActivityTaskCanceled@ event.
--
--
--
-- /See:/ 'activityTaskCanceledEventAttributes' smart constructor.
data ActivityTaskCanceledEventAttributes = ActivityTaskCanceledEventAttributes'
  { _aLatestCancelRequestedEventId ::
      !(Maybe Integer),
    _aDetails ::
      !(Maybe Text),
    _aScheduledEventId ::
      !Integer,
    _aStartedEventId ::
      !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActivityTaskCanceledEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aLatestCancelRequestedEventId' - If set, contains the ID of the last @ActivityTaskCancelRequested@ event recorded for this activity task. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- * 'aDetails' - Details of the cancellation.
--
-- * 'aScheduledEventId' - The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- * 'aStartedEventId' - The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
activityTaskCanceledEventAttributes ::
  -- | 'aScheduledEventId'
  Integer ->
  -- | 'aStartedEventId'
  Integer ->
  ActivityTaskCanceledEventAttributes
activityTaskCanceledEventAttributes
  pScheduledEventId_
  pStartedEventId_ =
    ActivityTaskCanceledEventAttributes'
      { _aLatestCancelRequestedEventId =
          Nothing,
        _aDetails = Nothing,
        _aScheduledEventId = pScheduledEventId_,
        _aStartedEventId = pStartedEventId_
      }

-- | If set, contains the ID of the last @ActivityTaskCancelRequested@ event recorded for this activity task. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
aLatestCancelRequestedEventId :: Lens' ActivityTaskCanceledEventAttributes (Maybe Integer)
aLatestCancelRequestedEventId = lens _aLatestCancelRequestedEventId (\s a -> s {_aLatestCancelRequestedEventId = a})

-- | Details of the cancellation.
aDetails :: Lens' ActivityTaskCanceledEventAttributes (Maybe Text)
aDetails = lens _aDetails (\s a -> s {_aDetails = a})

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
aScheduledEventId :: Lens' ActivityTaskCanceledEventAttributes Integer
aScheduledEventId = lens _aScheduledEventId (\s a -> s {_aScheduledEventId = a})

-- | The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
aStartedEventId :: Lens' ActivityTaskCanceledEventAttributes Integer
aStartedEventId = lens _aStartedEventId (\s a -> s {_aStartedEventId = a})

instance FromJSON ActivityTaskCanceledEventAttributes where
  parseJSON =
    withObject
      "ActivityTaskCanceledEventAttributes"
      ( \x ->
          ActivityTaskCanceledEventAttributes'
            <$> (x .:? "latestCancelRequestedEventId")
            <*> (x .:? "details")
            <*> (x .: "scheduledEventId")
            <*> (x .: "startedEventId")
      )

instance Hashable ActivityTaskCanceledEventAttributes

instance NFData ActivityTaskCanceledEventAttributes
