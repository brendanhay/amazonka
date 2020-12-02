{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ActivityTaskFailedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityTaskFailedEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the details of the @ActivityTaskFailed@ event.
--
--
--
-- /See:/ 'activityTaskFailedEventAttributes' smart constructor.
data ActivityTaskFailedEventAttributes = ActivityTaskFailedEventAttributes'
  { _atfeaReason ::
      !(Maybe Text),
    _atfeaDetails ::
      !(Maybe Text),
    _atfeaScheduledEventId ::
      !Integer,
    _atfeaStartedEventId ::
      !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActivityTaskFailedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atfeaReason' - The reason provided for the failure.
--
-- * 'atfeaDetails' - The details of the failure.
--
-- * 'atfeaScheduledEventId' - The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- * 'atfeaStartedEventId' - The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
activityTaskFailedEventAttributes ::
  -- | 'atfeaScheduledEventId'
  Integer ->
  -- | 'atfeaStartedEventId'
  Integer ->
  ActivityTaskFailedEventAttributes
activityTaskFailedEventAttributes
  pScheduledEventId_
  pStartedEventId_ =
    ActivityTaskFailedEventAttributes'
      { _atfeaReason = Nothing,
        _atfeaDetails = Nothing,
        _atfeaScheduledEventId = pScheduledEventId_,
        _atfeaStartedEventId = pStartedEventId_
      }

-- | The reason provided for the failure.
atfeaReason :: Lens' ActivityTaskFailedEventAttributes (Maybe Text)
atfeaReason = lens _atfeaReason (\s a -> s {_atfeaReason = a})

-- | The details of the failure.
atfeaDetails :: Lens' ActivityTaskFailedEventAttributes (Maybe Text)
atfeaDetails = lens _atfeaDetails (\s a -> s {_atfeaDetails = a})

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
atfeaScheduledEventId :: Lens' ActivityTaskFailedEventAttributes Integer
atfeaScheduledEventId = lens _atfeaScheduledEventId (\s a -> s {_atfeaScheduledEventId = a})

-- | The ID of the @ActivityTaskStarted@ event recorded when this activity task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
atfeaStartedEventId :: Lens' ActivityTaskFailedEventAttributes Integer
atfeaStartedEventId = lens _atfeaStartedEventId (\s a -> s {_atfeaStartedEventId = a})

instance FromJSON ActivityTaskFailedEventAttributes where
  parseJSON =
    withObject
      "ActivityTaskFailedEventAttributes"
      ( \x ->
          ActivityTaskFailedEventAttributes'
            <$> (x .:? "reason")
            <*> (x .:? "details")
            <*> (x .: "scheduledEventId")
            <*> (x .: "startedEventId")
      )

instance Hashable ActivityTaskFailedEventAttributes

instance NFData ActivityTaskFailedEventAttributes
