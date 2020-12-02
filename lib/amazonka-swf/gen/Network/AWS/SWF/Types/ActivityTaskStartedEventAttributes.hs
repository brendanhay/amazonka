{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ActivityTaskStartedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityTaskStartedEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the details of the @ActivityTaskStarted@ event.
--
--
--
-- /See:/ 'activityTaskStartedEventAttributes' smart constructor.
data ActivityTaskStartedEventAttributes = ActivityTaskStartedEventAttributes'
  { _atseaIdentity ::
      !(Maybe Text),
    _atseaScheduledEventId ::
      !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActivityTaskStartedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atseaIdentity' - Identity of the worker that was assigned this task. This aids diagnostics when problems arise. The form of this identity is user defined.
--
-- * 'atseaScheduledEventId' - The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
activityTaskStartedEventAttributes ::
  -- | 'atseaScheduledEventId'
  Integer ->
  ActivityTaskStartedEventAttributes
activityTaskStartedEventAttributes pScheduledEventId_ =
  ActivityTaskStartedEventAttributes'
    { _atseaIdentity = Nothing,
      _atseaScheduledEventId = pScheduledEventId_
    }

-- | Identity of the worker that was assigned this task. This aids diagnostics when problems arise. The form of this identity is user defined.
atseaIdentity :: Lens' ActivityTaskStartedEventAttributes (Maybe Text)
atseaIdentity = lens _atseaIdentity (\s a -> s {_atseaIdentity = a})

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
atseaScheduledEventId :: Lens' ActivityTaskStartedEventAttributes Integer
atseaScheduledEventId = lens _atseaScheduledEventId (\s a -> s {_atseaScheduledEventId = a})

instance FromJSON ActivityTaskStartedEventAttributes where
  parseJSON =
    withObject
      "ActivityTaskStartedEventAttributes"
      ( \x ->
          ActivityTaskStartedEventAttributes'
            <$> (x .:? "identity") <*> (x .: "scheduledEventId")
      )

instance Hashable ActivityTaskStartedEventAttributes

instance NFData ActivityTaskStartedEventAttributes
