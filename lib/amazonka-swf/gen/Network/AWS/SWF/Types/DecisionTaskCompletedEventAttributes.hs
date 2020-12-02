{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.DecisionTaskCompletedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.DecisionTaskCompletedEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the details of the @DecisionTaskCompleted@ event.
--
--
--
-- /See:/ 'decisionTaskCompletedEventAttributes' smart constructor.
data DecisionTaskCompletedEventAttributes = DecisionTaskCompletedEventAttributes'
  { _dtceaExecutionContext ::
      !(Maybe Text),
    _dtceaScheduledEventId ::
      !Integer,
    _dtceaStartedEventId ::
      !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DecisionTaskCompletedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtceaExecutionContext' - User defined context for the workflow execution.
--
-- * 'dtceaScheduledEventId' - The ID of the @DecisionTaskScheduled@ event that was recorded when this decision task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- * 'dtceaStartedEventId' - The ID of the @DecisionTaskStarted@ event recorded when this decision task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
decisionTaskCompletedEventAttributes ::
  -- | 'dtceaScheduledEventId'
  Integer ->
  -- | 'dtceaStartedEventId'
  Integer ->
  DecisionTaskCompletedEventAttributes
decisionTaskCompletedEventAttributes
  pScheduledEventId_
  pStartedEventId_ =
    DecisionTaskCompletedEventAttributes'
      { _dtceaExecutionContext =
          Nothing,
        _dtceaScheduledEventId = pScheduledEventId_,
        _dtceaStartedEventId = pStartedEventId_
      }

-- | User defined context for the workflow execution.
dtceaExecutionContext :: Lens' DecisionTaskCompletedEventAttributes (Maybe Text)
dtceaExecutionContext = lens _dtceaExecutionContext (\s a -> s {_dtceaExecutionContext = a})

-- | The ID of the @DecisionTaskScheduled@ event that was recorded when this decision task was scheduled. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
dtceaScheduledEventId :: Lens' DecisionTaskCompletedEventAttributes Integer
dtceaScheduledEventId = lens _dtceaScheduledEventId (\s a -> s {_dtceaScheduledEventId = a})

-- | The ID of the @DecisionTaskStarted@ event recorded when this decision task was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
dtceaStartedEventId :: Lens' DecisionTaskCompletedEventAttributes Integer
dtceaStartedEventId = lens _dtceaStartedEventId (\s a -> s {_dtceaStartedEventId = a})

instance FromJSON DecisionTaskCompletedEventAttributes where
  parseJSON =
    withObject
      "DecisionTaskCompletedEventAttributes"
      ( \x ->
          DecisionTaskCompletedEventAttributes'
            <$> (x .:? "executionContext")
            <*> (x .: "scheduledEventId")
            <*> (x .: "startedEventId")
      )

instance Hashable DecisionTaskCompletedEventAttributes

instance NFData DecisionTaskCompletedEventAttributes
