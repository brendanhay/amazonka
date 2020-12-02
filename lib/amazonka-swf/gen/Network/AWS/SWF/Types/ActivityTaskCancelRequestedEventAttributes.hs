{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ActivityTaskCancelRequestedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityTaskCancelRequestedEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the details of the @ActivityTaskCancelRequested@ event.
--
--
--
-- /See:/ 'activityTaskCancelRequestedEventAttributes' smart constructor.
data ActivityTaskCancelRequestedEventAttributes = ActivityTaskCancelRequestedEventAttributes'
  { _atcreaDecisionTaskCompletedEventId ::
      !Integer,
    _atcreaActivityId ::
      !Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ActivityTaskCancelRequestedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atcreaDecisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @RequestCancelActivityTask@ decision for this cancellation request. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- * 'atcreaActivityId' - The unique ID of the task.
activityTaskCancelRequestedEventAttributes ::
  -- | 'atcreaDecisionTaskCompletedEventId'
  Integer ->
  -- | 'atcreaActivityId'
  Text ->
  ActivityTaskCancelRequestedEventAttributes
activityTaskCancelRequestedEventAttributes
  pDecisionTaskCompletedEventId_
  pActivityId_ =
    ActivityTaskCancelRequestedEventAttributes'
      { _atcreaDecisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_,
        _atcreaActivityId = pActivityId_
      }

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @RequestCancelActivityTask@ decision for this cancellation request. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
atcreaDecisionTaskCompletedEventId :: Lens' ActivityTaskCancelRequestedEventAttributes Integer
atcreaDecisionTaskCompletedEventId = lens _atcreaDecisionTaskCompletedEventId (\s a -> s {_atcreaDecisionTaskCompletedEventId = a})

-- | The unique ID of the task.
atcreaActivityId :: Lens' ActivityTaskCancelRequestedEventAttributes Text
atcreaActivityId = lens _atcreaActivityId (\s a -> s {_atcreaActivityId = a})

instance FromJSON ActivityTaskCancelRequestedEventAttributes where
  parseJSON =
    withObject
      "ActivityTaskCancelRequestedEventAttributes"
      ( \x ->
          ActivityTaskCancelRequestedEventAttributes'
            <$> (x .: "decisionTaskCompletedEventId") <*> (x .: "activityId")
      )

instance Hashable ActivityTaskCancelRequestedEventAttributes

instance NFData ActivityTaskCancelRequestedEventAttributes
