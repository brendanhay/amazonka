{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionCanceledEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionCanceledEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the details of the @WorkflowExecutionCanceled@ event.
--
--
--
-- /See:/ 'workflowExecutionCanceledEventAttributes' smart constructor.
data WorkflowExecutionCanceledEventAttributes = WorkflowExecutionCanceledEventAttributes'
  { _wDetails ::
      !( Maybe
           Text
       ),
    _wDecisionTaskCompletedEventId ::
      !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkflowExecutionCanceledEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wDetails' - The details of the cancellation.
--
-- * 'wDecisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @CancelWorkflowExecution@ decision for this cancellation request. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
workflowExecutionCanceledEventAttributes ::
  -- | 'wDecisionTaskCompletedEventId'
  Integer ->
  WorkflowExecutionCanceledEventAttributes
workflowExecutionCanceledEventAttributes
  pDecisionTaskCompletedEventId_ =
    WorkflowExecutionCanceledEventAttributes'
      { _wDetails = Nothing,
        _wDecisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | The details of the cancellation.
wDetails :: Lens' WorkflowExecutionCanceledEventAttributes (Maybe Text)
wDetails = lens _wDetails (\s a -> s {_wDetails = a})

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @CancelWorkflowExecution@ decision for this cancellation request. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
wDecisionTaskCompletedEventId :: Lens' WorkflowExecutionCanceledEventAttributes Integer
wDecisionTaskCompletedEventId = lens _wDecisionTaskCompletedEventId (\s a -> s {_wDecisionTaskCompletedEventId = a})

instance FromJSON WorkflowExecutionCanceledEventAttributes where
  parseJSON =
    withObject
      "WorkflowExecutionCanceledEventAttributes"
      ( \x ->
          WorkflowExecutionCanceledEventAttributes'
            <$> (x .:? "details") <*> (x .: "decisionTaskCompletedEventId")
      )

instance Hashable WorkflowExecutionCanceledEventAttributes

instance NFData WorkflowExecutionCanceledEventAttributes
