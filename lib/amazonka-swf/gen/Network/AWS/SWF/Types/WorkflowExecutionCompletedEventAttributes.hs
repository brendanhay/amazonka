{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionCompletedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionCompletedEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the details of the @WorkflowExecutionCompleted@ event.
--
--
--
-- /See:/ 'workflowExecutionCompletedEventAttributes' smart constructor.
data WorkflowExecutionCompletedEventAttributes = WorkflowExecutionCompletedEventAttributes'
  { _weceaResult ::
      !( Maybe
           Text
       ),
    _weceaDecisionTaskCompletedEventId ::
      !Integer
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'WorkflowExecutionCompletedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'weceaResult' - The result produced by the workflow execution upon successful completion.
--
-- * 'weceaDecisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @CompleteWorkflowExecution@ decision to complete this execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
workflowExecutionCompletedEventAttributes ::
  -- | 'weceaDecisionTaskCompletedEventId'
  Integer ->
  WorkflowExecutionCompletedEventAttributes
workflowExecutionCompletedEventAttributes
  pDecisionTaskCompletedEventId_ =
    WorkflowExecutionCompletedEventAttributes'
      { _weceaResult =
          Nothing,
        _weceaDecisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | The result produced by the workflow execution upon successful completion.
weceaResult :: Lens' WorkflowExecutionCompletedEventAttributes (Maybe Text)
weceaResult = lens _weceaResult (\s a -> s {_weceaResult = a})

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @CompleteWorkflowExecution@ decision to complete this execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
weceaDecisionTaskCompletedEventId :: Lens' WorkflowExecutionCompletedEventAttributes Integer
weceaDecisionTaskCompletedEventId = lens _weceaDecisionTaskCompletedEventId (\s a -> s {_weceaDecisionTaskCompletedEventId = a})

instance FromJSON WorkflowExecutionCompletedEventAttributes where
  parseJSON =
    withObject
      "WorkflowExecutionCompletedEventAttributes"
      ( \x ->
          WorkflowExecutionCompletedEventAttributes'
            <$> (x .:? "result") <*> (x .: "decisionTaskCompletedEventId")
      )

instance Hashable WorkflowExecutionCompletedEventAttributes

instance NFData WorkflowExecutionCompletedEventAttributes
