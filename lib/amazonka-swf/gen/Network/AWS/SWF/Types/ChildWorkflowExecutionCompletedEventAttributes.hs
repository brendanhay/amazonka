{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ChildWorkflowExecutionCompletedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ChildWorkflowExecutionCompletedEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.WorkflowExecution
import Network.AWS.SWF.Types.WorkflowType

-- | Provides the details of the @ChildWorkflowExecutionCompleted@ event.
--
--
--
-- /See:/ 'childWorkflowExecutionCompletedEventAttributes' smart constructor.
data ChildWorkflowExecutionCompletedEventAttributes = ChildWorkflowExecutionCompletedEventAttributes'
  { _cweceaResult ::
      !( Maybe
           Text
       ),
    _cweceaWorkflowExecution ::
      !WorkflowExecution,
    _cweceaWorkflowType ::
      !WorkflowType,
    _cweceaInitiatedEventId ::
      !Integer,
    _cweceaStartedEventId ::
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

-- | Creates a value of 'ChildWorkflowExecutionCompletedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cweceaResult' - The result of the child workflow execution.
--
-- * 'cweceaWorkflowExecution' - The child workflow execution that was completed.
--
-- * 'cweceaWorkflowType' - The type of the child workflow execution.
--
-- * 'cweceaInitiatedEventId' - The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- * 'cweceaStartedEventId' - The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
childWorkflowExecutionCompletedEventAttributes ::
  -- | 'cweceaWorkflowExecution'
  WorkflowExecution ->
  -- | 'cweceaWorkflowType'
  WorkflowType ->
  -- | 'cweceaInitiatedEventId'
  Integer ->
  -- | 'cweceaStartedEventId'
  Integer ->
  ChildWorkflowExecutionCompletedEventAttributes
childWorkflowExecutionCompletedEventAttributes
  pWorkflowExecution_
  pWorkflowType_
  pInitiatedEventId_
  pStartedEventId_ =
    ChildWorkflowExecutionCompletedEventAttributes'
      { _cweceaResult =
          Nothing,
        _cweceaWorkflowExecution = pWorkflowExecution_,
        _cweceaWorkflowType = pWorkflowType_,
        _cweceaInitiatedEventId = pInitiatedEventId_,
        _cweceaStartedEventId = pStartedEventId_
      }

-- | The result of the child workflow execution.
cweceaResult :: Lens' ChildWorkflowExecutionCompletedEventAttributes (Maybe Text)
cweceaResult = lens _cweceaResult (\s a -> s {_cweceaResult = a})

-- | The child workflow execution that was completed.
cweceaWorkflowExecution :: Lens' ChildWorkflowExecutionCompletedEventAttributes WorkflowExecution
cweceaWorkflowExecution = lens _cweceaWorkflowExecution (\s a -> s {_cweceaWorkflowExecution = a})

-- | The type of the child workflow execution.
cweceaWorkflowType :: Lens' ChildWorkflowExecutionCompletedEventAttributes WorkflowType
cweceaWorkflowType = lens _cweceaWorkflowType (\s a -> s {_cweceaWorkflowType = a})

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
cweceaInitiatedEventId :: Lens' ChildWorkflowExecutionCompletedEventAttributes Integer
cweceaInitiatedEventId = lens _cweceaInitiatedEventId (\s a -> s {_cweceaInitiatedEventId = a})

-- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
cweceaStartedEventId :: Lens' ChildWorkflowExecutionCompletedEventAttributes Integer
cweceaStartedEventId = lens _cweceaStartedEventId (\s a -> s {_cweceaStartedEventId = a})

instance FromJSON ChildWorkflowExecutionCompletedEventAttributes where
  parseJSON =
    withObject
      "ChildWorkflowExecutionCompletedEventAttributes"
      ( \x ->
          ChildWorkflowExecutionCompletedEventAttributes'
            <$> (x .:? "result")
            <*> (x .: "workflowExecution")
            <*> (x .: "workflowType")
            <*> (x .: "initiatedEventId")
            <*> (x .: "startedEventId")
      )

instance Hashable ChildWorkflowExecutionCompletedEventAttributes

instance NFData ChildWorkflowExecutionCompletedEventAttributes
