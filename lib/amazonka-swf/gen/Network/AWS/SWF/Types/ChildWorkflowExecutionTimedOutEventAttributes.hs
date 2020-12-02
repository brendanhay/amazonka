{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ChildWorkflowExecutionTimedOutEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ChildWorkflowExecutionTimedOutEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.WorkflowExecution
import Network.AWS.SWF.Types.WorkflowExecutionTimeoutType
import Network.AWS.SWF.Types.WorkflowType

-- | Provides the details of the @ChildWorkflowExecutionTimedOut@ event.
--
--
--
-- /See:/ 'childWorkflowExecutionTimedOutEventAttributes' smart constructor.
data ChildWorkflowExecutionTimedOutEventAttributes = ChildWorkflowExecutionTimedOutEventAttributes'
  { _cwetoeaWorkflowExecution ::
      !WorkflowExecution,
    _cwetoeaWorkflowType ::
      !WorkflowType,
    _cwetoeaTimeoutType ::
      !WorkflowExecutionTimeoutType,
    _cwetoeaInitiatedEventId ::
      !Integer,
    _cwetoeaStartedEventId ::
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

-- | Creates a value of 'ChildWorkflowExecutionTimedOutEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwetoeaWorkflowExecution' - The child workflow execution that timed out.
--
-- * 'cwetoeaWorkflowType' - The type of the child workflow execution.
--
-- * 'cwetoeaTimeoutType' - The type of the timeout that caused the child workflow execution to time out.
--
-- * 'cwetoeaInitiatedEventId' - The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- * 'cwetoeaStartedEventId' - The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
childWorkflowExecutionTimedOutEventAttributes ::
  -- | 'cwetoeaWorkflowExecution'
  WorkflowExecution ->
  -- | 'cwetoeaWorkflowType'
  WorkflowType ->
  -- | 'cwetoeaTimeoutType'
  WorkflowExecutionTimeoutType ->
  -- | 'cwetoeaInitiatedEventId'
  Integer ->
  -- | 'cwetoeaStartedEventId'
  Integer ->
  ChildWorkflowExecutionTimedOutEventAttributes
childWorkflowExecutionTimedOutEventAttributes
  pWorkflowExecution_
  pWorkflowType_
  pTimeoutType_
  pInitiatedEventId_
  pStartedEventId_ =
    ChildWorkflowExecutionTimedOutEventAttributes'
      { _cwetoeaWorkflowExecution =
          pWorkflowExecution_,
        _cwetoeaWorkflowType = pWorkflowType_,
        _cwetoeaTimeoutType = pTimeoutType_,
        _cwetoeaInitiatedEventId = pInitiatedEventId_,
        _cwetoeaStartedEventId = pStartedEventId_
      }

-- | The child workflow execution that timed out.
cwetoeaWorkflowExecution :: Lens' ChildWorkflowExecutionTimedOutEventAttributes WorkflowExecution
cwetoeaWorkflowExecution = lens _cwetoeaWorkflowExecution (\s a -> s {_cwetoeaWorkflowExecution = a})

-- | The type of the child workflow execution.
cwetoeaWorkflowType :: Lens' ChildWorkflowExecutionTimedOutEventAttributes WorkflowType
cwetoeaWorkflowType = lens _cwetoeaWorkflowType (\s a -> s {_cwetoeaWorkflowType = a})

-- | The type of the timeout that caused the child workflow execution to time out.
cwetoeaTimeoutType :: Lens' ChildWorkflowExecutionTimedOutEventAttributes WorkflowExecutionTimeoutType
cwetoeaTimeoutType = lens _cwetoeaTimeoutType (\s a -> s {_cwetoeaTimeoutType = a})

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
cwetoeaInitiatedEventId :: Lens' ChildWorkflowExecutionTimedOutEventAttributes Integer
cwetoeaInitiatedEventId = lens _cwetoeaInitiatedEventId (\s a -> s {_cwetoeaInitiatedEventId = a})

-- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
cwetoeaStartedEventId :: Lens' ChildWorkflowExecutionTimedOutEventAttributes Integer
cwetoeaStartedEventId = lens _cwetoeaStartedEventId (\s a -> s {_cwetoeaStartedEventId = a})

instance FromJSON ChildWorkflowExecutionTimedOutEventAttributes where
  parseJSON =
    withObject
      "ChildWorkflowExecutionTimedOutEventAttributes"
      ( \x ->
          ChildWorkflowExecutionTimedOutEventAttributes'
            <$> (x .: "workflowExecution")
            <*> (x .: "workflowType")
            <*> (x .: "timeoutType")
            <*> (x .: "initiatedEventId")
            <*> (x .: "startedEventId")
      )

instance Hashable ChildWorkflowExecutionTimedOutEventAttributes

instance NFData ChildWorkflowExecutionTimedOutEventAttributes
