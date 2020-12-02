{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ChildWorkflowExecutionTerminatedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ChildWorkflowExecutionTerminatedEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.WorkflowExecution
import Network.AWS.SWF.Types.WorkflowType

-- | Provides the details of the @ChildWorkflowExecutionTerminated@ event.
--
--
--
-- /See:/ 'childWorkflowExecutionTerminatedEventAttributes' smart constructor.
data ChildWorkflowExecutionTerminatedEventAttributes = ChildWorkflowExecutionTerminatedEventAttributes'
  { _cweteaWorkflowExecution ::
      !WorkflowExecution,
    _cweteaWorkflowType ::
      !WorkflowType,
    _cweteaInitiatedEventId ::
      !Integer,
    _cweteaStartedEventId ::
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

-- | Creates a value of 'ChildWorkflowExecutionTerminatedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cweteaWorkflowExecution' - The child workflow execution that was terminated.
--
-- * 'cweteaWorkflowType' - The type of the child workflow execution.
--
-- * 'cweteaInitiatedEventId' - The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- * 'cweteaStartedEventId' - The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
childWorkflowExecutionTerminatedEventAttributes ::
  -- | 'cweteaWorkflowExecution'
  WorkflowExecution ->
  -- | 'cweteaWorkflowType'
  WorkflowType ->
  -- | 'cweteaInitiatedEventId'
  Integer ->
  -- | 'cweteaStartedEventId'
  Integer ->
  ChildWorkflowExecutionTerminatedEventAttributes
childWorkflowExecutionTerminatedEventAttributes
  pWorkflowExecution_
  pWorkflowType_
  pInitiatedEventId_
  pStartedEventId_ =
    ChildWorkflowExecutionTerminatedEventAttributes'
      { _cweteaWorkflowExecution =
          pWorkflowExecution_,
        _cweteaWorkflowType = pWorkflowType_,
        _cweteaInitiatedEventId = pInitiatedEventId_,
        _cweteaStartedEventId = pStartedEventId_
      }

-- | The child workflow execution that was terminated.
cweteaWorkflowExecution :: Lens' ChildWorkflowExecutionTerminatedEventAttributes WorkflowExecution
cweteaWorkflowExecution = lens _cweteaWorkflowExecution (\s a -> s {_cweteaWorkflowExecution = a})

-- | The type of the child workflow execution.
cweteaWorkflowType :: Lens' ChildWorkflowExecutionTerminatedEventAttributes WorkflowType
cweteaWorkflowType = lens _cweteaWorkflowType (\s a -> s {_cweteaWorkflowType = a})

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
cweteaInitiatedEventId :: Lens' ChildWorkflowExecutionTerminatedEventAttributes Integer
cweteaInitiatedEventId = lens _cweteaInitiatedEventId (\s a -> s {_cweteaInitiatedEventId = a})

-- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
cweteaStartedEventId :: Lens' ChildWorkflowExecutionTerminatedEventAttributes Integer
cweteaStartedEventId = lens _cweteaStartedEventId (\s a -> s {_cweteaStartedEventId = a})

instance FromJSON ChildWorkflowExecutionTerminatedEventAttributes where
  parseJSON =
    withObject
      "ChildWorkflowExecutionTerminatedEventAttributes"
      ( \x ->
          ChildWorkflowExecutionTerminatedEventAttributes'
            <$> (x .: "workflowExecution")
            <*> (x .: "workflowType")
            <*> (x .: "initiatedEventId")
            <*> (x .: "startedEventId")
      )

instance Hashable ChildWorkflowExecutionTerminatedEventAttributes

instance NFData ChildWorkflowExecutionTerminatedEventAttributes
