{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ChildWorkflowExecutionCanceledEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ChildWorkflowExecutionCanceledEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.WorkflowExecution
import Network.AWS.SWF.Types.WorkflowType

-- | Provide details of the @ChildWorkflowExecutionCanceled@ event.
--
--
--
-- /See:/ 'childWorkflowExecutionCanceledEventAttributes' smart constructor.
data ChildWorkflowExecutionCanceledEventAttributes = ChildWorkflowExecutionCanceledEventAttributes'
  { _cDetails ::
      !( Maybe
           Text
       ),
    _cWorkflowExecution ::
      !WorkflowExecution,
    _cWorkflowType ::
      !WorkflowType,
    _cInitiatedEventId ::
      !Integer,
    _cStartedEventId ::
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

-- | Creates a value of 'ChildWorkflowExecutionCanceledEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cDetails' - Details of the cancellation (if provided).
--
-- * 'cWorkflowExecution' - The child workflow execution that was canceled.
--
-- * 'cWorkflowType' - The type of the child workflow execution.
--
-- * 'cInitiatedEventId' - The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- * 'cStartedEventId' - The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
childWorkflowExecutionCanceledEventAttributes ::
  -- | 'cWorkflowExecution'
  WorkflowExecution ->
  -- | 'cWorkflowType'
  WorkflowType ->
  -- | 'cInitiatedEventId'
  Integer ->
  -- | 'cStartedEventId'
  Integer ->
  ChildWorkflowExecutionCanceledEventAttributes
childWorkflowExecutionCanceledEventAttributes
  pWorkflowExecution_
  pWorkflowType_
  pInitiatedEventId_
  pStartedEventId_ =
    ChildWorkflowExecutionCanceledEventAttributes'
      { _cDetails =
          Nothing,
        _cWorkflowExecution = pWorkflowExecution_,
        _cWorkflowType = pWorkflowType_,
        _cInitiatedEventId = pInitiatedEventId_,
        _cStartedEventId = pStartedEventId_
      }

-- | Details of the cancellation (if provided).
cDetails :: Lens' ChildWorkflowExecutionCanceledEventAttributes (Maybe Text)
cDetails = lens _cDetails (\s a -> s {_cDetails = a})

-- | The child workflow execution that was canceled.
cWorkflowExecution :: Lens' ChildWorkflowExecutionCanceledEventAttributes WorkflowExecution
cWorkflowExecution = lens _cWorkflowExecution (\s a -> s {_cWorkflowExecution = a})

-- | The type of the child workflow execution.
cWorkflowType :: Lens' ChildWorkflowExecutionCanceledEventAttributes WorkflowType
cWorkflowType = lens _cWorkflowType (\s a -> s {_cWorkflowType = a})

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
cInitiatedEventId :: Lens' ChildWorkflowExecutionCanceledEventAttributes Integer
cInitiatedEventId = lens _cInitiatedEventId (\s a -> s {_cInitiatedEventId = a})

-- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
cStartedEventId :: Lens' ChildWorkflowExecutionCanceledEventAttributes Integer
cStartedEventId = lens _cStartedEventId (\s a -> s {_cStartedEventId = a})

instance FromJSON ChildWorkflowExecutionCanceledEventAttributes where
  parseJSON =
    withObject
      "ChildWorkflowExecutionCanceledEventAttributes"
      ( \x ->
          ChildWorkflowExecutionCanceledEventAttributes'
            <$> (x .:? "details")
            <*> (x .: "workflowExecution")
            <*> (x .: "workflowType")
            <*> (x .: "initiatedEventId")
            <*> (x .: "startedEventId")
      )

instance Hashable ChildWorkflowExecutionCanceledEventAttributes

instance NFData ChildWorkflowExecutionCanceledEventAttributes
