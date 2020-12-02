{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ChildWorkflowExecutionFailedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ChildWorkflowExecutionFailedEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.WorkflowExecution
import Network.AWS.SWF.Types.WorkflowType

-- | Provides the details of the @ChildWorkflowExecutionFailed@ event.
--
--
--
-- /See:/ 'childWorkflowExecutionFailedEventAttributes' smart constructor.
data ChildWorkflowExecutionFailedEventAttributes = ChildWorkflowExecutionFailedEventAttributes'
  { _cwefeaReason ::
      !( Maybe
           Text
       ),
    _cwefeaDetails ::
      !( Maybe
           Text
       ),
    _cwefeaWorkflowExecution ::
      !WorkflowExecution,
    _cwefeaWorkflowType ::
      !WorkflowType,
    _cwefeaInitiatedEventId ::
      !Integer,
    _cwefeaStartedEventId ::
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

-- | Creates a value of 'ChildWorkflowExecutionFailedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwefeaReason' - The reason for the failure (if provided).
--
-- * 'cwefeaDetails' - The details of the failure (if provided).
--
-- * 'cwefeaWorkflowExecution' - The child workflow execution that failed.
--
-- * 'cwefeaWorkflowType' - The type of the child workflow execution.
--
-- * 'cwefeaInitiatedEventId' - The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- * 'cwefeaStartedEventId' - The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
childWorkflowExecutionFailedEventAttributes ::
  -- | 'cwefeaWorkflowExecution'
  WorkflowExecution ->
  -- | 'cwefeaWorkflowType'
  WorkflowType ->
  -- | 'cwefeaInitiatedEventId'
  Integer ->
  -- | 'cwefeaStartedEventId'
  Integer ->
  ChildWorkflowExecutionFailedEventAttributes
childWorkflowExecutionFailedEventAttributes
  pWorkflowExecution_
  pWorkflowType_
  pInitiatedEventId_
  pStartedEventId_ =
    ChildWorkflowExecutionFailedEventAttributes'
      { _cwefeaReason =
          Nothing,
        _cwefeaDetails = Nothing,
        _cwefeaWorkflowExecution = pWorkflowExecution_,
        _cwefeaWorkflowType = pWorkflowType_,
        _cwefeaInitiatedEventId = pInitiatedEventId_,
        _cwefeaStartedEventId = pStartedEventId_
      }

-- | The reason for the failure (if provided).
cwefeaReason :: Lens' ChildWorkflowExecutionFailedEventAttributes (Maybe Text)
cwefeaReason = lens _cwefeaReason (\s a -> s {_cwefeaReason = a})

-- | The details of the failure (if provided).
cwefeaDetails :: Lens' ChildWorkflowExecutionFailedEventAttributes (Maybe Text)
cwefeaDetails = lens _cwefeaDetails (\s a -> s {_cwefeaDetails = a})

-- | The child workflow execution that failed.
cwefeaWorkflowExecution :: Lens' ChildWorkflowExecutionFailedEventAttributes WorkflowExecution
cwefeaWorkflowExecution = lens _cwefeaWorkflowExecution (\s a -> s {_cwefeaWorkflowExecution = a})

-- | The type of the child workflow execution.
cwefeaWorkflowType :: Lens' ChildWorkflowExecutionFailedEventAttributes WorkflowType
cwefeaWorkflowType = lens _cwefeaWorkflowType (\s a -> s {_cwefeaWorkflowType = a})

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
cwefeaInitiatedEventId :: Lens' ChildWorkflowExecutionFailedEventAttributes Integer
cwefeaInitiatedEventId = lens _cwefeaInitiatedEventId (\s a -> s {_cwefeaInitiatedEventId = a})

-- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
cwefeaStartedEventId :: Lens' ChildWorkflowExecutionFailedEventAttributes Integer
cwefeaStartedEventId = lens _cwefeaStartedEventId (\s a -> s {_cwefeaStartedEventId = a})

instance FromJSON ChildWorkflowExecutionFailedEventAttributes where
  parseJSON =
    withObject
      "ChildWorkflowExecutionFailedEventAttributes"
      ( \x ->
          ChildWorkflowExecutionFailedEventAttributes'
            <$> (x .:? "reason")
            <*> (x .:? "details")
            <*> (x .: "workflowExecution")
            <*> (x .: "workflowType")
            <*> (x .: "initiatedEventId")
            <*> (x .: "startedEventId")
      )

instance Hashable ChildWorkflowExecutionFailedEventAttributes

instance NFData ChildWorkflowExecutionFailedEventAttributes
