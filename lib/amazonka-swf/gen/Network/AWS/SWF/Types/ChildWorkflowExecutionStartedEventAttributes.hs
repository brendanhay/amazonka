{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ChildWorkflowExecutionStartedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ChildWorkflowExecutionStartedEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.WorkflowExecution
import Network.AWS.SWF.Types.WorkflowType

-- | Provides the details of the @ChildWorkflowExecutionStarted@ event.
--
--
--
-- /See:/ 'childWorkflowExecutionStartedEventAttributes' smart constructor.
data ChildWorkflowExecutionStartedEventAttributes = ChildWorkflowExecutionStartedEventAttributes'
  { _cweseaWorkflowExecution ::
      !WorkflowExecution,
    _cweseaWorkflowType ::
      !WorkflowType,
    _cweseaInitiatedEventId ::
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

-- | Creates a value of 'ChildWorkflowExecutionStartedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cweseaWorkflowExecution' - The child workflow execution that was started.
--
-- * 'cweseaWorkflowType' - The type of the child workflow execution.
--
-- * 'cweseaInitiatedEventId' - The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
childWorkflowExecutionStartedEventAttributes ::
  -- | 'cweseaWorkflowExecution'
  WorkflowExecution ->
  -- | 'cweseaWorkflowType'
  WorkflowType ->
  -- | 'cweseaInitiatedEventId'
  Integer ->
  ChildWorkflowExecutionStartedEventAttributes
childWorkflowExecutionStartedEventAttributes
  pWorkflowExecution_
  pWorkflowType_
  pInitiatedEventId_ =
    ChildWorkflowExecutionStartedEventAttributes'
      { _cweseaWorkflowExecution =
          pWorkflowExecution_,
        _cweseaWorkflowType = pWorkflowType_,
        _cweseaInitiatedEventId = pInitiatedEventId_
      }

-- | The child workflow execution that was started.
cweseaWorkflowExecution :: Lens' ChildWorkflowExecutionStartedEventAttributes WorkflowExecution
cweseaWorkflowExecution = lens _cweseaWorkflowExecution (\s a -> s {_cweseaWorkflowExecution = a})

-- | The type of the child workflow execution.
cweseaWorkflowType :: Lens' ChildWorkflowExecutionStartedEventAttributes WorkflowType
cweseaWorkflowType = lens _cweseaWorkflowType (\s a -> s {_cweseaWorkflowType = a})

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
cweseaInitiatedEventId :: Lens' ChildWorkflowExecutionStartedEventAttributes Integer
cweseaInitiatedEventId = lens _cweseaInitiatedEventId (\s a -> s {_cweseaInitiatedEventId = a})

instance FromJSON ChildWorkflowExecutionStartedEventAttributes where
  parseJSON =
    withObject
      "ChildWorkflowExecutionStartedEventAttributes"
      ( \x ->
          ChildWorkflowExecutionStartedEventAttributes'
            <$> (x .: "workflowExecution")
            <*> (x .: "workflowType")
            <*> (x .: "initiatedEventId")
      )

instance Hashable ChildWorkflowExecutionStartedEventAttributes

instance NFData ChildWorkflowExecutionStartedEventAttributes
