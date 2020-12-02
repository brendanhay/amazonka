{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionTerminatedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionTerminatedEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.ChildPolicy
import Network.AWS.SWF.Types.WorkflowExecutionTerminatedCause

-- | Provides the details of the @WorkflowExecutionTerminated@ event.
--
--
--
-- /See:/ 'workflowExecutionTerminatedEventAttributes' smart constructor.
data WorkflowExecutionTerminatedEventAttributes = WorkflowExecutionTerminatedEventAttributes'
  { _weteaCause ::
      !( Maybe
           WorkflowExecutionTerminatedCause
       ),
    _weteaReason ::
      !( Maybe
           Text
       ),
    _weteaDetails ::
      !( Maybe
           Text
       ),
    _weteaChildPolicy ::
      !ChildPolicy
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'WorkflowExecutionTerminatedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'weteaCause' - If set, indicates that the workflow execution was automatically terminated, and specifies the cause. This happens if the parent workflow execution times out or is terminated and the child policy is set to terminate child executions.
--
-- * 'weteaReason' - The reason provided for the termination.
--
-- * 'weteaDetails' - The details provided for the termination.
--
-- * 'weteaChildPolicy' - The policy used for the child workflow executions of this workflow execution. The supported child policies are:     * @TERMINATE@ – The child executions are terminated.     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.     * @ABANDON@ – No action is taken. The child executions continue to run.
workflowExecutionTerminatedEventAttributes ::
  -- | 'weteaChildPolicy'
  ChildPolicy ->
  WorkflowExecutionTerminatedEventAttributes
workflowExecutionTerminatedEventAttributes pChildPolicy_ =
  WorkflowExecutionTerminatedEventAttributes'
    { _weteaCause =
        Nothing,
      _weteaReason = Nothing,
      _weteaDetails = Nothing,
      _weteaChildPolicy = pChildPolicy_
    }

-- | If set, indicates that the workflow execution was automatically terminated, and specifies the cause. This happens if the parent workflow execution times out or is terminated and the child policy is set to terminate child executions.
weteaCause :: Lens' WorkflowExecutionTerminatedEventAttributes (Maybe WorkflowExecutionTerminatedCause)
weteaCause = lens _weteaCause (\s a -> s {_weteaCause = a})

-- | The reason provided for the termination.
weteaReason :: Lens' WorkflowExecutionTerminatedEventAttributes (Maybe Text)
weteaReason = lens _weteaReason (\s a -> s {_weteaReason = a})

-- | The details provided for the termination.
weteaDetails :: Lens' WorkflowExecutionTerminatedEventAttributes (Maybe Text)
weteaDetails = lens _weteaDetails (\s a -> s {_weteaDetails = a})

-- | The policy used for the child workflow executions of this workflow execution. The supported child policies are:     * @TERMINATE@ – The child executions are terminated.     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.     * @ABANDON@ – No action is taken. The child executions continue to run.
weteaChildPolicy :: Lens' WorkflowExecutionTerminatedEventAttributes ChildPolicy
weteaChildPolicy = lens _weteaChildPolicy (\s a -> s {_weteaChildPolicy = a})

instance FromJSON WorkflowExecutionTerminatedEventAttributes where
  parseJSON =
    withObject
      "WorkflowExecutionTerminatedEventAttributes"
      ( \x ->
          WorkflowExecutionTerminatedEventAttributes'
            <$> (x .:? "cause")
            <*> (x .:? "reason")
            <*> (x .:? "details")
            <*> (x .: "childPolicy")
      )

instance Hashable WorkflowExecutionTerminatedEventAttributes

instance NFData WorkflowExecutionTerminatedEventAttributes
