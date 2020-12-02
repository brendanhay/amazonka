{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionTimedOutEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionTimedOutEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.ChildPolicy
import Network.AWS.SWF.Types.WorkflowExecutionTimeoutType

-- | Provides the details of the @WorkflowExecutionTimedOut@ event.
--
--
--
-- /See:/ 'workflowExecutionTimedOutEventAttributes' smart constructor.
data WorkflowExecutionTimedOutEventAttributes = WorkflowExecutionTimedOutEventAttributes'
  { _wetoeaTimeoutType ::
      !WorkflowExecutionTimeoutType,
    _wetoeaChildPolicy ::
      !ChildPolicy
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkflowExecutionTimedOutEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wetoeaTimeoutType' - The type of timeout that caused this event.
--
-- * 'wetoeaChildPolicy' - The policy used for the child workflow executions of this workflow execution. The supported child policies are:     * @TERMINATE@ – The child executions are terminated.     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.     * @ABANDON@ – No action is taken. The child executions continue to run.
workflowExecutionTimedOutEventAttributes ::
  -- | 'wetoeaTimeoutType'
  WorkflowExecutionTimeoutType ->
  -- | 'wetoeaChildPolicy'
  ChildPolicy ->
  WorkflowExecutionTimedOutEventAttributes
workflowExecutionTimedOutEventAttributes
  pTimeoutType_
  pChildPolicy_ =
    WorkflowExecutionTimedOutEventAttributes'
      { _wetoeaTimeoutType =
          pTimeoutType_,
        _wetoeaChildPolicy = pChildPolicy_
      }

-- | The type of timeout that caused this event.
wetoeaTimeoutType :: Lens' WorkflowExecutionTimedOutEventAttributes WorkflowExecutionTimeoutType
wetoeaTimeoutType = lens _wetoeaTimeoutType (\s a -> s {_wetoeaTimeoutType = a})

-- | The policy used for the child workflow executions of this workflow execution. The supported child policies are:     * @TERMINATE@ – The child executions are terminated.     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.     * @ABANDON@ – No action is taken. The child executions continue to run.
wetoeaChildPolicy :: Lens' WorkflowExecutionTimedOutEventAttributes ChildPolicy
wetoeaChildPolicy = lens _wetoeaChildPolicy (\s a -> s {_wetoeaChildPolicy = a})

instance FromJSON WorkflowExecutionTimedOutEventAttributes where
  parseJSON =
    withObject
      "WorkflowExecutionTimedOutEventAttributes"
      ( \x ->
          WorkflowExecutionTimedOutEventAttributes'
            <$> (x .: "timeoutType") <*> (x .: "childPolicy")
      )

instance Hashable WorkflowExecutionTimedOutEventAttributes

instance NFData WorkflowExecutionTimedOutEventAttributes
