{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionOpenCounts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionOpenCounts where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the counts of open tasks, child workflow executions and timers for a workflow execution.
--
--
--
-- /See:/ 'workflowExecutionOpenCounts' smart constructor.
data WorkflowExecutionOpenCounts = WorkflowExecutionOpenCounts'
  { _weocOpenLambdaFunctions ::
      !(Maybe Nat),
    _weocOpenActivityTasks :: !Nat,
    _weocOpenDecisionTasks :: !Nat,
    _weocOpenTimers :: !Nat,
    _weocOpenChildWorkflowExecutions ::
      !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkflowExecutionOpenCounts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'weocOpenLambdaFunctions' - The count of Lambda tasks whose status is @OPEN@ .
--
-- * 'weocOpenActivityTasks' - The count of activity tasks whose status is @OPEN@ .
--
-- * 'weocOpenDecisionTasks' - The count of decision tasks whose status is OPEN. A workflow execution can have at most one open decision task.
--
-- * 'weocOpenTimers' - The count of timers started by this workflow execution that have not fired yet.
--
-- * 'weocOpenChildWorkflowExecutions' - The count of child workflow executions whose status is @OPEN@ .
workflowExecutionOpenCounts ::
  -- | 'weocOpenActivityTasks'
  Natural ->
  -- | 'weocOpenDecisionTasks'
  Natural ->
  -- | 'weocOpenTimers'
  Natural ->
  -- | 'weocOpenChildWorkflowExecutions'
  Natural ->
  WorkflowExecutionOpenCounts
workflowExecutionOpenCounts
  pOpenActivityTasks_
  pOpenDecisionTasks_
  pOpenTimers_
  pOpenChildWorkflowExecutions_ =
    WorkflowExecutionOpenCounts'
      { _weocOpenLambdaFunctions = Nothing,
        _weocOpenActivityTasks = _Nat # pOpenActivityTasks_,
        _weocOpenDecisionTasks = _Nat # pOpenDecisionTasks_,
        _weocOpenTimers = _Nat # pOpenTimers_,
        _weocOpenChildWorkflowExecutions =
          _Nat # pOpenChildWorkflowExecutions_
      }

-- | The count of Lambda tasks whose status is @OPEN@ .
weocOpenLambdaFunctions :: Lens' WorkflowExecutionOpenCounts (Maybe Natural)
weocOpenLambdaFunctions = lens _weocOpenLambdaFunctions (\s a -> s {_weocOpenLambdaFunctions = a}) . mapping _Nat

-- | The count of activity tasks whose status is @OPEN@ .
weocOpenActivityTasks :: Lens' WorkflowExecutionOpenCounts Natural
weocOpenActivityTasks = lens _weocOpenActivityTasks (\s a -> s {_weocOpenActivityTasks = a}) . _Nat

-- | The count of decision tasks whose status is OPEN. A workflow execution can have at most one open decision task.
weocOpenDecisionTasks :: Lens' WorkflowExecutionOpenCounts Natural
weocOpenDecisionTasks = lens _weocOpenDecisionTasks (\s a -> s {_weocOpenDecisionTasks = a}) . _Nat

-- | The count of timers started by this workflow execution that have not fired yet.
weocOpenTimers :: Lens' WorkflowExecutionOpenCounts Natural
weocOpenTimers = lens _weocOpenTimers (\s a -> s {_weocOpenTimers = a}) . _Nat

-- | The count of child workflow executions whose status is @OPEN@ .
weocOpenChildWorkflowExecutions :: Lens' WorkflowExecutionOpenCounts Natural
weocOpenChildWorkflowExecutions = lens _weocOpenChildWorkflowExecutions (\s a -> s {_weocOpenChildWorkflowExecutions = a}) . _Nat

instance FromJSON WorkflowExecutionOpenCounts where
  parseJSON =
    withObject
      "WorkflowExecutionOpenCounts"
      ( \x ->
          WorkflowExecutionOpenCounts'
            <$> (x .:? "openLambdaFunctions")
            <*> (x .: "openActivityTasks")
            <*> (x .: "openDecisionTasks")
            <*> (x .: "openTimers")
            <*> (x .: "openChildWorkflowExecutions")
      )

instance Hashable WorkflowExecutionOpenCounts

instance NFData WorkflowExecutionOpenCounts
