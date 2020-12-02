{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionInfo where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.CloseStatus
import Network.AWS.SWF.Types.ExecutionStatus
import Network.AWS.SWF.Types.WorkflowExecution
import Network.AWS.SWF.Types.WorkflowType

-- | Contains information about a workflow execution.
--
--
--
-- /See:/ 'workflowExecutionInfo' smart constructor.
data WorkflowExecutionInfo = WorkflowExecutionInfo'
  { _weiParent ::
      !(Maybe WorkflowExecution),
    _weiTagList :: !(Maybe [Text]),
    _weiCloseStatus :: !(Maybe CloseStatus),
    _weiCloseTimestamp :: !(Maybe POSIX),
    _weiCancelRequested :: !(Maybe Bool),
    _weiExecution :: !WorkflowExecution,
    _weiWorkflowType :: !WorkflowType,
    _weiStartTimestamp :: !POSIX,
    _weiExecutionStatus :: !ExecutionStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkflowExecutionInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'weiParent' - If this workflow execution is a child of another execution then contains the workflow execution that started this execution.
--
-- * 'weiTagList' - The list of tags associated with the workflow execution. Tags can be used to identify and list workflow executions of interest through the visibility APIs. A workflow execution can have a maximum of 5 tags.
--
-- * 'weiCloseStatus' - If the execution status is closed then this specifies how the execution was closed:     * @COMPLETED@ – the execution was successfully completed.     * @CANCELED@ – the execution was canceled.Cancellation allows the implementation to gracefully clean up before the execution is closed.     * @TERMINATED@ – the execution was force terminated.     * @FAILED@ – the execution failed to complete.     * @TIMED_OUT@ – the execution did not complete in the alloted time and was automatically timed out.     * @CONTINUED_AS_NEW@ – the execution is logically continued. This means the current execution was completed and a new execution was started to carry on the workflow.
--
-- * 'weiCloseTimestamp' - The time when the workflow execution was closed. Set only if the execution status is CLOSED.
--
-- * 'weiCancelRequested' - Set to true if a cancellation is requested for this workflow execution.
--
-- * 'weiExecution' - The workflow execution this information is about.
--
-- * 'weiWorkflowType' - The type of the workflow execution.
--
-- * 'weiStartTimestamp' - The time when the execution was started.
--
-- * 'weiExecutionStatus' - The current status of the execution.
workflowExecutionInfo ::
  -- | 'weiExecution'
  WorkflowExecution ->
  -- | 'weiWorkflowType'
  WorkflowType ->
  -- | 'weiStartTimestamp'
  UTCTime ->
  -- | 'weiExecutionStatus'
  ExecutionStatus ->
  WorkflowExecutionInfo
workflowExecutionInfo
  pExecution_
  pWorkflowType_
  pStartTimestamp_
  pExecutionStatus_ =
    WorkflowExecutionInfo'
      { _weiParent = Nothing,
        _weiTagList = Nothing,
        _weiCloseStatus = Nothing,
        _weiCloseTimestamp = Nothing,
        _weiCancelRequested = Nothing,
        _weiExecution = pExecution_,
        _weiWorkflowType = pWorkflowType_,
        _weiStartTimestamp = _Time # pStartTimestamp_,
        _weiExecutionStatus = pExecutionStatus_
      }

-- | If this workflow execution is a child of another execution then contains the workflow execution that started this execution.
weiParent :: Lens' WorkflowExecutionInfo (Maybe WorkflowExecution)
weiParent = lens _weiParent (\s a -> s {_weiParent = a})

-- | The list of tags associated with the workflow execution. Tags can be used to identify and list workflow executions of interest through the visibility APIs. A workflow execution can have a maximum of 5 tags.
weiTagList :: Lens' WorkflowExecutionInfo [Text]
weiTagList = lens _weiTagList (\s a -> s {_weiTagList = a}) . _Default . _Coerce

-- | If the execution status is closed then this specifies how the execution was closed:     * @COMPLETED@ – the execution was successfully completed.     * @CANCELED@ – the execution was canceled.Cancellation allows the implementation to gracefully clean up before the execution is closed.     * @TERMINATED@ – the execution was force terminated.     * @FAILED@ – the execution failed to complete.     * @TIMED_OUT@ – the execution did not complete in the alloted time and was automatically timed out.     * @CONTINUED_AS_NEW@ – the execution is logically continued. This means the current execution was completed and a new execution was started to carry on the workflow.
weiCloseStatus :: Lens' WorkflowExecutionInfo (Maybe CloseStatus)
weiCloseStatus = lens _weiCloseStatus (\s a -> s {_weiCloseStatus = a})

-- | The time when the workflow execution was closed. Set only if the execution status is CLOSED.
weiCloseTimestamp :: Lens' WorkflowExecutionInfo (Maybe UTCTime)
weiCloseTimestamp = lens _weiCloseTimestamp (\s a -> s {_weiCloseTimestamp = a}) . mapping _Time

-- | Set to true if a cancellation is requested for this workflow execution.
weiCancelRequested :: Lens' WorkflowExecutionInfo (Maybe Bool)
weiCancelRequested = lens _weiCancelRequested (\s a -> s {_weiCancelRequested = a})

-- | The workflow execution this information is about.
weiExecution :: Lens' WorkflowExecutionInfo WorkflowExecution
weiExecution = lens _weiExecution (\s a -> s {_weiExecution = a})

-- | The type of the workflow execution.
weiWorkflowType :: Lens' WorkflowExecutionInfo WorkflowType
weiWorkflowType = lens _weiWorkflowType (\s a -> s {_weiWorkflowType = a})

-- | The time when the execution was started.
weiStartTimestamp :: Lens' WorkflowExecutionInfo UTCTime
weiStartTimestamp = lens _weiStartTimestamp (\s a -> s {_weiStartTimestamp = a}) . _Time

-- | The current status of the execution.
weiExecutionStatus :: Lens' WorkflowExecutionInfo ExecutionStatus
weiExecutionStatus = lens _weiExecutionStatus (\s a -> s {_weiExecutionStatus = a})

instance FromJSON WorkflowExecutionInfo where
  parseJSON =
    withObject
      "WorkflowExecutionInfo"
      ( \x ->
          WorkflowExecutionInfo'
            <$> (x .:? "parent")
            <*> (x .:? "tagList" .!= mempty)
            <*> (x .:? "closeStatus")
            <*> (x .:? "closeTimestamp")
            <*> (x .:? "cancelRequested")
            <*> (x .: "execution")
            <*> (x .: "workflowType")
            <*> (x .: "startTimestamp")
            <*> (x .: "executionStatus")
      )

instance Hashable WorkflowExecutionInfo

instance NFData WorkflowExecutionInfo
