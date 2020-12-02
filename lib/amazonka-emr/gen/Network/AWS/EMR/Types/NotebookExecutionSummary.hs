{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.NotebookExecutionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.NotebookExecutionSummary where

import Network.AWS.EMR.Types.NotebookExecutionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- |
--
--
--
-- /See:/ 'notebookExecutionSummary' smart constructor.
data NotebookExecutionSummary = NotebookExecutionSummary'
  { _nesStatus ::
      !(Maybe NotebookExecutionStatus),
    _nesEditorId :: !(Maybe Text),
    _nesStartTime :: !(Maybe POSIX),
    _nesNotebookExecutionId :: !(Maybe Text),
    _nesNotebookExecutionName ::
      !(Maybe Text),
    _nesEndTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NotebookExecutionSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nesStatus' - The status of the notebook execution.     * @START_PENDING@ indicates that the cluster has received the execution request but execution has not begun.     * @STARTING@ indicates that the execution is starting on the cluster.     * @RUNNING@ indicates that the execution is being processed by the cluster.     * @FINISHING@ indicates that execution processing is in the final stages.     * @FINISHED@ indicates that the execution has completed without error.     * @FAILING@ indicates that the execution is failing and will not finish successfully.     * @FAILED@ indicates that the execution failed.     * @STOP_PENDING@ indicates that the cluster has received a @StopNotebookExecution@ request and the stop is pending.     * @STOPPING@ indicates that the cluster is in the process of stopping the execution as a result of a @StopNotebookExecution@ request.     * @STOPPED@ indicates that the execution stopped because of a @StopNotebookExecution@ request.
--
-- * 'nesEditorId' - The unique identifier of the editor associated with the notebook execution.
--
-- * 'nesStartTime' - The timestamp when notebook execution started.
--
-- * 'nesNotebookExecutionId' - The unique identifier of the notebook execution.
--
-- * 'nesNotebookExecutionName' - The name of the notebook execution.
--
-- * 'nesEndTime' - The timestamp when notebook execution started.
notebookExecutionSummary ::
  NotebookExecutionSummary
notebookExecutionSummary =
  NotebookExecutionSummary'
    { _nesStatus = Nothing,
      _nesEditorId = Nothing,
      _nesStartTime = Nothing,
      _nesNotebookExecutionId = Nothing,
      _nesNotebookExecutionName = Nothing,
      _nesEndTime = Nothing
    }

-- | The status of the notebook execution.     * @START_PENDING@ indicates that the cluster has received the execution request but execution has not begun.     * @STARTING@ indicates that the execution is starting on the cluster.     * @RUNNING@ indicates that the execution is being processed by the cluster.     * @FINISHING@ indicates that execution processing is in the final stages.     * @FINISHED@ indicates that the execution has completed without error.     * @FAILING@ indicates that the execution is failing and will not finish successfully.     * @FAILED@ indicates that the execution failed.     * @STOP_PENDING@ indicates that the cluster has received a @StopNotebookExecution@ request and the stop is pending.     * @STOPPING@ indicates that the cluster is in the process of stopping the execution as a result of a @StopNotebookExecution@ request.     * @STOPPED@ indicates that the execution stopped because of a @StopNotebookExecution@ request.
nesStatus :: Lens' NotebookExecutionSummary (Maybe NotebookExecutionStatus)
nesStatus = lens _nesStatus (\s a -> s {_nesStatus = a})

-- | The unique identifier of the editor associated with the notebook execution.
nesEditorId :: Lens' NotebookExecutionSummary (Maybe Text)
nesEditorId = lens _nesEditorId (\s a -> s {_nesEditorId = a})

-- | The timestamp when notebook execution started.
nesStartTime :: Lens' NotebookExecutionSummary (Maybe UTCTime)
nesStartTime = lens _nesStartTime (\s a -> s {_nesStartTime = a}) . mapping _Time

-- | The unique identifier of the notebook execution.
nesNotebookExecutionId :: Lens' NotebookExecutionSummary (Maybe Text)
nesNotebookExecutionId = lens _nesNotebookExecutionId (\s a -> s {_nesNotebookExecutionId = a})

-- | The name of the notebook execution.
nesNotebookExecutionName :: Lens' NotebookExecutionSummary (Maybe Text)
nesNotebookExecutionName = lens _nesNotebookExecutionName (\s a -> s {_nesNotebookExecutionName = a})

-- | The timestamp when notebook execution started.
nesEndTime :: Lens' NotebookExecutionSummary (Maybe UTCTime)
nesEndTime = lens _nesEndTime (\s a -> s {_nesEndTime = a}) . mapping _Time

instance FromJSON NotebookExecutionSummary where
  parseJSON =
    withObject
      "NotebookExecutionSummary"
      ( \x ->
          NotebookExecutionSummary'
            <$> (x .:? "Status")
            <*> (x .:? "EditorId")
            <*> (x .:? "StartTime")
            <*> (x .:? "NotebookExecutionId")
            <*> (x .:? "NotebookExecutionName")
            <*> (x .:? "EndTime")
      )

instance Hashable NotebookExecutionSummary

instance NFData NotebookExecutionSummary
