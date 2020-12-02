{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.WorkflowRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.WorkflowRun where

import Network.AWS.Glue.Types.WorkflowGraph
import Network.AWS.Glue.Types.WorkflowRunStatistics
import Network.AWS.Glue.Types.WorkflowRunStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A workflow run is an execution of a workflow providing all the runtime information.
--
--
--
-- /See:/ 'workflowRun' smart constructor.
data WorkflowRun = WorkflowRun'
  { _wrCompletedOn :: !(Maybe POSIX),
    _wrStatus :: !(Maybe WorkflowRunStatus),
    _wrGraph :: !(Maybe WorkflowGraph),
    _wrStartedOn :: !(Maybe POSIX),
    _wrWorkflowRunId :: !(Maybe Text),
    _wrName :: !(Maybe Text),
    _wrPreviousRunId :: !(Maybe Text),
    _wrStatistics :: !(Maybe WorkflowRunStatistics),
    _wrErrorMessage :: !(Maybe Text),
    _wrWorkflowRunProperties :: !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkflowRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wrCompletedOn' - The date and time when the workflow run completed.
--
-- * 'wrStatus' - The status of the workflow run.
--
-- * 'wrGraph' - The graph representing all the AWS Glue components that belong to the workflow as nodes and directed connections between them as edges.
--
-- * 'wrStartedOn' - The date and time when the workflow run was started.
--
-- * 'wrWorkflowRunId' - The ID of this workflow run.
--
-- * 'wrName' - Name of the workflow that was executed.
--
-- * 'wrPreviousRunId' - The ID of the previous workflow run.
--
-- * 'wrStatistics' - The statistics of the run.
--
-- * 'wrErrorMessage' - This error message describes any error that may have occurred in starting the workflow run. Currently the only error message is "Concurrent runs exceeded for workflow: @foo@ ."
--
-- * 'wrWorkflowRunProperties' - The workflow run properties which were set during the run.
workflowRun ::
  WorkflowRun
workflowRun =
  WorkflowRun'
    { _wrCompletedOn = Nothing,
      _wrStatus = Nothing,
      _wrGraph = Nothing,
      _wrStartedOn = Nothing,
      _wrWorkflowRunId = Nothing,
      _wrName = Nothing,
      _wrPreviousRunId = Nothing,
      _wrStatistics = Nothing,
      _wrErrorMessage = Nothing,
      _wrWorkflowRunProperties = Nothing
    }

-- | The date and time when the workflow run completed.
wrCompletedOn :: Lens' WorkflowRun (Maybe UTCTime)
wrCompletedOn = lens _wrCompletedOn (\s a -> s {_wrCompletedOn = a}) . mapping _Time

-- | The status of the workflow run.
wrStatus :: Lens' WorkflowRun (Maybe WorkflowRunStatus)
wrStatus = lens _wrStatus (\s a -> s {_wrStatus = a})

-- | The graph representing all the AWS Glue components that belong to the workflow as nodes and directed connections between them as edges.
wrGraph :: Lens' WorkflowRun (Maybe WorkflowGraph)
wrGraph = lens _wrGraph (\s a -> s {_wrGraph = a})

-- | The date and time when the workflow run was started.
wrStartedOn :: Lens' WorkflowRun (Maybe UTCTime)
wrStartedOn = lens _wrStartedOn (\s a -> s {_wrStartedOn = a}) . mapping _Time

-- | The ID of this workflow run.
wrWorkflowRunId :: Lens' WorkflowRun (Maybe Text)
wrWorkflowRunId = lens _wrWorkflowRunId (\s a -> s {_wrWorkflowRunId = a})

-- | Name of the workflow that was executed.
wrName :: Lens' WorkflowRun (Maybe Text)
wrName = lens _wrName (\s a -> s {_wrName = a})

-- | The ID of the previous workflow run.
wrPreviousRunId :: Lens' WorkflowRun (Maybe Text)
wrPreviousRunId = lens _wrPreviousRunId (\s a -> s {_wrPreviousRunId = a})

-- | The statistics of the run.
wrStatistics :: Lens' WorkflowRun (Maybe WorkflowRunStatistics)
wrStatistics = lens _wrStatistics (\s a -> s {_wrStatistics = a})

-- | This error message describes any error that may have occurred in starting the workflow run. Currently the only error message is "Concurrent runs exceeded for workflow: @foo@ ."
wrErrorMessage :: Lens' WorkflowRun (Maybe Text)
wrErrorMessage = lens _wrErrorMessage (\s a -> s {_wrErrorMessage = a})

-- | The workflow run properties which were set during the run.
wrWorkflowRunProperties :: Lens' WorkflowRun (HashMap Text (Text))
wrWorkflowRunProperties = lens _wrWorkflowRunProperties (\s a -> s {_wrWorkflowRunProperties = a}) . _Default . _Map

instance FromJSON WorkflowRun where
  parseJSON =
    withObject
      "WorkflowRun"
      ( \x ->
          WorkflowRun'
            <$> (x .:? "CompletedOn")
            <*> (x .:? "Status")
            <*> (x .:? "Graph")
            <*> (x .:? "StartedOn")
            <*> (x .:? "WorkflowRunId")
            <*> (x .:? "Name")
            <*> (x .:? "PreviousRunId")
            <*> (x .:? "Statistics")
            <*> (x .:? "ErrorMessage")
            <*> (x .:? "WorkflowRunProperties" .!= mempty)
      )

instance Hashable WorkflowRun

instance NFData WorkflowRun
