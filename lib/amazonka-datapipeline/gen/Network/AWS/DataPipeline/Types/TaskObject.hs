{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.TaskObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.TaskObject where

import Network.AWS.DataPipeline.Types.PipelineObject
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a pipeline task that is assigned to a task runner.
--
--
--
-- /See:/ 'taskObject' smart constructor.
data TaskObject = TaskObject'
  { _toPipelineId :: !(Maybe Text),
    _toAttemptId :: !(Maybe Text),
    _toTaskId :: !(Maybe Text),
    _toObjects :: !(Maybe (Map Text (PipelineObject)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TaskObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'toPipelineId' - The ID of the pipeline that provided the task.
--
-- * 'toAttemptId' - The ID of the pipeline task attempt object. AWS Data Pipeline uses this value to track how many times a task is attempted.
--
-- * 'toTaskId' - An internal identifier for the task. This ID is passed to the 'SetTaskStatus' and 'ReportTaskProgress' actions.
--
-- * 'toObjects' - Connection information for the location where the task runner will publish the output of the task.
taskObject ::
  TaskObject
taskObject =
  TaskObject'
    { _toPipelineId = Nothing,
      _toAttemptId = Nothing,
      _toTaskId = Nothing,
      _toObjects = Nothing
    }

-- | The ID of the pipeline that provided the task.
toPipelineId :: Lens' TaskObject (Maybe Text)
toPipelineId = lens _toPipelineId (\s a -> s {_toPipelineId = a})

-- | The ID of the pipeline task attempt object. AWS Data Pipeline uses this value to track how many times a task is attempted.
toAttemptId :: Lens' TaskObject (Maybe Text)
toAttemptId = lens _toAttemptId (\s a -> s {_toAttemptId = a})

-- | An internal identifier for the task. This ID is passed to the 'SetTaskStatus' and 'ReportTaskProgress' actions.
toTaskId :: Lens' TaskObject (Maybe Text)
toTaskId = lens _toTaskId (\s a -> s {_toTaskId = a})

-- | Connection information for the location where the task runner will publish the output of the task.
toObjects :: Lens' TaskObject (HashMap Text (PipelineObject))
toObjects = lens _toObjects (\s a -> s {_toObjects = a}) . _Default . _Map

instance FromJSON TaskObject where
  parseJSON =
    withObject
      "TaskObject"
      ( \x ->
          TaskObject'
            <$> (x .:? "pipelineId")
            <*> (x .:? "attemptId")
            <*> (x .:? "taskId")
            <*> (x .:? "objects" .!= mempty)
      )

instance Hashable TaskObject

instance NFData TaskObject
