{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TaskRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TaskRun where

import Network.AWS.Glue.Types.TaskRunProperties
import Network.AWS.Glue.Types.TaskStatusType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The sampling parameters that are associated with the machine learning transform.
--
--
--
-- /See:/ 'taskRun' smart constructor.
data TaskRun = TaskRun'
  { _trCompletedOn :: !(Maybe POSIX),
    _trStatus :: !(Maybe TaskStatusType),
    _trLastModifiedOn :: !(Maybe POSIX),
    _trErrorString :: !(Maybe Text),
    _trStartedOn :: !(Maybe POSIX),
    _trLogGroupName :: !(Maybe Text),
    _trExecutionTime :: !(Maybe Int),
    _trProperties :: !(Maybe TaskRunProperties),
    _trTransformId :: !(Maybe Text),
    _trTaskRunId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TaskRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trCompletedOn' - The last point in time that the requested task run was completed.
--
-- * 'trStatus' - The current status of the requested task run.
--
-- * 'trLastModifiedOn' - The last point in time that the requested task run was updated.
--
-- * 'trErrorString' - The list of error strings associated with this task run.
--
-- * 'trStartedOn' - The date and time that this task run started.
--
-- * 'trLogGroupName' - The names of the log group for secure logging, associated with this task run.
--
-- * 'trExecutionTime' - The amount of time (in seconds) that the task run consumed resources.
--
-- * 'trProperties' - Specifies configuration properties associated with this task run.
--
-- * 'trTransformId' - The unique identifier for the transform.
--
-- * 'trTaskRunId' - The unique identifier for this task run.
taskRun ::
  TaskRun
taskRun =
  TaskRun'
    { _trCompletedOn = Nothing,
      _trStatus = Nothing,
      _trLastModifiedOn = Nothing,
      _trErrorString = Nothing,
      _trStartedOn = Nothing,
      _trLogGroupName = Nothing,
      _trExecutionTime = Nothing,
      _trProperties = Nothing,
      _trTransformId = Nothing,
      _trTaskRunId = Nothing
    }

-- | The last point in time that the requested task run was completed.
trCompletedOn :: Lens' TaskRun (Maybe UTCTime)
trCompletedOn = lens _trCompletedOn (\s a -> s {_trCompletedOn = a}) . mapping _Time

-- | The current status of the requested task run.
trStatus :: Lens' TaskRun (Maybe TaskStatusType)
trStatus = lens _trStatus (\s a -> s {_trStatus = a})

-- | The last point in time that the requested task run was updated.
trLastModifiedOn :: Lens' TaskRun (Maybe UTCTime)
trLastModifiedOn = lens _trLastModifiedOn (\s a -> s {_trLastModifiedOn = a}) . mapping _Time

-- | The list of error strings associated with this task run.
trErrorString :: Lens' TaskRun (Maybe Text)
trErrorString = lens _trErrorString (\s a -> s {_trErrorString = a})

-- | The date and time that this task run started.
trStartedOn :: Lens' TaskRun (Maybe UTCTime)
trStartedOn = lens _trStartedOn (\s a -> s {_trStartedOn = a}) . mapping _Time

-- | The names of the log group for secure logging, associated with this task run.
trLogGroupName :: Lens' TaskRun (Maybe Text)
trLogGroupName = lens _trLogGroupName (\s a -> s {_trLogGroupName = a})

-- | The amount of time (in seconds) that the task run consumed resources.
trExecutionTime :: Lens' TaskRun (Maybe Int)
trExecutionTime = lens _trExecutionTime (\s a -> s {_trExecutionTime = a})

-- | Specifies configuration properties associated with this task run.
trProperties :: Lens' TaskRun (Maybe TaskRunProperties)
trProperties = lens _trProperties (\s a -> s {_trProperties = a})

-- | The unique identifier for the transform.
trTransformId :: Lens' TaskRun (Maybe Text)
trTransformId = lens _trTransformId (\s a -> s {_trTransformId = a})

-- | The unique identifier for this task run.
trTaskRunId :: Lens' TaskRun (Maybe Text)
trTaskRunId = lens _trTaskRunId (\s a -> s {_trTaskRunId = a})

instance FromJSON TaskRun where
  parseJSON =
    withObject
      "TaskRun"
      ( \x ->
          TaskRun'
            <$> (x .:? "CompletedOn")
            <*> (x .:? "Status")
            <*> (x .:? "LastModifiedOn")
            <*> (x .:? "ErrorString")
            <*> (x .:? "StartedOn")
            <*> (x .:? "LogGroupName")
            <*> (x .:? "ExecutionTime")
            <*> (x .:? "Properties")
            <*> (x .:? "TransformId")
            <*> (x .:? "TaskRunId")
      )

instance Hashable TaskRun

instance NFData TaskRun
