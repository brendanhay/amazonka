{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowTask where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.LoggingInfo
import Network.AWS.SSM.Types.MaintenanceWindowTaskParameterValueExpression
import Network.AWS.SSM.Types.MaintenanceWindowTaskType
import Network.AWS.SSM.Types.Target

-- | Information about a task defined for a maintenance window.
--
--
--
-- /See:/ 'maintenanceWindowTask' smart constructor.
data MaintenanceWindowTask = MaintenanceWindowTask'
  { _mwtServiceRoleARN ::
      !(Maybe Text),
    _mwtWindowTaskId :: !(Maybe Text),
    _mwtTaskParameters ::
      !( Maybe
           ( Sensitive
               ( Map
                   Text
                   ( Sensitive
                       MaintenanceWindowTaskParameterValueExpression
                   )
               )
           )
       ),
    _mwtPriority :: !(Maybe Nat),
    _mwtTaskARN :: !(Maybe Text),
    _mwtMaxErrors :: !(Maybe Text),
    _mwtName :: !(Maybe Text),
    _mwtTargets :: !(Maybe [Target]),
    _mwtLoggingInfo :: !(Maybe LoggingInfo),
    _mwtType :: !(Maybe MaintenanceWindowTaskType),
    _mwtDescription :: !(Maybe (Sensitive Text)),
    _mwtMaxConcurrency :: !(Maybe Text),
    _mwtWindowId :: !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'MaintenanceWindowTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwtServiceRoleARN' - The ARN of the IAM service role to use to publish Amazon Simple Notification Service (Amazon SNS) notifications for maintenance window Run Command tasks.
--
-- * 'mwtWindowTaskId' - The task ID.
--
-- * 'mwtTaskParameters' - The parameters that should be passed to the task when it is run.
--
-- * 'mwtPriority' - The priority of the task in the maintenance window. The lower the number, the higher the priority. Tasks that have the same priority are scheduled in parallel.
--
-- * 'mwtTaskARN' - The resource that the task uses during execution. For RUN_COMMAND and AUTOMATION task types, @TaskArn@ is the Systems Manager document name or ARN. For LAMBDA tasks, it's the function name or ARN. For STEP_FUNCTIONS tasks, it's the state machine ARN.
--
-- * 'mwtMaxErrors' - The maximum number of errors allowed before this task stops being scheduled.
--
-- * 'mwtName' - The task name.
--
-- * 'mwtTargets' - The targets (either instances or tags). Instances are specified using Key=instanceids,Values=<instanceid1>,<instanceid2>. Tags are specified using Key=<tag name>,Values=<tag value>.
--
-- * 'mwtLoggingInfo' - Information about an S3 bucket to write task-level logs to.
--
-- * 'mwtType' - The type of task. The type can be one of the following: RUN_COMMAND, AUTOMATION, LAMBDA, or STEP_FUNCTIONS.
--
-- * 'mwtDescription' - A description of the task.
--
-- * 'mwtMaxConcurrency' - The maximum number of targets this task can be run for, in parallel.
--
-- * 'mwtWindowId' - The ID of the maintenance window where the task is registered.
maintenanceWindowTask ::
  MaintenanceWindowTask
maintenanceWindowTask =
  MaintenanceWindowTask'
    { _mwtServiceRoleARN = Nothing,
      _mwtWindowTaskId = Nothing,
      _mwtTaskParameters = Nothing,
      _mwtPriority = Nothing,
      _mwtTaskARN = Nothing,
      _mwtMaxErrors = Nothing,
      _mwtName = Nothing,
      _mwtTargets = Nothing,
      _mwtLoggingInfo = Nothing,
      _mwtType = Nothing,
      _mwtDescription = Nothing,
      _mwtMaxConcurrency = Nothing,
      _mwtWindowId = Nothing
    }

-- | The ARN of the IAM service role to use to publish Amazon Simple Notification Service (Amazon SNS) notifications for maintenance window Run Command tasks.
mwtServiceRoleARN :: Lens' MaintenanceWindowTask (Maybe Text)
mwtServiceRoleARN = lens _mwtServiceRoleARN (\s a -> s {_mwtServiceRoleARN = a})

-- | The task ID.
mwtWindowTaskId :: Lens' MaintenanceWindowTask (Maybe Text)
mwtWindowTaskId = lens _mwtWindowTaskId (\s a -> s {_mwtWindowTaskId = a})

-- | The parameters that should be passed to the task when it is run.
mwtTaskParameters :: Lens' MaintenanceWindowTask (Maybe (HashMap Text (MaintenanceWindowTaskParameterValueExpression)))
mwtTaskParameters = lens _mwtTaskParameters (\s a -> s {_mwtTaskParameters = a}) . mapping (_Sensitive . _Map)

-- | The priority of the task in the maintenance window. The lower the number, the higher the priority. Tasks that have the same priority are scheduled in parallel.
mwtPriority :: Lens' MaintenanceWindowTask (Maybe Natural)
mwtPriority = lens _mwtPriority (\s a -> s {_mwtPriority = a}) . mapping _Nat

-- | The resource that the task uses during execution. For RUN_COMMAND and AUTOMATION task types, @TaskArn@ is the Systems Manager document name or ARN. For LAMBDA tasks, it's the function name or ARN. For STEP_FUNCTIONS tasks, it's the state machine ARN.
mwtTaskARN :: Lens' MaintenanceWindowTask (Maybe Text)
mwtTaskARN = lens _mwtTaskARN (\s a -> s {_mwtTaskARN = a})

-- | The maximum number of errors allowed before this task stops being scheduled.
mwtMaxErrors :: Lens' MaintenanceWindowTask (Maybe Text)
mwtMaxErrors = lens _mwtMaxErrors (\s a -> s {_mwtMaxErrors = a})

-- | The task name.
mwtName :: Lens' MaintenanceWindowTask (Maybe Text)
mwtName = lens _mwtName (\s a -> s {_mwtName = a})

-- | The targets (either instances or tags). Instances are specified using Key=instanceids,Values=<instanceid1>,<instanceid2>. Tags are specified using Key=<tag name>,Values=<tag value>.
mwtTargets :: Lens' MaintenanceWindowTask [Target]
mwtTargets = lens _mwtTargets (\s a -> s {_mwtTargets = a}) . _Default . _Coerce

-- | Information about an S3 bucket to write task-level logs to.
mwtLoggingInfo :: Lens' MaintenanceWindowTask (Maybe LoggingInfo)
mwtLoggingInfo = lens _mwtLoggingInfo (\s a -> s {_mwtLoggingInfo = a})

-- | The type of task. The type can be one of the following: RUN_COMMAND, AUTOMATION, LAMBDA, or STEP_FUNCTIONS.
mwtType :: Lens' MaintenanceWindowTask (Maybe MaintenanceWindowTaskType)
mwtType = lens _mwtType (\s a -> s {_mwtType = a})

-- | A description of the task.
mwtDescription :: Lens' MaintenanceWindowTask (Maybe Text)
mwtDescription = lens _mwtDescription (\s a -> s {_mwtDescription = a}) . mapping _Sensitive

-- | The maximum number of targets this task can be run for, in parallel.
mwtMaxConcurrency :: Lens' MaintenanceWindowTask (Maybe Text)
mwtMaxConcurrency = lens _mwtMaxConcurrency (\s a -> s {_mwtMaxConcurrency = a})

-- | The ID of the maintenance window where the task is registered.
mwtWindowId :: Lens' MaintenanceWindowTask (Maybe Text)
mwtWindowId = lens _mwtWindowId (\s a -> s {_mwtWindowId = a})

instance FromJSON MaintenanceWindowTask where
  parseJSON =
    withObject
      "MaintenanceWindowTask"
      ( \x ->
          MaintenanceWindowTask'
            <$> (x .:? "ServiceRoleArn")
            <*> (x .:? "WindowTaskId")
            <*> (x .:? "TaskParameters" .!= mempty)
            <*> (x .:? "Priority")
            <*> (x .:? "TaskArn")
            <*> (x .:? "MaxErrors")
            <*> (x .:? "Name")
            <*> (x .:? "Targets" .!= mempty)
            <*> (x .:? "LoggingInfo")
            <*> (x .:? "Type")
            <*> (x .:? "Description")
            <*> (x .:? "MaxConcurrency")
            <*> (x .:? "WindowId")
      )

instance Hashable MaintenanceWindowTask

instance NFData MaintenanceWindowTask
