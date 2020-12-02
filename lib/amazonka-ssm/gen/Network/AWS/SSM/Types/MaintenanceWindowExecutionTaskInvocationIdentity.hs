{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowExecutionTaskInvocationIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowExecutionTaskInvocationIdentity where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.MaintenanceWindowExecutionStatus
import Network.AWS.SSM.Types.MaintenanceWindowTaskType

-- | Describes the information about a task invocation for a particular target as part of a task execution performed as part of a maintenance window execution.
--
--
--
-- /See:/ 'maintenanceWindowExecutionTaskInvocationIdentity' smart constructor.
data MaintenanceWindowExecutionTaskInvocationIdentity = MaintenanceWindowExecutionTaskInvocationIdentity'
  { _mwetiiStatus ::
      !( Maybe
           MaintenanceWindowExecutionStatus
       ),
    _mwetiiExecutionId ::
      !( Maybe
           Text
       ),
    _mwetiiTaskExecutionId ::
      !( Maybe
           Text
       ),
    _mwetiiStartTime ::
      !( Maybe
           POSIX
       ),
    _mwetiiInvocationId ::
      !( Maybe
           Text
       ),
    _mwetiiOwnerInformation ::
      !( Maybe
           ( Sensitive
               Text
           )
       ),
    _mwetiiTaskType ::
      !( Maybe
           MaintenanceWindowTaskType
       ),
    _mwetiiWindowTargetId ::
      !( Maybe
           Text
       ),
    _mwetiiWindowExecutionId ::
      !( Maybe
           Text
       ),
    _mwetiiStatusDetails ::
      !( Maybe
           Text
       ),
    _mwetiiEndTime ::
      !( Maybe
           POSIX
       ),
    _mwetiiParameters ::
      !( Maybe
           ( Sensitive
               Text
           )
       )
  }
  deriving
    ( Eq,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'MaintenanceWindowExecutionTaskInvocationIdentity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwetiiStatus' - The status of the task invocation.
--
-- * 'mwetiiExecutionId' - The ID of the action performed in the service that actually handled the task invocation. If the task type is RUN_COMMAND, this value is the command ID.
--
-- * 'mwetiiTaskExecutionId' - The ID of the specific task execution in the maintenance window execution.
--
-- * 'mwetiiStartTime' - The time the invocation started.
--
-- * 'mwetiiInvocationId' - The ID of the task invocation.
--
-- * 'mwetiiOwnerInformation' - User-provided value that was specified when the target was registered with the maintenance window. This was also included in any CloudWatch events raised during the task invocation.
--
-- * 'mwetiiTaskType' - The task type.
--
-- * 'mwetiiWindowTargetId' - The ID of the target definition in this maintenance window the invocation was performed for.
--
-- * 'mwetiiWindowExecutionId' - The ID of the maintenance window execution that ran the task.
--
-- * 'mwetiiStatusDetails' - The details explaining the status of the task invocation. Only available for certain Status values.
--
-- * 'mwetiiEndTime' - The time the invocation finished.
--
-- * 'mwetiiParameters' - The parameters that were provided for the invocation when it was run.
maintenanceWindowExecutionTaskInvocationIdentity ::
  MaintenanceWindowExecutionTaskInvocationIdentity
maintenanceWindowExecutionTaskInvocationIdentity =
  MaintenanceWindowExecutionTaskInvocationIdentity'
    { _mwetiiStatus =
        Nothing,
      _mwetiiExecutionId = Nothing,
      _mwetiiTaskExecutionId = Nothing,
      _mwetiiStartTime = Nothing,
      _mwetiiInvocationId = Nothing,
      _mwetiiOwnerInformation = Nothing,
      _mwetiiTaskType = Nothing,
      _mwetiiWindowTargetId = Nothing,
      _mwetiiWindowExecutionId = Nothing,
      _mwetiiStatusDetails = Nothing,
      _mwetiiEndTime = Nothing,
      _mwetiiParameters = Nothing
    }

-- | The status of the task invocation.
mwetiiStatus :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe MaintenanceWindowExecutionStatus)
mwetiiStatus = lens _mwetiiStatus (\s a -> s {_mwetiiStatus = a})

-- | The ID of the action performed in the service that actually handled the task invocation. If the task type is RUN_COMMAND, this value is the command ID.
mwetiiExecutionId :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe Text)
mwetiiExecutionId = lens _mwetiiExecutionId (\s a -> s {_mwetiiExecutionId = a})

-- | The ID of the specific task execution in the maintenance window execution.
mwetiiTaskExecutionId :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe Text)
mwetiiTaskExecutionId = lens _mwetiiTaskExecutionId (\s a -> s {_mwetiiTaskExecutionId = a})

-- | The time the invocation started.
mwetiiStartTime :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe UTCTime)
mwetiiStartTime = lens _mwetiiStartTime (\s a -> s {_mwetiiStartTime = a}) . mapping _Time

-- | The ID of the task invocation.
mwetiiInvocationId :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe Text)
mwetiiInvocationId = lens _mwetiiInvocationId (\s a -> s {_mwetiiInvocationId = a})

-- | User-provided value that was specified when the target was registered with the maintenance window. This was also included in any CloudWatch events raised during the task invocation.
mwetiiOwnerInformation :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe Text)
mwetiiOwnerInformation = lens _mwetiiOwnerInformation (\s a -> s {_mwetiiOwnerInformation = a}) . mapping _Sensitive

-- | The task type.
mwetiiTaskType :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe MaintenanceWindowTaskType)
mwetiiTaskType = lens _mwetiiTaskType (\s a -> s {_mwetiiTaskType = a})

-- | The ID of the target definition in this maintenance window the invocation was performed for.
mwetiiWindowTargetId :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe Text)
mwetiiWindowTargetId = lens _mwetiiWindowTargetId (\s a -> s {_mwetiiWindowTargetId = a})

-- | The ID of the maintenance window execution that ran the task.
mwetiiWindowExecutionId :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe Text)
mwetiiWindowExecutionId = lens _mwetiiWindowExecutionId (\s a -> s {_mwetiiWindowExecutionId = a})

-- | The details explaining the status of the task invocation. Only available for certain Status values.
mwetiiStatusDetails :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe Text)
mwetiiStatusDetails = lens _mwetiiStatusDetails (\s a -> s {_mwetiiStatusDetails = a})

-- | The time the invocation finished.
mwetiiEndTime :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe UTCTime)
mwetiiEndTime = lens _mwetiiEndTime (\s a -> s {_mwetiiEndTime = a}) . mapping _Time

-- | The parameters that were provided for the invocation when it was run.
mwetiiParameters :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe Text)
mwetiiParameters = lens _mwetiiParameters (\s a -> s {_mwetiiParameters = a}) . mapping _Sensitive

instance FromJSON MaintenanceWindowExecutionTaskInvocationIdentity where
  parseJSON =
    withObject
      "MaintenanceWindowExecutionTaskInvocationIdentity"
      ( \x ->
          MaintenanceWindowExecutionTaskInvocationIdentity'
            <$> (x .:? "Status")
            <*> (x .:? "ExecutionId")
            <*> (x .:? "TaskExecutionId")
            <*> (x .:? "StartTime")
            <*> (x .:? "InvocationId")
            <*> (x .:? "OwnerInformation")
            <*> (x .:? "TaskType")
            <*> (x .:? "WindowTargetId")
            <*> (x .:? "WindowExecutionId")
            <*> (x .:? "StatusDetails")
            <*> (x .:? "EndTime")
            <*> (x .:? "Parameters")
      )

instance Hashable MaintenanceWindowExecutionTaskInvocationIdentity

instance NFData MaintenanceWindowExecutionTaskInvocationIdentity
