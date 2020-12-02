{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowExecution where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.MaintenanceWindowExecutionStatus

-- | Describes the information about an execution of a maintenance window.
--
--
--
-- /See:/ 'maintenanceWindowExecution' smart constructor.
data MaintenanceWindowExecution = MaintenanceWindowExecution'
  { _mweStatus ::
      !( Maybe
           MaintenanceWindowExecutionStatus
       ),
    _mweStartTime :: !(Maybe POSIX),
    _mweWindowExecutionId ::
      !(Maybe Text),
    _mweStatusDetails :: !(Maybe Text),
    _mweEndTime :: !(Maybe POSIX),
    _mweWindowId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MaintenanceWindowExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mweStatus' - The status of the execution.
--
-- * 'mweStartTime' - The time the execution started.
--
-- * 'mweWindowExecutionId' - The ID of the maintenance window execution.
--
-- * 'mweStatusDetails' - The details explaining the Status. Only available for certain status values.
--
-- * 'mweEndTime' - The time the execution finished.
--
-- * 'mweWindowId' - The ID of the maintenance window.
maintenanceWindowExecution ::
  MaintenanceWindowExecution
maintenanceWindowExecution =
  MaintenanceWindowExecution'
    { _mweStatus = Nothing,
      _mweStartTime = Nothing,
      _mweWindowExecutionId = Nothing,
      _mweStatusDetails = Nothing,
      _mweEndTime = Nothing,
      _mweWindowId = Nothing
    }

-- | The status of the execution.
mweStatus :: Lens' MaintenanceWindowExecution (Maybe MaintenanceWindowExecutionStatus)
mweStatus = lens _mweStatus (\s a -> s {_mweStatus = a})

-- | The time the execution started.
mweStartTime :: Lens' MaintenanceWindowExecution (Maybe UTCTime)
mweStartTime = lens _mweStartTime (\s a -> s {_mweStartTime = a}) . mapping _Time

-- | The ID of the maintenance window execution.
mweWindowExecutionId :: Lens' MaintenanceWindowExecution (Maybe Text)
mweWindowExecutionId = lens _mweWindowExecutionId (\s a -> s {_mweWindowExecutionId = a})

-- | The details explaining the Status. Only available for certain status values.
mweStatusDetails :: Lens' MaintenanceWindowExecution (Maybe Text)
mweStatusDetails = lens _mweStatusDetails (\s a -> s {_mweStatusDetails = a})

-- | The time the execution finished.
mweEndTime :: Lens' MaintenanceWindowExecution (Maybe UTCTime)
mweEndTime = lens _mweEndTime (\s a -> s {_mweEndTime = a}) . mapping _Time

-- | The ID of the maintenance window.
mweWindowId :: Lens' MaintenanceWindowExecution (Maybe Text)
mweWindowId = lens _mweWindowId (\s a -> s {_mweWindowId = a})

instance FromJSON MaintenanceWindowExecution where
  parseJSON =
    withObject
      "MaintenanceWindowExecution"
      ( \x ->
          MaintenanceWindowExecution'
            <$> (x .:? "Status")
            <*> (x .:? "StartTime")
            <*> (x .:? "WindowExecutionId")
            <*> (x .:? "StatusDetails")
            <*> (x .:? "EndTime")
            <*> (x .:? "WindowId")
      )

instance Hashable MaintenanceWindowExecution

instance NFData MaintenanceWindowExecution
