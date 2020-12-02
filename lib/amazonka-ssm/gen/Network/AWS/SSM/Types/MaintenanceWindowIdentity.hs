{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowIdentity where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the maintenance window.
--
--
--
-- /See:/ 'maintenanceWindowIdentity' smart constructor.
data MaintenanceWindowIdentity = MaintenanceWindowIdentity'
  { _mwiEnabled ::
      !(Maybe Bool),
    _mwiSchedule :: !(Maybe Text),
    _mwiNextExecutionTime :: !(Maybe Text),
    _mwiScheduleOffset :: !(Maybe Nat),
    _mwiEndDate :: !(Maybe Text),
    _mwiScheduleTimezone :: !(Maybe Text),
    _mwiStartDate :: !(Maybe Text),
    _mwiName :: !(Maybe Text),
    _mwiCutoff :: !(Maybe Nat),
    _mwiDescription ::
      !(Maybe (Sensitive Text)),
    _mwiDuration :: !(Maybe Nat),
    _mwiWindowId :: !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'MaintenanceWindowIdentity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwiEnabled' - Indicates whether the maintenance window is enabled.
--
-- * 'mwiSchedule' - The schedule of the maintenance window in the form of a cron or rate expression.
--
-- * 'mwiNextExecutionTime' - The next time the maintenance window will actually run, taking into account any specified times for the maintenance window to become active or inactive.
--
-- * 'mwiScheduleOffset' - The number of days to wait to run a maintenance window after the scheduled CRON expression date and time.
--
-- * 'mwiEndDate' - The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become inactive.
--
-- * 'mwiScheduleTimezone' - The time zone that the scheduled maintenance window executions are based on, in Internet Assigned Numbers Authority (IANA) format.
--
-- * 'mwiStartDate' - The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become active.
--
-- * 'mwiName' - The name of the maintenance window.
--
-- * 'mwiCutoff' - The number of hours before the end of the maintenance window that Systems Manager stops scheduling new tasks for execution.
--
-- * 'mwiDescription' - A description of the maintenance window.
--
-- * 'mwiDuration' - The duration of the maintenance window in hours.
--
-- * 'mwiWindowId' - The ID of the maintenance window.
maintenanceWindowIdentity ::
  MaintenanceWindowIdentity
maintenanceWindowIdentity =
  MaintenanceWindowIdentity'
    { _mwiEnabled = Nothing,
      _mwiSchedule = Nothing,
      _mwiNextExecutionTime = Nothing,
      _mwiScheduleOffset = Nothing,
      _mwiEndDate = Nothing,
      _mwiScheduleTimezone = Nothing,
      _mwiStartDate = Nothing,
      _mwiName = Nothing,
      _mwiCutoff = Nothing,
      _mwiDescription = Nothing,
      _mwiDuration = Nothing,
      _mwiWindowId = Nothing
    }

-- | Indicates whether the maintenance window is enabled.
mwiEnabled :: Lens' MaintenanceWindowIdentity (Maybe Bool)
mwiEnabled = lens _mwiEnabled (\s a -> s {_mwiEnabled = a})

-- | The schedule of the maintenance window in the form of a cron or rate expression.
mwiSchedule :: Lens' MaintenanceWindowIdentity (Maybe Text)
mwiSchedule = lens _mwiSchedule (\s a -> s {_mwiSchedule = a})

-- | The next time the maintenance window will actually run, taking into account any specified times for the maintenance window to become active or inactive.
mwiNextExecutionTime :: Lens' MaintenanceWindowIdentity (Maybe Text)
mwiNextExecutionTime = lens _mwiNextExecutionTime (\s a -> s {_mwiNextExecutionTime = a})

-- | The number of days to wait to run a maintenance window after the scheduled CRON expression date and time.
mwiScheduleOffset :: Lens' MaintenanceWindowIdentity (Maybe Natural)
mwiScheduleOffset = lens _mwiScheduleOffset (\s a -> s {_mwiScheduleOffset = a}) . mapping _Nat

-- | The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become inactive.
mwiEndDate :: Lens' MaintenanceWindowIdentity (Maybe Text)
mwiEndDate = lens _mwiEndDate (\s a -> s {_mwiEndDate = a})

-- | The time zone that the scheduled maintenance window executions are based on, in Internet Assigned Numbers Authority (IANA) format.
mwiScheduleTimezone :: Lens' MaintenanceWindowIdentity (Maybe Text)
mwiScheduleTimezone = lens _mwiScheduleTimezone (\s a -> s {_mwiScheduleTimezone = a})

-- | The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become active.
mwiStartDate :: Lens' MaintenanceWindowIdentity (Maybe Text)
mwiStartDate = lens _mwiStartDate (\s a -> s {_mwiStartDate = a})

-- | The name of the maintenance window.
mwiName :: Lens' MaintenanceWindowIdentity (Maybe Text)
mwiName = lens _mwiName (\s a -> s {_mwiName = a})

-- | The number of hours before the end of the maintenance window that Systems Manager stops scheduling new tasks for execution.
mwiCutoff :: Lens' MaintenanceWindowIdentity (Maybe Natural)
mwiCutoff = lens _mwiCutoff (\s a -> s {_mwiCutoff = a}) . mapping _Nat

-- | A description of the maintenance window.
mwiDescription :: Lens' MaintenanceWindowIdentity (Maybe Text)
mwiDescription = lens _mwiDescription (\s a -> s {_mwiDescription = a}) . mapping _Sensitive

-- | The duration of the maintenance window in hours.
mwiDuration :: Lens' MaintenanceWindowIdentity (Maybe Natural)
mwiDuration = lens _mwiDuration (\s a -> s {_mwiDuration = a}) . mapping _Nat

-- | The ID of the maintenance window.
mwiWindowId :: Lens' MaintenanceWindowIdentity (Maybe Text)
mwiWindowId = lens _mwiWindowId (\s a -> s {_mwiWindowId = a})

instance FromJSON MaintenanceWindowIdentity where
  parseJSON =
    withObject
      "MaintenanceWindowIdentity"
      ( \x ->
          MaintenanceWindowIdentity'
            <$> (x .:? "Enabled")
            <*> (x .:? "Schedule")
            <*> (x .:? "NextExecutionTime")
            <*> (x .:? "ScheduleOffset")
            <*> (x .:? "EndDate")
            <*> (x .:? "ScheduleTimezone")
            <*> (x .:? "StartDate")
            <*> (x .:? "Name")
            <*> (x .:? "Cutoff")
            <*> (x .:? "Description")
            <*> (x .:? "Duration")
            <*> (x .:? "WindowId")
      )

instance Hashable MaintenanceWindowIdentity

instance NFData MaintenanceWindowIdentity
