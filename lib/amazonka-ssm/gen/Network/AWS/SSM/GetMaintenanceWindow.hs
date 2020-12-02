{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetMaintenanceWindow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a maintenance window.
module Network.AWS.SSM.GetMaintenanceWindow
  ( -- * Creating a Request
    getMaintenanceWindow,
    GetMaintenanceWindow,

    -- * Request Lenses
    gmwWindowId,

    -- * Destructuring the Response
    getMaintenanceWindowResponse,
    GetMaintenanceWindowResponse,

    -- * Response Lenses
    gmwrsEnabled,
    gmwrsSchedule,
    gmwrsNextExecutionTime,
    gmwrsScheduleOffset,
    gmwrsEndDate,
    gmwrsScheduleTimezone,
    gmwrsStartDate,
    gmwrsCreatedDate,
    gmwrsName,
    gmwrsModifiedDate,
    gmwrsCutoff,
    gmwrsAllowUnassociatedTargets,
    gmwrsDescription,
    gmwrsDuration,
    gmwrsWindowId,
    gmwrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types

-- | /See:/ 'getMaintenanceWindow' smart constructor.
newtype GetMaintenanceWindow = GetMaintenanceWindow'
  { _gmwWindowId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetMaintenanceWindow' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmwWindowId' - The ID of the maintenance window for which you want to retrieve information.
getMaintenanceWindow ::
  -- | 'gmwWindowId'
  Text ->
  GetMaintenanceWindow
getMaintenanceWindow pWindowId_ =
  GetMaintenanceWindow' {_gmwWindowId = pWindowId_}

-- | The ID of the maintenance window for which you want to retrieve information.
gmwWindowId :: Lens' GetMaintenanceWindow Text
gmwWindowId = lens _gmwWindowId (\s a -> s {_gmwWindowId = a})

instance AWSRequest GetMaintenanceWindow where
  type Rs GetMaintenanceWindow = GetMaintenanceWindowResponse
  request = postJSON ssm
  response =
    receiveJSON
      ( \s h x ->
          GetMaintenanceWindowResponse'
            <$> (x .?> "Enabled")
            <*> (x .?> "Schedule")
            <*> (x .?> "NextExecutionTime")
            <*> (x .?> "ScheduleOffset")
            <*> (x .?> "EndDate")
            <*> (x .?> "ScheduleTimezone")
            <*> (x .?> "StartDate")
            <*> (x .?> "CreatedDate")
            <*> (x .?> "Name")
            <*> (x .?> "ModifiedDate")
            <*> (x .?> "Cutoff")
            <*> (x .?> "AllowUnassociatedTargets")
            <*> (x .?> "Description")
            <*> (x .?> "Duration")
            <*> (x .?> "WindowId")
            <*> (pure (fromEnum s))
      )

instance Hashable GetMaintenanceWindow

instance NFData GetMaintenanceWindow

instance ToHeaders GetMaintenanceWindow where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AmazonSSM.GetMaintenanceWindow" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetMaintenanceWindow where
  toJSON GetMaintenanceWindow' {..} =
    object (catMaybes [Just ("WindowId" .= _gmwWindowId)])

instance ToPath GetMaintenanceWindow where
  toPath = const "/"

instance ToQuery GetMaintenanceWindow where
  toQuery = const mempty

-- | /See:/ 'getMaintenanceWindowResponse' smart constructor.
data GetMaintenanceWindowResponse = GetMaintenanceWindowResponse'
  { _gmwrsEnabled ::
      !(Maybe Bool),
    _gmwrsSchedule :: !(Maybe Text),
    _gmwrsNextExecutionTime ::
      !(Maybe Text),
    _gmwrsScheduleOffset ::
      !(Maybe Nat),
    _gmwrsEndDate :: !(Maybe Text),
    _gmwrsScheduleTimezone ::
      !(Maybe Text),
    _gmwrsStartDate :: !(Maybe Text),
    _gmwrsCreatedDate ::
      !(Maybe POSIX),
    _gmwrsName :: !(Maybe Text),
    _gmwrsModifiedDate ::
      !(Maybe POSIX),
    _gmwrsCutoff :: !(Maybe Nat),
    _gmwrsAllowUnassociatedTargets ::
      !(Maybe Bool),
    _gmwrsDescription ::
      !(Maybe (Sensitive Text)),
    _gmwrsDuration :: !(Maybe Nat),
    _gmwrsWindowId :: !(Maybe Text),
    _gmwrsResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetMaintenanceWindowResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmwrsEnabled' - Indicates whether the maintenance window is enabled.
--
-- * 'gmwrsSchedule' - The schedule of the maintenance window in the form of a cron or rate expression.
--
-- * 'gmwrsNextExecutionTime' - The next time the maintenance window will actually run, taking into account any specified times for the maintenance window to become active or inactive.
--
-- * 'gmwrsScheduleOffset' - The number of days to wait to run a maintenance window after the scheduled CRON expression date and time.
--
-- * 'gmwrsEndDate' - The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become inactive. The maintenance window will not run after this specified time.
--
-- * 'gmwrsScheduleTimezone' - The time zone that the scheduled maintenance window executions are based on, in Internet Assigned Numbers Authority (IANA) format. For example: "America/Los_Angeles", "etc/UTC", or "Asia/Seoul". For more information, see the <https://www.iana.org/time-zones Time Zone Database> on the IANA website.
--
-- * 'gmwrsStartDate' - The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become active. The maintenance window will not run before this specified time.
--
-- * 'gmwrsCreatedDate' - The date the maintenance window was created.
--
-- * 'gmwrsName' - The name of the maintenance window.
--
-- * 'gmwrsModifiedDate' - The date the maintenance window was last modified.
--
-- * 'gmwrsCutoff' - The number of hours before the end of the maintenance window that Systems Manager stops scheduling new tasks for execution.
--
-- * 'gmwrsAllowUnassociatedTargets' - Whether targets must be registered with the maintenance window before tasks can be defined for those targets.
--
-- * 'gmwrsDescription' - The description of the maintenance window.
--
-- * 'gmwrsDuration' - The duration of the maintenance window in hours.
--
-- * 'gmwrsWindowId' - The ID of the created maintenance window.
--
-- * 'gmwrsResponseStatus' - -- | The response status code.
getMaintenanceWindowResponse ::
  -- | 'gmwrsResponseStatus'
  Int ->
  GetMaintenanceWindowResponse
getMaintenanceWindowResponse pResponseStatus_ =
  GetMaintenanceWindowResponse'
    { _gmwrsEnabled = Nothing,
      _gmwrsSchedule = Nothing,
      _gmwrsNextExecutionTime = Nothing,
      _gmwrsScheduleOffset = Nothing,
      _gmwrsEndDate = Nothing,
      _gmwrsScheduleTimezone = Nothing,
      _gmwrsStartDate = Nothing,
      _gmwrsCreatedDate = Nothing,
      _gmwrsName = Nothing,
      _gmwrsModifiedDate = Nothing,
      _gmwrsCutoff = Nothing,
      _gmwrsAllowUnassociatedTargets = Nothing,
      _gmwrsDescription = Nothing,
      _gmwrsDuration = Nothing,
      _gmwrsWindowId = Nothing,
      _gmwrsResponseStatus = pResponseStatus_
    }

-- | Indicates whether the maintenance window is enabled.
gmwrsEnabled :: Lens' GetMaintenanceWindowResponse (Maybe Bool)
gmwrsEnabled = lens _gmwrsEnabled (\s a -> s {_gmwrsEnabled = a})

-- | The schedule of the maintenance window in the form of a cron or rate expression.
gmwrsSchedule :: Lens' GetMaintenanceWindowResponse (Maybe Text)
gmwrsSchedule = lens _gmwrsSchedule (\s a -> s {_gmwrsSchedule = a})

-- | The next time the maintenance window will actually run, taking into account any specified times for the maintenance window to become active or inactive.
gmwrsNextExecutionTime :: Lens' GetMaintenanceWindowResponse (Maybe Text)
gmwrsNextExecutionTime = lens _gmwrsNextExecutionTime (\s a -> s {_gmwrsNextExecutionTime = a})

-- | The number of days to wait to run a maintenance window after the scheduled CRON expression date and time.
gmwrsScheduleOffset :: Lens' GetMaintenanceWindowResponse (Maybe Natural)
gmwrsScheduleOffset = lens _gmwrsScheduleOffset (\s a -> s {_gmwrsScheduleOffset = a}) . mapping _Nat

-- | The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become inactive. The maintenance window will not run after this specified time.
gmwrsEndDate :: Lens' GetMaintenanceWindowResponse (Maybe Text)
gmwrsEndDate = lens _gmwrsEndDate (\s a -> s {_gmwrsEndDate = a})

-- | The time zone that the scheduled maintenance window executions are based on, in Internet Assigned Numbers Authority (IANA) format. For example: "America/Los_Angeles", "etc/UTC", or "Asia/Seoul". For more information, see the <https://www.iana.org/time-zones Time Zone Database> on the IANA website.
gmwrsScheduleTimezone :: Lens' GetMaintenanceWindowResponse (Maybe Text)
gmwrsScheduleTimezone = lens _gmwrsScheduleTimezone (\s a -> s {_gmwrsScheduleTimezone = a})

-- | The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become active. The maintenance window will not run before this specified time.
gmwrsStartDate :: Lens' GetMaintenanceWindowResponse (Maybe Text)
gmwrsStartDate = lens _gmwrsStartDate (\s a -> s {_gmwrsStartDate = a})

-- | The date the maintenance window was created.
gmwrsCreatedDate :: Lens' GetMaintenanceWindowResponse (Maybe UTCTime)
gmwrsCreatedDate = lens _gmwrsCreatedDate (\s a -> s {_gmwrsCreatedDate = a}) . mapping _Time

-- | The name of the maintenance window.
gmwrsName :: Lens' GetMaintenanceWindowResponse (Maybe Text)
gmwrsName = lens _gmwrsName (\s a -> s {_gmwrsName = a})

-- | The date the maintenance window was last modified.
gmwrsModifiedDate :: Lens' GetMaintenanceWindowResponse (Maybe UTCTime)
gmwrsModifiedDate = lens _gmwrsModifiedDate (\s a -> s {_gmwrsModifiedDate = a}) . mapping _Time

-- | The number of hours before the end of the maintenance window that Systems Manager stops scheduling new tasks for execution.
gmwrsCutoff :: Lens' GetMaintenanceWindowResponse (Maybe Natural)
gmwrsCutoff = lens _gmwrsCutoff (\s a -> s {_gmwrsCutoff = a}) . mapping _Nat

-- | Whether targets must be registered with the maintenance window before tasks can be defined for those targets.
gmwrsAllowUnassociatedTargets :: Lens' GetMaintenanceWindowResponse (Maybe Bool)
gmwrsAllowUnassociatedTargets = lens _gmwrsAllowUnassociatedTargets (\s a -> s {_gmwrsAllowUnassociatedTargets = a})

-- | The description of the maintenance window.
gmwrsDescription :: Lens' GetMaintenanceWindowResponse (Maybe Text)
gmwrsDescription = lens _gmwrsDescription (\s a -> s {_gmwrsDescription = a}) . mapping _Sensitive

-- | The duration of the maintenance window in hours.
gmwrsDuration :: Lens' GetMaintenanceWindowResponse (Maybe Natural)
gmwrsDuration = lens _gmwrsDuration (\s a -> s {_gmwrsDuration = a}) . mapping _Nat

-- | The ID of the created maintenance window.
gmwrsWindowId :: Lens' GetMaintenanceWindowResponse (Maybe Text)
gmwrsWindowId = lens _gmwrsWindowId (\s a -> s {_gmwrsWindowId = a})

-- | -- | The response status code.
gmwrsResponseStatus :: Lens' GetMaintenanceWindowResponse Int
gmwrsResponseStatus = lens _gmwrsResponseStatus (\s a -> s {_gmwrsResponseStatus = a})

instance NFData GetMaintenanceWindowResponse
