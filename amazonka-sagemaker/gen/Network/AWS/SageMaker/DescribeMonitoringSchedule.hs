{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeMonitoringSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the schedule for a monitoring job.
module Network.AWS.SageMaker.DescribeMonitoringSchedule
  ( -- * Creating a Request
    DescribeMonitoringSchedule (..),
    newDescribeMonitoringSchedule,

    -- * Request Lenses
    describeMonitoringSchedule_monitoringScheduleName,

    -- * Destructuring the Response
    DescribeMonitoringScheduleResponse (..),
    newDescribeMonitoringScheduleResponse,

    -- * Response Lenses
    describeMonitoringScheduleResponse_endpointName,
    describeMonitoringScheduleResponse_monitoringType,
    describeMonitoringScheduleResponse_failureReason,
    describeMonitoringScheduleResponse_lastMonitoringExecutionSummary,
    describeMonitoringScheduleResponse_httpStatus,
    describeMonitoringScheduleResponse_monitoringScheduleArn,
    describeMonitoringScheduleResponse_monitoringScheduleName,
    describeMonitoringScheduleResponse_monitoringScheduleStatus,
    describeMonitoringScheduleResponse_creationTime,
    describeMonitoringScheduleResponse_lastModifiedTime,
    describeMonitoringScheduleResponse_monitoringScheduleConfig,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeMonitoringSchedule' smart constructor.
data DescribeMonitoringSchedule = DescribeMonitoringSchedule'
  { -- | Name of a previously created monitoring schedule.
    monitoringScheduleName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMonitoringSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monitoringScheduleName', 'describeMonitoringSchedule_monitoringScheduleName' - Name of a previously created monitoring schedule.
newDescribeMonitoringSchedule ::
  -- | 'monitoringScheduleName'
  Core.Text ->
  DescribeMonitoringSchedule
newDescribeMonitoringSchedule
  pMonitoringScheduleName_ =
    DescribeMonitoringSchedule'
      { monitoringScheduleName =
          pMonitoringScheduleName_
      }

-- | Name of a previously created monitoring schedule.
describeMonitoringSchedule_monitoringScheduleName :: Lens.Lens' DescribeMonitoringSchedule Core.Text
describeMonitoringSchedule_monitoringScheduleName = Lens.lens (\DescribeMonitoringSchedule' {monitoringScheduleName} -> monitoringScheduleName) (\s@DescribeMonitoringSchedule' {} a -> s {monitoringScheduleName = a} :: DescribeMonitoringSchedule)

instance Core.AWSRequest DescribeMonitoringSchedule where
  type
    AWSResponse DescribeMonitoringSchedule =
      DescribeMonitoringScheduleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMonitoringScheduleResponse'
            Core.<$> (x Core..?> "EndpointName")
            Core.<*> (x Core..?> "MonitoringType")
            Core.<*> (x Core..?> "FailureReason")
            Core.<*> (x Core..?> "LastMonitoringExecutionSummary")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "MonitoringScheduleArn")
            Core.<*> (x Core..:> "MonitoringScheduleName")
            Core.<*> (x Core..:> "MonitoringScheduleStatus")
            Core.<*> (x Core..:> "CreationTime")
            Core.<*> (x Core..:> "LastModifiedTime")
            Core.<*> (x Core..:> "MonitoringScheduleConfig")
      )

instance Core.Hashable DescribeMonitoringSchedule

instance Core.NFData DescribeMonitoringSchedule

instance Core.ToHeaders DescribeMonitoringSchedule where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeMonitoringSchedule" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeMonitoringSchedule where
  toJSON DescribeMonitoringSchedule' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "MonitoringScheduleName"
                  Core..= monitoringScheduleName
              )
          ]
      )

instance Core.ToPath DescribeMonitoringSchedule where
  toPath = Core.const "/"

instance Core.ToQuery DescribeMonitoringSchedule where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeMonitoringScheduleResponse' smart constructor.
data DescribeMonitoringScheduleResponse = DescribeMonitoringScheduleResponse'
  { -- | The name of the endpoint for the monitoring job.
    endpointName :: Core.Maybe Core.Text,
    -- | The type of the monitoring job that this schedule runs. This is one of
    -- the following values.
    --
    -- -   @DATA_QUALITY@ - The schedule is for a data quality monitoring job.
    --
    -- -   @MODEL_QUALITY@ - The schedule is for a model quality monitoring
    --     job.
    --
    -- -   @MODEL_BIAS@ - The schedule is for a bias monitoring job.
    --
    -- -   @MODEL_EXPLAINABILITY@ - The schedule is for an explainability
    --     monitoring job.
    monitoringType :: Core.Maybe MonitoringType,
    -- | A string, up to one KB in size, that contains the reason a monitoring
    -- job failed, if it failed.
    failureReason :: Core.Maybe Core.Text,
    -- | Describes metadata on the last execution to run, if there was one.
    lastMonitoringExecutionSummary :: Core.Maybe MonitoringExecutionSummary,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The Amazon Resource Name (ARN) of the monitoring schedule.
    monitoringScheduleArn :: Core.Text,
    -- | Name of the monitoring schedule.
    monitoringScheduleName :: Core.Text,
    -- | The status of an monitoring job.
    monitoringScheduleStatus :: ScheduleStatus,
    -- | The time at which the monitoring job was created.
    creationTime :: Core.POSIX,
    -- | The time at which the monitoring job was last modified.
    lastModifiedTime :: Core.POSIX,
    -- | The configuration object that specifies the monitoring schedule and
    -- defines the monitoring job.
    monitoringScheduleConfig :: MonitoringScheduleConfig
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMonitoringScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointName', 'describeMonitoringScheduleResponse_endpointName' - The name of the endpoint for the monitoring job.
--
-- 'monitoringType', 'describeMonitoringScheduleResponse_monitoringType' - The type of the monitoring job that this schedule runs. This is one of
-- the following values.
--
-- -   @DATA_QUALITY@ - The schedule is for a data quality monitoring job.
--
-- -   @MODEL_QUALITY@ - The schedule is for a model quality monitoring
--     job.
--
-- -   @MODEL_BIAS@ - The schedule is for a bias monitoring job.
--
-- -   @MODEL_EXPLAINABILITY@ - The schedule is for an explainability
--     monitoring job.
--
-- 'failureReason', 'describeMonitoringScheduleResponse_failureReason' - A string, up to one KB in size, that contains the reason a monitoring
-- job failed, if it failed.
--
-- 'lastMonitoringExecutionSummary', 'describeMonitoringScheduleResponse_lastMonitoringExecutionSummary' - Describes metadata on the last execution to run, if there was one.
--
-- 'httpStatus', 'describeMonitoringScheduleResponse_httpStatus' - The response's http status code.
--
-- 'monitoringScheduleArn', 'describeMonitoringScheduleResponse_monitoringScheduleArn' - The Amazon Resource Name (ARN) of the monitoring schedule.
--
-- 'monitoringScheduleName', 'describeMonitoringScheduleResponse_monitoringScheduleName' - Name of the monitoring schedule.
--
-- 'monitoringScheduleStatus', 'describeMonitoringScheduleResponse_monitoringScheduleStatus' - The status of an monitoring job.
--
-- 'creationTime', 'describeMonitoringScheduleResponse_creationTime' - The time at which the monitoring job was created.
--
-- 'lastModifiedTime', 'describeMonitoringScheduleResponse_lastModifiedTime' - The time at which the monitoring job was last modified.
--
-- 'monitoringScheduleConfig', 'describeMonitoringScheduleResponse_monitoringScheduleConfig' - The configuration object that specifies the monitoring schedule and
-- defines the monitoring job.
newDescribeMonitoringScheduleResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'monitoringScheduleArn'
  Core.Text ->
  -- | 'monitoringScheduleName'
  Core.Text ->
  -- | 'monitoringScheduleStatus'
  ScheduleStatus ->
  -- | 'creationTime'
  Core.UTCTime ->
  -- | 'lastModifiedTime'
  Core.UTCTime ->
  -- | 'monitoringScheduleConfig'
  MonitoringScheduleConfig ->
  DescribeMonitoringScheduleResponse
newDescribeMonitoringScheduleResponse
  pHttpStatus_
  pMonitoringScheduleArn_
  pMonitoringScheduleName_
  pMonitoringScheduleStatus_
  pCreationTime_
  pLastModifiedTime_
  pMonitoringScheduleConfig_ =
    DescribeMonitoringScheduleResponse'
      { endpointName =
          Core.Nothing,
        monitoringType = Core.Nothing,
        failureReason = Core.Nothing,
        lastMonitoringExecutionSummary =
          Core.Nothing,
        httpStatus = pHttpStatus_,
        monitoringScheduleArn =
          pMonitoringScheduleArn_,
        monitoringScheduleName =
          pMonitoringScheduleName_,
        monitoringScheduleStatus =
          pMonitoringScheduleStatus_,
        creationTime =
          Core._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Core._Time Lens.# pLastModifiedTime_,
        monitoringScheduleConfig =
          pMonitoringScheduleConfig_
      }

-- | The name of the endpoint for the monitoring job.
describeMonitoringScheduleResponse_endpointName :: Lens.Lens' DescribeMonitoringScheduleResponse (Core.Maybe Core.Text)
describeMonitoringScheduleResponse_endpointName = Lens.lens (\DescribeMonitoringScheduleResponse' {endpointName} -> endpointName) (\s@DescribeMonitoringScheduleResponse' {} a -> s {endpointName = a} :: DescribeMonitoringScheduleResponse)

-- | The type of the monitoring job that this schedule runs. This is one of
-- the following values.
--
-- -   @DATA_QUALITY@ - The schedule is for a data quality monitoring job.
--
-- -   @MODEL_QUALITY@ - The schedule is for a model quality monitoring
--     job.
--
-- -   @MODEL_BIAS@ - The schedule is for a bias monitoring job.
--
-- -   @MODEL_EXPLAINABILITY@ - The schedule is for an explainability
--     monitoring job.
describeMonitoringScheduleResponse_monitoringType :: Lens.Lens' DescribeMonitoringScheduleResponse (Core.Maybe MonitoringType)
describeMonitoringScheduleResponse_monitoringType = Lens.lens (\DescribeMonitoringScheduleResponse' {monitoringType} -> monitoringType) (\s@DescribeMonitoringScheduleResponse' {} a -> s {monitoringType = a} :: DescribeMonitoringScheduleResponse)

-- | A string, up to one KB in size, that contains the reason a monitoring
-- job failed, if it failed.
describeMonitoringScheduleResponse_failureReason :: Lens.Lens' DescribeMonitoringScheduleResponse (Core.Maybe Core.Text)
describeMonitoringScheduleResponse_failureReason = Lens.lens (\DescribeMonitoringScheduleResponse' {failureReason} -> failureReason) (\s@DescribeMonitoringScheduleResponse' {} a -> s {failureReason = a} :: DescribeMonitoringScheduleResponse)

-- | Describes metadata on the last execution to run, if there was one.
describeMonitoringScheduleResponse_lastMonitoringExecutionSummary :: Lens.Lens' DescribeMonitoringScheduleResponse (Core.Maybe MonitoringExecutionSummary)
describeMonitoringScheduleResponse_lastMonitoringExecutionSummary = Lens.lens (\DescribeMonitoringScheduleResponse' {lastMonitoringExecutionSummary} -> lastMonitoringExecutionSummary) (\s@DescribeMonitoringScheduleResponse' {} a -> s {lastMonitoringExecutionSummary = a} :: DescribeMonitoringScheduleResponse)

-- | The response's http status code.
describeMonitoringScheduleResponse_httpStatus :: Lens.Lens' DescribeMonitoringScheduleResponse Core.Int
describeMonitoringScheduleResponse_httpStatus = Lens.lens (\DescribeMonitoringScheduleResponse' {httpStatus} -> httpStatus) (\s@DescribeMonitoringScheduleResponse' {} a -> s {httpStatus = a} :: DescribeMonitoringScheduleResponse)

-- | The Amazon Resource Name (ARN) of the monitoring schedule.
describeMonitoringScheduleResponse_monitoringScheduleArn :: Lens.Lens' DescribeMonitoringScheduleResponse Core.Text
describeMonitoringScheduleResponse_monitoringScheduleArn = Lens.lens (\DescribeMonitoringScheduleResponse' {monitoringScheduleArn} -> monitoringScheduleArn) (\s@DescribeMonitoringScheduleResponse' {} a -> s {monitoringScheduleArn = a} :: DescribeMonitoringScheduleResponse)

-- | Name of the monitoring schedule.
describeMonitoringScheduleResponse_monitoringScheduleName :: Lens.Lens' DescribeMonitoringScheduleResponse Core.Text
describeMonitoringScheduleResponse_monitoringScheduleName = Lens.lens (\DescribeMonitoringScheduleResponse' {monitoringScheduleName} -> monitoringScheduleName) (\s@DescribeMonitoringScheduleResponse' {} a -> s {monitoringScheduleName = a} :: DescribeMonitoringScheduleResponse)

-- | The status of an monitoring job.
describeMonitoringScheduleResponse_monitoringScheduleStatus :: Lens.Lens' DescribeMonitoringScheduleResponse ScheduleStatus
describeMonitoringScheduleResponse_monitoringScheduleStatus = Lens.lens (\DescribeMonitoringScheduleResponse' {monitoringScheduleStatus} -> monitoringScheduleStatus) (\s@DescribeMonitoringScheduleResponse' {} a -> s {monitoringScheduleStatus = a} :: DescribeMonitoringScheduleResponse)

-- | The time at which the monitoring job was created.
describeMonitoringScheduleResponse_creationTime :: Lens.Lens' DescribeMonitoringScheduleResponse Core.UTCTime
describeMonitoringScheduleResponse_creationTime = Lens.lens (\DescribeMonitoringScheduleResponse' {creationTime} -> creationTime) (\s@DescribeMonitoringScheduleResponse' {} a -> s {creationTime = a} :: DescribeMonitoringScheduleResponse) Core.. Core._Time

-- | The time at which the monitoring job was last modified.
describeMonitoringScheduleResponse_lastModifiedTime :: Lens.Lens' DescribeMonitoringScheduleResponse Core.UTCTime
describeMonitoringScheduleResponse_lastModifiedTime = Lens.lens (\DescribeMonitoringScheduleResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeMonitoringScheduleResponse' {} a -> s {lastModifiedTime = a} :: DescribeMonitoringScheduleResponse) Core.. Core._Time

-- | The configuration object that specifies the monitoring schedule and
-- defines the monitoring job.
describeMonitoringScheduleResponse_monitoringScheduleConfig :: Lens.Lens' DescribeMonitoringScheduleResponse MonitoringScheduleConfig
describeMonitoringScheduleResponse_monitoringScheduleConfig = Lens.lens (\DescribeMonitoringScheduleResponse' {monitoringScheduleConfig} -> monitoringScheduleConfig) (\s@DescribeMonitoringScheduleResponse' {} a -> s {monitoringScheduleConfig = a} :: DescribeMonitoringScheduleResponse)

instance
  Core.NFData
    DescribeMonitoringScheduleResponse
