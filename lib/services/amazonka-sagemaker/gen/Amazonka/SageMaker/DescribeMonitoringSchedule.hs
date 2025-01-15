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
-- Module      : Amazonka.SageMaker.DescribeMonitoringSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the schedule for a monitoring job.
module Amazonka.SageMaker.DescribeMonitoringSchedule
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
    describeMonitoringScheduleResponse_failureReason,
    describeMonitoringScheduleResponse_lastMonitoringExecutionSummary,
    describeMonitoringScheduleResponse_monitoringType,
    describeMonitoringScheduleResponse_httpStatus,
    describeMonitoringScheduleResponse_monitoringScheduleArn,
    describeMonitoringScheduleResponse_monitoringScheduleName,
    describeMonitoringScheduleResponse_monitoringScheduleStatus,
    describeMonitoringScheduleResponse_creationTime,
    describeMonitoringScheduleResponse_lastModifiedTime,
    describeMonitoringScheduleResponse_monitoringScheduleConfig,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeMonitoringSchedule' smart constructor.
data DescribeMonitoringSchedule = DescribeMonitoringSchedule'
  { -- | Name of a previously created monitoring schedule.
    monitoringScheduleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeMonitoringSchedule
newDescribeMonitoringSchedule
  pMonitoringScheduleName_ =
    DescribeMonitoringSchedule'
      { monitoringScheduleName =
          pMonitoringScheduleName_
      }

-- | Name of a previously created monitoring schedule.
describeMonitoringSchedule_monitoringScheduleName :: Lens.Lens' DescribeMonitoringSchedule Prelude.Text
describeMonitoringSchedule_monitoringScheduleName = Lens.lens (\DescribeMonitoringSchedule' {monitoringScheduleName} -> monitoringScheduleName) (\s@DescribeMonitoringSchedule' {} a -> s {monitoringScheduleName = a} :: DescribeMonitoringSchedule)

instance Core.AWSRequest DescribeMonitoringSchedule where
  type
    AWSResponse DescribeMonitoringSchedule =
      DescribeMonitoringScheduleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMonitoringScheduleResponse'
            Prelude.<$> (x Data..?> "EndpointName")
            Prelude.<*> (x Data..?> "FailureReason")
            Prelude.<*> (x Data..?> "LastMonitoringExecutionSummary")
            Prelude.<*> (x Data..?> "MonitoringType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "MonitoringScheduleArn")
            Prelude.<*> (x Data..:> "MonitoringScheduleName")
            Prelude.<*> (x Data..:> "MonitoringScheduleStatus")
            Prelude.<*> (x Data..:> "CreationTime")
            Prelude.<*> (x Data..:> "LastModifiedTime")
            Prelude.<*> (x Data..:> "MonitoringScheduleConfig")
      )

instance Prelude.Hashable DescribeMonitoringSchedule where
  hashWithSalt _salt DescribeMonitoringSchedule' {..} =
    _salt `Prelude.hashWithSalt` monitoringScheduleName

instance Prelude.NFData DescribeMonitoringSchedule where
  rnf DescribeMonitoringSchedule' {..} =
    Prelude.rnf monitoringScheduleName

instance Data.ToHeaders DescribeMonitoringSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DescribeMonitoringSchedule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeMonitoringSchedule where
  toJSON DescribeMonitoringSchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "MonitoringScheduleName"
                  Data..= monitoringScheduleName
              )
          ]
      )

instance Data.ToPath DescribeMonitoringSchedule where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeMonitoringSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeMonitoringScheduleResponse' smart constructor.
data DescribeMonitoringScheduleResponse = DescribeMonitoringScheduleResponse'
  { -- | The name of the endpoint for the monitoring job.
    endpointName :: Prelude.Maybe Prelude.Text,
    -- | A string, up to one KB in size, that contains the reason a monitoring
    -- job failed, if it failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | Describes metadata on the last execution to run, if there was one.
    lastMonitoringExecutionSummary :: Prelude.Maybe MonitoringExecutionSummary,
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
    monitoringType :: Prelude.Maybe MonitoringType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the monitoring schedule.
    monitoringScheduleArn :: Prelude.Text,
    -- | Name of the monitoring schedule.
    monitoringScheduleName :: Prelude.Text,
    -- | The status of an monitoring job.
    monitoringScheduleStatus :: ScheduleStatus,
    -- | The time at which the monitoring job was created.
    creationTime :: Data.POSIX,
    -- | The time at which the monitoring job was last modified.
    lastModifiedTime :: Data.POSIX,
    -- | The configuration object that specifies the monitoring schedule and
    -- defines the monitoring job.
    monitoringScheduleConfig :: MonitoringScheduleConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'failureReason', 'describeMonitoringScheduleResponse_failureReason' - A string, up to one KB in size, that contains the reason a monitoring
-- job failed, if it failed.
--
-- 'lastMonitoringExecutionSummary', 'describeMonitoringScheduleResponse_lastMonitoringExecutionSummary' - Describes metadata on the last execution to run, if there was one.
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
  Prelude.Int ->
  -- | 'monitoringScheduleArn'
  Prelude.Text ->
  -- | 'monitoringScheduleName'
  Prelude.Text ->
  -- | 'monitoringScheduleStatus'
  ScheduleStatus ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
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
          Prelude.Nothing,
        failureReason = Prelude.Nothing,
        lastMonitoringExecutionSummary =
          Prelude.Nothing,
        monitoringType = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        monitoringScheduleArn =
          pMonitoringScheduleArn_,
        monitoringScheduleName =
          pMonitoringScheduleName_,
        monitoringScheduleStatus =
          pMonitoringScheduleStatus_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_,
        monitoringScheduleConfig =
          pMonitoringScheduleConfig_
      }

-- | The name of the endpoint for the monitoring job.
describeMonitoringScheduleResponse_endpointName :: Lens.Lens' DescribeMonitoringScheduleResponse (Prelude.Maybe Prelude.Text)
describeMonitoringScheduleResponse_endpointName = Lens.lens (\DescribeMonitoringScheduleResponse' {endpointName} -> endpointName) (\s@DescribeMonitoringScheduleResponse' {} a -> s {endpointName = a} :: DescribeMonitoringScheduleResponse)

-- | A string, up to one KB in size, that contains the reason a monitoring
-- job failed, if it failed.
describeMonitoringScheduleResponse_failureReason :: Lens.Lens' DescribeMonitoringScheduleResponse (Prelude.Maybe Prelude.Text)
describeMonitoringScheduleResponse_failureReason = Lens.lens (\DescribeMonitoringScheduleResponse' {failureReason} -> failureReason) (\s@DescribeMonitoringScheduleResponse' {} a -> s {failureReason = a} :: DescribeMonitoringScheduleResponse)

-- | Describes metadata on the last execution to run, if there was one.
describeMonitoringScheduleResponse_lastMonitoringExecutionSummary :: Lens.Lens' DescribeMonitoringScheduleResponse (Prelude.Maybe MonitoringExecutionSummary)
describeMonitoringScheduleResponse_lastMonitoringExecutionSummary = Lens.lens (\DescribeMonitoringScheduleResponse' {lastMonitoringExecutionSummary} -> lastMonitoringExecutionSummary) (\s@DescribeMonitoringScheduleResponse' {} a -> s {lastMonitoringExecutionSummary = a} :: DescribeMonitoringScheduleResponse)

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
describeMonitoringScheduleResponse_monitoringType :: Lens.Lens' DescribeMonitoringScheduleResponse (Prelude.Maybe MonitoringType)
describeMonitoringScheduleResponse_monitoringType = Lens.lens (\DescribeMonitoringScheduleResponse' {monitoringType} -> monitoringType) (\s@DescribeMonitoringScheduleResponse' {} a -> s {monitoringType = a} :: DescribeMonitoringScheduleResponse)

-- | The response's http status code.
describeMonitoringScheduleResponse_httpStatus :: Lens.Lens' DescribeMonitoringScheduleResponse Prelude.Int
describeMonitoringScheduleResponse_httpStatus = Lens.lens (\DescribeMonitoringScheduleResponse' {httpStatus} -> httpStatus) (\s@DescribeMonitoringScheduleResponse' {} a -> s {httpStatus = a} :: DescribeMonitoringScheduleResponse)

-- | The Amazon Resource Name (ARN) of the monitoring schedule.
describeMonitoringScheduleResponse_monitoringScheduleArn :: Lens.Lens' DescribeMonitoringScheduleResponse Prelude.Text
describeMonitoringScheduleResponse_monitoringScheduleArn = Lens.lens (\DescribeMonitoringScheduleResponse' {monitoringScheduleArn} -> monitoringScheduleArn) (\s@DescribeMonitoringScheduleResponse' {} a -> s {monitoringScheduleArn = a} :: DescribeMonitoringScheduleResponse)

-- | Name of the monitoring schedule.
describeMonitoringScheduleResponse_monitoringScheduleName :: Lens.Lens' DescribeMonitoringScheduleResponse Prelude.Text
describeMonitoringScheduleResponse_monitoringScheduleName = Lens.lens (\DescribeMonitoringScheduleResponse' {monitoringScheduleName} -> monitoringScheduleName) (\s@DescribeMonitoringScheduleResponse' {} a -> s {monitoringScheduleName = a} :: DescribeMonitoringScheduleResponse)

-- | The status of an monitoring job.
describeMonitoringScheduleResponse_monitoringScheduleStatus :: Lens.Lens' DescribeMonitoringScheduleResponse ScheduleStatus
describeMonitoringScheduleResponse_monitoringScheduleStatus = Lens.lens (\DescribeMonitoringScheduleResponse' {monitoringScheduleStatus} -> monitoringScheduleStatus) (\s@DescribeMonitoringScheduleResponse' {} a -> s {monitoringScheduleStatus = a} :: DescribeMonitoringScheduleResponse)

-- | The time at which the monitoring job was created.
describeMonitoringScheduleResponse_creationTime :: Lens.Lens' DescribeMonitoringScheduleResponse Prelude.UTCTime
describeMonitoringScheduleResponse_creationTime = Lens.lens (\DescribeMonitoringScheduleResponse' {creationTime} -> creationTime) (\s@DescribeMonitoringScheduleResponse' {} a -> s {creationTime = a} :: DescribeMonitoringScheduleResponse) Prelude.. Data._Time

-- | The time at which the monitoring job was last modified.
describeMonitoringScheduleResponse_lastModifiedTime :: Lens.Lens' DescribeMonitoringScheduleResponse Prelude.UTCTime
describeMonitoringScheduleResponse_lastModifiedTime = Lens.lens (\DescribeMonitoringScheduleResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeMonitoringScheduleResponse' {} a -> s {lastModifiedTime = a} :: DescribeMonitoringScheduleResponse) Prelude.. Data._Time

-- | The configuration object that specifies the monitoring schedule and
-- defines the monitoring job.
describeMonitoringScheduleResponse_monitoringScheduleConfig :: Lens.Lens' DescribeMonitoringScheduleResponse MonitoringScheduleConfig
describeMonitoringScheduleResponse_monitoringScheduleConfig = Lens.lens (\DescribeMonitoringScheduleResponse' {monitoringScheduleConfig} -> monitoringScheduleConfig) (\s@DescribeMonitoringScheduleResponse' {} a -> s {monitoringScheduleConfig = a} :: DescribeMonitoringScheduleResponse)

instance
  Prelude.NFData
    DescribeMonitoringScheduleResponse
  where
  rnf DescribeMonitoringScheduleResponse' {..} =
    Prelude.rnf endpointName `Prelude.seq`
      Prelude.rnf failureReason `Prelude.seq`
        Prelude.rnf lastMonitoringExecutionSummary `Prelude.seq`
          Prelude.rnf monitoringType `Prelude.seq`
            Prelude.rnf httpStatus `Prelude.seq`
              Prelude.rnf monitoringScheduleArn `Prelude.seq`
                Prelude.rnf monitoringScheduleName `Prelude.seq`
                  Prelude.rnf monitoringScheduleStatus `Prelude.seq`
                    Prelude.rnf creationTime `Prelude.seq`
                      Prelude.rnf lastModifiedTime `Prelude.seq`
                        Prelude.rnf monitoringScheduleConfig
