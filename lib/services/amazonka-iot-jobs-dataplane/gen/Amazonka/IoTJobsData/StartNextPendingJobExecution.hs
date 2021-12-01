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
-- Module      : Amazonka.IoTJobsData.StartNextPendingJobExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets and starts the next pending (status IN_PROGRESS or QUEUED) job
-- execution for a thing.
module Amazonka.IoTJobsData.StartNextPendingJobExecution
  ( -- * Creating a Request
    StartNextPendingJobExecution (..),
    newStartNextPendingJobExecution,

    -- * Request Lenses
    startNextPendingJobExecution_stepTimeoutInMinutes,
    startNextPendingJobExecution_statusDetails,
    startNextPendingJobExecution_thingName,

    -- * Destructuring the Response
    StartNextPendingJobExecutionResponse (..),
    newStartNextPendingJobExecutionResponse,

    -- * Response Lenses
    startNextPendingJobExecutionResponse_execution,
    startNextPendingJobExecutionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTJobsData.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartNextPendingJobExecution' smart constructor.
data StartNextPendingJobExecution = StartNextPendingJobExecution'
  { -- | Specifies the amount of time this device has to finish execution of this
    -- job. If the job execution status is not set to a terminal state before
    -- this timer expires, or before the timer is reset (by calling
    -- @UpdateJobExecution@, setting the status to @IN_PROGRESS@ and specifying
    -- a new timeout value in field @stepTimeoutInMinutes@) the job execution
    -- status will be automatically set to @TIMED_OUT@. Note that setting this
    -- timeout has no effect on that job execution timeout which may have been
    -- specified when the job was created (@CreateJob@ using field
    -- @timeoutConfig@).
    stepTimeoutInMinutes :: Prelude.Maybe Prelude.Integer,
    -- | A collection of name\/value pairs that describe the status of the job
    -- execution. If not specified, the statusDetails are unchanged.
    statusDetails :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the thing associated with the device.
    thingName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartNextPendingJobExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stepTimeoutInMinutes', 'startNextPendingJobExecution_stepTimeoutInMinutes' - Specifies the amount of time this device has to finish execution of this
-- job. If the job execution status is not set to a terminal state before
-- this timer expires, or before the timer is reset (by calling
-- @UpdateJobExecution@, setting the status to @IN_PROGRESS@ and specifying
-- a new timeout value in field @stepTimeoutInMinutes@) the job execution
-- status will be automatically set to @TIMED_OUT@. Note that setting this
-- timeout has no effect on that job execution timeout which may have been
-- specified when the job was created (@CreateJob@ using field
-- @timeoutConfig@).
--
-- 'statusDetails', 'startNextPendingJobExecution_statusDetails' - A collection of name\/value pairs that describe the status of the job
-- execution. If not specified, the statusDetails are unchanged.
--
-- 'thingName', 'startNextPendingJobExecution_thingName' - The name of the thing associated with the device.
newStartNextPendingJobExecution ::
  -- | 'thingName'
  Prelude.Text ->
  StartNextPendingJobExecution
newStartNextPendingJobExecution pThingName_ =
  StartNextPendingJobExecution'
    { stepTimeoutInMinutes =
        Prelude.Nothing,
      statusDetails = Prelude.Nothing,
      thingName = pThingName_
    }

-- | Specifies the amount of time this device has to finish execution of this
-- job. If the job execution status is not set to a terminal state before
-- this timer expires, or before the timer is reset (by calling
-- @UpdateJobExecution@, setting the status to @IN_PROGRESS@ and specifying
-- a new timeout value in field @stepTimeoutInMinutes@) the job execution
-- status will be automatically set to @TIMED_OUT@. Note that setting this
-- timeout has no effect on that job execution timeout which may have been
-- specified when the job was created (@CreateJob@ using field
-- @timeoutConfig@).
startNextPendingJobExecution_stepTimeoutInMinutes :: Lens.Lens' StartNextPendingJobExecution (Prelude.Maybe Prelude.Integer)
startNextPendingJobExecution_stepTimeoutInMinutes = Lens.lens (\StartNextPendingJobExecution' {stepTimeoutInMinutes} -> stepTimeoutInMinutes) (\s@StartNextPendingJobExecution' {} a -> s {stepTimeoutInMinutes = a} :: StartNextPendingJobExecution)

-- | A collection of name\/value pairs that describe the status of the job
-- execution. If not specified, the statusDetails are unchanged.
startNextPendingJobExecution_statusDetails :: Lens.Lens' StartNextPendingJobExecution (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startNextPendingJobExecution_statusDetails = Lens.lens (\StartNextPendingJobExecution' {statusDetails} -> statusDetails) (\s@StartNextPendingJobExecution' {} a -> s {statusDetails = a} :: StartNextPendingJobExecution) Prelude.. Lens.mapping Lens.coerced

-- | The name of the thing associated with the device.
startNextPendingJobExecution_thingName :: Lens.Lens' StartNextPendingJobExecution Prelude.Text
startNextPendingJobExecution_thingName = Lens.lens (\StartNextPendingJobExecution' {thingName} -> thingName) (\s@StartNextPendingJobExecution' {} a -> s {thingName = a} :: StartNextPendingJobExecution)

instance Core.AWSRequest StartNextPendingJobExecution where
  type
    AWSResponse StartNextPendingJobExecution =
      StartNextPendingJobExecutionResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartNextPendingJobExecutionResponse'
            Prelude.<$> (x Core..?> "execution")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartNextPendingJobExecution
  where
  hashWithSalt salt' StartNextPendingJobExecution' {..} =
    salt' `Prelude.hashWithSalt` thingName
      `Prelude.hashWithSalt` statusDetails
      `Prelude.hashWithSalt` stepTimeoutInMinutes

instance Prelude.NFData StartNextPendingJobExecution where
  rnf StartNextPendingJobExecution' {..} =
    Prelude.rnf stepTimeoutInMinutes
      `Prelude.seq` Prelude.rnf thingName
      `Prelude.seq` Prelude.rnf statusDetails

instance Core.ToHeaders StartNextPendingJobExecution where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON StartNextPendingJobExecution where
  toJSON StartNextPendingJobExecution' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("stepTimeoutInMinutes" Core..=)
              Prelude.<$> stepTimeoutInMinutes,
            ("statusDetails" Core..=) Prelude.<$> statusDetails
          ]
      )

instance Core.ToPath StartNextPendingJobExecution where
  toPath StartNextPendingJobExecution' {..} =
    Prelude.mconcat
      ["/things/", Core.toBS thingName, "/jobs/$next"]

instance Core.ToQuery StartNextPendingJobExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartNextPendingJobExecutionResponse' smart constructor.
data StartNextPendingJobExecutionResponse = StartNextPendingJobExecutionResponse'
  { -- | A JobExecution object.
    execution :: Prelude.Maybe JobExecution,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartNextPendingJobExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'execution', 'startNextPendingJobExecutionResponse_execution' - A JobExecution object.
--
-- 'httpStatus', 'startNextPendingJobExecutionResponse_httpStatus' - The response's http status code.
newStartNextPendingJobExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartNextPendingJobExecutionResponse
newStartNextPendingJobExecutionResponse pHttpStatus_ =
  StartNextPendingJobExecutionResponse'
    { execution =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A JobExecution object.
startNextPendingJobExecutionResponse_execution :: Lens.Lens' StartNextPendingJobExecutionResponse (Prelude.Maybe JobExecution)
startNextPendingJobExecutionResponse_execution = Lens.lens (\StartNextPendingJobExecutionResponse' {execution} -> execution) (\s@StartNextPendingJobExecutionResponse' {} a -> s {execution = a} :: StartNextPendingJobExecutionResponse)

-- | The response's http status code.
startNextPendingJobExecutionResponse_httpStatus :: Lens.Lens' StartNextPendingJobExecutionResponse Prelude.Int
startNextPendingJobExecutionResponse_httpStatus = Lens.lens (\StartNextPendingJobExecutionResponse' {httpStatus} -> httpStatus) (\s@StartNextPendingJobExecutionResponse' {} a -> s {httpStatus = a} :: StartNextPendingJobExecutionResponse)

instance
  Prelude.NFData
    StartNextPendingJobExecutionResponse
  where
  rnf StartNextPendingJobExecutionResponse' {..} =
    Prelude.rnf execution
      `Prelude.seq` Prelude.rnf httpStatus
