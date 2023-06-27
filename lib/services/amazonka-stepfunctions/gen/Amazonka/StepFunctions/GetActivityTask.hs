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
-- Module      : Amazonka.StepFunctions.GetActivityTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used by workers to retrieve a task (with the specified activity ARN)
-- which has been scheduled for execution by a running state machine. This
-- initiates a long poll, where the service holds the HTTP connection open
-- and responds as soon as a task becomes available (i.e. an execution of a
-- task of this type is needed.) The maximum time the service holds on to
-- the request before responding is 60 seconds. If no task is available
-- within 60 seconds, the poll returns a @taskToken@ with a null string.
--
-- This API action isn\'t logged in CloudTrail.
--
-- Workers should set their client side socket timeout to at least 65
-- seconds (5 seconds higher than the maximum time the service may hold the
-- poll request).
--
-- Polling with @GetActivityTask@ can cause latency in some
-- implementations. See
-- <https://docs.aws.amazon.com/step-functions/latest/dg/bp-activity-pollers.html Avoid Latency When Polling for Activity Tasks>
-- in the Step Functions Developer Guide.
module Amazonka.StepFunctions.GetActivityTask
  ( -- * Creating a Request
    GetActivityTask (..),
    newGetActivityTask,

    -- * Request Lenses
    getActivityTask_workerName,
    getActivityTask_activityArn,

    -- * Destructuring the Response
    GetActivityTaskResponse (..),
    newGetActivityTaskResponse,

    -- * Response Lenses
    getActivityTaskResponse_input,
    getActivityTaskResponse_taskToken,
    getActivityTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StepFunctions.Types

-- | /See:/ 'newGetActivityTask' smart constructor.
data GetActivityTask = GetActivityTask'
  { -- | You can provide an arbitrary name in order to identify the worker that
    -- the task is assigned to. This name is used when it is logged in the
    -- execution history.
    workerName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the activity to retrieve tasks from
    -- (assigned when you create the task using CreateActivity.)
    activityArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetActivityTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workerName', 'getActivityTask_workerName' - You can provide an arbitrary name in order to identify the worker that
-- the task is assigned to. This name is used when it is logged in the
-- execution history.
--
-- 'activityArn', 'getActivityTask_activityArn' - The Amazon Resource Name (ARN) of the activity to retrieve tasks from
-- (assigned when you create the task using CreateActivity.)
newGetActivityTask ::
  -- | 'activityArn'
  Prelude.Text ->
  GetActivityTask
newGetActivityTask pActivityArn_ =
  GetActivityTask'
    { workerName = Prelude.Nothing,
      activityArn = pActivityArn_
    }

-- | You can provide an arbitrary name in order to identify the worker that
-- the task is assigned to. This name is used when it is logged in the
-- execution history.
getActivityTask_workerName :: Lens.Lens' GetActivityTask (Prelude.Maybe Prelude.Text)
getActivityTask_workerName = Lens.lens (\GetActivityTask' {workerName} -> workerName) (\s@GetActivityTask' {} a -> s {workerName = a} :: GetActivityTask)

-- | The Amazon Resource Name (ARN) of the activity to retrieve tasks from
-- (assigned when you create the task using CreateActivity.)
getActivityTask_activityArn :: Lens.Lens' GetActivityTask Prelude.Text
getActivityTask_activityArn = Lens.lens (\GetActivityTask' {activityArn} -> activityArn) (\s@GetActivityTask' {} a -> s {activityArn = a} :: GetActivityTask)

instance Core.AWSRequest GetActivityTask where
  type
    AWSResponse GetActivityTask =
      GetActivityTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetActivityTaskResponse'
            Prelude.<$> (x Data..?> "input")
            Prelude.<*> (x Data..?> "taskToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetActivityTask where
  hashWithSalt _salt GetActivityTask' {..} =
    _salt
      `Prelude.hashWithSalt` workerName
      `Prelude.hashWithSalt` activityArn

instance Prelude.NFData GetActivityTask where
  rnf GetActivityTask' {..} =
    Prelude.rnf workerName
      `Prelude.seq` Prelude.rnf activityArn

instance Data.ToHeaders GetActivityTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSStepFunctions.GetActivityTask" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetActivityTask where
  toJSON GetActivityTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("workerName" Data..=) Prelude.<$> workerName,
            Prelude.Just ("activityArn" Data..= activityArn)
          ]
      )

instance Data.ToPath GetActivityTask where
  toPath = Prelude.const "/"

instance Data.ToQuery GetActivityTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetActivityTaskResponse' smart constructor.
data GetActivityTaskResponse = GetActivityTaskResponse'
  { -- | The string that contains the JSON input data for the task. Length
    -- constraints apply to the payload size, and are expressed as bytes in
    -- UTF-8 encoding.
    input :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A token that identifies the scheduled task. This token must be copied
    -- and included in subsequent calls to SendTaskHeartbeat, SendTaskSuccess
    -- or SendTaskFailure in order to report the progress or completion of the
    -- task.
    taskToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetActivityTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'input', 'getActivityTaskResponse_input' - The string that contains the JSON input data for the task. Length
-- constraints apply to the payload size, and are expressed as bytes in
-- UTF-8 encoding.
--
-- 'taskToken', 'getActivityTaskResponse_taskToken' - A token that identifies the scheduled task. This token must be copied
-- and included in subsequent calls to SendTaskHeartbeat, SendTaskSuccess
-- or SendTaskFailure in order to report the progress or completion of the
-- task.
--
-- 'httpStatus', 'getActivityTaskResponse_httpStatus' - The response's http status code.
newGetActivityTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetActivityTaskResponse
newGetActivityTaskResponse pHttpStatus_ =
  GetActivityTaskResponse'
    { input = Prelude.Nothing,
      taskToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The string that contains the JSON input data for the task. Length
-- constraints apply to the payload size, and are expressed as bytes in
-- UTF-8 encoding.
getActivityTaskResponse_input :: Lens.Lens' GetActivityTaskResponse (Prelude.Maybe Prelude.Text)
getActivityTaskResponse_input = Lens.lens (\GetActivityTaskResponse' {input} -> input) (\s@GetActivityTaskResponse' {} a -> s {input = a} :: GetActivityTaskResponse) Prelude.. Lens.mapping Data._Sensitive

-- | A token that identifies the scheduled task. This token must be copied
-- and included in subsequent calls to SendTaskHeartbeat, SendTaskSuccess
-- or SendTaskFailure in order to report the progress or completion of the
-- task.
getActivityTaskResponse_taskToken :: Lens.Lens' GetActivityTaskResponse (Prelude.Maybe Prelude.Text)
getActivityTaskResponse_taskToken = Lens.lens (\GetActivityTaskResponse' {taskToken} -> taskToken) (\s@GetActivityTaskResponse' {} a -> s {taskToken = a} :: GetActivityTaskResponse)

-- | The response's http status code.
getActivityTaskResponse_httpStatus :: Lens.Lens' GetActivityTaskResponse Prelude.Int
getActivityTaskResponse_httpStatus = Lens.lens (\GetActivityTaskResponse' {httpStatus} -> httpStatus) (\s@GetActivityTaskResponse' {} a -> s {httpStatus = a} :: GetActivityTaskResponse)

instance Prelude.NFData GetActivityTaskResponse where
  rnf GetActivityTaskResponse' {..} =
    Prelude.rnf input
      `Prelude.seq` Prelude.rnf taskToken
      `Prelude.seq` Prelude.rnf httpStatus
