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
-- Module      : Amazonka.DataPipeline.PollForTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Task runners call @PollForTask@ to receive a task to perform from AWS
-- Data Pipeline. The task runner specifies which tasks it can perform by
-- setting a value for the @workerGroup@ parameter. The task returned can
-- come from any of the pipelines that match the @workerGroup@ value passed
-- in by the task runner and that was launched using the IAM user
-- credentials specified by the task runner.
--
-- If tasks are ready in the work queue, @PollForTask@ returns a response
-- immediately. If no tasks are available in the queue, @PollForTask@ uses
-- long-polling and holds on to a poll connection for up to a 90 seconds,
-- during which time the first newly scheduled task is handed to the task
-- runner. To accomodate this, set the socket timeout in your task runner
-- to 90 seconds. The task runner should not call @PollForTask@ again on
-- the same @workerGroup@ until it receives a response, and this can take
-- up to 90 seconds.
module Amazonka.DataPipeline.PollForTask
  ( -- * Creating a Request
    PollForTask (..),
    newPollForTask,

    -- * Request Lenses
    pollForTask_hostname,
    pollForTask_instanceIdentity,
    pollForTask_workerGroup,

    -- * Destructuring the Response
    PollForTaskResponse (..),
    newPollForTaskResponse,

    -- * Response Lenses
    pollForTaskResponse_taskObject,
    pollForTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataPipeline.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for PollForTask.
--
-- /See:/ 'newPollForTask' smart constructor.
data PollForTask = PollForTask'
  { -- | The public DNS name of the calling task runner.
    hostname :: Prelude.Maybe Prelude.Text,
    -- | Identity information for the EC2 instance that is hosting the task
    -- runner. You can get this value from the instance using
    -- @http:\/\/169.254.169.254\/latest\/meta-data\/instance-id@. For more
    -- information, see
    -- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AESDG-chapter-instancedata.html Instance Metadata>
    -- in the /Amazon Elastic Compute Cloud User Guide./ Passing in this value
    -- proves that your task runner is running on an EC2 instance, and ensures
    -- the proper AWS Data Pipeline service charges are applied to your
    -- pipeline.
    instanceIdentity :: Prelude.Maybe InstanceIdentity,
    -- | The type of task the task runner is configured to accept and process.
    -- The worker group is set as a field on objects in the pipeline when they
    -- are created. You can only specify a single value for @workerGroup@ in
    -- the call to @PollForTask@. There are no wildcard values permitted in
    -- @workerGroup@; the string must be an exact, case-sensitive, match.
    workerGroup :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PollForTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostname', 'pollForTask_hostname' - The public DNS name of the calling task runner.
--
-- 'instanceIdentity', 'pollForTask_instanceIdentity' - Identity information for the EC2 instance that is hosting the task
-- runner. You can get this value from the instance using
-- @http:\/\/169.254.169.254\/latest\/meta-data\/instance-id@. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AESDG-chapter-instancedata.html Instance Metadata>
-- in the /Amazon Elastic Compute Cloud User Guide./ Passing in this value
-- proves that your task runner is running on an EC2 instance, and ensures
-- the proper AWS Data Pipeline service charges are applied to your
-- pipeline.
--
-- 'workerGroup', 'pollForTask_workerGroup' - The type of task the task runner is configured to accept and process.
-- The worker group is set as a field on objects in the pipeline when they
-- are created. You can only specify a single value for @workerGroup@ in
-- the call to @PollForTask@. There are no wildcard values permitted in
-- @workerGroup@; the string must be an exact, case-sensitive, match.
newPollForTask ::
  -- | 'workerGroup'
  Prelude.Text ->
  PollForTask
newPollForTask pWorkerGroup_ =
  PollForTask'
    { hostname = Prelude.Nothing,
      instanceIdentity = Prelude.Nothing,
      workerGroup = pWorkerGroup_
    }

-- | The public DNS name of the calling task runner.
pollForTask_hostname :: Lens.Lens' PollForTask (Prelude.Maybe Prelude.Text)
pollForTask_hostname = Lens.lens (\PollForTask' {hostname} -> hostname) (\s@PollForTask' {} a -> s {hostname = a} :: PollForTask)

-- | Identity information for the EC2 instance that is hosting the task
-- runner. You can get this value from the instance using
-- @http:\/\/169.254.169.254\/latest\/meta-data\/instance-id@. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AESDG-chapter-instancedata.html Instance Metadata>
-- in the /Amazon Elastic Compute Cloud User Guide./ Passing in this value
-- proves that your task runner is running on an EC2 instance, and ensures
-- the proper AWS Data Pipeline service charges are applied to your
-- pipeline.
pollForTask_instanceIdentity :: Lens.Lens' PollForTask (Prelude.Maybe InstanceIdentity)
pollForTask_instanceIdentity = Lens.lens (\PollForTask' {instanceIdentity} -> instanceIdentity) (\s@PollForTask' {} a -> s {instanceIdentity = a} :: PollForTask)

-- | The type of task the task runner is configured to accept and process.
-- The worker group is set as a field on objects in the pipeline when they
-- are created. You can only specify a single value for @workerGroup@ in
-- the call to @PollForTask@. There are no wildcard values permitted in
-- @workerGroup@; the string must be an exact, case-sensitive, match.
pollForTask_workerGroup :: Lens.Lens' PollForTask Prelude.Text
pollForTask_workerGroup = Lens.lens (\PollForTask' {workerGroup} -> workerGroup) (\s@PollForTask' {} a -> s {workerGroup = a} :: PollForTask)

instance Core.AWSRequest PollForTask where
  type AWSResponse PollForTask = PollForTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PollForTaskResponse'
            Prelude.<$> (x Data..?> "taskObject")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PollForTask where
  hashWithSalt _salt PollForTask' {..} =
    _salt
      `Prelude.hashWithSalt` hostname
      `Prelude.hashWithSalt` instanceIdentity
      `Prelude.hashWithSalt` workerGroup

instance Prelude.NFData PollForTask where
  rnf PollForTask' {..} =
    Prelude.rnf hostname
      `Prelude.seq` Prelude.rnf instanceIdentity
      `Prelude.seq` Prelude.rnf workerGroup

instance Data.ToHeaders PollForTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("DataPipeline.PollForTask" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PollForTask where
  toJSON PollForTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("hostname" Data..=) Prelude.<$> hostname,
            ("instanceIdentity" Data..=)
              Prelude.<$> instanceIdentity,
            Prelude.Just ("workerGroup" Data..= workerGroup)
          ]
      )

instance Data.ToPath PollForTask where
  toPath = Prelude.const "/"

instance Data.ToQuery PollForTask where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the output of PollForTask.
--
-- /See:/ 'newPollForTaskResponse' smart constructor.
data PollForTaskResponse = PollForTaskResponse'
  { -- | The information needed to complete the task that is being assigned to
    -- the task runner. One of the fields returned in this object is @taskId@,
    -- which contains an identifier for the task being assigned. The calling
    -- task runner uses @taskId@ in subsequent calls to ReportTaskProgress and
    -- SetTaskStatus.
    taskObject :: Prelude.Maybe TaskObject,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PollForTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskObject', 'pollForTaskResponse_taskObject' - The information needed to complete the task that is being assigned to
-- the task runner. One of the fields returned in this object is @taskId@,
-- which contains an identifier for the task being assigned. The calling
-- task runner uses @taskId@ in subsequent calls to ReportTaskProgress and
-- SetTaskStatus.
--
-- 'httpStatus', 'pollForTaskResponse_httpStatus' - The response's http status code.
newPollForTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PollForTaskResponse
newPollForTaskResponse pHttpStatus_ =
  PollForTaskResponse'
    { taskObject = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The information needed to complete the task that is being assigned to
-- the task runner. One of the fields returned in this object is @taskId@,
-- which contains an identifier for the task being assigned. The calling
-- task runner uses @taskId@ in subsequent calls to ReportTaskProgress and
-- SetTaskStatus.
pollForTaskResponse_taskObject :: Lens.Lens' PollForTaskResponse (Prelude.Maybe TaskObject)
pollForTaskResponse_taskObject = Lens.lens (\PollForTaskResponse' {taskObject} -> taskObject) (\s@PollForTaskResponse' {} a -> s {taskObject = a} :: PollForTaskResponse)

-- | The response's http status code.
pollForTaskResponse_httpStatus :: Lens.Lens' PollForTaskResponse Prelude.Int
pollForTaskResponse_httpStatus = Lens.lens (\PollForTaskResponse' {httpStatus} -> httpStatus) (\s@PollForTaskResponse' {} a -> s {httpStatus = a} :: PollForTaskResponse)

instance Prelude.NFData PollForTaskResponse where
  rnf PollForTaskResponse' {..} =
    Prelude.rnf taskObject
      `Prelude.seq` Prelude.rnf httpStatus
