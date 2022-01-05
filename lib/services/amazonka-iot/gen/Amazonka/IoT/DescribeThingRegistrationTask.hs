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
-- Module      : Amazonka.IoT.DescribeThingRegistrationTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a bulk thing provisioning task.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DescribeThingRegistrationTask>
-- action.
module Amazonka.IoT.DescribeThingRegistrationTask
  ( -- * Creating a Request
    DescribeThingRegistrationTask (..),
    newDescribeThingRegistrationTask,

    -- * Request Lenses
    describeThingRegistrationTask_taskId,

    -- * Destructuring the Response
    DescribeThingRegistrationTaskResponse (..),
    newDescribeThingRegistrationTaskResponse,

    -- * Response Lenses
    describeThingRegistrationTaskResponse_status,
    describeThingRegistrationTaskResponse_lastModifiedDate,
    describeThingRegistrationTaskResponse_inputFileKey,
    describeThingRegistrationTaskResponse_taskId,
    describeThingRegistrationTaskResponse_creationDate,
    describeThingRegistrationTaskResponse_percentageProgress,
    describeThingRegistrationTaskResponse_templateBody,
    describeThingRegistrationTaskResponse_successCount,
    describeThingRegistrationTaskResponse_message,
    describeThingRegistrationTaskResponse_failureCount,
    describeThingRegistrationTaskResponse_inputFileBucket,
    describeThingRegistrationTaskResponse_roleArn,
    describeThingRegistrationTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeThingRegistrationTask' smart constructor.
data DescribeThingRegistrationTask = DescribeThingRegistrationTask'
  { -- | The task ID.
    taskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeThingRegistrationTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskId', 'describeThingRegistrationTask_taskId' - The task ID.
newDescribeThingRegistrationTask ::
  -- | 'taskId'
  Prelude.Text ->
  DescribeThingRegistrationTask
newDescribeThingRegistrationTask pTaskId_ =
  DescribeThingRegistrationTask' {taskId = pTaskId_}

-- | The task ID.
describeThingRegistrationTask_taskId :: Lens.Lens' DescribeThingRegistrationTask Prelude.Text
describeThingRegistrationTask_taskId = Lens.lens (\DescribeThingRegistrationTask' {taskId} -> taskId) (\s@DescribeThingRegistrationTask' {} a -> s {taskId = a} :: DescribeThingRegistrationTask)

instance
  Core.AWSRequest
    DescribeThingRegistrationTask
  where
  type
    AWSResponse DescribeThingRegistrationTask =
      DescribeThingRegistrationTaskResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeThingRegistrationTaskResponse'
            Prelude.<$> (x Core..?> "status")
            Prelude.<*> (x Core..?> "lastModifiedDate")
            Prelude.<*> (x Core..?> "inputFileKey")
            Prelude.<*> (x Core..?> "taskId")
            Prelude.<*> (x Core..?> "creationDate")
            Prelude.<*> (x Core..?> "percentageProgress")
            Prelude.<*> (x Core..?> "templateBody")
            Prelude.<*> (x Core..?> "successCount")
            Prelude.<*> (x Core..?> "message")
            Prelude.<*> (x Core..?> "failureCount")
            Prelude.<*> (x Core..?> "inputFileBucket")
            Prelude.<*> (x Core..?> "roleArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeThingRegistrationTask
  where
  hashWithSalt _salt DescribeThingRegistrationTask' {..} =
    _salt `Prelude.hashWithSalt` taskId

instance Prelude.NFData DescribeThingRegistrationTask where
  rnf DescribeThingRegistrationTask' {..} =
    Prelude.rnf taskId

instance Core.ToHeaders DescribeThingRegistrationTask where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeThingRegistrationTask where
  toPath DescribeThingRegistrationTask' {..} =
    Prelude.mconcat
      ["/thing-registration-tasks/", Core.toBS taskId]

instance Core.ToQuery DescribeThingRegistrationTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeThingRegistrationTaskResponse' smart constructor.
data DescribeThingRegistrationTaskResponse = DescribeThingRegistrationTaskResponse'
  { -- | The status of the bulk thing provisioning task.
    status :: Prelude.Maybe TaskStatus,
    -- | The date when the task was last modified.
    lastModifiedDate :: Prelude.Maybe Core.POSIX,
    -- | The input file key.
    inputFileKey :: Prelude.Maybe Prelude.Text,
    -- | The task ID.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | The task creation date.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The progress of the bulk provisioning task expressed as a percentage.
    percentageProgress :: Prelude.Maybe Prelude.Natural,
    -- | The task\'s template.
    templateBody :: Prelude.Maybe Prelude.Text,
    -- | The number of things successfully provisioned.
    successCount :: Prelude.Maybe Prelude.Int,
    -- | The message.
    message :: Prelude.Maybe Prelude.Text,
    -- | The number of things that failed to be provisioned.
    failureCount :: Prelude.Maybe Prelude.Int,
    -- | The S3 bucket that contains the input file.
    inputFileBucket :: Prelude.Maybe Prelude.Text,
    -- | The role ARN that grants access to the input file bucket.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeThingRegistrationTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'describeThingRegistrationTaskResponse_status' - The status of the bulk thing provisioning task.
--
-- 'lastModifiedDate', 'describeThingRegistrationTaskResponse_lastModifiedDate' - The date when the task was last modified.
--
-- 'inputFileKey', 'describeThingRegistrationTaskResponse_inputFileKey' - The input file key.
--
-- 'taskId', 'describeThingRegistrationTaskResponse_taskId' - The task ID.
--
-- 'creationDate', 'describeThingRegistrationTaskResponse_creationDate' - The task creation date.
--
-- 'percentageProgress', 'describeThingRegistrationTaskResponse_percentageProgress' - The progress of the bulk provisioning task expressed as a percentage.
--
-- 'templateBody', 'describeThingRegistrationTaskResponse_templateBody' - The task\'s template.
--
-- 'successCount', 'describeThingRegistrationTaskResponse_successCount' - The number of things successfully provisioned.
--
-- 'message', 'describeThingRegistrationTaskResponse_message' - The message.
--
-- 'failureCount', 'describeThingRegistrationTaskResponse_failureCount' - The number of things that failed to be provisioned.
--
-- 'inputFileBucket', 'describeThingRegistrationTaskResponse_inputFileBucket' - The S3 bucket that contains the input file.
--
-- 'roleArn', 'describeThingRegistrationTaskResponse_roleArn' - The role ARN that grants access to the input file bucket.
--
-- 'httpStatus', 'describeThingRegistrationTaskResponse_httpStatus' - The response's http status code.
newDescribeThingRegistrationTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeThingRegistrationTaskResponse
newDescribeThingRegistrationTaskResponse pHttpStatus_ =
  DescribeThingRegistrationTaskResponse'
    { status =
        Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      inputFileKey = Prelude.Nothing,
      taskId = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      percentageProgress = Prelude.Nothing,
      templateBody = Prelude.Nothing,
      successCount = Prelude.Nothing,
      message = Prelude.Nothing,
      failureCount = Prelude.Nothing,
      inputFileBucket = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the bulk thing provisioning task.
describeThingRegistrationTaskResponse_status :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe TaskStatus)
describeThingRegistrationTaskResponse_status = Lens.lens (\DescribeThingRegistrationTaskResponse' {status} -> status) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {status = a} :: DescribeThingRegistrationTaskResponse)

-- | The date when the task was last modified.
describeThingRegistrationTaskResponse_lastModifiedDate :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.UTCTime)
describeThingRegistrationTaskResponse_lastModifiedDate = Lens.lens (\DescribeThingRegistrationTaskResponse' {lastModifiedDate} -> lastModifiedDate) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {lastModifiedDate = a} :: DescribeThingRegistrationTaskResponse) Prelude.. Lens.mapping Core._Time

-- | The input file key.
describeThingRegistrationTaskResponse_inputFileKey :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.Text)
describeThingRegistrationTaskResponse_inputFileKey = Lens.lens (\DescribeThingRegistrationTaskResponse' {inputFileKey} -> inputFileKey) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {inputFileKey = a} :: DescribeThingRegistrationTaskResponse)

-- | The task ID.
describeThingRegistrationTaskResponse_taskId :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.Text)
describeThingRegistrationTaskResponse_taskId = Lens.lens (\DescribeThingRegistrationTaskResponse' {taskId} -> taskId) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {taskId = a} :: DescribeThingRegistrationTaskResponse)

-- | The task creation date.
describeThingRegistrationTaskResponse_creationDate :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.UTCTime)
describeThingRegistrationTaskResponse_creationDate = Lens.lens (\DescribeThingRegistrationTaskResponse' {creationDate} -> creationDate) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {creationDate = a} :: DescribeThingRegistrationTaskResponse) Prelude.. Lens.mapping Core._Time

-- | The progress of the bulk provisioning task expressed as a percentage.
describeThingRegistrationTaskResponse_percentageProgress :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.Natural)
describeThingRegistrationTaskResponse_percentageProgress = Lens.lens (\DescribeThingRegistrationTaskResponse' {percentageProgress} -> percentageProgress) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {percentageProgress = a} :: DescribeThingRegistrationTaskResponse)

-- | The task\'s template.
describeThingRegistrationTaskResponse_templateBody :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.Text)
describeThingRegistrationTaskResponse_templateBody = Lens.lens (\DescribeThingRegistrationTaskResponse' {templateBody} -> templateBody) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {templateBody = a} :: DescribeThingRegistrationTaskResponse)

-- | The number of things successfully provisioned.
describeThingRegistrationTaskResponse_successCount :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.Int)
describeThingRegistrationTaskResponse_successCount = Lens.lens (\DescribeThingRegistrationTaskResponse' {successCount} -> successCount) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {successCount = a} :: DescribeThingRegistrationTaskResponse)

-- | The message.
describeThingRegistrationTaskResponse_message :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.Text)
describeThingRegistrationTaskResponse_message = Lens.lens (\DescribeThingRegistrationTaskResponse' {message} -> message) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {message = a} :: DescribeThingRegistrationTaskResponse)

-- | The number of things that failed to be provisioned.
describeThingRegistrationTaskResponse_failureCount :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.Int)
describeThingRegistrationTaskResponse_failureCount = Lens.lens (\DescribeThingRegistrationTaskResponse' {failureCount} -> failureCount) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {failureCount = a} :: DescribeThingRegistrationTaskResponse)

-- | The S3 bucket that contains the input file.
describeThingRegistrationTaskResponse_inputFileBucket :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.Text)
describeThingRegistrationTaskResponse_inputFileBucket = Lens.lens (\DescribeThingRegistrationTaskResponse' {inputFileBucket} -> inputFileBucket) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {inputFileBucket = a} :: DescribeThingRegistrationTaskResponse)

-- | The role ARN that grants access to the input file bucket.
describeThingRegistrationTaskResponse_roleArn :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.Text)
describeThingRegistrationTaskResponse_roleArn = Lens.lens (\DescribeThingRegistrationTaskResponse' {roleArn} -> roleArn) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {roleArn = a} :: DescribeThingRegistrationTaskResponse)

-- | The response's http status code.
describeThingRegistrationTaskResponse_httpStatus :: Lens.Lens' DescribeThingRegistrationTaskResponse Prelude.Int
describeThingRegistrationTaskResponse_httpStatus = Lens.lens (\DescribeThingRegistrationTaskResponse' {httpStatus} -> httpStatus) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {httpStatus = a} :: DescribeThingRegistrationTaskResponse)

instance
  Prelude.NFData
    DescribeThingRegistrationTaskResponse
  where
  rnf DescribeThingRegistrationTaskResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf inputFileKey
      `Prelude.seq` Prelude.rnf taskId
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf percentageProgress
      `Prelude.seq` Prelude.rnf templateBody
      `Prelude.seq` Prelude.rnf successCount
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf failureCount
      `Prelude.seq` Prelude.rnf inputFileBucket
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf httpStatus
