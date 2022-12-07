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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    describeThingRegistrationTaskResponse_message,
    describeThingRegistrationTaskResponse_roleArn,
    describeThingRegistrationTaskResponse_lastModifiedDate,
    describeThingRegistrationTaskResponse_failureCount,
    describeThingRegistrationTaskResponse_taskId,
    describeThingRegistrationTaskResponse_templateBody,
    describeThingRegistrationTaskResponse_creationDate,
    describeThingRegistrationTaskResponse_status,
    describeThingRegistrationTaskResponse_percentageProgress,
    describeThingRegistrationTaskResponse_inputFileKey,
    describeThingRegistrationTaskResponse_inputFileBucket,
    describeThingRegistrationTaskResponse_successCount,
    describeThingRegistrationTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeThingRegistrationTaskResponse'
            Prelude.<$> (x Data..?> "message")
            Prelude.<*> (x Data..?> "roleArn")
            Prelude.<*> (x Data..?> "lastModifiedDate")
            Prelude.<*> (x Data..?> "failureCount")
            Prelude.<*> (x Data..?> "taskId")
            Prelude.<*> (x Data..?> "templateBody")
            Prelude.<*> (x Data..?> "creationDate")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "percentageProgress")
            Prelude.<*> (x Data..?> "inputFileKey")
            Prelude.<*> (x Data..?> "inputFileBucket")
            Prelude.<*> (x Data..?> "successCount")
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

instance Data.ToHeaders DescribeThingRegistrationTask where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeThingRegistrationTask where
  toPath DescribeThingRegistrationTask' {..} =
    Prelude.mconcat
      ["/thing-registration-tasks/", Data.toBS taskId]

instance Data.ToQuery DescribeThingRegistrationTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeThingRegistrationTaskResponse' smart constructor.
data DescribeThingRegistrationTaskResponse = DescribeThingRegistrationTaskResponse'
  { -- | The message.
    message :: Prelude.Maybe Prelude.Text,
    -- | The role ARN that grants access to the input file bucket.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The date when the task was last modified.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The number of things that failed to be provisioned.
    failureCount :: Prelude.Maybe Prelude.Int,
    -- | The task ID.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | The task\'s template.
    templateBody :: Prelude.Maybe Prelude.Text,
    -- | The task creation date.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The status of the bulk thing provisioning task.
    status :: Prelude.Maybe TaskStatus,
    -- | The progress of the bulk provisioning task expressed as a percentage.
    percentageProgress :: Prelude.Maybe Prelude.Natural,
    -- | The input file key.
    inputFileKey :: Prelude.Maybe Prelude.Text,
    -- | The S3 bucket that contains the input file.
    inputFileBucket :: Prelude.Maybe Prelude.Text,
    -- | The number of things successfully provisioned.
    successCount :: Prelude.Maybe Prelude.Int,
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
-- 'message', 'describeThingRegistrationTaskResponse_message' - The message.
--
-- 'roleArn', 'describeThingRegistrationTaskResponse_roleArn' - The role ARN that grants access to the input file bucket.
--
-- 'lastModifiedDate', 'describeThingRegistrationTaskResponse_lastModifiedDate' - The date when the task was last modified.
--
-- 'failureCount', 'describeThingRegistrationTaskResponse_failureCount' - The number of things that failed to be provisioned.
--
-- 'taskId', 'describeThingRegistrationTaskResponse_taskId' - The task ID.
--
-- 'templateBody', 'describeThingRegistrationTaskResponse_templateBody' - The task\'s template.
--
-- 'creationDate', 'describeThingRegistrationTaskResponse_creationDate' - The task creation date.
--
-- 'status', 'describeThingRegistrationTaskResponse_status' - The status of the bulk thing provisioning task.
--
-- 'percentageProgress', 'describeThingRegistrationTaskResponse_percentageProgress' - The progress of the bulk provisioning task expressed as a percentage.
--
-- 'inputFileKey', 'describeThingRegistrationTaskResponse_inputFileKey' - The input file key.
--
-- 'inputFileBucket', 'describeThingRegistrationTaskResponse_inputFileBucket' - The S3 bucket that contains the input file.
--
-- 'successCount', 'describeThingRegistrationTaskResponse_successCount' - The number of things successfully provisioned.
--
-- 'httpStatus', 'describeThingRegistrationTaskResponse_httpStatus' - The response's http status code.
newDescribeThingRegistrationTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeThingRegistrationTaskResponse
newDescribeThingRegistrationTaskResponse pHttpStatus_ =
  DescribeThingRegistrationTaskResponse'
    { message =
        Prelude.Nothing,
      roleArn = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      failureCount = Prelude.Nothing,
      taskId = Prelude.Nothing,
      templateBody = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      status = Prelude.Nothing,
      percentageProgress = Prelude.Nothing,
      inputFileKey = Prelude.Nothing,
      inputFileBucket = Prelude.Nothing,
      successCount = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The message.
describeThingRegistrationTaskResponse_message :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.Text)
describeThingRegistrationTaskResponse_message = Lens.lens (\DescribeThingRegistrationTaskResponse' {message} -> message) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {message = a} :: DescribeThingRegistrationTaskResponse)

-- | The role ARN that grants access to the input file bucket.
describeThingRegistrationTaskResponse_roleArn :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.Text)
describeThingRegistrationTaskResponse_roleArn = Lens.lens (\DescribeThingRegistrationTaskResponse' {roleArn} -> roleArn) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {roleArn = a} :: DescribeThingRegistrationTaskResponse)

-- | The date when the task was last modified.
describeThingRegistrationTaskResponse_lastModifiedDate :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.UTCTime)
describeThingRegistrationTaskResponse_lastModifiedDate = Lens.lens (\DescribeThingRegistrationTaskResponse' {lastModifiedDate} -> lastModifiedDate) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {lastModifiedDate = a} :: DescribeThingRegistrationTaskResponse) Prelude.. Lens.mapping Data._Time

-- | The number of things that failed to be provisioned.
describeThingRegistrationTaskResponse_failureCount :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.Int)
describeThingRegistrationTaskResponse_failureCount = Lens.lens (\DescribeThingRegistrationTaskResponse' {failureCount} -> failureCount) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {failureCount = a} :: DescribeThingRegistrationTaskResponse)

-- | The task ID.
describeThingRegistrationTaskResponse_taskId :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.Text)
describeThingRegistrationTaskResponse_taskId = Lens.lens (\DescribeThingRegistrationTaskResponse' {taskId} -> taskId) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {taskId = a} :: DescribeThingRegistrationTaskResponse)

-- | The task\'s template.
describeThingRegistrationTaskResponse_templateBody :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.Text)
describeThingRegistrationTaskResponse_templateBody = Lens.lens (\DescribeThingRegistrationTaskResponse' {templateBody} -> templateBody) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {templateBody = a} :: DescribeThingRegistrationTaskResponse)

-- | The task creation date.
describeThingRegistrationTaskResponse_creationDate :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.UTCTime)
describeThingRegistrationTaskResponse_creationDate = Lens.lens (\DescribeThingRegistrationTaskResponse' {creationDate} -> creationDate) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {creationDate = a} :: DescribeThingRegistrationTaskResponse) Prelude.. Lens.mapping Data._Time

-- | The status of the bulk thing provisioning task.
describeThingRegistrationTaskResponse_status :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe TaskStatus)
describeThingRegistrationTaskResponse_status = Lens.lens (\DescribeThingRegistrationTaskResponse' {status} -> status) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {status = a} :: DescribeThingRegistrationTaskResponse)

-- | The progress of the bulk provisioning task expressed as a percentage.
describeThingRegistrationTaskResponse_percentageProgress :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.Natural)
describeThingRegistrationTaskResponse_percentageProgress = Lens.lens (\DescribeThingRegistrationTaskResponse' {percentageProgress} -> percentageProgress) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {percentageProgress = a} :: DescribeThingRegistrationTaskResponse)

-- | The input file key.
describeThingRegistrationTaskResponse_inputFileKey :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.Text)
describeThingRegistrationTaskResponse_inputFileKey = Lens.lens (\DescribeThingRegistrationTaskResponse' {inputFileKey} -> inputFileKey) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {inputFileKey = a} :: DescribeThingRegistrationTaskResponse)

-- | The S3 bucket that contains the input file.
describeThingRegistrationTaskResponse_inputFileBucket :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.Text)
describeThingRegistrationTaskResponse_inputFileBucket = Lens.lens (\DescribeThingRegistrationTaskResponse' {inputFileBucket} -> inputFileBucket) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {inputFileBucket = a} :: DescribeThingRegistrationTaskResponse)

-- | The number of things successfully provisioned.
describeThingRegistrationTaskResponse_successCount :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.Int)
describeThingRegistrationTaskResponse_successCount = Lens.lens (\DescribeThingRegistrationTaskResponse' {successCount} -> successCount) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {successCount = a} :: DescribeThingRegistrationTaskResponse)

-- | The response's http status code.
describeThingRegistrationTaskResponse_httpStatus :: Lens.Lens' DescribeThingRegistrationTaskResponse Prelude.Int
describeThingRegistrationTaskResponse_httpStatus = Lens.lens (\DescribeThingRegistrationTaskResponse' {httpStatus} -> httpStatus) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {httpStatus = a} :: DescribeThingRegistrationTaskResponse)

instance
  Prelude.NFData
    DescribeThingRegistrationTaskResponse
  where
  rnf DescribeThingRegistrationTaskResponse' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf failureCount
      `Prelude.seq` Prelude.rnf taskId
      `Prelude.seq` Prelude.rnf templateBody
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf percentageProgress
      `Prelude.seq` Prelude.rnf inputFileKey
      `Prelude.seq` Prelude.rnf inputFileBucket
      `Prelude.seq` Prelude.rnf successCount
      `Prelude.seq` Prelude.rnf httpStatus
