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
-- Module      : Network.AWS.IoT.DescribeThingRegistrationTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a bulk thing provisioning task.
module Network.AWS.IoT.DescribeThingRegistrationTask
  ( -- * Creating a Request
    DescribeThingRegistrationTask (..),
    newDescribeThingRegistrationTask,

    -- * Request Lenses
    describeThingRegistrationTask_taskId,

    -- * Destructuring the Response
    DescribeThingRegistrationTaskResponse (..),
    newDescribeThingRegistrationTaskResponse,

    -- * Response Lenses
    describeThingRegistrationTaskResponse_inputFileKey,
    describeThingRegistrationTaskResponse_lastModifiedDate,
    describeThingRegistrationTaskResponse_status,
    describeThingRegistrationTaskResponse_roleArn,
    describeThingRegistrationTaskResponse_message,
    describeThingRegistrationTaskResponse_taskId,
    describeThingRegistrationTaskResponse_percentageProgress,
    describeThingRegistrationTaskResponse_creationDate,
    describeThingRegistrationTaskResponse_inputFileBucket,
    describeThingRegistrationTaskResponse_failureCount,
    describeThingRegistrationTaskResponse_successCount,
    describeThingRegistrationTaskResponse_templateBody,
    describeThingRegistrationTaskResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
            Prelude.<$> (x Core..?> "inputFileKey")
            Prelude.<*> (x Core..?> "lastModifiedDate")
            Prelude.<*> (x Core..?> "status")
            Prelude.<*> (x Core..?> "roleArn")
            Prelude.<*> (x Core..?> "message")
            Prelude.<*> (x Core..?> "taskId")
            Prelude.<*> (x Core..?> "percentageProgress")
            Prelude.<*> (x Core..?> "creationDate")
            Prelude.<*> (x Core..?> "inputFileBucket")
            Prelude.<*> (x Core..?> "failureCount")
            Prelude.<*> (x Core..?> "successCount")
            Prelude.<*> (x Core..?> "templateBody")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeThingRegistrationTask

instance Prelude.NFData DescribeThingRegistrationTask

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
  { -- | The input file key.
    inputFileKey :: Prelude.Maybe Prelude.Text,
    -- | The date when the task was last modified.
    lastModifiedDate :: Prelude.Maybe Core.POSIX,
    -- | The status of the bulk thing provisioning task.
    status :: Prelude.Maybe TaskStatus,
    -- | The role ARN that grants access to the input file bucket.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The message.
    message :: Prelude.Maybe Prelude.Text,
    -- | The task ID.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | The progress of the bulk provisioning task expressed as a percentage.
    percentageProgress :: Prelude.Maybe Prelude.Natural,
    -- | The task creation date.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The S3 bucket that contains the input file.
    inputFileBucket :: Prelude.Maybe Prelude.Text,
    -- | The number of things that failed to be provisioned.
    failureCount :: Prelude.Maybe Prelude.Int,
    -- | The number of things successfully provisioned.
    successCount :: Prelude.Maybe Prelude.Int,
    -- | The task\'s template.
    templateBody :: Prelude.Maybe Prelude.Text,
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
-- 'inputFileKey', 'describeThingRegistrationTaskResponse_inputFileKey' - The input file key.
--
-- 'lastModifiedDate', 'describeThingRegistrationTaskResponse_lastModifiedDate' - The date when the task was last modified.
--
-- 'status', 'describeThingRegistrationTaskResponse_status' - The status of the bulk thing provisioning task.
--
-- 'roleArn', 'describeThingRegistrationTaskResponse_roleArn' - The role ARN that grants access to the input file bucket.
--
-- 'message', 'describeThingRegistrationTaskResponse_message' - The message.
--
-- 'taskId', 'describeThingRegistrationTaskResponse_taskId' - The task ID.
--
-- 'percentageProgress', 'describeThingRegistrationTaskResponse_percentageProgress' - The progress of the bulk provisioning task expressed as a percentage.
--
-- 'creationDate', 'describeThingRegistrationTaskResponse_creationDate' - The task creation date.
--
-- 'inputFileBucket', 'describeThingRegistrationTaskResponse_inputFileBucket' - The S3 bucket that contains the input file.
--
-- 'failureCount', 'describeThingRegistrationTaskResponse_failureCount' - The number of things that failed to be provisioned.
--
-- 'successCount', 'describeThingRegistrationTaskResponse_successCount' - The number of things successfully provisioned.
--
-- 'templateBody', 'describeThingRegistrationTaskResponse_templateBody' - The task\'s template.
--
-- 'httpStatus', 'describeThingRegistrationTaskResponse_httpStatus' - The response's http status code.
newDescribeThingRegistrationTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeThingRegistrationTaskResponse
newDescribeThingRegistrationTaskResponse pHttpStatus_ =
  DescribeThingRegistrationTaskResponse'
    { inputFileKey =
        Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      status = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      message = Prelude.Nothing,
      taskId = Prelude.Nothing,
      percentageProgress = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      inputFileBucket = Prelude.Nothing,
      failureCount = Prelude.Nothing,
      successCount = Prelude.Nothing,
      templateBody = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The input file key.
describeThingRegistrationTaskResponse_inputFileKey :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.Text)
describeThingRegistrationTaskResponse_inputFileKey = Lens.lens (\DescribeThingRegistrationTaskResponse' {inputFileKey} -> inputFileKey) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {inputFileKey = a} :: DescribeThingRegistrationTaskResponse)

-- | The date when the task was last modified.
describeThingRegistrationTaskResponse_lastModifiedDate :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.UTCTime)
describeThingRegistrationTaskResponse_lastModifiedDate = Lens.lens (\DescribeThingRegistrationTaskResponse' {lastModifiedDate} -> lastModifiedDate) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {lastModifiedDate = a} :: DescribeThingRegistrationTaskResponse) Prelude.. Lens.mapping Core._Time

-- | The status of the bulk thing provisioning task.
describeThingRegistrationTaskResponse_status :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe TaskStatus)
describeThingRegistrationTaskResponse_status = Lens.lens (\DescribeThingRegistrationTaskResponse' {status} -> status) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {status = a} :: DescribeThingRegistrationTaskResponse)

-- | The role ARN that grants access to the input file bucket.
describeThingRegistrationTaskResponse_roleArn :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.Text)
describeThingRegistrationTaskResponse_roleArn = Lens.lens (\DescribeThingRegistrationTaskResponse' {roleArn} -> roleArn) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {roleArn = a} :: DescribeThingRegistrationTaskResponse)

-- | The message.
describeThingRegistrationTaskResponse_message :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.Text)
describeThingRegistrationTaskResponse_message = Lens.lens (\DescribeThingRegistrationTaskResponse' {message} -> message) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {message = a} :: DescribeThingRegistrationTaskResponse)

-- | The task ID.
describeThingRegistrationTaskResponse_taskId :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.Text)
describeThingRegistrationTaskResponse_taskId = Lens.lens (\DescribeThingRegistrationTaskResponse' {taskId} -> taskId) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {taskId = a} :: DescribeThingRegistrationTaskResponse)

-- | The progress of the bulk provisioning task expressed as a percentage.
describeThingRegistrationTaskResponse_percentageProgress :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.Natural)
describeThingRegistrationTaskResponse_percentageProgress = Lens.lens (\DescribeThingRegistrationTaskResponse' {percentageProgress} -> percentageProgress) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {percentageProgress = a} :: DescribeThingRegistrationTaskResponse)

-- | The task creation date.
describeThingRegistrationTaskResponse_creationDate :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.UTCTime)
describeThingRegistrationTaskResponse_creationDate = Lens.lens (\DescribeThingRegistrationTaskResponse' {creationDate} -> creationDate) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {creationDate = a} :: DescribeThingRegistrationTaskResponse) Prelude.. Lens.mapping Core._Time

-- | The S3 bucket that contains the input file.
describeThingRegistrationTaskResponse_inputFileBucket :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.Text)
describeThingRegistrationTaskResponse_inputFileBucket = Lens.lens (\DescribeThingRegistrationTaskResponse' {inputFileBucket} -> inputFileBucket) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {inputFileBucket = a} :: DescribeThingRegistrationTaskResponse)

-- | The number of things that failed to be provisioned.
describeThingRegistrationTaskResponse_failureCount :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.Int)
describeThingRegistrationTaskResponse_failureCount = Lens.lens (\DescribeThingRegistrationTaskResponse' {failureCount} -> failureCount) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {failureCount = a} :: DescribeThingRegistrationTaskResponse)

-- | The number of things successfully provisioned.
describeThingRegistrationTaskResponse_successCount :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.Int)
describeThingRegistrationTaskResponse_successCount = Lens.lens (\DescribeThingRegistrationTaskResponse' {successCount} -> successCount) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {successCount = a} :: DescribeThingRegistrationTaskResponse)

-- | The task\'s template.
describeThingRegistrationTaskResponse_templateBody :: Lens.Lens' DescribeThingRegistrationTaskResponse (Prelude.Maybe Prelude.Text)
describeThingRegistrationTaskResponse_templateBody = Lens.lens (\DescribeThingRegistrationTaskResponse' {templateBody} -> templateBody) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {templateBody = a} :: DescribeThingRegistrationTaskResponse)

-- | The response's http status code.
describeThingRegistrationTaskResponse_httpStatus :: Lens.Lens' DescribeThingRegistrationTaskResponse Prelude.Int
describeThingRegistrationTaskResponse_httpStatus = Lens.lens (\DescribeThingRegistrationTaskResponse' {httpStatus} -> httpStatus) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {httpStatus = a} :: DescribeThingRegistrationTaskResponse)

instance
  Prelude.NFData
    DescribeThingRegistrationTaskResponse
