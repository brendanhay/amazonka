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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeThingRegistrationTask' smart constructor.
data DescribeThingRegistrationTask = DescribeThingRegistrationTask'
  { -- | The task ID.
    taskId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeThingRegistrationTask
newDescribeThingRegistrationTask pTaskId_ =
  DescribeThingRegistrationTask' {taskId = pTaskId_}

-- | The task ID.
describeThingRegistrationTask_taskId :: Lens.Lens' DescribeThingRegistrationTask Core.Text
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
            Core.<$> (x Core..?> "inputFileKey")
            Core.<*> (x Core..?> "lastModifiedDate")
            Core.<*> (x Core..?> "status")
            Core.<*> (x Core..?> "roleArn")
            Core.<*> (x Core..?> "message")
            Core.<*> (x Core..?> "taskId")
            Core.<*> (x Core..?> "percentageProgress")
            Core.<*> (x Core..?> "creationDate")
            Core.<*> (x Core..?> "inputFileBucket")
            Core.<*> (x Core..?> "failureCount")
            Core.<*> (x Core..?> "successCount")
            Core.<*> (x Core..?> "templateBody")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeThingRegistrationTask

instance Core.NFData DescribeThingRegistrationTask

instance Core.ToHeaders DescribeThingRegistrationTask where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeThingRegistrationTask where
  toPath DescribeThingRegistrationTask' {..} =
    Core.mconcat
      ["/thing-registration-tasks/", Core.toBS taskId]

instance Core.ToQuery DescribeThingRegistrationTask where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeThingRegistrationTaskResponse' smart constructor.
data DescribeThingRegistrationTaskResponse = DescribeThingRegistrationTaskResponse'
  { -- | The input file key.
    inputFileKey :: Core.Maybe Core.Text,
    -- | The date when the task was last modified.
    lastModifiedDate :: Core.Maybe Core.POSIX,
    -- | The status of the bulk thing provisioning task.
    status :: Core.Maybe TaskStatus,
    -- | The role ARN that grants access to the input file bucket.
    roleArn :: Core.Maybe Core.Text,
    -- | The message.
    message :: Core.Maybe Core.Text,
    -- | The task ID.
    taskId :: Core.Maybe Core.Text,
    -- | The progress of the bulk provisioning task expressed as a percentage.
    percentageProgress :: Core.Maybe Core.Natural,
    -- | The task creation date.
    creationDate :: Core.Maybe Core.POSIX,
    -- | The S3 bucket that contains the input file.
    inputFileBucket :: Core.Maybe Core.Text,
    -- | The number of things that failed to be provisioned.
    failureCount :: Core.Maybe Core.Int,
    -- | The number of things successfully provisioned.
    successCount :: Core.Maybe Core.Int,
    -- | The task\'s template.
    templateBody :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeThingRegistrationTaskResponse
newDescribeThingRegistrationTaskResponse pHttpStatus_ =
  DescribeThingRegistrationTaskResponse'
    { inputFileKey =
        Core.Nothing,
      lastModifiedDate = Core.Nothing,
      status = Core.Nothing,
      roleArn = Core.Nothing,
      message = Core.Nothing,
      taskId = Core.Nothing,
      percentageProgress = Core.Nothing,
      creationDate = Core.Nothing,
      inputFileBucket = Core.Nothing,
      failureCount = Core.Nothing,
      successCount = Core.Nothing,
      templateBody = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The input file key.
describeThingRegistrationTaskResponse_inputFileKey :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Core.Text)
describeThingRegistrationTaskResponse_inputFileKey = Lens.lens (\DescribeThingRegistrationTaskResponse' {inputFileKey} -> inputFileKey) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {inputFileKey = a} :: DescribeThingRegistrationTaskResponse)

-- | The date when the task was last modified.
describeThingRegistrationTaskResponse_lastModifiedDate :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Core.UTCTime)
describeThingRegistrationTaskResponse_lastModifiedDate = Lens.lens (\DescribeThingRegistrationTaskResponse' {lastModifiedDate} -> lastModifiedDate) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {lastModifiedDate = a} :: DescribeThingRegistrationTaskResponse) Core.. Lens.mapping Core._Time

-- | The status of the bulk thing provisioning task.
describeThingRegistrationTaskResponse_status :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe TaskStatus)
describeThingRegistrationTaskResponse_status = Lens.lens (\DescribeThingRegistrationTaskResponse' {status} -> status) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {status = a} :: DescribeThingRegistrationTaskResponse)

-- | The role ARN that grants access to the input file bucket.
describeThingRegistrationTaskResponse_roleArn :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Core.Text)
describeThingRegistrationTaskResponse_roleArn = Lens.lens (\DescribeThingRegistrationTaskResponse' {roleArn} -> roleArn) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {roleArn = a} :: DescribeThingRegistrationTaskResponse)

-- | The message.
describeThingRegistrationTaskResponse_message :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Core.Text)
describeThingRegistrationTaskResponse_message = Lens.lens (\DescribeThingRegistrationTaskResponse' {message} -> message) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {message = a} :: DescribeThingRegistrationTaskResponse)

-- | The task ID.
describeThingRegistrationTaskResponse_taskId :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Core.Text)
describeThingRegistrationTaskResponse_taskId = Lens.lens (\DescribeThingRegistrationTaskResponse' {taskId} -> taskId) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {taskId = a} :: DescribeThingRegistrationTaskResponse)

-- | The progress of the bulk provisioning task expressed as a percentage.
describeThingRegistrationTaskResponse_percentageProgress :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Core.Natural)
describeThingRegistrationTaskResponse_percentageProgress = Lens.lens (\DescribeThingRegistrationTaskResponse' {percentageProgress} -> percentageProgress) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {percentageProgress = a} :: DescribeThingRegistrationTaskResponse)

-- | The task creation date.
describeThingRegistrationTaskResponse_creationDate :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Core.UTCTime)
describeThingRegistrationTaskResponse_creationDate = Lens.lens (\DescribeThingRegistrationTaskResponse' {creationDate} -> creationDate) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {creationDate = a} :: DescribeThingRegistrationTaskResponse) Core.. Lens.mapping Core._Time

-- | The S3 bucket that contains the input file.
describeThingRegistrationTaskResponse_inputFileBucket :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Core.Text)
describeThingRegistrationTaskResponse_inputFileBucket = Lens.lens (\DescribeThingRegistrationTaskResponse' {inputFileBucket} -> inputFileBucket) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {inputFileBucket = a} :: DescribeThingRegistrationTaskResponse)

-- | The number of things that failed to be provisioned.
describeThingRegistrationTaskResponse_failureCount :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Core.Int)
describeThingRegistrationTaskResponse_failureCount = Lens.lens (\DescribeThingRegistrationTaskResponse' {failureCount} -> failureCount) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {failureCount = a} :: DescribeThingRegistrationTaskResponse)

-- | The number of things successfully provisioned.
describeThingRegistrationTaskResponse_successCount :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Core.Int)
describeThingRegistrationTaskResponse_successCount = Lens.lens (\DescribeThingRegistrationTaskResponse' {successCount} -> successCount) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {successCount = a} :: DescribeThingRegistrationTaskResponse)

-- | The task\'s template.
describeThingRegistrationTaskResponse_templateBody :: Lens.Lens' DescribeThingRegistrationTaskResponse (Core.Maybe Core.Text)
describeThingRegistrationTaskResponse_templateBody = Lens.lens (\DescribeThingRegistrationTaskResponse' {templateBody} -> templateBody) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {templateBody = a} :: DescribeThingRegistrationTaskResponse)

-- | The response's http status code.
describeThingRegistrationTaskResponse_httpStatus :: Lens.Lens' DescribeThingRegistrationTaskResponse Core.Int
describeThingRegistrationTaskResponse_httpStatus = Lens.lens (\DescribeThingRegistrationTaskResponse' {httpStatus} -> httpStatus) (\s@DescribeThingRegistrationTaskResponse' {} a -> s {httpStatus = a} :: DescribeThingRegistrationTaskResponse)

instance
  Core.NFData
    DescribeThingRegistrationTaskResponse
