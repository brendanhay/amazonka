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
-- Module      : Network.AWS.WorkMail.DescribeMailboxExportJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the current status of a mailbox export job.
module Network.AWS.WorkMail.DescribeMailboxExportJob
  ( -- * Creating a Request
    DescribeMailboxExportJob (..),
    newDescribeMailboxExportJob,

    -- * Request Lenses
    describeMailboxExportJob_jobId,
    describeMailboxExportJob_organizationId,

    -- * Destructuring the Response
    DescribeMailboxExportJobResponse (..),
    newDescribeMailboxExportJobResponse,

    -- * Response Lenses
    describeMailboxExportJobResponse_estimatedProgress,
    describeMailboxExportJobResponse_roleArn,
    describeMailboxExportJobResponse_entityId,
    describeMailboxExportJobResponse_startTime,
    describeMailboxExportJobResponse_s3Path,
    describeMailboxExportJobResponse_endTime,
    describeMailboxExportJobResponse_state,
    describeMailboxExportJobResponse_kmsKeyArn,
    describeMailboxExportJobResponse_s3BucketName,
    describeMailboxExportJobResponse_errorInfo,
    describeMailboxExportJobResponse_description,
    describeMailboxExportJobResponse_s3Prefix,
    describeMailboxExportJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newDescribeMailboxExportJob' smart constructor.
data DescribeMailboxExportJob = DescribeMailboxExportJob'
  { -- | The mailbox export job ID.
    jobId :: Core.Text,
    -- | The organization ID.
    organizationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMailboxExportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'describeMailboxExportJob_jobId' - The mailbox export job ID.
--
-- 'organizationId', 'describeMailboxExportJob_organizationId' - The organization ID.
newDescribeMailboxExportJob ::
  -- | 'jobId'
  Core.Text ->
  -- | 'organizationId'
  Core.Text ->
  DescribeMailboxExportJob
newDescribeMailboxExportJob pJobId_ pOrganizationId_ =
  DescribeMailboxExportJob'
    { jobId = pJobId_,
      organizationId = pOrganizationId_
    }

-- | The mailbox export job ID.
describeMailboxExportJob_jobId :: Lens.Lens' DescribeMailboxExportJob Core.Text
describeMailboxExportJob_jobId = Lens.lens (\DescribeMailboxExportJob' {jobId} -> jobId) (\s@DescribeMailboxExportJob' {} a -> s {jobId = a} :: DescribeMailboxExportJob)

-- | The organization ID.
describeMailboxExportJob_organizationId :: Lens.Lens' DescribeMailboxExportJob Core.Text
describeMailboxExportJob_organizationId = Lens.lens (\DescribeMailboxExportJob' {organizationId} -> organizationId) (\s@DescribeMailboxExportJob' {} a -> s {organizationId = a} :: DescribeMailboxExportJob)

instance Core.AWSRequest DescribeMailboxExportJob where
  type
    AWSResponse DescribeMailboxExportJob =
      DescribeMailboxExportJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMailboxExportJobResponse'
            Core.<$> (x Core..?> "EstimatedProgress")
            Core.<*> (x Core..?> "RoleArn")
            Core.<*> (x Core..?> "EntityId")
            Core.<*> (x Core..?> "StartTime")
            Core.<*> (x Core..?> "S3Path")
            Core.<*> (x Core..?> "EndTime")
            Core.<*> (x Core..?> "State")
            Core.<*> (x Core..?> "KmsKeyArn")
            Core.<*> (x Core..?> "S3BucketName")
            Core.<*> (x Core..?> "ErrorInfo")
            Core.<*> (x Core..?> "Description")
            Core.<*> (x Core..?> "S3Prefix")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeMailboxExportJob

instance Core.NFData DescribeMailboxExportJob

instance Core.ToHeaders DescribeMailboxExportJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.DescribeMailboxExportJob" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeMailboxExportJob where
  toJSON DescribeMailboxExportJob' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("JobId" Core..= jobId),
            Core.Just ("OrganizationId" Core..= organizationId)
          ]
      )

instance Core.ToPath DescribeMailboxExportJob where
  toPath = Core.const "/"

instance Core.ToQuery DescribeMailboxExportJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeMailboxExportJobResponse' smart constructor.
data DescribeMailboxExportJobResponse = DescribeMailboxExportJobResponse'
  { -- | The estimated progress of the mailbox export job, in percentage points.
    estimatedProgress :: Core.Maybe Core.Natural,
    -- | The ARN of the AWS Identity and Access Management (IAM) role that grants
    -- write permission to the Amazon Simple Storage Service (Amazon S3)
    -- bucket.
    roleArn :: Core.Maybe Core.Text,
    -- | The identifier of the user or resource associated with the mailbox.
    entityId :: Core.Maybe Core.Text,
    -- | The mailbox export job start timestamp.
    startTime :: Core.Maybe Core.POSIX,
    -- | The path to the S3 bucket and file that the mailbox export job is
    -- exporting to.
    s3Path :: Core.Maybe Core.Text,
    -- | The mailbox export job end timestamp.
    endTime :: Core.Maybe Core.POSIX,
    -- | The state of the mailbox export job.
    state :: Core.Maybe MailboxExportJobState,
    -- | The Amazon Resource Name (ARN) of the symmetric AWS Key Management
    -- Service (AWS KMS) key that encrypts the exported mailbox content.
    kmsKeyArn :: Core.Maybe Core.Text,
    -- | The name of the S3 bucket.
    s3BucketName :: Core.Maybe Core.Text,
    -- | Error information for failed mailbox export jobs.
    errorInfo :: Core.Maybe Core.Text,
    -- | The mailbox export job description.
    description :: Core.Maybe Core.Text,
    -- | The S3 bucket prefix.
    s3Prefix :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMailboxExportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'estimatedProgress', 'describeMailboxExportJobResponse_estimatedProgress' - The estimated progress of the mailbox export job, in percentage points.
--
-- 'roleArn', 'describeMailboxExportJobResponse_roleArn' - The ARN of the AWS Identity and Access Management (IAM) role that grants
-- write permission to the Amazon Simple Storage Service (Amazon S3)
-- bucket.
--
-- 'entityId', 'describeMailboxExportJobResponse_entityId' - The identifier of the user or resource associated with the mailbox.
--
-- 'startTime', 'describeMailboxExportJobResponse_startTime' - The mailbox export job start timestamp.
--
-- 's3Path', 'describeMailboxExportJobResponse_s3Path' - The path to the S3 bucket and file that the mailbox export job is
-- exporting to.
--
-- 'endTime', 'describeMailboxExportJobResponse_endTime' - The mailbox export job end timestamp.
--
-- 'state', 'describeMailboxExportJobResponse_state' - The state of the mailbox export job.
--
-- 'kmsKeyArn', 'describeMailboxExportJobResponse_kmsKeyArn' - The Amazon Resource Name (ARN) of the symmetric AWS Key Management
-- Service (AWS KMS) key that encrypts the exported mailbox content.
--
-- 's3BucketName', 'describeMailboxExportJobResponse_s3BucketName' - The name of the S3 bucket.
--
-- 'errorInfo', 'describeMailboxExportJobResponse_errorInfo' - Error information for failed mailbox export jobs.
--
-- 'description', 'describeMailboxExportJobResponse_description' - The mailbox export job description.
--
-- 's3Prefix', 'describeMailboxExportJobResponse_s3Prefix' - The S3 bucket prefix.
--
-- 'httpStatus', 'describeMailboxExportJobResponse_httpStatus' - The response's http status code.
newDescribeMailboxExportJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeMailboxExportJobResponse
newDescribeMailboxExportJobResponse pHttpStatus_ =
  DescribeMailboxExportJobResponse'
    { estimatedProgress =
        Core.Nothing,
      roleArn = Core.Nothing,
      entityId = Core.Nothing,
      startTime = Core.Nothing,
      s3Path = Core.Nothing,
      endTime = Core.Nothing,
      state = Core.Nothing,
      kmsKeyArn = Core.Nothing,
      s3BucketName = Core.Nothing,
      errorInfo = Core.Nothing,
      description = Core.Nothing,
      s3Prefix = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The estimated progress of the mailbox export job, in percentage points.
describeMailboxExportJobResponse_estimatedProgress :: Lens.Lens' DescribeMailboxExportJobResponse (Core.Maybe Core.Natural)
describeMailboxExportJobResponse_estimatedProgress = Lens.lens (\DescribeMailboxExportJobResponse' {estimatedProgress} -> estimatedProgress) (\s@DescribeMailboxExportJobResponse' {} a -> s {estimatedProgress = a} :: DescribeMailboxExportJobResponse)

-- | The ARN of the AWS Identity and Access Management (IAM) role that grants
-- write permission to the Amazon Simple Storage Service (Amazon S3)
-- bucket.
describeMailboxExportJobResponse_roleArn :: Lens.Lens' DescribeMailboxExportJobResponse (Core.Maybe Core.Text)
describeMailboxExportJobResponse_roleArn = Lens.lens (\DescribeMailboxExportJobResponse' {roleArn} -> roleArn) (\s@DescribeMailboxExportJobResponse' {} a -> s {roleArn = a} :: DescribeMailboxExportJobResponse)

-- | The identifier of the user or resource associated with the mailbox.
describeMailboxExportJobResponse_entityId :: Lens.Lens' DescribeMailboxExportJobResponse (Core.Maybe Core.Text)
describeMailboxExportJobResponse_entityId = Lens.lens (\DescribeMailboxExportJobResponse' {entityId} -> entityId) (\s@DescribeMailboxExportJobResponse' {} a -> s {entityId = a} :: DescribeMailboxExportJobResponse)

-- | The mailbox export job start timestamp.
describeMailboxExportJobResponse_startTime :: Lens.Lens' DescribeMailboxExportJobResponse (Core.Maybe Core.UTCTime)
describeMailboxExportJobResponse_startTime = Lens.lens (\DescribeMailboxExportJobResponse' {startTime} -> startTime) (\s@DescribeMailboxExportJobResponse' {} a -> s {startTime = a} :: DescribeMailboxExportJobResponse) Core.. Lens.mapping Core._Time

-- | The path to the S3 bucket and file that the mailbox export job is
-- exporting to.
describeMailboxExportJobResponse_s3Path :: Lens.Lens' DescribeMailboxExportJobResponse (Core.Maybe Core.Text)
describeMailboxExportJobResponse_s3Path = Lens.lens (\DescribeMailboxExportJobResponse' {s3Path} -> s3Path) (\s@DescribeMailboxExportJobResponse' {} a -> s {s3Path = a} :: DescribeMailboxExportJobResponse)

-- | The mailbox export job end timestamp.
describeMailboxExportJobResponse_endTime :: Lens.Lens' DescribeMailboxExportJobResponse (Core.Maybe Core.UTCTime)
describeMailboxExportJobResponse_endTime = Lens.lens (\DescribeMailboxExportJobResponse' {endTime} -> endTime) (\s@DescribeMailboxExportJobResponse' {} a -> s {endTime = a} :: DescribeMailboxExportJobResponse) Core.. Lens.mapping Core._Time

-- | The state of the mailbox export job.
describeMailboxExportJobResponse_state :: Lens.Lens' DescribeMailboxExportJobResponse (Core.Maybe MailboxExportJobState)
describeMailboxExportJobResponse_state = Lens.lens (\DescribeMailboxExportJobResponse' {state} -> state) (\s@DescribeMailboxExportJobResponse' {} a -> s {state = a} :: DescribeMailboxExportJobResponse)

-- | The Amazon Resource Name (ARN) of the symmetric AWS Key Management
-- Service (AWS KMS) key that encrypts the exported mailbox content.
describeMailboxExportJobResponse_kmsKeyArn :: Lens.Lens' DescribeMailboxExportJobResponse (Core.Maybe Core.Text)
describeMailboxExportJobResponse_kmsKeyArn = Lens.lens (\DescribeMailboxExportJobResponse' {kmsKeyArn} -> kmsKeyArn) (\s@DescribeMailboxExportJobResponse' {} a -> s {kmsKeyArn = a} :: DescribeMailboxExportJobResponse)

-- | The name of the S3 bucket.
describeMailboxExportJobResponse_s3BucketName :: Lens.Lens' DescribeMailboxExportJobResponse (Core.Maybe Core.Text)
describeMailboxExportJobResponse_s3BucketName = Lens.lens (\DescribeMailboxExportJobResponse' {s3BucketName} -> s3BucketName) (\s@DescribeMailboxExportJobResponse' {} a -> s {s3BucketName = a} :: DescribeMailboxExportJobResponse)

-- | Error information for failed mailbox export jobs.
describeMailboxExportJobResponse_errorInfo :: Lens.Lens' DescribeMailboxExportJobResponse (Core.Maybe Core.Text)
describeMailboxExportJobResponse_errorInfo = Lens.lens (\DescribeMailboxExportJobResponse' {errorInfo} -> errorInfo) (\s@DescribeMailboxExportJobResponse' {} a -> s {errorInfo = a} :: DescribeMailboxExportJobResponse)

-- | The mailbox export job description.
describeMailboxExportJobResponse_description :: Lens.Lens' DescribeMailboxExportJobResponse (Core.Maybe Core.Text)
describeMailboxExportJobResponse_description = Lens.lens (\DescribeMailboxExportJobResponse' {description} -> description) (\s@DescribeMailboxExportJobResponse' {} a -> s {description = a} :: DescribeMailboxExportJobResponse)

-- | The S3 bucket prefix.
describeMailboxExportJobResponse_s3Prefix :: Lens.Lens' DescribeMailboxExportJobResponse (Core.Maybe Core.Text)
describeMailboxExportJobResponse_s3Prefix = Lens.lens (\DescribeMailboxExportJobResponse' {s3Prefix} -> s3Prefix) (\s@DescribeMailboxExportJobResponse' {} a -> s {s3Prefix = a} :: DescribeMailboxExportJobResponse)

-- | The response's http status code.
describeMailboxExportJobResponse_httpStatus :: Lens.Lens' DescribeMailboxExportJobResponse Core.Int
describeMailboxExportJobResponse_httpStatus = Lens.lens (\DescribeMailboxExportJobResponse' {httpStatus} -> httpStatus) (\s@DescribeMailboxExportJobResponse' {} a -> s {httpStatus = a} :: DescribeMailboxExportJobResponse)

instance Core.NFData DescribeMailboxExportJobResponse
