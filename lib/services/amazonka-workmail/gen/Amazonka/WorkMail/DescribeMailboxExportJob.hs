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
-- Module      : Amazonka.WorkMail.DescribeMailboxExportJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the current status of a mailbox export job.
module Amazonka.WorkMail.DescribeMailboxExportJob
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
    describeMailboxExportJobResponse_entityId,
    describeMailboxExportJobResponse_roleArn,
    describeMailboxExportJobResponse_s3BucketName,
    describeMailboxExportJobResponse_state,
    describeMailboxExportJobResponse_endTime,
    describeMailboxExportJobResponse_description,
    describeMailboxExportJobResponse_kmsKeyArn,
    describeMailboxExportJobResponse_s3Path,
    describeMailboxExportJobResponse_errorInfo,
    describeMailboxExportJobResponse_startTime,
    describeMailboxExportJobResponse_s3Prefix,
    describeMailboxExportJobResponse_estimatedProgress,
    describeMailboxExportJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newDescribeMailboxExportJob' smart constructor.
data DescribeMailboxExportJob = DescribeMailboxExportJob'
  { -- | The mailbox export job ID.
    jobId :: Prelude.Text,
    -- | The organization ID.
    organizationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'organizationId'
  Prelude.Text ->
  DescribeMailboxExportJob
newDescribeMailboxExportJob pJobId_ pOrganizationId_ =
  DescribeMailboxExportJob'
    { jobId = pJobId_,
      organizationId = pOrganizationId_
    }

-- | The mailbox export job ID.
describeMailboxExportJob_jobId :: Lens.Lens' DescribeMailboxExportJob Prelude.Text
describeMailboxExportJob_jobId = Lens.lens (\DescribeMailboxExportJob' {jobId} -> jobId) (\s@DescribeMailboxExportJob' {} a -> s {jobId = a} :: DescribeMailboxExportJob)

-- | The organization ID.
describeMailboxExportJob_organizationId :: Lens.Lens' DescribeMailboxExportJob Prelude.Text
describeMailboxExportJob_organizationId = Lens.lens (\DescribeMailboxExportJob' {organizationId} -> organizationId) (\s@DescribeMailboxExportJob' {} a -> s {organizationId = a} :: DescribeMailboxExportJob)

instance Core.AWSRequest DescribeMailboxExportJob where
  type
    AWSResponse DescribeMailboxExportJob =
      DescribeMailboxExportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMailboxExportJobResponse'
            Prelude.<$> (x Core..?> "EntityId")
            Prelude.<*> (x Core..?> "RoleArn")
            Prelude.<*> (x Core..?> "S3BucketName")
            Prelude.<*> (x Core..?> "State")
            Prelude.<*> (x Core..?> "EndTime")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "KmsKeyArn")
            Prelude.<*> (x Core..?> "S3Path")
            Prelude.<*> (x Core..?> "ErrorInfo")
            Prelude.<*> (x Core..?> "StartTime")
            Prelude.<*> (x Core..?> "S3Prefix")
            Prelude.<*> (x Core..?> "EstimatedProgress")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeMailboxExportJob where
  hashWithSalt _salt DescribeMailboxExportJob' {..} =
    _salt `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` organizationId

instance Prelude.NFData DescribeMailboxExportJob where
  rnf DescribeMailboxExportJob' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf organizationId

instance Core.ToHeaders DescribeMailboxExportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.DescribeMailboxExportJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeMailboxExportJob where
  toJSON DescribeMailboxExportJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("JobId" Core..= jobId),
            Prelude.Just
              ("OrganizationId" Core..= organizationId)
          ]
      )

instance Core.ToPath DescribeMailboxExportJob where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeMailboxExportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeMailboxExportJobResponse' smart constructor.
data DescribeMailboxExportJobResponse = DescribeMailboxExportJobResponse'
  { -- | The identifier of the user or resource associated with the mailbox.
    entityId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the AWS Identity and Access Management (IAM) role that grants
    -- write permission to the Amazon Simple Storage Service (Amazon S3)
    -- bucket.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket.
    s3BucketName :: Prelude.Maybe Prelude.Text,
    -- | The state of the mailbox export job.
    state :: Prelude.Maybe MailboxExportJobState,
    -- | The mailbox export job end timestamp.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The mailbox export job description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the symmetric AWS Key Management
    -- Service (AWS KMS) key that encrypts the exported mailbox content.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The path to the S3 bucket and file that the mailbox export job is
    -- exporting to.
    s3Path :: Prelude.Maybe Prelude.Text,
    -- | Error information for failed mailbox export jobs.
    errorInfo :: Prelude.Maybe Prelude.Text,
    -- | The mailbox export job start timestamp.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The S3 bucket prefix.
    s3Prefix :: Prelude.Maybe Prelude.Text,
    -- | The estimated progress of the mailbox export job, in percentage points.
    estimatedProgress :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMailboxExportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entityId', 'describeMailboxExportJobResponse_entityId' - The identifier of the user or resource associated with the mailbox.
--
-- 'roleArn', 'describeMailboxExportJobResponse_roleArn' - The ARN of the AWS Identity and Access Management (IAM) role that grants
-- write permission to the Amazon Simple Storage Service (Amazon S3)
-- bucket.
--
-- 's3BucketName', 'describeMailboxExportJobResponse_s3BucketName' - The name of the S3 bucket.
--
-- 'state', 'describeMailboxExportJobResponse_state' - The state of the mailbox export job.
--
-- 'endTime', 'describeMailboxExportJobResponse_endTime' - The mailbox export job end timestamp.
--
-- 'description', 'describeMailboxExportJobResponse_description' - The mailbox export job description.
--
-- 'kmsKeyArn', 'describeMailboxExportJobResponse_kmsKeyArn' - The Amazon Resource Name (ARN) of the symmetric AWS Key Management
-- Service (AWS KMS) key that encrypts the exported mailbox content.
--
-- 's3Path', 'describeMailboxExportJobResponse_s3Path' - The path to the S3 bucket and file that the mailbox export job is
-- exporting to.
--
-- 'errorInfo', 'describeMailboxExportJobResponse_errorInfo' - Error information for failed mailbox export jobs.
--
-- 'startTime', 'describeMailboxExportJobResponse_startTime' - The mailbox export job start timestamp.
--
-- 's3Prefix', 'describeMailboxExportJobResponse_s3Prefix' - The S3 bucket prefix.
--
-- 'estimatedProgress', 'describeMailboxExportJobResponse_estimatedProgress' - The estimated progress of the mailbox export job, in percentage points.
--
-- 'httpStatus', 'describeMailboxExportJobResponse_httpStatus' - The response's http status code.
newDescribeMailboxExportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeMailboxExportJobResponse
newDescribeMailboxExportJobResponse pHttpStatus_ =
  DescribeMailboxExportJobResponse'
    { entityId =
        Prelude.Nothing,
      roleArn = Prelude.Nothing,
      s3BucketName = Prelude.Nothing,
      state = Prelude.Nothing,
      endTime = Prelude.Nothing,
      description = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      s3Path = Prelude.Nothing,
      errorInfo = Prelude.Nothing,
      startTime = Prelude.Nothing,
      s3Prefix = Prelude.Nothing,
      estimatedProgress = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the user or resource associated with the mailbox.
describeMailboxExportJobResponse_entityId :: Lens.Lens' DescribeMailboxExportJobResponse (Prelude.Maybe Prelude.Text)
describeMailboxExportJobResponse_entityId = Lens.lens (\DescribeMailboxExportJobResponse' {entityId} -> entityId) (\s@DescribeMailboxExportJobResponse' {} a -> s {entityId = a} :: DescribeMailboxExportJobResponse)

-- | The ARN of the AWS Identity and Access Management (IAM) role that grants
-- write permission to the Amazon Simple Storage Service (Amazon S3)
-- bucket.
describeMailboxExportJobResponse_roleArn :: Lens.Lens' DescribeMailboxExportJobResponse (Prelude.Maybe Prelude.Text)
describeMailboxExportJobResponse_roleArn = Lens.lens (\DescribeMailboxExportJobResponse' {roleArn} -> roleArn) (\s@DescribeMailboxExportJobResponse' {} a -> s {roleArn = a} :: DescribeMailboxExportJobResponse)

-- | The name of the S3 bucket.
describeMailboxExportJobResponse_s3BucketName :: Lens.Lens' DescribeMailboxExportJobResponse (Prelude.Maybe Prelude.Text)
describeMailboxExportJobResponse_s3BucketName = Lens.lens (\DescribeMailboxExportJobResponse' {s3BucketName} -> s3BucketName) (\s@DescribeMailboxExportJobResponse' {} a -> s {s3BucketName = a} :: DescribeMailboxExportJobResponse)

-- | The state of the mailbox export job.
describeMailboxExportJobResponse_state :: Lens.Lens' DescribeMailboxExportJobResponse (Prelude.Maybe MailboxExportJobState)
describeMailboxExportJobResponse_state = Lens.lens (\DescribeMailboxExportJobResponse' {state} -> state) (\s@DescribeMailboxExportJobResponse' {} a -> s {state = a} :: DescribeMailboxExportJobResponse)

-- | The mailbox export job end timestamp.
describeMailboxExportJobResponse_endTime :: Lens.Lens' DescribeMailboxExportJobResponse (Prelude.Maybe Prelude.UTCTime)
describeMailboxExportJobResponse_endTime = Lens.lens (\DescribeMailboxExportJobResponse' {endTime} -> endTime) (\s@DescribeMailboxExportJobResponse' {} a -> s {endTime = a} :: DescribeMailboxExportJobResponse) Prelude.. Lens.mapping Core._Time

-- | The mailbox export job description.
describeMailboxExportJobResponse_description :: Lens.Lens' DescribeMailboxExportJobResponse (Prelude.Maybe Prelude.Text)
describeMailboxExportJobResponse_description = Lens.lens (\DescribeMailboxExportJobResponse' {description} -> description) (\s@DescribeMailboxExportJobResponse' {} a -> s {description = a} :: DescribeMailboxExportJobResponse)

-- | The Amazon Resource Name (ARN) of the symmetric AWS Key Management
-- Service (AWS KMS) key that encrypts the exported mailbox content.
describeMailboxExportJobResponse_kmsKeyArn :: Lens.Lens' DescribeMailboxExportJobResponse (Prelude.Maybe Prelude.Text)
describeMailboxExportJobResponse_kmsKeyArn = Lens.lens (\DescribeMailboxExportJobResponse' {kmsKeyArn} -> kmsKeyArn) (\s@DescribeMailboxExportJobResponse' {} a -> s {kmsKeyArn = a} :: DescribeMailboxExportJobResponse)

-- | The path to the S3 bucket and file that the mailbox export job is
-- exporting to.
describeMailboxExportJobResponse_s3Path :: Lens.Lens' DescribeMailboxExportJobResponse (Prelude.Maybe Prelude.Text)
describeMailboxExportJobResponse_s3Path = Lens.lens (\DescribeMailboxExportJobResponse' {s3Path} -> s3Path) (\s@DescribeMailboxExportJobResponse' {} a -> s {s3Path = a} :: DescribeMailboxExportJobResponse)

-- | Error information for failed mailbox export jobs.
describeMailboxExportJobResponse_errorInfo :: Lens.Lens' DescribeMailboxExportJobResponse (Prelude.Maybe Prelude.Text)
describeMailboxExportJobResponse_errorInfo = Lens.lens (\DescribeMailboxExportJobResponse' {errorInfo} -> errorInfo) (\s@DescribeMailboxExportJobResponse' {} a -> s {errorInfo = a} :: DescribeMailboxExportJobResponse)

-- | The mailbox export job start timestamp.
describeMailboxExportJobResponse_startTime :: Lens.Lens' DescribeMailboxExportJobResponse (Prelude.Maybe Prelude.UTCTime)
describeMailboxExportJobResponse_startTime = Lens.lens (\DescribeMailboxExportJobResponse' {startTime} -> startTime) (\s@DescribeMailboxExportJobResponse' {} a -> s {startTime = a} :: DescribeMailboxExportJobResponse) Prelude.. Lens.mapping Core._Time

-- | The S3 bucket prefix.
describeMailboxExportJobResponse_s3Prefix :: Lens.Lens' DescribeMailboxExportJobResponse (Prelude.Maybe Prelude.Text)
describeMailboxExportJobResponse_s3Prefix = Lens.lens (\DescribeMailboxExportJobResponse' {s3Prefix} -> s3Prefix) (\s@DescribeMailboxExportJobResponse' {} a -> s {s3Prefix = a} :: DescribeMailboxExportJobResponse)

-- | The estimated progress of the mailbox export job, in percentage points.
describeMailboxExportJobResponse_estimatedProgress :: Lens.Lens' DescribeMailboxExportJobResponse (Prelude.Maybe Prelude.Natural)
describeMailboxExportJobResponse_estimatedProgress = Lens.lens (\DescribeMailboxExportJobResponse' {estimatedProgress} -> estimatedProgress) (\s@DescribeMailboxExportJobResponse' {} a -> s {estimatedProgress = a} :: DescribeMailboxExportJobResponse)

-- | The response's http status code.
describeMailboxExportJobResponse_httpStatus :: Lens.Lens' DescribeMailboxExportJobResponse Prelude.Int
describeMailboxExportJobResponse_httpStatus = Lens.lens (\DescribeMailboxExportJobResponse' {httpStatus} -> httpStatus) (\s@DescribeMailboxExportJobResponse' {} a -> s {httpStatus = a} :: DescribeMailboxExportJobResponse)

instance
  Prelude.NFData
    DescribeMailboxExportJobResponse
  where
  rnf DescribeMailboxExportJobResponse' {..} =
    Prelude.rnf entityId
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf s3BucketName
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf s3Path
      `Prelude.seq` Prelude.rnf errorInfo
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf s3Prefix
      `Prelude.seq` Prelude.rnf estimatedProgress
      `Prelude.seq` Prelude.rnf httpStatus
