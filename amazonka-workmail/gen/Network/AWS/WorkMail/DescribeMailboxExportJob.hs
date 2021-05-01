{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newDescribeMailboxExportJob' smart constructor.
data DescribeMailboxExportJob = DescribeMailboxExportJob'
  { -- | The mailbox export job ID.
    jobId :: Prelude.Text,
    -- | The organization ID.
    organizationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DescribeMailboxExportJob where
  type
    Rs DescribeMailboxExportJob =
      DescribeMailboxExportJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMailboxExportJobResponse'
            Prelude.<$> (x Prelude..?> "EstimatedProgress")
            Prelude.<*> (x Prelude..?> "RoleArn")
            Prelude.<*> (x Prelude..?> "EntityId")
            Prelude.<*> (x Prelude..?> "StartTime")
            Prelude.<*> (x Prelude..?> "S3Path")
            Prelude.<*> (x Prelude..?> "EndTime")
            Prelude.<*> (x Prelude..?> "State")
            Prelude.<*> (x Prelude..?> "KmsKeyArn")
            Prelude.<*> (x Prelude..?> "S3BucketName")
            Prelude.<*> (x Prelude..?> "ErrorInfo")
            Prelude.<*> (x Prelude..?> "Description")
            Prelude.<*> (x Prelude..?> "S3Prefix")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeMailboxExportJob

instance Prelude.NFData DescribeMailboxExportJob

instance Prelude.ToHeaders DescribeMailboxExportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkMailService.DescribeMailboxExportJob" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeMailboxExportJob where
  toJSON DescribeMailboxExportJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("JobId" Prelude..= jobId),
            Prelude.Just
              ("OrganizationId" Prelude..= organizationId)
          ]
      )

instance Prelude.ToPath DescribeMailboxExportJob where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeMailboxExportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeMailboxExportJobResponse' smart constructor.
data DescribeMailboxExportJobResponse = DescribeMailboxExportJobResponse'
  { -- | The estimated progress of the mailbox export job, in percentage points.
    estimatedProgress :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the AWS Identity and Access Management (IAM) role that grants
    -- write permission to the Amazon Simple Storage Service (Amazon S3)
    -- bucket.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the user or resource associated with the mailbox.
    entityId :: Prelude.Maybe Prelude.Text,
    -- | The mailbox export job start timestamp.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | The path to the S3 bucket and file that the mailbox export job is
    -- exporting to.
    s3Path :: Prelude.Maybe Prelude.Text,
    -- | The mailbox export job end timestamp.
    endTime :: Prelude.Maybe Prelude.POSIX,
    -- | The state of the mailbox export job.
    state :: Prelude.Maybe MailboxExportJobState,
    -- | The Amazon Resource Name (ARN) of the symmetric AWS Key Management
    -- Service (AWS KMS) key that encrypts the exported mailbox content.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket.
    s3BucketName :: Prelude.Maybe Prelude.Text,
    -- | Error information for failed mailbox export jobs.
    errorInfo :: Prelude.Maybe Prelude.Text,
    -- | The mailbox export job description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The S3 bucket prefix.
    s3Prefix :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeMailboxExportJobResponse
newDescribeMailboxExportJobResponse pHttpStatus_ =
  DescribeMailboxExportJobResponse'
    { estimatedProgress =
        Prelude.Nothing,
      roleArn = Prelude.Nothing,
      entityId = Prelude.Nothing,
      startTime = Prelude.Nothing,
      s3Path = Prelude.Nothing,
      endTime = Prelude.Nothing,
      state = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      s3BucketName = Prelude.Nothing,
      errorInfo = Prelude.Nothing,
      description = Prelude.Nothing,
      s3Prefix = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The estimated progress of the mailbox export job, in percentage points.
describeMailboxExportJobResponse_estimatedProgress :: Lens.Lens' DescribeMailboxExportJobResponse (Prelude.Maybe Prelude.Natural)
describeMailboxExportJobResponse_estimatedProgress = Lens.lens (\DescribeMailboxExportJobResponse' {estimatedProgress} -> estimatedProgress) (\s@DescribeMailboxExportJobResponse' {} a -> s {estimatedProgress = a} :: DescribeMailboxExportJobResponse)

-- | The ARN of the AWS Identity and Access Management (IAM) role that grants
-- write permission to the Amazon Simple Storage Service (Amazon S3)
-- bucket.
describeMailboxExportJobResponse_roleArn :: Lens.Lens' DescribeMailboxExportJobResponse (Prelude.Maybe Prelude.Text)
describeMailboxExportJobResponse_roleArn = Lens.lens (\DescribeMailboxExportJobResponse' {roleArn} -> roleArn) (\s@DescribeMailboxExportJobResponse' {} a -> s {roleArn = a} :: DescribeMailboxExportJobResponse)

-- | The identifier of the user or resource associated with the mailbox.
describeMailboxExportJobResponse_entityId :: Lens.Lens' DescribeMailboxExportJobResponse (Prelude.Maybe Prelude.Text)
describeMailboxExportJobResponse_entityId = Lens.lens (\DescribeMailboxExportJobResponse' {entityId} -> entityId) (\s@DescribeMailboxExportJobResponse' {} a -> s {entityId = a} :: DescribeMailboxExportJobResponse)

-- | The mailbox export job start timestamp.
describeMailboxExportJobResponse_startTime :: Lens.Lens' DescribeMailboxExportJobResponse (Prelude.Maybe Prelude.UTCTime)
describeMailboxExportJobResponse_startTime = Lens.lens (\DescribeMailboxExportJobResponse' {startTime} -> startTime) (\s@DescribeMailboxExportJobResponse' {} a -> s {startTime = a} :: DescribeMailboxExportJobResponse) Prelude.. Lens.mapping Prelude._Time

-- | The path to the S3 bucket and file that the mailbox export job is
-- exporting to.
describeMailboxExportJobResponse_s3Path :: Lens.Lens' DescribeMailboxExportJobResponse (Prelude.Maybe Prelude.Text)
describeMailboxExportJobResponse_s3Path = Lens.lens (\DescribeMailboxExportJobResponse' {s3Path} -> s3Path) (\s@DescribeMailboxExportJobResponse' {} a -> s {s3Path = a} :: DescribeMailboxExportJobResponse)

-- | The mailbox export job end timestamp.
describeMailboxExportJobResponse_endTime :: Lens.Lens' DescribeMailboxExportJobResponse (Prelude.Maybe Prelude.UTCTime)
describeMailboxExportJobResponse_endTime = Lens.lens (\DescribeMailboxExportJobResponse' {endTime} -> endTime) (\s@DescribeMailboxExportJobResponse' {} a -> s {endTime = a} :: DescribeMailboxExportJobResponse) Prelude.. Lens.mapping Prelude._Time

-- | The state of the mailbox export job.
describeMailboxExportJobResponse_state :: Lens.Lens' DescribeMailboxExportJobResponse (Prelude.Maybe MailboxExportJobState)
describeMailboxExportJobResponse_state = Lens.lens (\DescribeMailboxExportJobResponse' {state} -> state) (\s@DescribeMailboxExportJobResponse' {} a -> s {state = a} :: DescribeMailboxExportJobResponse)

-- | The Amazon Resource Name (ARN) of the symmetric AWS Key Management
-- Service (AWS KMS) key that encrypts the exported mailbox content.
describeMailboxExportJobResponse_kmsKeyArn :: Lens.Lens' DescribeMailboxExportJobResponse (Prelude.Maybe Prelude.Text)
describeMailboxExportJobResponse_kmsKeyArn = Lens.lens (\DescribeMailboxExportJobResponse' {kmsKeyArn} -> kmsKeyArn) (\s@DescribeMailboxExportJobResponse' {} a -> s {kmsKeyArn = a} :: DescribeMailboxExportJobResponse)

-- | The name of the S3 bucket.
describeMailboxExportJobResponse_s3BucketName :: Lens.Lens' DescribeMailboxExportJobResponse (Prelude.Maybe Prelude.Text)
describeMailboxExportJobResponse_s3BucketName = Lens.lens (\DescribeMailboxExportJobResponse' {s3BucketName} -> s3BucketName) (\s@DescribeMailboxExportJobResponse' {} a -> s {s3BucketName = a} :: DescribeMailboxExportJobResponse)

-- | Error information for failed mailbox export jobs.
describeMailboxExportJobResponse_errorInfo :: Lens.Lens' DescribeMailboxExportJobResponse (Prelude.Maybe Prelude.Text)
describeMailboxExportJobResponse_errorInfo = Lens.lens (\DescribeMailboxExportJobResponse' {errorInfo} -> errorInfo) (\s@DescribeMailboxExportJobResponse' {} a -> s {errorInfo = a} :: DescribeMailboxExportJobResponse)

-- | The mailbox export job description.
describeMailboxExportJobResponse_description :: Lens.Lens' DescribeMailboxExportJobResponse (Prelude.Maybe Prelude.Text)
describeMailboxExportJobResponse_description = Lens.lens (\DescribeMailboxExportJobResponse' {description} -> description) (\s@DescribeMailboxExportJobResponse' {} a -> s {description = a} :: DescribeMailboxExportJobResponse)

-- | The S3 bucket prefix.
describeMailboxExportJobResponse_s3Prefix :: Lens.Lens' DescribeMailboxExportJobResponse (Prelude.Maybe Prelude.Text)
describeMailboxExportJobResponse_s3Prefix = Lens.lens (\DescribeMailboxExportJobResponse' {s3Prefix} -> s3Prefix) (\s@DescribeMailboxExportJobResponse' {} a -> s {s3Prefix = a} :: DescribeMailboxExportJobResponse)

-- | The response's http status code.
describeMailboxExportJobResponse_httpStatus :: Lens.Lens' DescribeMailboxExportJobResponse Prelude.Int
describeMailboxExportJobResponse_httpStatus = Lens.lens (\DescribeMailboxExportJobResponse' {httpStatus} -> httpStatus) (\s@DescribeMailboxExportJobResponse' {} a -> s {httpStatus = a} :: DescribeMailboxExportJobResponse)

instance
  Prelude.NFData
    DescribeMailboxExportJobResponse
