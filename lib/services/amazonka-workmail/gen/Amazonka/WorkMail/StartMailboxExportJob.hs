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
-- Module      : Amazonka.WorkMail.StartMailboxExportJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a mailbox export job to export MIME-format email messages and
-- calendar items from the specified mailbox to the specified Amazon Simple
-- Storage Service (Amazon S3) bucket. For more information, see
-- <https://docs.aws.amazon.com/workmail/latest/adminguide/mail-export.html Exporting mailbox content>
-- in the /WorkMail Administrator Guide/.
module Amazonka.WorkMail.StartMailboxExportJob
  ( -- * Creating a Request
    StartMailboxExportJob (..),
    newStartMailboxExportJob,

    -- * Request Lenses
    startMailboxExportJob_description,
    startMailboxExportJob_clientToken,
    startMailboxExportJob_organizationId,
    startMailboxExportJob_entityId,
    startMailboxExportJob_roleArn,
    startMailboxExportJob_kmsKeyArn,
    startMailboxExportJob_s3BucketName,
    startMailboxExportJob_s3Prefix,

    -- * Destructuring the Response
    StartMailboxExportJobResponse (..),
    newStartMailboxExportJobResponse,

    -- * Response Lenses
    startMailboxExportJobResponse_jobId,
    startMailboxExportJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newStartMailboxExportJob' smart constructor.
data StartMailboxExportJob = StartMailboxExportJob'
  { -- | The mailbox export job description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The idempotency token for the client request.
    clientToken :: Prelude.Text,
    -- | The identifier associated with the organization.
    organizationId :: Prelude.Text,
    -- | The identifier of the user or resource associated with the mailbox.
    entityId :: Prelude.Text,
    -- | The ARN of the AWS Identity and Access Management (IAM) role that grants
    -- write permission to the S3 bucket.
    roleArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the symmetric AWS Key Management
    -- Service (AWS KMS) key that encrypts the exported mailbox content.
    kmsKeyArn :: Prelude.Text,
    -- | The name of the S3 bucket.
    s3BucketName :: Prelude.Text,
    -- | The S3 bucket prefix.
    s3Prefix :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartMailboxExportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'startMailboxExportJob_description' - The mailbox export job description.
--
-- 'clientToken', 'startMailboxExportJob_clientToken' - The idempotency token for the client request.
--
-- 'organizationId', 'startMailboxExportJob_organizationId' - The identifier associated with the organization.
--
-- 'entityId', 'startMailboxExportJob_entityId' - The identifier of the user or resource associated with the mailbox.
--
-- 'roleArn', 'startMailboxExportJob_roleArn' - The ARN of the AWS Identity and Access Management (IAM) role that grants
-- write permission to the S3 bucket.
--
-- 'kmsKeyArn', 'startMailboxExportJob_kmsKeyArn' - The Amazon Resource Name (ARN) of the symmetric AWS Key Management
-- Service (AWS KMS) key that encrypts the exported mailbox content.
--
-- 's3BucketName', 'startMailboxExportJob_s3BucketName' - The name of the S3 bucket.
--
-- 's3Prefix', 'startMailboxExportJob_s3Prefix' - The S3 bucket prefix.
newStartMailboxExportJob ::
  -- | 'clientToken'
  Prelude.Text ->
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'entityId'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'kmsKeyArn'
  Prelude.Text ->
  -- | 's3BucketName'
  Prelude.Text ->
  -- | 's3Prefix'
  Prelude.Text ->
  StartMailboxExportJob
newStartMailboxExportJob
  pClientToken_
  pOrganizationId_
  pEntityId_
  pRoleArn_
  pKmsKeyArn_
  pS3BucketName_
  pS3Prefix_ =
    StartMailboxExportJob'
      { description =
          Prelude.Nothing,
        clientToken = pClientToken_,
        organizationId = pOrganizationId_,
        entityId = pEntityId_,
        roleArn = pRoleArn_,
        kmsKeyArn = pKmsKeyArn_,
        s3BucketName = pS3BucketName_,
        s3Prefix = pS3Prefix_
      }

-- | The mailbox export job description.
startMailboxExportJob_description :: Lens.Lens' StartMailboxExportJob (Prelude.Maybe Prelude.Text)
startMailboxExportJob_description = Lens.lens (\StartMailboxExportJob' {description} -> description) (\s@StartMailboxExportJob' {} a -> s {description = a} :: StartMailboxExportJob)

-- | The idempotency token for the client request.
startMailboxExportJob_clientToken :: Lens.Lens' StartMailboxExportJob Prelude.Text
startMailboxExportJob_clientToken = Lens.lens (\StartMailboxExportJob' {clientToken} -> clientToken) (\s@StartMailboxExportJob' {} a -> s {clientToken = a} :: StartMailboxExportJob)

-- | The identifier associated with the organization.
startMailboxExportJob_organizationId :: Lens.Lens' StartMailboxExportJob Prelude.Text
startMailboxExportJob_organizationId = Lens.lens (\StartMailboxExportJob' {organizationId} -> organizationId) (\s@StartMailboxExportJob' {} a -> s {organizationId = a} :: StartMailboxExportJob)

-- | The identifier of the user or resource associated with the mailbox.
startMailboxExportJob_entityId :: Lens.Lens' StartMailboxExportJob Prelude.Text
startMailboxExportJob_entityId = Lens.lens (\StartMailboxExportJob' {entityId} -> entityId) (\s@StartMailboxExportJob' {} a -> s {entityId = a} :: StartMailboxExportJob)

-- | The ARN of the AWS Identity and Access Management (IAM) role that grants
-- write permission to the S3 bucket.
startMailboxExportJob_roleArn :: Lens.Lens' StartMailboxExportJob Prelude.Text
startMailboxExportJob_roleArn = Lens.lens (\StartMailboxExportJob' {roleArn} -> roleArn) (\s@StartMailboxExportJob' {} a -> s {roleArn = a} :: StartMailboxExportJob)

-- | The Amazon Resource Name (ARN) of the symmetric AWS Key Management
-- Service (AWS KMS) key that encrypts the exported mailbox content.
startMailboxExportJob_kmsKeyArn :: Lens.Lens' StartMailboxExportJob Prelude.Text
startMailboxExportJob_kmsKeyArn = Lens.lens (\StartMailboxExportJob' {kmsKeyArn} -> kmsKeyArn) (\s@StartMailboxExportJob' {} a -> s {kmsKeyArn = a} :: StartMailboxExportJob)

-- | The name of the S3 bucket.
startMailboxExportJob_s3BucketName :: Lens.Lens' StartMailboxExportJob Prelude.Text
startMailboxExportJob_s3BucketName = Lens.lens (\StartMailboxExportJob' {s3BucketName} -> s3BucketName) (\s@StartMailboxExportJob' {} a -> s {s3BucketName = a} :: StartMailboxExportJob)

-- | The S3 bucket prefix.
startMailboxExportJob_s3Prefix :: Lens.Lens' StartMailboxExportJob Prelude.Text
startMailboxExportJob_s3Prefix = Lens.lens (\StartMailboxExportJob' {s3Prefix} -> s3Prefix) (\s@StartMailboxExportJob' {} a -> s {s3Prefix = a} :: StartMailboxExportJob)

instance Core.AWSRequest StartMailboxExportJob where
  type
    AWSResponse StartMailboxExportJob =
      StartMailboxExportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartMailboxExportJobResponse'
            Prelude.<$> (x Data..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartMailboxExportJob where
  hashWithSalt _salt StartMailboxExportJob' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` entityId
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` s3BucketName
      `Prelude.hashWithSalt` s3Prefix

instance Prelude.NFData StartMailboxExportJob where
  rnf StartMailboxExportJob' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf entityId
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf s3BucketName
      `Prelude.seq` Prelude.rnf s3Prefix

instance Data.ToHeaders StartMailboxExportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.StartMailboxExportJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartMailboxExportJob where
  toJSON StartMailboxExportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            Prelude.Just ("ClientToken" Data..= clientToken),
            Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("EntityId" Data..= entityId),
            Prelude.Just ("RoleArn" Data..= roleArn),
            Prelude.Just ("KmsKeyArn" Data..= kmsKeyArn),
            Prelude.Just ("S3BucketName" Data..= s3BucketName),
            Prelude.Just ("S3Prefix" Data..= s3Prefix)
          ]
      )

instance Data.ToPath StartMailboxExportJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StartMailboxExportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartMailboxExportJobResponse' smart constructor.
data StartMailboxExportJobResponse = StartMailboxExportJobResponse'
  { -- | The job ID.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartMailboxExportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'startMailboxExportJobResponse_jobId' - The job ID.
--
-- 'httpStatus', 'startMailboxExportJobResponse_httpStatus' - The response's http status code.
newStartMailboxExportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartMailboxExportJobResponse
newStartMailboxExportJobResponse pHttpStatus_ =
  StartMailboxExportJobResponse'
    { jobId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The job ID.
startMailboxExportJobResponse_jobId :: Lens.Lens' StartMailboxExportJobResponse (Prelude.Maybe Prelude.Text)
startMailboxExportJobResponse_jobId = Lens.lens (\StartMailboxExportJobResponse' {jobId} -> jobId) (\s@StartMailboxExportJobResponse' {} a -> s {jobId = a} :: StartMailboxExportJobResponse)

-- | The response's http status code.
startMailboxExportJobResponse_httpStatus :: Lens.Lens' StartMailboxExportJobResponse Prelude.Int
startMailboxExportJobResponse_httpStatus = Lens.lens (\StartMailboxExportJobResponse' {httpStatus} -> httpStatus) (\s@StartMailboxExportJobResponse' {} a -> s {httpStatus = a} :: StartMailboxExportJobResponse)

instance Prelude.NFData StartMailboxExportJobResponse where
  rnf StartMailboxExportJobResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf httpStatus
