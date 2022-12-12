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
-- Module      : Amazonka.IoT.DescribeJobTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a job template.
module Amazonka.IoT.DescribeJobTemplate
  ( -- * Creating a Request
    DescribeJobTemplate (..),
    newDescribeJobTemplate,

    -- * Request Lenses
    describeJobTemplate_jobTemplateId,

    -- * Destructuring the Response
    DescribeJobTemplateResponse (..),
    newDescribeJobTemplateResponse,

    -- * Response Lenses
    describeJobTemplateResponse_abortConfig,
    describeJobTemplateResponse_createdAt,
    describeJobTemplateResponse_description,
    describeJobTemplateResponse_document,
    describeJobTemplateResponse_documentSource,
    describeJobTemplateResponse_jobExecutionsRetryConfig,
    describeJobTemplateResponse_jobExecutionsRolloutConfig,
    describeJobTemplateResponse_jobTemplateArn,
    describeJobTemplateResponse_jobTemplateId,
    describeJobTemplateResponse_presignedUrlConfig,
    describeJobTemplateResponse_timeoutConfig,
    describeJobTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeJobTemplate' smart constructor.
data DescribeJobTemplate = DescribeJobTemplate'
  { -- | The unique identifier of the job template.
    jobTemplateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeJobTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobTemplateId', 'describeJobTemplate_jobTemplateId' - The unique identifier of the job template.
newDescribeJobTemplate ::
  -- | 'jobTemplateId'
  Prelude.Text ->
  DescribeJobTemplate
newDescribeJobTemplate pJobTemplateId_ =
  DescribeJobTemplate'
    { jobTemplateId =
        pJobTemplateId_
    }

-- | The unique identifier of the job template.
describeJobTemplate_jobTemplateId :: Lens.Lens' DescribeJobTemplate Prelude.Text
describeJobTemplate_jobTemplateId = Lens.lens (\DescribeJobTemplate' {jobTemplateId} -> jobTemplateId) (\s@DescribeJobTemplate' {} a -> s {jobTemplateId = a} :: DescribeJobTemplate)

instance Core.AWSRequest DescribeJobTemplate where
  type
    AWSResponse DescribeJobTemplate =
      DescribeJobTemplateResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeJobTemplateResponse'
            Prelude.<$> (x Data..?> "abortConfig")
            Prelude.<*> (x Data..?> "createdAt")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "document")
            Prelude.<*> (x Data..?> "documentSource")
            Prelude.<*> (x Data..?> "jobExecutionsRetryConfig")
            Prelude.<*> (x Data..?> "jobExecutionsRolloutConfig")
            Prelude.<*> (x Data..?> "jobTemplateArn")
            Prelude.<*> (x Data..?> "jobTemplateId")
            Prelude.<*> (x Data..?> "presignedUrlConfig")
            Prelude.<*> (x Data..?> "timeoutConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeJobTemplate where
  hashWithSalt _salt DescribeJobTemplate' {..} =
    _salt `Prelude.hashWithSalt` jobTemplateId

instance Prelude.NFData DescribeJobTemplate where
  rnf DescribeJobTemplate' {..} =
    Prelude.rnf jobTemplateId

instance Data.ToHeaders DescribeJobTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeJobTemplate where
  toPath DescribeJobTemplate' {..} =
    Prelude.mconcat
      ["/job-templates/", Data.toBS jobTemplateId]

instance Data.ToQuery DescribeJobTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeJobTemplateResponse' smart constructor.
data DescribeJobTemplateResponse = DescribeJobTemplateResponse'
  { abortConfig :: Prelude.Maybe AbortConfig,
    -- | The time, in seconds since the epoch, when the job template was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | A description of the job template.
    description :: Prelude.Maybe Prelude.Text,
    -- | The job document.
    document :: Prelude.Maybe Prelude.Text,
    -- | An S3 link to the job document.
    documentSource :: Prelude.Maybe Prelude.Text,
    -- | The configuration that determines how many retries are allowed for each
    -- failure type for a job.
    jobExecutionsRetryConfig :: Prelude.Maybe JobExecutionsRetryConfig,
    jobExecutionsRolloutConfig :: Prelude.Maybe JobExecutionsRolloutConfig,
    -- | The ARN of the job template.
    jobTemplateArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the job template.
    jobTemplateId :: Prelude.Maybe Prelude.Text,
    presignedUrlConfig :: Prelude.Maybe PresignedUrlConfig,
    timeoutConfig :: Prelude.Maybe TimeoutConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeJobTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'abortConfig', 'describeJobTemplateResponse_abortConfig' - Undocumented member.
--
-- 'createdAt', 'describeJobTemplateResponse_createdAt' - The time, in seconds since the epoch, when the job template was created.
--
-- 'description', 'describeJobTemplateResponse_description' - A description of the job template.
--
-- 'document', 'describeJobTemplateResponse_document' - The job document.
--
-- 'documentSource', 'describeJobTemplateResponse_documentSource' - An S3 link to the job document.
--
-- 'jobExecutionsRetryConfig', 'describeJobTemplateResponse_jobExecutionsRetryConfig' - The configuration that determines how many retries are allowed for each
-- failure type for a job.
--
-- 'jobExecutionsRolloutConfig', 'describeJobTemplateResponse_jobExecutionsRolloutConfig' - Undocumented member.
--
-- 'jobTemplateArn', 'describeJobTemplateResponse_jobTemplateArn' - The ARN of the job template.
--
-- 'jobTemplateId', 'describeJobTemplateResponse_jobTemplateId' - The unique identifier of the job template.
--
-- 'presignedUrlConfig', 'describeJobTemplateResponse_presignedUrlConfig' - Undocumented member.
--
-- 'timeoutConfig', 'describeJobTemplateResponse_timeoutConfig' - Undocumented member.
--
-- 'httpStatus', 'describeJobTemplateResponse_httpStatus' - The response's http status code.
newDescribeJobTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeJobTemplateResponse
newDescribeJobTemplateResponse pHttpStatus_ =
  DescribeJobTemplateResponse'
    { abortConfig =
        Prelude.Nothing,
      createdAt = Prelude.Nothing,
      description = Prelude.Nothing,
      document = Prelude.Nothing,
      documentSource = Prelude.Nothing,
      jobExecutionsRetryConfig = Prelude.Nothing,
      jobExecutionsRolloutConfig = Prelude.Nothing,
      jobTemplateArn = Prelude.Nothing,
      jobTemplateId = Prelude.Nothing,
      presignedUrlConfig = Prelude.Nothing,
      timeoutConfig = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeJobTemplateResponse_abortConfig :: Lens.Lens' DescribeJobTemplateResponse (Prelude.Maybe AbortConfig)
describeJobTemplateResponse_abortConfig = Lens.lens (\DescribeJobTemplateResponse' {abortConfig} -> abortConfig) (\s@DescribeJobTemplateResponse' {} a -> s {abortConfig = a} :: DescribeJobTemplateResponse)

-- | The time, in seconds since the epoch, when the job template was created.
describeJobTemplateResponse_createdAt :: Lens.Lens' DescribeJobTemplateResponse (Prelude.Maybe Prelude.UTCTime)
describeJobTemplateResponse_createdAt = Lens.lens (\DescribeJobTemplateResponse' {createdAt} -> createdAt) (\s@DescribeJobTemplateResponse' {} a -> s {createdAt = a} :: DescribeJobTemplateResponse) Prelude.. Lens.mapping Data._Time

-- | A description of the job template.
describeJobTemplateResponse_description :: Lens.Lens' DescribeJobTemplateResponse (Prelude.Maybe Prelude.Text)
describeJobTemplateResponse_description = Lens.lens (\DescribeJobTemplateResponse' {description} -> description) (\s@DescribeJobTemplateResponse' {} a -> s {description = a} :: DescribeJobTemplateResponse)

-- | The job document.
describeJobTemplateResponse_document :: Lens.Lens' DescribeJobTemplateResponse (Prelude.Maybe Prelude.Text)
describeJobTemplateResponse_document = Lens.lens (\DescribeJobTemplateResponse' {document} -> document) (\s@DescribeJobTemplateResponse' {} a -> s {document = a} :: DescribeJobTemplateResponse)

-- | An S3 link to the job document.
describeJobTemplateResponse_documentSource :: Lens.Lens' DescribeJobTemplateResponse (Prelude.Maybe Prelude.Text)
describeJobTemplateResponse_documentSource = Lens.lens (\DescribeJobTemplateResponse' {documentSource} -> documentSource) (\s@DescribeJobTemplateResponse' {} a -> s {documentSource = a} :: DescribeJobTemplateResponse)

-- | The configuration that determines how many retries are allowed for each
-- failure type for a job.
describeJobTemplateResponse_jobExecutionsRetryConfig :: Lens.Lens' DescribeJobTemplateResponse (Prelude.Maybe JobExecutionsRetryConfig)
describeJobTemplateResponse_jobExecutionsRetryConfig = Lens.lens (\DescribeJobTemplateResponse' {jobExecutionsRetryConfig} -> jobExecutionsRetryConfig) (\s@DescribeJobTemplateResponse' {} a -> s {jobExecutionsRetryConfig = a} :: DescribeJobTemplateResponse)

-- | Undocumented member.
describeJobTemplateResponse_jobExecutionsRolloutConfig :: Lens.Lens' DescribeJobTemplateResponse (Prelude.Maybe JobExecutionsRolloutConfig)
describeJobTemplateResponse_jobExecutionsRolloutConfig = Lens.lens (\DescribeJobTemplateResponse' {jobExecutionsRolloutConfig} -> jobExecutionsRolloutConfig) (\s@DescribeJobTemplateResponse' {} a -> s {jobExecutionsRolloutConfig = a} :: DescribeJobTemplateResponse)

-- | The ARN of the job template.
describeJobTemplateResponse_jobTemplateArn :: Lens.Lens' DescribeJobTemplateResponse (Prelude.Maybe Prelude.Text)
describeJobTemplateResponse_jobTemplateArn = Lens.lens (\DescribeJobTemplateResponse' {jobTemplateArn} -> jobTemplateArn) (\s@DescribeJobTemplateResponse' {} a -> s {jobTemplateArn = a} :: DescribeJobTemplateResponse)

-- | The unique identifier of the job template.
describeJobTemplateResponse_jobTemplateId :: Lens.Lens' DescribeJobTemplateResponse (Prelude.Maybe Prelude.Text)
describeJobTemplateResponse_jobTemplateId = Lens.lens (\DescribeJobTemplateResponse' {jobTemplateId} -> jobTemplateId) (\s@DescribeJobTemplateResponse' {} a -> s {jobTemplateId = a} :: DescribeJobTemplateResponse)

-- | Undocumented member.
describeJobTemplateResponse_presignedUrlConfig :: Lens.Lens' DescribeJobTemplateResponse (Prelude.Maybe PresignedUrlConfig)
describeJobTemplateResponse_presignedUrlConfig = Lens.lens (\DescribeJobTemplateResponse' {presignedUrlConfig} -> presignedUrlConfig) (\s@DescribeJobTemplateResponse' {} a -> s {presignedUrlConfig = a} :: DescribeJobTemplateResponse)

-- | Undocumented member.
describeJobTemplateResponse_timeoutConfig :: Lens.Lens' DescribeJobTemplateResponse (Prelude.Maybe TimeoutConfig)
describeJobTemplateResponse_timeoutConfig = Lens.lens (\DescribeJobTemplateResponse' {timeoutConfig} -> timeoutConfig) (\s@DescribeJobTemplateResponse' {} a -> s {timeoutConfig = a} :: DescribeJobTemplateResponse)

-- | The response's http status code.
describeJobTemplateResponse_httpStatus :: Lens.Lens' DescribeJobTemplateResponse Prelude.Int
describeJobTemplateResponse_httpStatus = Lens.lens (\DescribeJobTemplateResponse' {httpStatus} -> httpStatus) (\s@DescribeJobTemplateResponse' {} a -> s {httpStatus = a} :: DescribeJobTemplateResponse)

instance Prelude.NFData DescribeJobTemplateResponse where
  rnf DescribeJobTemplateResponse' {..} =
    Prelude.rnf abortConfig
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf document
      `Prelude.seq` Prelude.rnf documentSource
      `Prelude.seq` Prelude.rnf jobExecutionsRetryConfig
      `Prelude.seq` Prelude.rnf jobExecutionsRolloutConfig
      `Prelude.seq` Prelude.rnf jobTemplateArn
      `Prelude.seq` Prelude.rnf jobTemplateId
      `Prelude.seq` Prelude.rnf presignedUrlConfig
      `Prelude.seq` Prelude.rnf timeoutConfig
      `Prelude.seq` Prelude.rnf httpStatus
