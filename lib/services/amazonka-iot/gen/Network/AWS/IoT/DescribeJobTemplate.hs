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
-- Module      : Network.AWS.IoT.DescribeJobTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a job template.
module Network.AWS.IoT.DescribeJobTemplate
  ( -- * Creating a Request
    DescribeJobTemplate (..),
    newDescribeJobTemplate,

    -- * Request Lenses
    describeJobTemplate_jobTemplateId,

    -- * Destructuring the Response
    DescribeJobTemplateResponse (..),
    newDescribeJobTemplateResponse,

    -- * Response Lenses
    describeJobTemplateResponse_jobExecutionsRolloutConfig,
    describeJobTemplateResponse_timeoutConfig,
    describeJobTemplateResponse_createdAt,
    describeJobTemplateResponse_jobTemplateId,
    describeJobTemplateResponse_documentSource,
    describeJobTemplateResponse_document,
    describeJobTemplateResponse_presignedUrlConfig,
    describeJobTemplateResponse_description,
    describeJobTemplateResponse_abortConfig,
    describeJobTemplateResponse_jobTemplateArn,
    describeJobTemplateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeJobTemplateResponse'
            Prelude.<$> (x Core..?> "jobExecutionsRolloutConfig")
            Prelude.<*> (x Core..?> "timeoutConfig")
            Prelude.<*> (x Core..?> "createdAt")
            Prelude.<*> (x Core..?> "jobTemplateId")
            Prelude.<*> (x Core..?> "documentSource")
            Prelude.<*> (x Core..?> "document")
            Prelude.<*> (x Core..?> "presignedUrlConfig")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "abortConfig")
            Prelude.<*> (x Core..?> "jobTemplateArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeJobTemplate

instance Prelude.NFData DescribeJobTemplate

instance Core.ToHeaders DescribeJobTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeJobTemplate where
  toPath DescribeJobTemplate' {..} =
    Prelude.mconcat
      ["/job-templates/", Core.toBS jobTemplateId]

instance Core.ToQuery DescribeJobTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeJobTemplateResponse' smart constructor.
data DescribeJobTemplateResponse = DescribeJobTemplateResponse'
  { jobExecutionsRolloutConfig :: Prelude.Maybe JobExecutionsRolloutConfig,
    timeoutConfig :: Prelude.Maybe TimeoutConfig,
    -- | The time, in seconds since the epoch, when the job template was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The unique identifier of the job template.
    jobTemplateId :: Prelude.Maybe Prelude.Text,
    -- | An S3 link to the job document.
    documentSource :: Prelude.Maybe Prelude.Text,
    -- | The job document.
    document :: Prelude.Maybe Prelude.Text,
    presignedUrlConfig :: Prelude.Maybe PresignedUrlConfig,
    -- | A description of the job template.
    description :: Prelude.Maybe Prelude.Text,
    abortConfig :: Prelude.Maybe AbortConfig,
    -- | The ARN of the job template.
    jobTemplateArn :: Prelude.Maybe Prelude.Text,
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
-- 'jobExecutionsRolloutConfig', 'describeJobTemplateResponse_jobExecutionsRolloutConfig' - Undocumented member.
--
-- 'timeoutConfig', 'describeJobTemplateResponse_timeoutConfig' - Undocumented member.
--
-- 'createdAt', 'describeJobTemplateResponse_createdAt' - The time, in seconds since the epoch, when the job template was created.
--
-- 'jobTemplateId', 'describeJobTemplateResponse_jobTemplateId' - The unique identifier of the job template.
--
-- 'documentSource', 'describeJobTemplateResponse_documentSource' - An S3 link to the job document.
--
-- 'document', 'describeJobTemplateResponse_document' - The job document.
--
-- 'presignedUrlConfig', 'describeJobTemplateResponse_presignedUrlConfig' - Undocumented member.
--
-- 'description', 'describeJobTemplateResponse_description' - A description of the job template.
--
-- 'abortConfig', 'describeJobTemplateResponse_abortConfig' - Undocumented member.
--
-- 'jobTemplateArn', 'describeJobTemplateResponse_jobTemplateArn' - The ARN of the job template.
--
-- 'httpStatus', 'describeJobTemplateResponse_httpStatus' - The response's http status code.
newDescribeJobTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeJobTemplateResponse
newDescribeJobTemplateResponse pHttpStatus_ =
  DescribeJobTemplateResponse'
    { jobExecutionsRolloutConfig =
        Prelude.Nothing,
      timeoutConfig = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      jobTemplateId = Prelude.Nothing,
      documentSource = Prelude.Nothing,
      document = Prelude.Nothing,
      presignedUrlConfig = Prelude.Nothing,
      description = Prelude.Nothing,
      abortConfig = Prelude.Nothing,
      jobTemplateArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeJobTemplateResponse_jobExecutionsRolloutConfig :: Lens.Lens' DescribeJobTemplateResponse (Prelude.Maybe JobExecutionsRolloutConfig)
describeJobTemplateResponse_jobExecutionsRolloutConfig = Lens.lens (\DescribeJobTemplateResponse' {jobExecutionsRolloutConfig} -> jobExecutionsRolloutConfig) (\s@DescribeJobTemplateResponse' {} a -> s {jobExecutionsRolloutConfig = a} :: DescribeJobTemplateResponse)

-- | Undocumented member.
describeJobTemplateResponse_timeoutConfig :: Lens.Lens' DescribeJobTemplateResponse (Prelude.Maybe TimeoutConfig)
describeJobTemplateResponse_timeoutConfig = Lens.lens (\DescribeJobTemplateResponse' {timeoutConfig} -> timeoutConfig) (\s@DescribeJobTemplateResponse' {} a -> s {timeoutConfig = a} :: DescribeJobTemplateResponse)

-- | The time, in seconds since the epoch, when the job template was created.
describeJobTemplateResponse_createdAt :: Lens.Lens' DescribeJobTemplateResponse (Prelude.Maybe Prelude.UTCTime)
describeJobTemplateResponse_createdAt = Lens.lens (\DescribeJobTemplateResponse' {createdAt} -> createdAt) (\s@DescribeJobTemplateResponse' {} a -> s {createdAt = a} :: DescribeJobTemplateResponse) Prelude.. Lens.mapping Core._Time

-- | The unique identifier of the job template.
describeJobTemplateResponse_jobTemplateId :: Lens.Lens' DescribeJobTemplateResponse (Prelude.Maybe Prelude.Text)
describeJobTemplateResponse_jobTemplateId = Lens.lens (\DescribeJobTemplateResponse' {jobTemplateId} -> jobTemplateId) (\s@DescribeJobTemplateResponse' {} a -> s {jobTemplateId = a} :: DescribeJobTemplateResponse)

-- | An S3 link to the job document.
describeJobTemplateResponse_documentSource :: Lens.Lens' DescribeJobTemplateResponse (Prelude.Maybe Prelude.Text)
describeJobTemplateResponse_documentSource = Lens.lens (\DescribeJobTemplateResponse' {documentSource} -> documentSource) (\s@DescribeJobTemplateResponse' {} a -> s {documentSource = a} :: DescribeJobTemplateResponse)

-- | The job document.
describeJobTemplateResponse_document :: Lens.Lens' DescribeJobTemplateResponse (Prelude.Maybe Prelude.Text)
describeJobTemplateResponse_document = Lens.lens (\DescribeJobTemplateResponse' {document} -> document) (\s@DescribeJobTemplateResponse' {} a -> s {document = a} :: DescribeJobTemplateResponse)

-- | Undocumented member.
describeJobTemplateResponse_presignedUrlConfig :: Lens.Lens' DescribeJobTemplateResponse (Prelude.Maybe PresignedUrlConfig)
describeJobTemplateResponse_presignedUrlConfig = Lens.lens (\DescribeJobTemplateResponse' {presignedUrlConfig} -> presignedUrlConfig) (\s@DescribeJobTemplateResponse' {} a -> s {presignedUrlConfig = a} :: DescribeJobTemplateResponse)

-- | A description of the job template.
describeJobTemplateResponse_description :: Lens.Lens' DescribeJobTemplateResponse (Prelude.Maybe Prelude.Text)
describeJobTemplateResponse_description = Lens.lens (\DescribeJobTemplateResponse' {description} -> description) (\s@DescribeJobTemplateResponse' {} a -> s {description = a} :: DescribeJobTemplateResponse)

-- | Undocumented member.
describeJobTemplateResponse_abortConfig :: Lens.Lens' DescribeJobTemplateResponse (Prelude.Maybe AbortConfig)
describeJobTemplateResponse_abortConfig = Lens.lens (\DescribeJobTemplateResponse' {abortConfig} -> abortConfig) (\s@DescribeJobTemplateResponse' {} a -> s {abortConfig = a} :: DescribeJobTemplateResponse)

-- | The ARN of the job template.
describeJobTemplateResponse_jobTemplateArn :: Lens.Lens' DescribeJobTemplateResponse (Prelude.Maybe Prelude.Text)
describeJobTemplateResponse_jobTemplateArn = Lens.lens (\DescribeJobTemplateResponse' {jobTemplateArn} -> jobTemplateArn) (\s@DescribeJobTemplateResponse' {} a -> s {jobTemplateArn = a} :: DescribeJobTemplateResponse)

-- | The response's http status code.
describeJobTemplateResponse_httpStatus :: Lens.Lens' DescribeJobTemplateResponse Prelude.Int
describeJobTemplateResponse_httpStatus = Lens.lens (\DescribeJobTemplateResponse' {httpStatus} -> httpStatus) (\s@DescribeJobTemplateResponse' {} a -> s {httpStatus = a} :: DescribeJobTemplateResponse)

instance Prelude.NFData DescribeJobTemplateResponse
