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
-- Module      : Amazonka.IoT.CreateJobTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a job template.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CreateJobTemplate>
-- action.
module Amazonka.IoT.CreateJobTemplate
  ( -- * Creating a Request
    CreateJobTemplate (..),
    newCreateJobTemplate,

    -- * Request Lenses
    createJobTemplate_jobExecutionsRolloutConfig,
    createJobTemplate_jobArn,
    createJobTemplate_documentSource,
    createJobTemplate_abortConfig,
    createJobTemplate_presignedUrlConfig,
    createJobTemplate_document,
    createJobTemplate_timeoutConfig,
    createJobTemplate_tags,
    createJobTemplate_jobTemplateId,
    createJobTemplate_description,

    -- * Destructuring the Response
    CreateJobTemplateResponse (..),
    newCreateJobTemplateResponse,

    -- * Response Lenses
    createJobTemplateResponse_jobTemplateId,
    createJobTemplateResponse_jobTemplateArn,
    createJobTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateJobTemplate' smart constructor.
data CreateJobTemplate = CreateJobTemplate'
  { jobExecutionsRolloutConfig :: Prelude.Maybe JobExecutionsRolloutConfig,
    -- | The ARN of the job to use as the basis for the job template.
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | An S3 link to the job document to use in the template. Required if you
    -- don\'t specify a value for @document@.
    --
    -- If the job document resides in an S3 bucket, you must use a placeholder
    -- link when specifying the document.
    --
    -- The placeholder link is of the following form:
    --
    -- @${aws:iot:s3-presigned-url:https:\/\/s3.amazonaws.com\/bucket\/key}@
    --
    -- where /bucket/ is your bucket name and /key/ is the object in the bucket
    -- to which you are linking.
    documentSource :: Prelude.Maybe Prelude.Text,
    abortConfig :: Prelude.Maybe AbortConfig,
    presignedUrlConfig :: Prelude.Maybe PresignedUrlConfig,
    -- | The job document. Required if you don\'t specify a value for
    -- @documentSource@.
    document :: Prelude.Maybe Prelude.Text,
    timeoutConfig :: Prelude.Maybe TimeoutConfig,
    -- | Metadata that can be used to manage the job template.
    tags :: Prelude.Maybe [Tag],
    -- | A unique identifier for the job template. We recommend using a UUID.
    -- Alpha-numeric characters, \"-\", and \"_\" are valid for use here.
    jobTemplateId :: Prelude.Text,
    -- | A description of the job document.
    description :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateJobTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobExecutionsRolloutConfig', 'createJobTemplate_jobExecutionsRolloutConfig' - Undocumented member.
--
-- 'jobArn', 'createJobTemplate_jobArn' - The ARN of the job to use as the basis for the job template.
--
-- 'documentSource', 'createJobTemplate_documentSource' - An S3 link to the job document to use in the template. Required if you
-- don\'t specify a value for @document@.
--
-- If the job document resides in an S3 bucket, you must use a placeholder
-- link when specifying the document.
--
-- The placeholder link is of the following form:
--
-- @${aws:iot:s3-presigned-url:https:\/\/s3.amazonaws.com\/bucket\/key}@
--
-- where /bucket/ is your bucket name and /key/ is the object in the bucket
-- to which you are linking.
--
-- 'abortConfig', 'createJobTemplate_abortConfig' - Undocumented member.
--
-- 'presignedUrlConfig', 'createJobTemplate_presignedUrlConfig' - Undocumented member.
--
-- 'document', 'createJobTemplate_document' - The job document. Required if you don\'t specify a value for
-- @documentSource@.
--
-- 'timeoutConfig', 'createJobTemplate_timeoutConfig' - Undocumented member.
--
-- 'tags', 'createJobTemplate_tags' - Metadata that can be used to manage the job template.
--
-- 'jobTemplateId', 'createJobTemplate_jobTemplateId' - A unique identifier for the job template. We recommend using a UUID.
-- Alpha-numeric characters, \"-\", and \"_\" are valid for use here.
--
-- 'description', 'createJobTemplate_description' - A description of the job document.
newCreateJobTemplate ::
  -- | 'jobTemplateId'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  CreateJobTemplate
newCreateJobTemplate pJobTemplateId_ pDescription_ =
  CreateJobTemplate'
    { jobExecutionsRolloutConfig =
        Prelude.Nothing,
      jobArn = Prelude.Nothing,
      documentSource = Prelude.Nothing,
      abortConfig = Prelude.Nothing,
      presignedUrlConfig = Prelude.Nothing,
      document = Prelude.Nothing,
      timeoutConfig = Prelude.Nothing,
      tags = Prelude.Nothing,
      jobTemplateId = pJobTemplateId_,
      description = pDescription_
    }

-- | Undocumented member.
createJobTemplate_jobExecutionsRolloutConfig :: Lens.Lens' CreateJobTemplate (Prelude.Maybe JobExecutionsRolloutConfig)
createJobTemplate_jobExecutionsRolloutConfig = Lens.lens (\CreateJobTemplate' {jobExecutionsRolloutConfig} -> jobExecutionsRolloutConfig) (\s@CreateJobTemplate' {} a -> s {jobExecutionsRolloutConfig = a} :: CreateJobTemplate)

-- | The ARN of the job to use as the basis for the job template.
createJobTemplate_jobArn :: Lens.Lens' CreateJobTemplate (Prelude.Maybe Prelude.Text)
createJobTemplate_jobArn = Lens.lens (\CreateJobTemplate' {jobArn} -> jobArn) (\s@CreateJobTemplate' {} a -> s {jobArn = a} :: CreateJobTemplate)

-- | An S3 link to the job document to use in the template. Required if you
-- don\'t specify a value for @document@.
--
-- If the job document resides in an S3 bucket, you must use a placeholder
-- link when specifying the document.
--
-- The placeholder link is of the following form:
--
-- @${aws:iot:s3-presigned-url:https:\/\/s3.amazonaws.com\/bucket\/key}@
--
-- where /bucket/ is your bucket name and /key/ is the object in the bucket
-- to which you are linking.
createJobTemplate_documentSource :: Lens.Lens' CreateJobTemplate (Prelude.Maybe Prelude.Text)
createJobTemplate_documentSource = Lens.lens (\CreateJobTemplate' {documentSource} -> documentSource) (\s@CreateJobTemplate' {} a -> s {documentSource = a} :: CreateJobTemplate)

-- | Undocumented member.
createJobTemplate_abortConfig :: Lens.Lens' CreateJobTemplate (Prelude.Maybe AbortConfig)
createJobTemplate_abortConfig = Lens.lens (\CreateJobTemplate' {abortConfig} -> abortConfig) (\s@CreateJobTemplate' {} a -> s {abortConfig = a} :: CreateJobTemplate)

-- | Undocumented member.
createJobTemplate_presignedUrlConfig :: Lens.Lens' CreateJobTemplate (Prelude.Maybe PresignedUrlConfig)
createJobTemplate_presignedUrlConfig = Lens.lens (\CreateJobTemplate' {presignedUrlConfig} -> presignedUrlConfig) (\s@CreateJobTemplate' {} a -> s {presignedUrlConfig = a} :: CreateJobTemplate)

-- | The job document. Required if you don\'t specify a value for
-- @documentSource@.
createJobTemplate_document :: Lens.Lens' CreateJobTemplate (Prelude.Maybe Prelude.Text)
createJobTemplate_document = Lens.lens (\CreateJobTemplate' {document} -> document) (\s@CreateJobTemplate' {} a -> s {document = a} :: CreateJobTemplate)

-- | Undocumented member.
createJobTemplate_timeoutConfig :: Lens.Lens' CreateJobTemplate (Prelude.Maybe TimeoutConfig)
createJobTemplate_timeoutConfig = Lens.lens (\CreateJobTemplate' {timeoutConfig} -> timeoutConfig) (\s@CreateJobTemplate' {} a -> s {timeoutConfig = a} :: CreateJobTemplate)

-- | Metadata that can be used to manage the job template.
createJobTemplate_tags :: Lens.Lens' CreateJobTemplate (Prelude.Maybe [Tag])
createJobTemplate_tags = Lens.lens (\CreateJobTemplate' {tags} -> tags) (\s@CreateJobTemplate' {} a -> s {tags = a} :: CreateJobTemplate) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for the job template. We recommend using a UUID.
-- Alpha-numeric characters, \"-\", and \"_\" are valid for use here.
createJobTemplate_jobTemplateId :: Lens.Lens' CreateJobTemplate Prelude.Text
createJobTemplate_jobTemplateId = Lens.lens (\CreateJobTemplate' {jobTemplateId} -> jobTemplateId) (\s@CreateJobTemplate' {} a -> s {jobTemplateId = a} :: CreateJobTemplate)

-- | A description of the job document.
createJobTemplate_description :: Lens.Lens' CreateJobTemplate Prelude.Text
createJobTemplate_description = Lens.lens (\CreateJobTemplate' {description} -> description) (\s@CreateJobTemplate' {} a -> s {description = a} :: CreateJobTemplate)

instance Core.AWSRequest CreateJobTemplate where
  type
    AWSResponse CreateJobTemplate =
      CreateJobTemplateResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateJobTemplateResponse'
            Prelude.<$> (x Core..?> "jobTemplateId")
            Prelude.<*> (x Core..?> "jobTemplateArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateJobTemplate

instance Prelude.NFData CreateJobTemplate

instance Core.ToHeaders CreateJobTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateJobTemplate where
  toJSON CreateJobTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("jobExecutionsRolloutConfig" Core..=)
              Prelude.<$> jobExecutionsRolloutConfig,
            ("jobArn" Core..=) Prelude.<$> jobArn,
            ("documentSource" Core..=)
              Prelude.<$> documentSource,
            ("abortConfig" Core..=) Prelude.<$> abortConfig,
            ("presignedUrlConfig" Core..=)
              Prelude.<$> presignedUrlConfig,
            ("document" Core..=) Prelude.<$> document,
            ("timeoutConfig" Core..=) Prelude.<$> timeoutConfig,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("description" Core..= description)
          ]
      )

instance Core.ToPath CreateJobTemplate where
  toPath CreateJobTemplate' {..} =
    Prelude.mconcat
      ["/job-templates/", Core.toBS jobTemplateId]

instance Core.ToQuery CreateJobTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateJobTemplateResponse' smart constructor.
data CreateJobTemplateResponse = CreateJobTemplateResponse'
  { -- | The unique identifier of the job template.
    jobTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the job template.
    jobTemplateArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateJobTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobTemplateId', 'createJobTemplateResponse_jobTemplateId' - The unique identifier of the job template.
--
-- 'jobTemplateArn', 'createJobTemplateResponse_jobTemplateArn' - The ARN of the job template.
--
-- 'httpStatus', 'createJobTemplateResponse_httpStatus' - The response's http status code.
newCreateJobTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateJobTemplateResponse
newCreateJobTemplateResponse pHttpStatus_ =
  CreateJobTemplateResponse'
    { jobTemplateId =
        Prelude.Nothing,
      jobTemplateArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier of the job template.
createJobTemplateResponse_jobTemplateId :: Lens.Lens' CreateJobTemplateResponse (Prelude.Maybe Prelude.Text)
createJobTemplateResponse_jobTemplateId = Lens.lens (\CreateJobTemplateResponse' {jobTemplateId} -> jobTemplateId) (\s@CreateJobTemplateResponse' {} a -> s {jobTemplateId = a} :: CreateJobTemplateResponse)

-- | The ARN of the job template.
createJobTemplateResponse_jobTemplateArn :: Lens.Lens' CreateJobTemplateResponse (Prelude.Maybe Prelude.Text)
createJobTemplateResponse_jobTemplateArn = Lens.lens (\CreateJobTemplateResponse' {jobTemplateArn} -> jobTemplateArn) (\s@CreateJobTemplateResponse' {} a -> s {jobTemplateArn = a} :: CreateJobTemplateResponse)

-- | The response's http status code.
createJobTemplateResponse_httpStatus :: Lens.Lens' CreateJobTemplateResponse Prelude.Int
createJobTemplateResponse_httpStatus = Lens.lens (\CreateJobTemplateResponse' {httpStatus} -> httpStatus) (\s@CreateJobTemplateResponse' {} a -> s {httpStatus = a} :: CreateJobTemplateResponse)

instance Prelude.NFData CreateJobTemplateResponse
