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
-- Module      : Amazonka.FraudDetector.CreateBatchImportJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a batch import job.
module Amazonka.FraudDetector.CreateBatchImportJob
  ( -- * Creating a Request
    CreateBatchImportJob (..),
    newCreateBatchImportJob,

    -- * Request Lenses
    createBatchImportJob_tags,
    createBatchImportJob_jobId,
    createBatchImportJob_inputPath,
    createBatchImportJob_outputPath,
    createBatchImportJob_eventTypeName,
    createBatchImportJob_iamRoleArn,

    -- * Destructuring the Response
    CreateBatchImportJobResponse (..),
    newCreateBatchImportJobResponse,

    -- * Response Lenses
    createBatchImportJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateBatchImportJob' smart constructor.
data CreateBatchImportJob = CreateBatchImportJob'
  { -- | A collection of key-value pairs associated with this request.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the batch import job. The ID cannot be of a past job, unless
    -- the job exists in @CREATE_FAILED@ state.
    jobId :: Prelude.Text,
    -- | The URI that points to the Amazon S3 location of your data file.
    inputPath :: Prelude.Text,
    -- | The URI that points to the Amazon S3 location for storing your results.
    outputPath :: Prelude.Text,
    -- | The name of the event type.
    eventTypeName :: Prelude.Text,
    -- | The ARN of the IAM role created for Amazon S3 bucket that holds your
    -- data file.
    --
    -- The IAM role must have read permissions to your input S3 bucket and
    -- write permissions to your output S3 bucket. For more information about
    -- bucket permissions, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/example-policies-s3.html User policy examples>
    -- in the /Amazon S3 User Guide/.
    iamRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBatchImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createBatchImportJob_tags' - A collection of key-value pairs associated with this request.
--
-- 'jobId', 'createBatchImportJob_jobId' - The ID of the batch import job. The ID cannot be of a past job, unless
-- the job exists in @CREATE_FAILED@ state.
--
-- 'inputPath', 'createBatchImportJob_inputPath' - The URI that points to the Amazon S3 location of your data file.
--
-- 'outputPath', 'createBatchImportJob_outputPath' - The URI that points to the Amazon S3 location for storing your results.
--
-- 'eventTypeName', 'createBatchImportJob_eventTypeName' - The name of the event type.
--
-- 'iamRoleArn', 'createBatchImportJob_iamRoleArn' - The ARN of the IAM role created for Amazon S3 bucket that holds your
-- data file.
--
-- The IAM role must have read permissions to your input S3 bucket and
-- write permissions to your output S3 bucket. For more information about
-- bucket permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/example-policies-s3.html User policy examples>
-- in the /Amazon S3 User Guide/.
newCreateBatchImportJob ::
  -- | 'jobId'
  Prelude.Text ->
  -- | 'inputPath'
  Prelude.Text ->
  -- | 'outputPath'
  Prelude.Text ->
  -- | 'eventTypeName'
  Prelude.Text ->
  -- | 'iamRoleArn'
  Prelude.Text ->
  CreateBatchImportJob
newCreateBatchImportJob
  pJobId_
  pInputPath_
  pOutputPath_
  pEventTypeName_
  pIamRoleArn_ =
    CreateBatchImportJob'
      { tags = Prelude.Nothing,
        jobId = pJobId_,
        inputPath = pInputPath_,
        outputPath = pOutputPath_,
        eventTypeName = pEventTypeName_,
        iamRoleArn = pIamRoleArn_
      }

-- | A collection of key-value pairs associated with this request.
createBatchImportJob_tags :: Lens.Lens' CreateBatchImportJob (Prelude.Maybe [Tag])
createBatchImportJob_tags = Lens.lens (\CreateBatchImportJob' {tags} -> tags) (\s@CreateBatchImportJob' {} a -> s {tags = a} :: CreateBatchImportJob) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the batch import job. The ID cannot be of a past job, unless
-- the job exists in @CREATE_FAILED@ state.
createBatchImportJob_jobId :: Lens.Lens' CreateBatchImportJob Prelude.Text
createBatchImportJob_jobId = Lens.lens (\CreateBatchImportJob' {jobId} -> jobId) (\s@CreateBatchImportJob' {} a -> s {jobId = a} :: CreateBatchImportJob)

-- | The URI that points to the Amazon S3 location of your data file.
createBatchImportJob_inputPath :: Lens.Lens' CreateBatchImportJob Prelude.Text
createBatchImportJob_inputPath = Lens.lens (\CreateBatchImportJob' {inputPath} -> inputPath) (\s@CreateBatchImportJob' {} a -> s {inputPath = a} :: CreateBatchImportJob)

-- | The URI that points to the Amazon S3 location for storing your results.
createBatchImportJob_outputPath :: Lens.Lens' CreateBatchImportJob Prelude.Text
createBatchImportJob_outputPath = Lens.lens (\CreateBatchImportJob' {outputPath} -> outputPath) (\s@CreateBatchImportJob' {} a -> s {outputPath = a} :: CreateBatchImportJob)

-- | The name of the event type.
createBatchImportJob_eventTypeName :: Lens.Lens' CreateBatchImportJob Prelude.Text
createBatchImportJob_eventTypeName = Lens.lens (\CreateBatchImportJob' {eventTypeName} -> eventTypeName) (\s@CreateBatchImportJob' {} a -> s {eventTypeName = a} :: CreateBatchImportJob)

-- | The ARN of the IAM role created for Amazon S3 bucket that holds your
-- data file.
--
-- The IAM role must have read permissions to your input S3 bucket and
-- write permissions to your output S3 bucket. For more information about
-- bucket permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/example-policies-s3.html User policy examples>
-- in the /Amazon S3 User Guide/.
createBatchImportJob_iamRoleArn :: Lens.Lens' CreateBatchImportJob Prelude.Text
createBatchImportJob_iamRoleArn = Lens.lens (\CreateBatchImportJob' {iamRoleArn} -> iamRoleArn) (\s@CreateBatchImportJob' {} a -> s {iamRoleArn = a} :: CreateBatchImportJob)

instance Core.AWSRequest CreateBatchImportJob where
  type
    AWSResponse CreateBatchImportJob =
      CreateBatchImportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateBatchImportJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBatchImportJob where
  hashWithSalt _salt CreateBatchImportJob' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` inputPath
      `Prelude.hashWithSalt` outputPath
      `Prelude.hashWithSalt` eventTypeName
      `Prelude.hashWithSalt` iamRoleArn

instance Prelude.NFData CreateBatchImportJob where
  rnf CreateBatchImportJob' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf inputPath
      `Prelude.seq` Prelude.rnf outputPath
      `Prelude.seq` Prelude.rnf eventTypeName
      `Prelude.seq` Prelude.rnf iamRoleArn

instance Data.ToHeaders CreateBatchImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.CreateBatchImportJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateBatchImportJob where
  toJSON CreateBatchImportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("jobId" Data..= jobId),
            Prelude.Just ("inputPath" Data..= inputPath),
            Prelude.Just ("outputPath" Data..= outputPath),
            Prelude.Just ("eventTypeName" Data..= eventTypeName),
            Prelude.Just ("iamRoleArn" Data..= iamRoleArn)
          ]
      )

instance Data.ToPath CreateBatchImportJob where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateBatchImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBatchImportJobResponse' smart constructor.
data CreateBatchImportJobResponse = CreateBatchImportJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBatchImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createBatchImportJobResponse_httpStatus' - The response's http status code.
newCreateBatchImportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBatchImportJobResponse
newCreateBatchImportJobResponse pHttpStatus_ =
  CreateBatchImportJobResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createBatchImportJobResponse_httpStatus :: Lens.Lens' CreateBatchImportJobResponse Prelude.Int
createBatchImportJobResponse_httpStatus = Lens.lens (\CreateBatchImportJobResponse' {httpStatus} -> httpStatus) (\s@CreateBatchImportJobResponse' {} a -> s {httpStatus = a} :: CreateBatchImportJobResponse)

instance Prelude.NFData CreateBatchImportJobResponse where
  rnf CreateBatchImportJobResponse' {..} =
    Prelude.rnf httpStatus
