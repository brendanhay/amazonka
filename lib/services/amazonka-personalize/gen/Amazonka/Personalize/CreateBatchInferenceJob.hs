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
-- Module      : Amazonka.Personalize.CreateBatchInferenceJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a batch inference job. The operation can handle up to 50 million
-- records and the input file must be in JSON format. For more information,
-- see
-- <https://docs.aws.amazon.com/personalize/latest/dg/creating-batch-inference-job.html Creating a batch inference job>.
module Amazonka.Personalize.CreateBatchInferenceJob
  ( -- * Creating a Request
    CreateBatchInferenceJob (..),
    newCreateBatchInferenceJob,

    -- * Request Lenses
    createBatchInferenceJob_batchInferenceJobConfig,
    createBatchInferenceJob_filterArn,
    createBatchInferenceJob_numResults,
    createBatchInferenceJob_tags,
    createBatchInferenceJob_jobName,
    createBatchInferenceJob_solutionVersionArn,
    createBatchInferenceJob_jobInput,
    createBatchInferenceJob_jobOutput,
    createBatchInferenceJob_roleArn,

    -- * Destructuring the Response
    CreateBatchInferenceJobResponse (..),
    newCreateBatchInferenceJobResponse,

    -- * Response Lenses
    createBatchInferenceJobResponse_batchInferenceJobArn,
    createBatchInferenceJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateBatchInferenceJob' smart constructor.
data CreateBatchInferenceJob = CreateBatchInferenceJob'
  { -- | The configuration details of a batch inference job.
    batchInferenceJobConfig :: Prelude.Maybe BatchInferenceJobConfig,
    -- | The ARN of the filter to apply to the batch inference job. For more
    -- information on using filters, see
    -- <https://docs.aws.amazon.com/personalize/latest/dg/filter-batch.html Filtering batch recommendations>.
    filterArn :: Prelude.Maybe Prelude.Text,
    -- | The number of recommendations to retrieve.
    numResults :: Prelude.Maybe Prelude.Int,
    -- | A list of
    -- <https://docs.aws.amazon.com/personalize/latest/dg/tagging-resources.html tags>
    -- to apply to the batch inference job.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the batch inference job to create.
    jobName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the solution version that will be used
    -- to generate the batch inference recommendations.
    solutionVersionArn :: Prelude.Text,
    -- | The Amazon S3 path that leads to the input file to base your
    -- recommendations on. The input material must be in JSON format.
    jobInput :: BatchInferenceJobInput,
    -- | The path to the Amazon S3 bucket where the job\'s output will be stored.
    jobOutput :: BatchInferenceJobOutput,
    -- | The ARN of the Amazon Identity and Access Management role that has
    -- permissions to read and write to your input and output Amazon S3 buckets
    -- respectively.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBatchInferenceJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchInferenceJobConfig', 'createBatchInferenceJob_batchInferenceJobConfig' - The configuration details of a batch inference job.
--
-- 'filterArn', 'createBatchInferenceJob_filterArn' - The ARN of the filter to apply to the batch inference job. For more
-- information on using filters, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/filter-batch.html Filtering batch recommendations>.
--
-- 'numResults', 'createBatchInferenceJob_numResults' - The number of recommendations to retrieve.
--
-- 'tags', 'createBatchInferenceJob_tags' - A list of
-- <https://docs.aws.amazon.com/personalize/latest/dg/tagging-resources.html tags>
-- to apply to the batch inference job.
--
-- 'jobName', 'createBatchInferenceJob_jobName' - The name of the batch inference job to create.
--
-- 'solutionVersionArn', 'createBatchInferenceJob_solutionVersionArn' - The Amazon Resource Name (ARN) of the solution version that will be used
-- to generate the batch inference recommendations.
--
-- 'jobInput', 'createBatchInferenceJob_jobInput' - The Amazon S3 path that leads to the input file to base your
-- recommendations on. The input material must be in JSON format.
--
-- 'jobOutput', 'createBatchInferenceJob_jobOutput' - The path to the Amazon S3 bucket where the job\'s output will be stored.
--
-- 'roleArn', 'createBatchInferenceJob_roleArn' - The ARN of the Amazon Identity and Access Management role that has
-- permissions to read and write to your input and output Amazon S3 buckets
-- respectively.
newCreateBatchInferenceJob ::
  -- | 'jobName'
  Prelude.Text ->
  -- | 'solutionVersionArn'
  Prelude.Text ->
  -- | 'jobInput'
  BatchInferenceJobInput ->
  -- | 'jobOutput'
  BatchInferenceJobOutput ->
  -- | 'roleArn'
  Prelude.Text ->
  CreateBatchInferenceJob
newCreateBatchInferenceJob
  pJobName_
  pSolutionVersionArn_
  pJobInput_
  pJobOutput_
  pRoleArn_ =
    CreateBatchInferenceJob'
      { batchInferenceJobConfig =
          Prelude.Nothing,
        filterArn = Prelude.Nothing,
        numResults = Prelude.Nothing,
        tags = Prelude.Nothing,
        jobName = pJobName_,
        solutionVersionArn = pSolutionVersionArn_,
        jobInput = pJobInput_,
        jobOutput = pJobOutput_,
        roleArn = pRoleArn_
      }

-- | The configuration details of a batch inference job.
createBatchInferenceJob_batchInferenceJobConfig :: Lens.Lens' CreateBatchInferenceJob (Prelude.Maybe BatchInferenceJobConfig)
createBatchInferenceJob_batchInferenceJobConfig = Lens.lens (\CreateBatchInferenceJob' {batchInferenceJobConfig} -> batchInferenceJobConfig) (\s@CreateBatchInferenceJob' {} a -> s {batchInferenceJobConfig = a} :: CreateBatchInferenceJob)

-- | The ARN of the filter to apply to the batch inference job. For more
-- information on using filters, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/filter-batch.html Filtering batch recommendations>.
createBatchInferenceJob_filterArn :: Lens.Lens' CreateBatchInferenceJob (Prelude.Maybe Prelude.Text)
createBatchInferenceJob_filterArn = Lens.lens (\CreateBatchInferenceJob' {filterArn} -> filterArn) (\s@CreateBatchInferenceJob' {} a -> s {filterArn = a} :: CreateBatchInferenceJob)

-- | The number of recommendations to retrieve.
createBatchInferenceJob_numResults :: Lens.Lens' CreateBatchInferenceJob (Prelude.Maybe Prelude.Int)
createBatchInferenceJob_numResults = Lens.lens (\CreateBatchInferenceJob' {numResults} -> numResults) (\s@CreateBatchInferenceJob' {} a -> s {numResults = a} :: CreateBatchInferenceJob)

-- | A list of
-- <https://docs.aws.amazon.com/personalize/latest/dg/tagging-resources.html tags>
-- to apply to the batch inference job.
createBatchInferenceJob_tags :: Lens.Lens' CreateBatchInferenceJob (Prelude.Maybe [Tag])
createBatchInferenceJob_tags = Lens.lens (\CreateBatchInferenceJob' {tags} -> tags) (\s@CreateBatchInferenceJob' {} a -> s {tags = a} :: CreateBatchInferenceJob) Prelude.. Lens.mapping Lens.coerced

-- | The name of the batch inference job to create.
createBatchInferenceJob_jobName :: Lens.Lens' CreateBatchInferenceJob Prelude.Text
createBatchInferenceJob_jobName = Lens.lens (\CreateBatchInferenceJob' {jobName} -> jobName) (\s@CreateBatchInferenceJob' {} a -> s {jobName = a} :: CreateBatchInferenceJob)

-- | The Amazon Resource Name (ARN) of the solution version that will be used
-- to generate the batch inference recommendations.
createBatchInferenceJob_solutionVersionArn :: Lens.Lens' CreateBatchInferenceJob Prelude.Text
createBatchInferenceJob_solutionVersionArn = Lens.lens (\CreateBatchInferenceJob' {solutionVersionArn} -> solutionVersionArn) (\s@CreateBatchInferenceJob' {} a -> s {solutionVersionArn = a} :: CreateBatchInferenceJob)

-- | The Amazon S3 path that leads to the input file to base your
-- recommendations on. The input material must be in JSON format.
createBatchInferenceJob_jobInput :: Lens.Lens' CreateBatchInferenceJob BatchInferenceJobInput
createBatchInferenceJob_jobInput = Lens.lens (\CreateBatchInferenceJob' {jobInput} -> jobInput) (\s@CreateBatchInferenceJob' {} a -> s {jobInput = a} :: CreateBatchInferenceJob)

-- | The path to the Amazon S3 bucket where the job\'s output will be stored.
createBatchInferenceJob_jobOutput :: Lens.Lens' CreateBatchInferenceJob BatchInferenceJobOutput
createBatchInferenceJob_jobOutput = Lens.lens (\CreateBatchInferenceJob' {jobOutput} -> jobOutput) (\s@CreateBatchInferenceJob' {} a -> s {jobOutput = a} :: CreateBatchInferenceJob)

-- | The ARN of the Amazon Identity and Access Management role that has
-- permissions to read and write to your input and output Amazon S3 buckets
-- respectively.
createBatchInferenceJob_roleArn :: Lens.Lens' CreateBatchInferenceJob Prelude.Text
createBatchInferenceJob_roleArn = Lens.lens (\CreateBatchInferenceJob' {roleArn} -> roleArn) (\s@CreateBatchInferenceJob' {} a -> s {roleArn = a} :: CreateBatchInferenceJob)

instance Core.AWSRequest CreateBatchInferenceJob where
  type
    AWSResponse CreateBatchInferenceJob =
      CreateBatchInferenceJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBatchInferenceJobResponse'
            Prelude.<$> (x Data..?> "batchInferenceJobArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBatchInferenceJob where
  hashWithSalt _salt CreateBatchInferenceJob' {..} =
    _salt
      `Prelude.hashWithSalt` batchInferenceJobConfig
      `Prelude.hashWithSalt` filterArn
      `Prelude.hashWithSalt` numResults
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` solutionVersionArn
      `Prelude.hashWithSalt` jobInput
      `Prelude.hashWithSalt` jobOutput
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData CreateBatchInferenceJob where
  rnf CreateBatchInferenceJob' {..} =
    Prelude.rnf batchInferenceJobConfig
      `Prelude.seq` Prelude.rnf filterArn
      `Prelude.seq` Prelude.rnf numResults
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf solutionVersionArn
      `Prelude.seq` Prelude.rnf jobInput
      `Prelude.seq` Prelude.rnf jobOutput
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToHeaders CreateBatchInferenceJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.CreateBatchInferenceJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateBatchInferenceJob where
  toJSON CreateBatchInferenceJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("batchInferenceJobConfig" Data..=)
              Prelude.<$> batchInferenceJobConfig,
            ("filterArn" Data..=) Prelude.<$> filterArn,
            ("numResults" Data..=) Prelude.<$> numResults,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("jobName" Data..= jobName),
            Prelude.Just
              ("solutionVersionArn" Data..= solutionVersionArn),
            Prelude.Just ("jobInput" Data..= jobInput),
            Prelude.Just ("jobOutput" Data..= jobOutput),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath CreateBatchInferenceJob where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateBatchInferenceJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBatchInferenceJobResponse' smart constructor.
data CreateBatchInferenceJobResponse = CreateBatchInferenceJobResponse'
  { -- | The ARN of the batch inference job.
    batchInferenceJobArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBatchInferenceJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchInferenceJobArn', 'createBatchInferenceJobResponse_batchInferenceJobArn' - The ARN of the batch inference job.
--
-- 'httpStatus', 'createBatchInferenceJobResponse_httpStatus' - The response's http status code.
newCreateBatchInferenceJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBatchInferenceJobResponse
newCreateBatchInferenceJobResponse pHttpStatus_ =
  CreateBatchInferenceJobResponse'
    { batchInferenceJobArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the batch inference job.
createBatchInferenceJobResponse_batchInferenceJobArn :: Lens.Lens' CreateBatchInferenceJobResponse (Prelude.Maybe Prelude.Text)
createBatchInferenceJobResponse_batchInferenceJobArn = Lens.lens (\CreateBatchInferenceJobResponse' {batchInferenceJobArn} -> batchInferenceJobArn) (\s@CreateBatchInferenceJobResponse' {} a -> s {batchInferenceJobArn = a} :: CreateBatchInferenceJobResponse)

-- | The response's http status code.
createBatchInferenceJobResponse_httpStatus :: Lens.Lens' CreateBatchInferenceJobResponse Prelude.Int
createBatchInferenceJobResponse_httpStatus = Lens.lens (\CreateBatchInferenceJobResponse' {httpStatus} -> httpStatus) (\s@CreateBatchInferenceJobResponse' {} a -> s {httpStatus = a} :: CreateBatchInferenceJobResponse)

instance
  Prelude.NFData
    CreateBatchInferenceJobResponse
  where
  rnf CreateBatchInferenceJobResponse' {..} =
    Prelude.rnf batchInferenceJobArn
      `Prelude.seq` Prelude.rnf httpStatus
