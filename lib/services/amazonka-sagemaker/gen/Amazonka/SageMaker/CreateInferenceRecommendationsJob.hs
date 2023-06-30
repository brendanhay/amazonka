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
-- Module      : Amazonka.SageMaker.CreateInferenceRecommendationsJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a recommendation job. You can create either an instance
-- recommendation or load test job.
module Amazonka.SageMaker.CreateInferenceRecommendationsJob
  ( -- * Creating a Request
    CreateInferenceRecommendationsJob (..),
    newCreateInferenceRecommendationsJob,

    -- * Request Lenses
    createInferenceRecommendationsJob_jobDescription,
    createInferenceRecommendationsJob_outputConfig,
    createInferenceRecommendationsJob_stoppingConditions,
    createInferenceRecommendationsJob_tags,
    createInferenceRecommendationsJob_jobName,
    createInferenceRecommendationsJob_jobType,
    createInferenceRecommendationsJob_roleArn,
    createInferenceRecommendationsJob_inputConfig,

    -- * Destructuring the Response
    CreateInferenceRecommendationsJobResponse (..),
    newCreateInferenceRecommendationsJobResponse,

    -- * Response Lenses
    createInferenceRecommendationsJobResponse_httpStatus,
    createInferenceRecommendationsJobResponse_jobArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateInferenceRecommendationsJob' smart constructor.
data CreateInferenceRecommendationsJob = CreateInferenceRecommendationsJob'
  { -- | Description of the recommendation job.
    jobDescription :: Prelude.Maybe Prelude.Text,
    -- | Provides information about the output artifacts and the KMS key to use
    -- for Amazon S3 server-side encryption.
    outputConfig :: Prelude.Maybe RecommendationJobOutputConfig,
    -- | A set of conditions for stopping a recommendation job. If any of the
    -- conditions are met, the job is automatically stopped.
    stoppingConditions :: Prelude.Maybe RecommendationJobStoppingConditions,
    -- | The metadata that you apply to Amazon Web Services resources to help you
    -- categorize and organize them. Each tag consists of a key and a value,
    -- both of which you define. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
    -- in the Amazon Web Services General Reference.
    tags :: Prelude.Maybe [Tag],
    -- | A name for the recommendation job. The name must be unique within the
    -- Amazon Web Services Region and within your Amazon Web Services account.
    jobName :: Prelude.Text,
    -- | Defines the type of recommendation job. Specify @Default@ to initiate an
    -- instance recommendation and @Advanced@ to initiate a load test. If left
    -- unspecified, Amazon SageMaker Inference Recommender will run an instance
    -- recommendation (@DEFAULT@) job.
    jobType :: RecommendationJobType,
    -- | The Amazon Resource Name (ARN) of an IAM role that enables Amazon
    -- SageMaker to perform tasks on your behalf.
    roleArn :: Prelude.Text,
    -- | Provides information about the versioned model package Amazon Resource
    -- Name (ARN), the traffic pattern, and endpoint configurations.
    inputConfig :: RecommendationJobInputConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInferenceRecommendationsJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobDescription', 'createInferenceRecommendationsJob_jobDescription' - Description of the recommendation job.
--
-- 'outputConfig', 'createInferenceRecommendationsJob_outputConfig' - Provides information about the output artifacts and the KMS key to use
-- for Amazon S3 server-side encryption.
--
-- 'stoppingConditions', 'createInferenceRecommendationsJob_stoppingConditions' - A set of conditions for stopping a recommendation job. If any of the
-- conditions are met, the job is automatically stopped.
--
-- 'tags', 'createInferenceRecommendationsJob_tags' - The metadata that you apply to Amazon Web Services resources to help you
-- categorize and organize them. Each tag consists of a key and a value,
-- both of which you define. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- in the Amazon Web Services General Reference.
--
-- 'jobName', 'createInferenceRecommendationsJob_jobName' - A name for the recommendation job. The name must be unique within the
-- Amazon Web Services Region and within your Amazon Web Services account.
--
-- 'jobType', 'createInferenceRecommendationsJob_jobType' - Defines the type of recommendation job. Specify @Default@ to initiate an
-- instance recommendation and @Advanced@ to initiate a load test. If left
-- unspecified, Amazon SageMaker Inference Recommender will run an instance
-- recommendation (@DEFAULT@) job.
--
-- 'roleArn', 'createInferenceRecommendationsJob_roleArn' - The Amazon Resource Name (ARN) of an IAM role that enables Amazon
-- SageMaker to perform tasks on your behalf.
--
-- 'inputConfig', 'createInferenceRecommendationsJob_inputConfig' - Provides information about the versioned model package Amazon Resource
-- Name (ARN), the traffic pattern, and endpoint configurations.
newCreateInferenceRecommendationsJob ::
  -- | 'jobName'
  Prelude.Text ->
  -- | 'jobType'
  RecommendationJobType ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'inputConfig'
  RecommendationJobInputConfig ->
  CreateInferenceRecommendationsJob
newCreateInferenceRecommendationsJob
  pJobName_
  pJobType_
  pRoleArn_
  pInputConfig_ =
    CreateInferenceRecommendationsJob'
      { jobDescription =
          Prelude.Nothing,
        outputConfig = Prelude.Nothing,
        stoppingConditions = Prelude.Nothing,
        tags = Prelude.Nothing,
        jobName = pJobName_,
        jobType = pJobType_,
        roleArn = pRoleArn_,
        inputConfig = pInputConfig_
      }

-- | Description of the recommendation job.
createInferenceRecommendationsJob_jobDescription :: Lens.Lens' CreateInferenceRecommendationsJob (Prelude.Maybe Prelude.Text)
createInferenceRecommendationsJob_jobDescription = Lens.lens (\CreateInferenceRecommendationsJob' {jobDescription} -> jobDescription) (\s@CreateInferenceRecommendationsJob' {} a -> s {jobDescription = a} :: CreateInferenceRecommendationsJob)

-- | Provides information about the output artifacts and the KMS key to use
-- for Amazon S3 server-side encryption.
createInferenceRecommendationsJob_outputConfig :: Lens.Lens' CreateInferenceRecommendationsJob (Prelude.Maybe RecommendationJobOutputConfig)
createInferenceRecommendationsJob_outputConfig = Lens.lens (\CreateInferenceRecommendationsJob' {outputConfig} -> outputConfig) (\s@CreateInferenceRecommendationsJob' {} a -> s {outputConfig = a} :: CreateInferenceRecommendationsJob)

-- | A set of conditions for stopping a recommendation job. If any of the
-- conditions are met, the job is automatically stopped.
createInferenceRecommendationsJob_stoppingConditions :: Lens.Lens' CreateInferenceRecommendationsJob (Prelude.Maybe RecommendationJobStoppingConditions)
createInferenceRecommendationsJob_stoppingConditions = Lens.lens (\CreateInferenceRecommendationsJob' {stoppingConditions} -> stoppingConditions) (\s@CreateInferenceRecommendationsJob' {} a -> s {stoppingConditions = a} :: CreateInferenceRecommendationsJob)

-- | The metadata that you apply to Amazon Web Services resources to help you
-- categorize and organize them. Each tag consists of a key and a value,
-- both of which you define. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- in the Amazon Web Services General Reference.
createInferenceRecommendationsJob_tags :: Lens.Lens' CreateInferenceRecommendationsJob (Prelude.Maybe [Tag])
createInferenceRecommendationsJob_tags = Lens.lens (\CreateInferenceRecommendationsJob' {tags} -> tags) (\s@CreateInferenceRecommendationsJob' {} a -> s {tags = a} :: CreateInferenceRecommendationsJob) Prelude.. Lens.mapping Lens.coerced

-- | A name for the recommendation job. The name must be unique within the
-- Amazon Web Services Region and within your Amazon Web Services account.
createInferenceRecommendationsJob_jobName :: Lens.Lens' CreateInferenceRecommendationsJob Prelude.Text
createInferenceRecommendationsJob_jobName = Lens.lens (\CreateInferenceRecommendationsJob' {jobName} -> jobName) (\s@CreateInferenceRecommendationsJob' {} a -> s {jobName = a} :: CreateInferenceRecommendationsJob)

-- | Defines the type of recommendation job. Specify @Default@ to initiate an
-- instance recommendation and @Advanced@ to initiate a load test. If left
-- unspecified, Amazon SageMaker Inference Recommender will run an instance
-- recommendation (@DEFAULT@) job.
createInferenceRecommendationsJob_jobType :: Lens.Lens' CreateInferenceRecommendationsJob RecommendationJobType
createInferenceRecommendationsJob_jobType = Lens.lens (\CreateInferenceRecommendationsJob' {jobType} -> jobType) (\s@CreateInferenceRecommendationsJob' {} a -> s {jobType = a} :: CreateInferenceRecommendationsJob)

-- | The Amazon Resource Name (ARN) of an IAM role that enables Amazon
-- SageMaker to perform tasks on your behalf.
createInferenceRecommendationsJob_roleArn :: Lens.Lens' CreateInferenceRecommendationsJob Prelude.Text
createInferenceRecommendationsJob_roleArn = Lens.lens (\CreateInferenceRecommendationsJob' {roleArn} -> roleArn) (\s@CreateInferenceRecommendationsJob' {} a -> s {roleArn = a} :: CreateInferenceRecommendationsJob)

-- | Provides information about the versioned model package Amazon Resource
-- Name (ARN), the traffic pattern, and endpoint configurations.
createInferenceRecommendationsJob_inputConfig :: Lens.Lens' CreateInferenceRecommendationsJob RecommendationJobInputConfig
createInferenceRecommendationsJob_inputConfig = Lens.lens (\CreateInferenceRecommendationsJob' {inputConfig} -> inputConfig) (\s@CreateInferenceRecommendationsJob' {} a -> s {inputConfig = a} :: CreateInferenceRecommendationsJob)

instance
  Core.AWSRequest
    CreateInferenceRecommendationsJob
  where
  type
    AWSResponse CreateInferenceRecommendationsJob =
      CreateInferenceRecommendationsJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateInferenceRecommendationsJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "JobArn")
      )

instance
  Prelude.Hashable
    CreateInferenceRecommendationsJob
  where
  hashWithSalt
    _salt
    CreateInferenceRecommendationsJob' {..} =
      _salt
        `Prelude.hashWithSalt` jobDescription
        `Prelude.hashWithSalt` outputConfig
        `Prelude.hashWithSalt` stoppingConditions
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` jobName
        `Prelude.hashWithSalt` jobType
        `Prelude.hashWithSalt` roleArn
        `Prelude.hashWithSalt` inputConfig

instance
  Prelude.NFData
    CreateInferenceRecommendationsJob
  where
  rnf CreateInferenceRecommendationsJob' {..} =
    Prelude.rnf jobDescription
      `Prelude.seq` Prelude.rnf outputConfig
      `Prelude.seq` Prelude.rnf stoppingConditions
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobType
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf inputConfig

instance
  Data.ToHeaders
    CreateInferenceRecommendationsJob
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.CreateInferenceRecommendationsJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    CreateInferenceRecommendationsJob
  where
  toJSON CreateInferenceRecommendationsJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("JobDescription" Data..=)
              Prelude.<$> jobDescription,
            ("OutputConfig" Data..=) Prelude.<$> outputConfig,
            ("StoppingConditions" Data..=)
              Prelude.<$> stoppingConditions,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("JobName" Data..= jobName),
            Prelude.Just ("JobType" Data..= jobType),
            Prelude.Just ("RoleArn" Data..= roleArn),
            Prelude.Just ("InputConfig" Data..= inputConfig)
          ]
      )

instance
  Data.ToPath
    CreateInferenceRecommendationsJob
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    CreateInferenceRecommendationsJob
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateInferenceRecommendationsJobResponse' smart constructor.
data CreateInferenceRecommendationsJobResponse = CreateInferenceRecommendationsJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the recommendation job.
    jobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInferenceRecommendationsJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createInferenceRecommendationsJobResponse_httpStatus' - The response's http status code.
--
-- 'jobArn', 'createInferenceRecommendationsJobResponse_jobArn' - The Amazon Resource Name (ARN) of the recommendation job.
newCreateInferenceRecommendationsJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'jobArn'
  Prelude.Text ->
  CreateInferenceRecommendationsJobResponse
newCreateInferenceRecommendationsJobResponse
  pHttpStatus_
  pJobArn_ =
    CreateInferenceRecommendationsJobResponse'
      { httpStatus =
          pHttpStatus_,
        jobArn = pJobArn_
      }

-- | The response's http status code.
createInferenceRecommendationsJobResponse_httpStatus :: Lens.Lens' CreateInferenceRecommendationsJobResponse Prelude.Int
createInferenceRecommendationsJobResponse_httpStatus = Lens.lens (\CreateInferenceRecommendationsJobResponse' {httpStatus} -> httpStatus) (\s@CreateInferenceRecommendationsJobResponse' {} a -> s {httpStatus = a} :: CreateInferenceRecommendationsJobResponse)

-- | The Amazon Resource Name (ARN) of the recommendation job.
createInferenceRecommendationsJobResponse_jobArn :: Lens.Lens' CreateInferenceRecommendationsJobResponse Prelude.Text
createInferenceRecommendationsJobResponse_jobArn = Lens.lens (\CreateInferenceRecommendationsJobResponse' {jobArn} -> jobArn) (\s@CreateInferenceRecommendationsJobResponse' {} a -> s {jobArn = a} :: CreateInferenceRecommendationsJobResponse)

instance
  Prelude.NFData
    CreateInferenceRecommendationsJobResponse
  where
  rnf CreateInferenceRecommendationsJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf jobArn
