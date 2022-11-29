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
-- Module      : Amazonka.SageMaker.DescribeInferenceRecommendationsJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides the results of the Inference Recommender job. One or more
-- recommendation jobs are returned.
module Amazonka.SageMaker.DescribeInferenceRecommendationsJob
  ( -- * Creating a Request
    DescribeInferenceRecommendationsJob (..),
    newDescribeInferenceRecommendationsJob,

    -- * Request Lenses
    describeInferenceRecommendationsJob_jobName,

    -- * Destructuring the Response
    DescribeInferenceRecommendationsJobResponse (..),
    newDescribeInferenceRecommendationsJobResponse,

    -- * Response Lenses
    describeInferenceRecommendationsJobResponse_endpointPerformances,
    describeInferenceRecommendationsJobResponse_inferenceRecommendations,
    describeInferenceRecommendationsJobResponse_stoppingConditions,
    describeInferenceRecommendationsJobResponse_completionTime,
    describeInferenceRecommendationsJobResponse_failureReason,
    describeInferenceRecommendationsJobResponse_jobDescription,
    describeInferenceRecommendationsJobResponse_httpStatus,
    describeInferenceRecommendationsJobResponse_jobName,
    describeInferenceRecommendationsJobResponse_jobType,
    describeInferenceRecommendationsJobResponse_jobArn,
    describeInferenceRecommendationsJobResponse_roleArn,
    describeInferenceRecommendationsJobResponse_status,
    describeInferenceRecommendationsJobResponse_creationTime,
    describeInferenceRecommendationsJobResponse_lastModifiedTime,
    describeInferenceRecommendationsJobResponse_inputConfig,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeInferenceRecommendationsJob' smart constructor.
data DescribeInferenceRecommendationsJob = DescribeInferenceRecommendationsJob'
  { -- | The name of the job. The name must be unique within an Amazon Web
    -- Services Region in the Amazon Web Services account.
    jobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInferenceRecommendationsJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobName', 'describeInferenceRecommendationsJob_jobName' - The name of the job. The name must be unique within an Amazon Web
-- Services Region in the Amazon Web Services account.
newDescribeInferenceRecommendationsJob ::
  -- | 'jobName'
  Prelude.Text ->
  DescribeInferenceRecommendationsJob
newDescribeInferenceRecommendationsJob pJobName_ =
  DescribeInferenceRecommendationsJob'
    { jobName =
        pJobName_
    }

-- | The name of the job. The name must be unique within an Amazon Web
-- Services Region in the Amazon Web Services account.
describeInferenceRecommendationsJob_jobName :: Lens.Lens' DescribeInferenceRecommendationsJob Prelude.Text
describeInferenceRecommendationsJob_jobName = Lens.lens (\DescribeInferenceRecommendationsJob' {jobName} -> jobName) (\s@DescribeInferenceRecommendationsJob' {} a -> s {jobName = a} :: DescribeInferenceRecommendationsJob)

instance
  Core.AWSRequest
    DescribeInferenceRecommendationsJob
  where
  type
    AWSResponse DescribeInferenceRecommendationsJob =
      DescribeInferenceRecommendationsJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInferenceRecommendationsJobResponse'
            Prelude.<$> ( x Core..?> "EndpointPerformances"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Core..?> "InferenceRecommendations")
              Prelude.<*> (x Core..?> "StoppingConditions")
              Prelude.<*> (x Core..?> "CompletionTime")
              Prelude.<*> (x Core..?> "FailureReason")
              Prelude.<*> (x Core..?> "JobDescription")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Core..:> "JobName")
              Prelude.<*> (x Core..:> "JobType")
              Prelude.<*> (x Core..:> "JobArn")
              Prelude.<*> (x Core..:> "RoleArn")
              Prelude.<*> (x Core..:> "Status")
              Prelude.<*> (x Core..:> "CreationTime")
              Prelude.<*> (x Core..:> "LastModifiedTime")
              Prelude.<*> (x Core..:> "InputConfig")
      )

instance
  Prelude.Hashable
    DescribeInferenceRecommendationsJob
  where
  hashWithSalt
    _salt
    DescribeInferenceRecommendationsJob' {..} =
      _salt `Prelude.hashWithSalt` jobName

instance
  Prelude.NFData
    DescribeInferenceRecommendationsJob
  where
  rnf DescribeInferenceRecommendationsJob' {..} =
    Prelude.rnf jobName

instance
  Core.ToHeaders
    DescribeInferenceRecommendationsJob
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeInferenceRecommendationsJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DescribeInferenceRecommendationsJob
  where
  toJSON DescribeInferenceRecommendationsJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobName" Core..= jobName)]
      )

instance
  Core.ToPath
    DescribeInferenceRecommendationsJob
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeInferenceRecommendationsJob
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeInferenceRecommendationsJobResponse' smart constructor.
data DescribeInferenceRecommendationsJobResponse = DescribeInferenceRecommendationsJobResponse'
  { -- | The performance results from running an Inference Recommender job on an
    -- existing endpoint.
    endpointPerformances :: Prelude.Maybe [EndpointPerformance],
    -- | The recommendations made by Inference Recommender.
    inferenceRecommendations :: Prelude.Maybe (Prelude.NonEmpty InferenceRecommendation),
    -- | The stopping conditions that you provided when you initiated the job.
    stoppingConditions :: Prelude.Maybe RecommendationJobStoppingConditions,
    -- | A timestamp that shows when the job completed.
    completionTime :: Prelude.Maybe Core.POSIX,
    -- | If the job fails, provides information why the job failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The job description that you provided when you initiated the job.
    jobDescription :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the job. The name must be unique within an Amazon Web
    -- Services Region in the Amazon Web Services account.
    jobName :: Prelude.Text,
    -- | The job type that you provided when you initiated the job.
    jobType :: RecommendationJobType,
    -- | The Amazon Resource Name (ARN) of the job.
    jobArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon Web Services Identity and
    -- Access Management (IAM) role you provided when you initiated the job.
    roleArn :: Prelude.Text,
    -- | The status of the job.
    status :: RecommendationJobStatus,
    -- | A timestamp that shows when the job was created.
    creationTime :: Core.POSIX,
    -- | A timestamp that shows when the job was last modified.
    lastModifiedTime :: Core.POSIX,
    -- | Returns information about the versioned model package Amazon Resource
    -- Name (ARN), the traffic pattern, and endpoint configurations you
    -- provided when you initiated the job.
    inputConfig :: RecommendationJobInputConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInferenceRecommendationsJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointPerformances', 'describeInferenceRecommendationsJobResponse_endpointPerformances' - The performance results from running an Inference Recommender job on an
-- existing endpoint.
--
-- 'inferenceRecommendations', 'describeInferenceRecommendationsJobResponse_inferenceRecommendations' - The recommendations made by Inference Recommender.
--
-- 'stoppingConditions', 'describeInferenceRecommendationsJobResponse_stoppingConditions' - The stopping conditions that you provided when you initiated the job.
--
-- 'completionTime', 'describeInferenceRecommendationsJobResponse_completionTime' - A timestamp that shows when the job completed.
--
-- 'failureReason', 'describeInferenceRecommendationsJobResponse_failureReason' - If the job fails, provides information why the job failed.
--
-- 'jobDescription', 'describeInferenceRecommendationsJobResponse_jobDescription' - The job description that you provided when you initiated the job.
--
-- 'httpStatus', 'describeInferenceRecommendationsJobResponse_httpStatus' - The response's http status code.
--
-- 'jobName', 'describeInferenceRecommendationsJobResponse_jobName' - The name of the job. The name must be unique within an Amazon Web
-- Services Region in the Amazon Web Services account.
--
-- 'jobType', 'describeInferenceRecommendationsJobResponse_jobType' - The job type that you provided when you initiated the job.
--
-- 'jobArn', 'describeInferenceRecommendationsJobResponse_jobArn' - The Amazon Resource Name (ARN) of the job.
--
-- 'roleArn', 'describeInferenceRecommendationsJobResponse_roleArn' - The Amazon Resource Name (ARN) of the Amazon Web Services Identity and
-- Access Management (IAM) role you provided when you initiated the job.
--
-- 'status', 'describeInferenceRecommendationsJobResponse_status' - The status of the job.
--
-- 'creationTime', 'describeInferenceRecommendationsJobResponse_creationTime' - A timestamp that shows when the job was created.
--
-- 'lastModifiedTime', 'describeInferenceRecommendationsJobResponse_lastModifiedTime' - A timestamp that shows when the job was last modified.
--
-- 'inputConfig', 'describeInferenceRecommendationsJobResponse_inputConfig' - Returns information about the versioned model package Amazon Resource
-- Name (ARN), the traffic pattern, and endpoint configurations you
-- provided when you initiated the job.
newDescribeInferenceRecommendationsJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'jobName'
  Prelude.Text ->
  -- | 'jobType'
  RecommendationJobType ->
  -- | 'jobArn'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'status'
  RecommendationJobStatus ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  -- | 'inputConfig'
  RecommendationJobInputConfig ->
  DescribeInferenceRecommendationsJobResponse
newDescribeInferenceRecommendationsJobResponse
  pHttpStatus_
  pJobName_
  pJobType_
  pJobArn_
  pRoleArn_
  pStatus_
  pCreationTime_
  pLastModifiedTime_
  pInputConfig_ =
    DescribeInferenceRecommendationsJobResponse'
      { endpointPerformances =
          Prelude.Nothing,
        inferenceRecommendations =
          Prelude.Nothing,
        stoppingConditions =
          Prelude.Nothing,
        completionTime =
          Prelude.Nothing,
        failureReason =
          Prelude.Nothing,
        jobDescription =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        jobName = pJobName_,
        jobType = pJobType_,
        jobArn = pJobArn_,
        roleArn = pRoleArn_,
        status = pStatus_,
        creationTime =
          Core._Time
            Lens.# pCreationTime_,
        lastModifiedTime =
          Core._Time
            Lens.# pLastModifiedTime_,
        inputConfig = pInputConfig_
      }

-- | The performance results from running an Inference Recommender job on an
-- existing endpoint.
describeInferenceRecommendationsJobResponse_endpointPerformances :: Lens.Lens' DescribeInferenceRecommendationsJobResponse (Prelude.Maybe [EndpointPerformance])
describeInferenceRecommendationsJobResponse_endpointPerformances = Lens.lens (\DescribeInferenceRecommendationsJobResponse' {endpointPerformances} -> endpointPerformances) (\s@DescribeInferenceRecommendationsJobResponse' {} a -> s {endpointPerformances = a} :: DescribeInferenceRecommendationsJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The recommendations made by Inference Recommender.
describeInferenceRecommendationsJobResponse_inferenceRecommendations :: Lens.Lens' DescribeInferenceRecommendationsJobResponse (Prelude.Maybe (Prelude.NonEmpty InferenceRecommendation))
describeInferenceRecommendationsJobResponse_inferenceRecommendations = Lens.lens (\DescribeInferenceRecommendationsJobResponse' {inferenceRecommendations} -> inferenceRecommendations) (\s@DescribeInferenceRecommendationsJobResponse' {} a -> s {inferenceRecommendations = a} :: DescribeInferenceRecommendationsJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The stopping conditions that you provided when you initiated the job.
describeInferenceRecommendationsJobResponse_stoppingConditions :: Lens.Lens' DescribeInferenceRecommendationsJobResponse (Prelude.Maybe RecommendationJobStoppingConditions)
describeInferenceRecommendationsJobResponse_stoppingConditions = Lens.lens (\DescribeInferenceRecommendationsJobResponse' {stoppingConditions} -> stoppingConditions) (\s@DescribeInferenceRecommendationsJobResponse' {} a -> s {stoppingConditions = a} :: DescribeInferenceRecommendationsJobResponse)

-- | A timestamp that shows when the job completed.
describeInferenceRecommendationsJobResponse_completionTime :: Lens.Lens' DescribeInferenceRecommendationsJobResponse (Prelude.Maybe Prelude.UTCTime)
describeInferenceRecommendationsJobResponse_completionTime = Lens.lens (\DescribeInferenceRecommendationsJobResponse' {completionTime} -> completionTime) (\s@DescribeInferenceRecommendationsJobResponse' {} a -> s {completionTime = a} :: DescribeInferenceRecommendationsJobResponse) Prelude.. Lens.mapping Core._Time

-- | If the job fails, provides information why the job failed.
describeInferenceRecommendationsJobResponse_failureReason :: Lens.Lens' DescribeInferenceRecommendationsJobResponse (Prelude.Maybe Prelude.Text)
describeInferenceRecommendationsJobResponse_failureReason = Lens.lens (\DescribeInferenceRecommendationsJobResponse' {failureReason} -> failureReason) (\s@DescribeInferenceRecommendationsJobResponse' {} a -> s {failureReason = a} :: DescribeInferenceRecommendationsJobResponse)

-- | The job description that you provided when you initiated the job.
describeInferenceRecommendationsJobResponse_jobDescription :: Lens.Lens' DescribeInferenceRecommendationsJobResponse (Prelude.Maybe Prelude.Text)
describeInferenceRecommendationsJobResponse_jobDescription = Lens.lens (\DescribeInferenceRecommendationsJobResponse' {jobDescription} -> jobDescription) (\s@DescribeInferenceRecommendationsJobResponse' {} a -> s {jobDescription = a} :: DescribeInferenceRecommendationsJobResponse)

-- | The response's http status code.
describeInferenceRecommendationsJobResponse_httpStatus :: Lens.Lens' DescribeInferenceRecommendationsJobResponse Prelude.Int
describeInferenceRecommendationsJobResponse_httpStatus = Lens.lens (\DescribeInferenceRecommendationsJobResponse' {httpStatus} -> httpStatus) (\s@DescribeInferenceRecommendationsJobResponse' {} a -> s {httpStatus = a} :: DescribeInferenceRecommendationsJobResponse)

-- | The name of the job. The name must be unique within an Amazon Web
-- Services Region in the Amazon Web Services account.
describeInferenceRecommendationsJobResponse_jobName :: Lens.Lens' DescribeInferenceRecommendationsJobResponse Prelude.Text
describeInferenceRecommendationsJobResponse_jobName = Lens.lens (\DescribeInferenceRecommendationsJobResponse' {jobName} -> jobName) (\s@DescribeInferenceRecommendationsJobResponse' {} a -> s {jobName = a} :: DescribeInferenceRecommendationsJobResponse)

-- | The job type that you provided when you initiated the job.
describeInferenceRecommendationsJobResponse_jobType :: Lens.Lens' DescribeInferenceRecommendationsJobResponse RecommendationJobType
describeInferenceRecommendationsJobResponse_jobType = Lens.lens (\DescribeInferenceRecommendationsJobResponse' {jobType} -> jobType) (\s@DescribeInferenceRecommendationsJobResponse' {} a -> s {jobType = a} :: DescribeInferenceRecommendationsJobResponse)

-- | The Amazon Resource Name (ARN) of the job.
describeInferenceRecommendationsJobResponse_jobArn :: Lens.Lens' DescribeInferenceRecommendationsJobResponse Prelude.Text
describeInferenceRecommendationsJobResponse_jobArn = Lens.lens (\DescribeInferenceRecommendationsJobResponse' {jobArn} -> jobArn) (\s@DescribeInferenceRecommendationsJobResponse' {} a -> s {jobArn = a} :: DescribeInferenceRecommendationsJobResponse)

-- | The Amazon Resource Name (ARN) of the Amazon Web Services Identity and
-- Access Management (IAM) role you provided when you initiated the job.
describeInferenceRecommendationsJobResponse_roleArn :: Lens.Lens' DescribeInferenceRecommendationsJobResponse Prelude.Text
describeInferenceRecommendationsJobResponse_roleArn = Lens.lens (\DescribeInferenceRecommendationsJobResponse' {roleArn} -> roleArn) (\s@DescribeInferenceRecommendationsJobResponse' {} a -> s {roleArn = a} :: DescribeInferenceRecommendationsJobResponse)

-- | The status of the job.
describeInferenceRecommendationsJobResponse_status :: Lens.Lens' DescribeInferenceRecommendationsJobResponse RecommendationJobStatus
describeInferenceRecommendationsJobResponse_status = Lens.lens (\DescribeInferenceRecommendationsJobResponse' {status} -> status) (\s@DescribeInferenceRecommendationsJobResponse' {} a -> s {status = a} :: DescribeInferenceRecommendationsJobResponse)

-- | A timestamp that shows when the job was created.
describeInferenceRecommendationsJobResponse_creationTime :: Lens.Lens' DescribeInferenceRecommendationsJobResponse Prelude.UTCTime
describeInferenceRecommendationsJobResponse_creationTime = Lens.lens (\DescribeInferenceRecommendationsJobResponse' {creationTime} -> creationTime) (\s@DescribeInferenceRecommendationsJobResponse' {} a -> s {creationTime = a} :: DescribeInferenceRecommendationsJobResponse) Prelude.. Core._Time

-- | A timestamp that shows when the job was last modified.
describeInferenceRecommendationsJobResponse_lastModifiedTime :: Lens.Lens' DescribeInferenceRecommendationsJobResponse Prelude.UTCTime
describeInferenceRecommendationsJobResponse_lastModifiedTime = Lens.lens (\DescribeInferenceRecommendationsJobResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeInferenceRecommendationsJobResponse' {} a -> s {lastModifiedTime = a} :: DescribeInferenceRecommendationsJobResponse) Prelude.. Core._Time

-- | Returns information about the versioned model package Amazon Resource
-- Name (ARN), the traffic pattern, and endpoint configurations you
-- provided when you initiated the job.
describeInferenceRecommendationsJobResponse_inputConfig :: Lens.Lens' DescribeInferenceRecommendationsJobResponse RecommendationJobInputConfig
describeInferenceRecommendationsJobResponse_inputConfig = Lens.lens (\DescribeInferenceRecommendationsJobResponse' {inputConfig} -> inputConfig) (\s@DescribeInferenceRecommendationsJobResponse' {} a -> s {inputConfig = a} :: DescribeInferenceRecommendationsJobResponse)

instance
  Prelude.NFData
    DescribeInferenceRecommendationsJobResponse
  where
  rnf DescribeInferenceRecommendationsJobResponse' {..} =
    Prelude.rnf endpointPerformances
      `Prelude.seq` Prelude.rnf inferenceRecommendations
      `Prelude.seq` Prelude.rnf stoppingConditions
      `Prelude.seq` Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf jobDescription
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobType
      `Prelude.seq` Prelude.rnf jobArn
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf inputConfig
