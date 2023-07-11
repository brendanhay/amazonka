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
-- Module      : Amazonka.LookoutVision.StartModelPackagingJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an Amazon Lookout for Vision model packaging job. A model
-- packaging job creates an AWS IoT Greengrass component for a Lookout for
-- Vision model. You can use the component to deploy your model to an edge
-- device managed by Greengrass.
--
-- Use the DescribeModelPackagingJob API to determine the current status of
-- the job. The model packaging job is complete if the value of @Status@ is
-- @SUCCEEDED@.
--
-- To deploy the component to the target device, use the component name and
-- component version with the AWS IoT Greengrass
-- <https://docs.aws.amazon.com/greengrass/v2/APIReference/API_CreateDeployment.html CreateDeployment>
-- API.
--
-- This operation requires the following permissions:
--
-- -   @lookoutvision:StartModelPackagingJob@
--
-- -   @s3:PutObject@
--
-- -   @s3:GetBucketLocation@
--
-- -   @kms:GenerateDataKey@
--
-- -   @greengrass:CreateComponentVersion@
--
-- -   @greengrass:DescribeComponent@
--
-- -   (Optional) @greengrass:TagResource@. Only required if you want to
--     tag the component.
--
-- For more information, see /Using your Amazon Lookout for Vision model on
-- an edge device/ in the Amazon Lookout for Vision Developer Guide.
module Amazonka.LookoutVision.StartModelPackagingJob
  ( -- * Creating a Request
    StartModelPackagingJob (..),
    newStartModelPackagingJob,

    -- * Request Lenses
    startModelPackagingJob_clientToken,
    startModelPackagingJob_description,
    startModelPackagingJob_jobName,
    startModelPackagingJob_projectName,
    startModelPackagingJob_modelVersion,
    startModelPackagingJob_configuration,

    -- * Destructuring the Response
    StartModelPackagingJobResponse (..),
    newStartModelPackagingJobResponse,

    -- * Response Lenses
    startModelPackagingJobResponse_jobName,
    startModelPackagingJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutVision.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartModelPackagingJob' smart constructor.
data StartModelPackagingJob = StartModelPackagingJob'
  { -- | ClientToken is an idempotency token that ensures a call to
    -- @StartModelPackagingJob@ completes only once. You choose the value to
    -- pass. For example, An issue might prevent you from getting a response
    -- from @StartModelPackagingJob@. In this case, safely retry your call to
    -- @StartModelPackagingJob@ by using the same @ClientToken@ parameter
    -- value.
    --
    -- If you don\'t supply a value for @ClientToken@, the AWS SDK you are
    -- using inserts a value for you. This prevents retries after a network
    -- error from making multiple dataset creation requests. You\'ll need to
    -- provide your own value for other use cases.
    --
    -- An error occurs if the other input parameters are not the same as in the
    -- first request. Using a different value for @ClientToken@ is considered a
    -- new call to @StartModelPackagingJob@. An idempotency token is active for
    -- 8 hours.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description for the model packaging job.
    description :: Prelude.Maybe Prelude.Text,
    -- | A name for the model packaging job. If you don\'t supply a value, the
    -- service creates a job name for you.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The name of the project which contains the version of the model that you
    -- want to package.
    projectName :: Prelude.Text,
    -- | The version of the model within the project that you want to package.
    modelVersion :: Prelude.Text,
    -- | The configuration for the model packaging job.
    configuration :: ModelPackagingConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartModelPackagingJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'startModelPackagingJob_clientToken' - ClientToken is an idempotency token that ensures a call to
-- @StartModelPackagingJob@ completes only once. You choose the value to
-- pass. For example, An issue might prevent you from getting a response
-- from @StartModelPackagingJob@. In this case, safely retry your call to
-- @StartModelPackagingJob@ by using the same @ClientToken@ parameter
-- value.
--
-- If you don\'t supply a value for @ClientToken@, the AWS SDK you are
-- using inserts a value for you. This prevents retries after a network
-- error from making multiple dataset creation requests. You\'ll need to
-- provide your own value for other use cases.
--
-- An error occurs if the other input parameters are not the same as in the
-- first request. Using a different value for @ClientToken@ is considered a
-- new call to @StartModelPackagingJob@. An idempotency token is active for
-- 8 hours.
--
-- 'description', 'startModelPackagingJob_description' - A description for the model packaging job.
--
-- 'jobName', 'startModelPackagingJob_jobName' - A name for the model packaging job. If you don\'t supply a value, the
-- service creates a job name for you.
--
-- 'projectName', 'startModelPackagingJob_projectName' - The name of the project which contains the version of the model that you
-- want to package.
--
-- 'modelVersion', 'startModelPackagingJob_modelVersion' - The version of the model within the project that you want to package.
--
-- 'configuration', 'startModelPackagingJob_configuration' - The configuration for the model packaging job.
newStartModelPackagingJob ::
  -- | 'projectName'
  Prelude.Text ->
  -- | 'modelVersion'
  Prelude.Text ->
  -- | 'configuration'
  ModelPackagingConfiguration ->
  StartModelPackagingJob
newStartModelPackagingJob
  pProjectName_
  pModelVersion_
  pConfiguration_ =
    StartModelPackagingJob'
      { clientToken =
          Prelude.Nothing,
        description = Prelude.Nothing,
        jobName = Prelude.Nothing,
        projectName = pProjectName_,
        modelVersion = pModelVersion_,
        configuration = pConfiguration_
      }

-- | ClientToken is an idempotency token that ensures a call to
-- @StartModelPackagingJob@ completes only once. You choose the value to
-- pass. For example, An issue might prevent you from getting a response
-- from @StartModelPackagingJob@. In this case, safely retry your call to
-- @StartModelPackagingJob@ by using the same @ClientToken@ parameter
-- value.
--
-- If you don\'t supply a value for @ClientToken@, the AWS SDK you are
-- using inserts a value for you. This prevents retries after a network
-- error from making multiple dataset creation requests. You\'ll need to
-- provide your own value for other use cases.
--
-- An error occurs if the other input parameters are not the same as in the
-- first request. Using a different value for @ClientToken@ is considered a
-- new call to @StartModelPackagingJob@. An idempotency token is active for
-- 8 hours.
startModelPackagingJob_clientToken :: Lens.Lens' StartModelPackagingJob (Prelude.Maybe Prelude.Text)
startModelPackagingJob_clientToken = Lens.lens (\StartModelPackagingJob' {clientToken} -> clientToken) (\s@StartModelPackagingJob' {} a -> s {clientToken = a} :: StartModelPackagingJob)

-- | A description for the model packaging job.
startModelPackagingJob_description :: Lens.Lens' StartModelPackagingJob (Prelude.Maybe Prelude.Text)
startModelPackagingJob_description = Lens.lens (\StartModelPackagingJob' {description} -> description) (\s@StartModelPackagingJob' {} a -> s {description = a} :: StartModelPackagingJob)

-- | A name for the model packaging job. If you don\'t supply a value, the
-- service creates a job name for you.
startModelPackagingJob_jobName :: Lens.Lens' StartModelPackagingJob (Prelude.Maybe Prelude.Text)
startModelPackagingJob_jobName = Lens.lens (\StartModelPackagingJob' {jobName} -> jobName) (\s@StartModelPackagingJob' {} a -> s {jobName = a} :: StartModelPackagingJob)

-- | The name of the project which contains the version of the model that you
-- want to package.
startModelPackagingJob_projectName :: Lens.Lens' StartModelPackagingJob Prelude.Text
startModelPackagingJob_projectName = Lens.lens (\StartModelPackagingJob' {projectName} -> projectName) (\s@StartModelPackagingJob' {} a -> s {projectName = a} :: StartModelPackagingJob)

-- | The version of the model within the project that you want to package.
startModelPackagingJob_modelVersion :: Lens.Lens' StartModelPackagingJob Prelude.Text
startModelPackagingJob_modelVersion = Lens.lens (\StartModelPackagingJob' {modelVersion} -> modelVersion) (\s@StartModelPackagingJob' {} a -> s {modelVersion = a} :: StartModelPackagingJob)

-- | The configuration for the model packaging job.
startModelPackagingJob_configuration :: Lens.Lens' StartModelPackagingJob ModelPackagingConfiguration
startModelPackagingJob_configuration = Lens.lens (\StartModelPackagingJob' {configuration} -> configuration) (\s@StartModelPackagingJob' {} a -> s {configuration = a} :: StartModelPackagingJob)

instance Core.AWSRequest StartModelPackagingJob where
  type
    AWSResponse StartModelPackagingJob =
      StartModelPackagingJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartModelPackagingJobResponse'
            Prelude.<$> (x Data..?> "JobName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartModelPackagingJob where
  hashWithSalt _salt StartModelPackagingJob' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` modelVersion
      `Prelude.hashWithSalt` configuration

instance Prelude.NFData StartModelPackagingJob where
  rnf StartModelPackagingJob' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf modelVersion
      `Prelude.seq` Prelude.rnf configuration

instance Data.ToHeaders StartModelPackagingJob where
  toHeaders StartModelPackagingJob' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON StartModelPackagingJob where
  toJSON StartModelPackagingJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("JobName" Data..=) Prelude.<$> jobName,
            Prelude.Just ("ModelVersion" Data..= modelVersion),
            Prelude.Just
              ("Configuration" Data..= configuration)
          ]
      )

instance Data.ToPath StartModelPackagingJob where
  toPath StartModelPackagingJob' {..} =
    Prelude.mconcat
      [ "/2020-11-20/projects/",
        Data.toBS projectName,
        "/modelpackagingjobs"
      ]

instance Data.ToQuery StartModelPackagingJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartModelPackagingJobResponse' smart constructor.
data StartModelPackagingJobResponse = StartModelPackagingJobResponse'
  { -- | The job name for the model packaging job. If you don\'t supply a job
    -- name in the @JobName@ input parameter, the service creates a job name
    -- for you.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartModelPackagingJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobName', 'startModelPackagingJobResponse_jobName' - The job name for the model packaging job. If you don\'t supply a job
-- name in the @JobName@ input parameter, the service creates a job name
-- for you.
--
-- 'httpStatus', 'startModelPackagingJobResponse_httpStatus' - The response's http status code.
newStartModelPackagingJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartModelPackagingJobResponse
newStartModelPackagingJobResponse pHttpStatus_ =
  StartModelPackagingJobResponse'
    { jobName =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The job name for the model packaging job. If you don\'t supply a job
-- name in the @JobName@ input parameter, the service creates a job name
-- for you.
startModelPackagingJobResponse_jobName :: Lens.Lens' StartModelPackagingJobResponse (Prelude.Maybe Prelude.Text)
startModelPackagingJobResponse_jobName = Lens.lens (\StartModelPackagingJobResponse' {jobName} -> jobName) (\s@StartModelPackagingJobResponse' {} a -> s {jobName = a} :: StartModelPackagingJobResponse)

-- | The response's http status code.
startModelPackagingJobResponse_httpStatus :: Lens.Lens' StartModelPackagingJobResponse Prelude.Int
startModelPackagingJobResponse_httpStatus = Lens.lens (\StartModelPackagingJobResponse' {httpStatus} -> httpStatus) (\s@StartModelPackagingJobResponse' {} a -> s {httpStatus = a} :: StartModelPackagingJobResponse)

instance
  Prelude.NFData
    StartModelPackagingJobResponse
  where
  rnf StartModelPackagingJobResponse' {..} =
    Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf httpStatus
