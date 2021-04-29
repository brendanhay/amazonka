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
-- Module      : Network.AWS.SageMaker.CreateCompilationJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a model compilation job. After the model has been compiled,
-- Amazon SageMaker saves the resulting model artifacts to an Amazon Simple
-- Storage Service (Amazon S3) bucket that you specify.
--
-- If you choose to host your model using Amazon SageMaker hosting
-- services, you can use the resulting model artifacts as part of the
-- model. You can also use the artifacts with AWS IoT Greengrass. In that
-- case, deploy them as an ML resource.
--
-- In the request body, you provide the following:
--
-- -   A name for the compilation job
--
-- -   Information about the input model artifacts
--
-- -   The output location for the compiled model and the device (target)
--     that the model runs on
--
-- -   The Amazon Resource Name (ARN) of the IAM role that Amazon SageMaker
--     assumes to perform the model compilation job.
--
-- You can also provide a @Tag@ to track the model compilation job\'s
-- resource use and costs. The response body contains the
-- @CompilationJobArn@ for the compiled job.
--
-- To stop a model compilation job, use StopCompilationJob. To get
-- information about a particular model compilation job, use
-- DescribeCompilationJob. To get information about multiple model
-- compilation jobs, use ListCompilationJobs.
module Network.AWS.SageMaker.CreateCompilationJob
  ( -- * Creating a Request
    CreateCompilationJob (..),
    newCreateCompilationJob,

    -- * Request Lenses
    createCompilationJob_tags,
    createCompilationJob_compilationJobName,
    createCompilationJob_roleArn,
    createCompilationJob_inputConfig,
    createCompilationJob_outputConfig,
    createCompilationJob_stoppingCondition,

    -- * Destructuring the Response
    CreateCompilationJobResponse (..),
    newCreateCompilationJobResponse,

    -- * Response Lenses
    createCompilationJobResponse_httpStatus,
    createCompilationJobResponse_compilationJobArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateCompilationJob' smart constructor.
data CreateCompilationJob = CreateCompilationJob'
  { -- | An array of key-value pairs. You can use tags to categorize your AWS
    -- resources in different ways, for example, by purpose, owner, or
    -- environment. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>.
    tags :: Prelude.Maybe [Tag],
    -- | A name for the model compilation job. The name must be unique within the
    -- AWS Region and within your AWS account.
    compilationJobName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an IAM role that enables Amazon
    -- SageMaker to perform tasks on your behalf.
    --
    -- During model compilation, Amazon SageMaker needs your permission to:
    --
    -- -   Read input data from an S3 bucket
    --
    -- -   Write model artifacts to an S3 bucket
    --
    -- -   Write logs to Amazon CloudWatch Logs
    --
    -- -   Publish metrics to Amazon CloudWatch
    --
    -- You grant permissions for all of these tasks to an IAM role. To pass
    -- this role to Amazon SageMaker, the caller of this API must have the
    -- @iam:PassRole@ permission. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles.>
    roleArn :: Prelude.Text,
    -- | Provides information about the location of input model artifacts, the
    -- name and shape of the expected data inputs, and the framework in which
    -- the model was trained.
    inputConfig :: InputConfig,
    -- | Provides information about the output location for the compiled model
    -- and the target device the model runs on.
    outputConfig :: OutputConfig,
    -- | Specifies a limit to how long a model compilation job can run. When the
    -- job reaches the time limit, Amazon SageMaker ends the compilation job.
    -- Use this API to cap model training costs.
    stoppingCondition :: StoppingCondition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateCompilationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createCompilationJob_tags' - An array of key-value pairs. You can use tags to categorize your AWS
-- resources in different ways, for example, by purpose, owner, or
-- environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>.
--
-- 'compilationJobName', 'createCompilationJob_compilationJobName' - A name for the model compilation job. The name must be unique within the
-- AWS Region and within your AWS account.
--
-- 'roleArn', 'createCompilationJob_roleArn' - The Amazon Resource Name (ARN) of an IAM role that enables Amazon
-- SageMaker to perform tasks on your behalf.
--
-- During model compilation, Amazon SageMaker needs your permission to:
--
-- -   Read input data from an S3 bucket
--
-- -   Write model artifacts to an S3 bucket
--
-- -   Write logs to Amazon CloudWatch Logs
--
-- -   Publish metrics to Amazon CloudWatch
--
-- You grant permissions for all of these tasks to an IAM role. To pass
-- this role to Amazon SageMaker, the caller of this API must have the
-- @iam:PassRole@ permission. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles.>
--
-- 'inputConfig', 'createCompilationJob_inputConfig' - Provides information about the location of input model artifacts, the
-- name and shape of the expected data inputs, and the framework in which
-- the model was trained.
--
-- 'outputConfig', 'createCompilationJob_outputConfig' - Provides information about the output location for the compiled model
-- and the target device the model runs on.
--
-- 'stoppingCondition', 'createCompilationJob_stoppingCondition' - Specifies a limit to how long a model compilation job can run. When the
-- job reaches the time limit, Amazon SageMaker ends the compilation job.
-- Use this API to cap model training costs.
newCreateCompilationJob ::
  -- | 'compilationJobName'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'inputConfig'
  InputConfig ->
  -- | 'outputConfig'
  OutputConfig ->
  -- | 'stoppingCondition'
  StoppingCondition ->
  CreateCompilationJob
newCreateCompilationJob
  pCompilationJobName_
  pRoleArn_
  pInputConfig_
  pOutputConfig_
  pStoppingCondition_ =
    CreateCompilationJob'
      { tags = Prelude.Nothing,
        compilationJobName = pCompilationJobName_,
        roleArn = pRoleArn_,
        inputConfig = pInputConfig_,
        outputConfig = pOutputConfig_,
        stoppingCondition = pStoppingCondition_
      }

-- | An array of key-value pairs. You can use tags to categorize your AWS
-- resources in different ways, for example, by purpose, owner, or
-- environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>.
createCompilationJob_tags :: Lens.Lens' CreateCompilationJob (Prelude.Maybe [Tag])
createCompilationJob_tags = Lens.lens (\CreateCompilationJob' {tags} -> tags) (\s@CreateCompilationJob' {} a -> s {tags = a} :: CreateCompilationJob) Prelude.. Lens.mapping Prelude._Coerce

-- | A name for the model compilation job. The name must be unique within the
-- AWS Region and within your AWS account.
createCompilationJob_compilationJobName :: Lens.Lens' CreateCompilationJob Prelude.Text
createCompilationJob_compilationJobName = Lens.lens (\CreateCompilationJob' {compilationJobName} -> compilationJobName) (\s@CreateCompilationJob' {} a -> s {compilationJobName = a} :: CreateCompilationJob)

-- | The Amazon Resource Name (ARN) of an IAM role that enables Amazon
-- SageMaker to perform tasks on your behalf.
--
-- During model compilation, Amazon SageMaker needs your permission to:
--
-- -   Read input data from an S3 bucket
--
-- -   Write model artifacts to an S3 bucket
--
-- -   Write logs to Amazon CloudWatch Logs
--
-- -   Publish metrics to Amazon CloudWatch
--
-- You grant permissions for all of these tasks to an IAM role. To pass
-- this role to Amazon SageMaker, the caller of this API must have the
-- @iam:PassRole@ permission. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles.>
createCompilationJob_roleArn :: Lens.Lens' CreateCompilationJob Prelude.Text
createCompilationJob_roleArn = Lens.lens (\CreateCompilationJob' {roleArn} -> roleArn) (\s@CreateCompilationJob' {} a -> s {roleArn = a} :: CreateCompilationJob)

-- | Provides information about the location of input model artifacts, the
-- name and shape of the expected data inputs, and the framework in which
-- the model was trained.
createCompilationJob_inputConfig :: Lens.Lens' CreateCompilationJob InputConfig
createCompilationJob_inputConfig = Lens.lens (\CreateCompilationJob' {inputConfig} -> inputConfig) (\s@CreateCompilationJob' {} a -> s {inputConfig = a} :: CreateCompilationJob)

-- | Provides information about the output location for the compiled model
-- and the target device the model runs on.
createCompilationJob_outputConfig :: Lens.Lens' CreateCompilationJob OutputConfig
createCompilationJob_outputConfig = Lens.lens (\CreateCompilationJob' {outputConfig} -> outputConfig) (\s@CreateCompilationJob' {} a -> s {outputConfig = a} :: CreateCompilationJob)

-- | Specifies a limit to how long a model compilation job can run. When the
-- job reaches the time limit, Amazon SageMaker ends the compilation job.
-- Use this API to cap model training costs.
createCompilationJob_stoppingCondition :: Lens.Lens' CreateCompilationJob StoppingCondition
createCompilationJob_stoppingCondition = Lens.lens (\CreateCompilationJob' {stoppingCondition} -> stoppingCondition) (\s@CreateCompilationJob' {} a -> s {stoppingCondition = a} :: CreateCompilationJob)

instance Prelude.AWSRequest CreateCompilationJob where
  type
    Rs CreateCompilationJob =
      CreateCompilationJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCompilationJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "CompilationJobArn")
      )

instance Prelude.Hashable CreateCompilationJob

instance Prelude.NFData CreateCompilationJob

instance Prelude.ToHeaders CreateCompilationJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.CreateCompilationJob" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateCompilationJob where
  toJSON CreateCompilationJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Tags" Prelude..=) Prelude.<$> tags,
            Prelude.Just
              ("CompilationJobName" Prelude..= compilationJobName),
            Prelude.Just ("RoleArn" Prelude..= roleArn),
            Prelude.Just ("InputConfig" Prelude..= inputConfig),
            Prelude.Just
              ("OutputConfig" Prelude..= outputConfig),
            Prelude.Just
              ("StoppingCondition" Prelude..= stoppingCondition)
          ]
      )

instance Prelude.ToPath CreateCompilationJob where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateCompilationJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCompilationJobResponse' smart constructor.
data CreateCompilationJobResponse = CreateCompilationJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | If the action is successful, the service sends back an HTTP 200
    -- response. Amazon SageMaker returns the following data in JSON format:
    --
    -- -   @CompilationJobArn@: The Amazon Resource Name (ARN) of the compiled
    --     job.
    compilationJobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateCompilationJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createCompilationJobResponse_httpStatus' - The response's http status code.
--
-- 'compilationJobArn', 'createCompilationJobResponse_compilationJobArn' - If the action is successful, the service sends back an HTTP 200
-- response. Amazon SageMaker returns the following data in JSON format:
--
-- -   @CompilationJobArn@: The Amazon Resource Name (ARN) of the compiled
--     job.
newCreateCompilationJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'compilationJobArn'
  Prelude.Text ->
  CreateCompilationJobResponse
newCreateCompilationJobResponse
  pHttpStatus_
  pCompilationJobArn_ =
    CreateCompilationJobResponse'
      { httpStatus =
          pHttpStatus_,
        compilationJobArn = pCompilationJobArn_
      }

-- | The response's http status code.
createCompilationJobResponse_httpStatus :: Lens.Lens' CreateCompilationJobResponse Prelude.Int
createCompilationJobResponse_httpStatus = Lens.lens (\CreateCompilationJobResponse' {httpStatus} -> httpStatus) (\s@CreateCompilationJobResponse' {} a -> s {httpStatus = a} :: CreateCompilationJobResponse)

-- | If the action is successful, the service sends back an HTTP 200
-- response. Amazon SageMaker returns the following data in JSON format:
--
-- -   @CompilationJobArn@: The Amazon Resource Name (ARN) of the compiled
--     job.
createCompilationJobResponse_compilationJobArn :: Lens.Lens' CreateCompilationJobResponse Prelude.Text
createCompilationJobResponse_compilationJobArn = Lens.lens (\CreateCompilationJobResponse' {compilationJobArn} -> compilationJobArn) (\s@CreateCompilationJobResponse' {} a -> s {compilationJobArn = a} :: CreateCompilationJobResponse)

instance Prelude.NFData CreateCompilationJobResponse
