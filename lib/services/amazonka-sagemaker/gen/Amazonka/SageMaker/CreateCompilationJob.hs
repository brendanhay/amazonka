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
-- Module      : Amazonka.SageMaker.CreateCompilationJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a model compilation job. After the model has been compiled,
-- Amazon SageMaker saves the resulting model artifacts to an Amazon Simple
-- Storage Service (Amazon S3) bucket that you specify.
--
-- If you choose to host your model using Amazon SageMaker hosting
-- services, you can use the resulting model artifacts as part of the
-- model. You can also use the artifacts with Amazon Web Services IoT
-- Greengrass. In that case, deploy them as an ML resource.
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
module Amazonka.SageMaker.CreateCompilationJob
  ( -- * Creating a Request
    CreateCompilationJob (..),
    newCreateCompilationJob,

    -- * Request Lenses
    createCompilationJob_inputConfig,
    createCompilationJob_modelPackageVersionArn,
    createCompilationJob_tags,
    createCompilationJob_vpcConfig,
    createCompilationJob_compilationJobName,
    createCompilationJob_roleArn,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateCompilationJob' smart constructor.
data CreateCompilationJob = CreateCompilationJob'
  { -- | Provides information about the location of input model artifacts, the
    -- name and shape of the expected data inputs, and the framework in which
    -- the model was trained.
    inputConfig :: Prelude.Maybe InputConfig,
    -- | The Amazon Resource Name (ARN) of a versioned model package. Provide
    -- either a @ModelPackageVersionArn@ or an @InputConfig@ object in the
    -- request syntax. The presence of both objects in the
    -- @CreateCompilationJob@ request will return an exception.
    modelPackageVersionArn :: Prelude.Maybe Prelude.Text,
    -- | An array of key-value pairs. You can use tags to categorize your Amazon
    -- Web Services resources in different ways, for example, by purpose,
    -- owner, or environment. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
    tags :: Prelude.Maybe [Tag],
    -- | A VpcConfig object that specifies the VPC that you want your compilation
    -- job to connect to. Control access to your models by configuring the VPC.
    -- For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/neo-vpc.html Protect Compilation Jobs by Using an Amazon Virtual Private Cloud>.
    vpcConfig :: Prelude.Maybe NeoVpcConfig,
    -- | A name for the model compilation job. The name must be unique within the
    -- Amazon Web Services Region and within your Amazon Web Services account.
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
    -- | Provides information about the output location for the compiled model
    -- and the target device the model runs on.
    outputConfig :: OutputConfig,
    -- | Specifies a limit to how long a model compilation job can run. When the
    -- job reaches the time limit, Amazon SageMaker ends the compilation job.
    -- Use this API to cap model training costs.
    stoppingCondition :: StoppingCondition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCompilationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputConfig', 'createCompilationJob_inputConfig' - Provides information about the location of input model artifacts, the
-- name and shape of the expected data inputs, and the framework in which
-- the model was trained.
--
-- 'modelPackageVersionArn', 'createCompilationJob_modelPackageVersionArn' - The Amazon Resource Name (ARN) of a versioned model package. Provide
-- either a @ModelPackageVersionArn@ or an @InputConfig@ object in the
-- request syntax. The presence of both objects in the
-- @CreateCompilationJob@ request will return an exception.
--
-- 'tags', 'createCompilationJob_tags' - An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, for example, by purpose,
-- owner, or environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
--
-- 'vpcConfig', 'createCompilationJob_vpcConfig' - A VpcConfig object that specifies the VPC that you want your compilation
-- job to connect to. Control access to your models by configuring the VPC.
-- For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/neo-vpc.html Protect Compilation Jobs by Using an Amazon Virtual Private Cloud>.
--
-- 'compilationJobName', 'createCompilationJob_compilationJobName' - A name for the model compilation job. The name must be unique within the
-- Amazon Web Services Region and within your Amazon Web Services account.
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
  -- | 'outputConfig'
  OutputConfig ->
  -- | 'stoppingCondition'
  StoppingCondition ->
  CreateCompilationJob
newCreateCompilationJob
  pCompilationJobName_
  pRoleArn_
  pOutputConfig_
  pStoppingCondition_ =
    CreateCompilationJob'
      { inputConfig =
          Prelude.Nothing,
        modelPackageVersionArn = Prelude.Nothing,
        tags = Prelude.Nothing,
        vpcConfig = Prelude.Nothing,
        compilationJobName = pCompilationJobName_,
        roleArn = pRoleArn_,
        outputConfig = pOutputConfig_,
        stoppingCondition = pStoppingCondition_
      }

-- | Provides information about the location of input model artifacts, the
-- name and shape of the expected data inputs, and the framework in which
-- the model was trained.
createCompilationJob_inputConfig :: Lens.Lens' CreateCompilationJob (Prelude.Maybe InputConfig)
createCompilationJob_inputConfig = Lens.lens (\CreateCompilationJob' {inputConfig} -> inputConfig) (\s@CreateCompilationJob' {} a -> s {inputConfig = a} :: CreateCompilationJob)

-- | The Amazon Resource Name (ARN) of a versioned model package. Provide
-- either a @ModelPackageVersionArn@ or an @InputConfig@ object in the
-- request syntax. The presence of both objects in the
-- @CreateCompilationJob@ request will return an exception.
createCompilationJob_modelPackageVersionArn :: Lens.Lens' CreateCompilationJob (Prelude.Maybe Prelude.Text)
createCompilationJob_modelPackageVersionArn = Lens.lens (\CreateCompilationJob' {modelPackageVersionArn} -> modelPackageVersionArn) (\s@CreateCompilationJob' {} a -> s {modelPackageVersionArn = a} :: CreateCompilationJob)

-- | An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, for example, by purpose,
-- owner, or environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
createCompilationJob_tags :: Lens.Lens' CreateCompilationJob (Prelude.Maybe [Tag])
createCompilationJob_tags = Lens.lens (\CreateCompilationJob' {tags} -> tags) (\s@CreateCompilationJob' {} a -> s {tags = a} :: CreateCompilationJob) Prelude.. Lens.mapping Lens.coerced

-- | A VpcConfig object that specifies the VPC that you want your compilation
-- job to connect to. Control access to your models by configuring the VPC.
-- For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/neo-vpc.html Protect Compilation Jobs by Using an Amazon Virtual Private Cloud>.
createCompilationJob_vpcConfig :: Lens.Lens' CreateCompilationJob (Prelude.Maybe NeoVpcConfig)
createCompilationJob_vpcConfig = Lens.lens (\CreateCompilationJob' {vpcConfig} -> vpcConfig) (\s@CreateCompilationJob' {} a -> s {vpcConfig = a} :: CreateCompilationJob)

-- | A name for the model compilation job. The name must be unique within the
-- Amazon Web Services Region and within your Amazon Web Services account.
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

-- | Provides information about the output location for the compiled model
-- and the target device the model runs on.
createCompilationJob_outputConfig :: Lens.Lens' CreateCompilationJob OutputConfig
createCompilationJob_outputConfig = Lens.lens (\CreateCompilationJob' {outputConfig} -> outputConfig) (\s@CreateCompilationJob' {} a -> s {outputConfig = a} :: CreateCompilationJob)

-- | Specifies a limit to how long a model compilation job can run. When the
-- job reaches the time limit, Amazon SageMaker ends the compilation job.
-- Use this API to cap model training costs.
createCompilationJob_stoppingCondition :: Lens.Lens' CreateCompilationJob StoppingCondition
createCompilationJob_stoppingCondition = Lens.lens (\CreateCompilationJob' {stoppingCondition} -> stoppingCondition) (\s@CreateCompilationJob' {} a -> s {stoppingCondition = a} :: CreateCompilationJob)

instance Core.AWSRequest CreateCompilationJob where
  type
    AWSResponse CreateCompilationJob =
      CreateCompilationJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCompilationJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "CompilationJobArn")
      )

instance Prelude.Hashable CreateCompilationJob where
  hashWithSalt _salt CreateCompilationJob' {..} =
    _salt
      `Prelude.hashWithSalt` inputConfig
      `Prelude.hashWithSalt` modelPackageVersionArn
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` compilationJobName
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` outputConfig
      `Prelude.hashWithSalt` stoppingCondition

instance Prelude.NFData CreateCompilationJob where
  rnf CreateCompilationJob' {..} =
    Prelude.rnf inputConfig
      `Prelude.seq` Prelude.rnf modelPackageVersionArn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf compilationJobName
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf outputConfig
      `Prelude.seq` Prelude.rnf stoppingCondition

instance Data.ToHeaders CreateCompilationJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.CreateCompilationJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCompilationJob where
  toJSON CreateCompilationJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InputConfig" Data..=) Prelude.<$> inputConfig,
            ("ModelPackageVersionArn" Data..=)
              Prelude.<$> modelPackageVersionArn,
            ("Tags" Data..=) Prelude.<$> tags,
            ("VpcConfig" Data..=) Prelude.<$> vpcConfig,
            Prelude.Just
              ("CompilationJobName" Data..= compilationJobName),
            Prelude.Just ("RoleArn" Data..= roleArn),
            Prelude.Just ("OutputConfig" Data..= outputConfig),
            Prelude.Just
              ("StoppingCondition" Data..= stoppingCondition)
          ]
      )

instance Data.ToPath CreateCompilationJob where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateCompilationJob where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CreateCompilationJobResponse where
  rnf CreateCompilationJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf compilationJobArn
