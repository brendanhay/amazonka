{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateCompilationJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a model compilation job. After the model has been compiled, Amazon SageMaker saves the resulting model artifacts to an Amazon Simple Storage Service (Amazon S3) bucket that you specify.
--
-- If you choose to host your model using Amazon SageMaker hosting services, you can use the resulting model artifacts as part of the model. You can also use the artifacts with AWS IoT Greengrass. In that case, deploy them as an ML resource.
-- In the request body, you provide the following:
--
--     * A name for the compilation job
--
--
--     * Information about the input model artifacts
--
--
--     * The output location for the compiled model and the device (target) that the model runs on
--
--
--     * The Amazon Resource Name (ARN) of the IAM role that Amazon SageMaker assumes to perform the model compilation job.
--
--
-- You can also provide a @Tag@ to track the model compilation job's resource use and costs. The response body contains the @CompilationJobArn@ for the compiled job.
-- To stop a model compilation job, use 'StopCompilationJob' . To get information about a particular model compilation job, use 'DescribeCompilationJob' . To get information about multiple model compilation jobs, use 'ListCompilationJobs' .
module Network.AWS.SageMaker.CreateCompilationJob
  ( -- * Creating a request
    CreateCompilationJob (..),
    mkCreateCompilationJob,

    -- ** Request lenses
    ccjStoppingCondition,
    ccjCompilationJobName,
    ccjInputConfig,
    ccjOutputConfig,
    ccjTags,
    ccjRoleARN,

    -- * Destructuring the response
    CreateCompilationJobResponse (..),
    mkCreateCompilationJobResponse,

    -- ** Response lenses
    ccjrsCompilationJobARN,
    ccjrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateCompilationJob' smart constructor.
data CreateCompilationJob = CreateCompilationJob'
  { -- | Specifies a limit to how long a model compilation job can run. When the job reaches the time limit, Amazon SageMaker ends the compilation job. Use this API to cap model training costs.
    stoppingCondition :: StoppingCondition,
    -- | A name for the model compilation job. The name must be unique within the AWS Region and within your AWS account.
    compilationJobName :: Lude.Text,
    -- | Provides information about the location of input model artifacts, the name and shape of the expected data inputs, and the framework in which the model was trained.
    inputConfig :: InputConfig,
    -- | Provides information about the output location for the compiled model and the target device the model runs on.
    outputConfig :: OutputConfig,
    -- | An array of key-value pairs that you want to use to organize and track your AWS resource costs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
    tags :: Lude.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) of an IAM role that enables Amazon SageMaker to perform tasks on your behalf.
    --
    -- During model compilation, Amazon SageMaker needs your permission to:
    --
    --     * Read input data from an S3 bucket
    --
    --
    --     * Write model artifacts to an S3 bucket
    --
    --
    --     * Write logs to Amazon CloudWatch Logs
    --
    --
    --     * Publish metrics to Amazon CloudWatch
    --
    --
    -- You grant permissions for all of these tasks to an IAM role. To pass this role to Amazon SageMaker, the caller of this API must have the @iam:PassRole@ permission. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles.>
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCompilationJob' with the minimum fields required to make a request.
--
-- * 'stoppingCondition' - Specifies a limit to how long a model compilation job can run. When the job reaches the time limit, Amazon SageMaker ends the compilation job. Use this API to cap model training costs.
-- * 'compilationJobName' - A name for the model compilation job. The name must be unique within the AWS Region and within your AWS account.
-- * 'inputConfig' - Provides information about the location of input model artifacts, the name and shape of the expected data inputs, and the framework in which the model was trained.
-- * 'outputConfig' - Provides information about the output location for the compiled model and the target device the model runs on.
-- * 'tags' - An array of key-value pairs that you want to use to organize and track your AWS resource costs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
-- * 'roleARN' - The Amazon Resource Name (ARN) of an IAM role that enables Amazon SageMaker to perform tasks on your behalf.
--
-- During model compilation, Amazon SageMaker needs your permission to:
--
--     * Read input data from an S3 bucket
--
--
--     * Write model artifacts to an S3 bucket
--
--
--     * Write logs to Amazon CloudWatch Logs
--
--
--     * Publish metrics to Amazon CloudWatch
--
--
-- You grant permissions for all of these tasks to an IAM role. To pass this role to Amazon SageMaker, the caller of this API must have the @iam:PassRole@ permission. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles.>
mkCreateCompilationJob ::
  -- | 'stoppingCondition'
  StoppingCondition ->
  -- | 'compilationJobName'
  Lude.Text ->
  -- | 'inputConfig'
  InputConfig ->
  -- | 'outputConfig'
  OutputConfig ->
  -- | 'roleARN'
  Lude.Text ->
  CreateCompilationJob
mkCreateCompilationJob
  pStoppingCondition_
  pCompilationJobName_
  pInputConfig_
  pOutputConfig_
  pRoleARN_ =
    CreateCompilationJob'
      { stoppingCondition = pStoppingCondition_,
        compilationJobName = pCompilationJobName_,
        inputConfig = pInputConfig_,
        outputConfig = pOutputConfig_,
        tags = Lude.Nothing,
        roleARN = pRoleARN_
      }

-- | Specifies a limit to how long a model compilation job can run. When the job reaches the time limit, Amazon SageMaker ends the compilation job. Use this API to cap model training costs.
--
-- /Note:/ Consider using 'stoppingCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccjStoppingCondition :: Lens.Lens' CreateCompilationJob StoppingCondition
ccjStoppingCondition = Lens.lens (stoppingCondition :: CreateCompilationJob -> StoppingCondition) (\s a -> s {stoppingCondition = a} :: CreateCompilationJob)
{-# DEPRECATED ccjStoppingCondition "Use generic-lens or generic-optics with 'stoppingCondition' instead." #-}

-- | A name for the model compilation job. The name must be unique within the AWS Region and within your AWS account.
--
-- /Note:/ Consider using 'compilationJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccjCompilationJobName :: Lens.Lens' CreateCompilationJob Lude.Text
ccjCompilationJobName = Lens.lens (compilationJobName :: CreateCompilationJob -> Lude.Text) (\s a -> s {compilationJobName = a} :: CreateCompilationJob)
{-# DEPRECATED ccjCompilationJobName "Use generic-lens or generic-optics with 'compilationJobName' instead." #-}

-- | Provides information about the location of input model artifacts, the name and shape of the expected data inputs, and the framework in which the model was trained.
--
-- /Note:/ Consider using 'inputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccjInputConfig :: Lens.Lens' CreateCompilationJob InputConfig
ccjInputConfig = Lens.lens (inputConfig :: CreateCompilationJob -> InputConfig) (\s a -> s {inputConfig = a} :: CreateCompilationJob)
{-# DEPRECATED ccjInputConfig "Use generic-lens or generic-optics with 'inputConfig' instead." #-}

-- | Provides information about the output location for the compiled model and the target device the model runs on.
--
-- /Note:/ Consider using 'outputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccjOutputConfig :: Lens.Lens' CreateCompilationJob OutputConfig
ccjOutputConfig = Lens.lens (outputConfig :: CreateCompilationJob -> OutputConfig) (\s a -> s {outputConfig = a} :: CreateCompilationJob)
{-# DEPRECATED ccjOutputConfig "Use generic-lens or generic-optics with 'outputConfig' instead." #-}

-- | An array of key-value pairs that you want to use to organize and track your AWS resource costs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccjTags :: Lens.Lens' CreateCompilationJob (Lude.Maybe [Tag])
ccjTags = Lens.lens (tags :: CreateCompilationJob -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateCompilationJob)
{-# DEPRECATED ccjTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The Amazon Resource Name (ARN) of an IAM role that enables Amazon SageMaker to perform tasks on your behalf.
--
-- During model compilation, Amazon SageMaker needs your permission to:
--
--     * Read input data from an S3 bucket
--
--
--     * Write model artifacts to an S3 bucket
--
--
--     * Write logs to Amazon CloudWatch Logs
--
--
--     * Publish metrics to Amazon CloudWatch
--
--
-- You grant permissions for all of these tasks to an IAM role. To pass this role to Amazon SageMaker, the caller of this API must have the @iam:PassRole@ permission. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles.>
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccjRoleARN :: Lens.Lens' CreateCompilationJob Lude.Text
ccjRoleARN = Lens.lens (roleARN :: CreateCompilationJob -> Lude.Text) (\s a -> s {roleARN = a} :: CreateCompilationJob)
{-# DEPRECATED ccjRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest CreateCompilationJob where
  type Rs CreateCompilationJob = CreateCompilationJobResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateCompilationJobResponse'
            Lude.<$> (x Lude..:> "CompilationJobArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCompilationJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.CreateCompilationJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateCompilationJob where
  toJSON CreateCompilationJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("StoppingCondition" Lude..= stoppingCondition),
            Lude.Just ("CompilationJobName" Lude..= compilationJobName),
            Lude.Just ("InputConfig" Lude..= inputConfig),
            Lude.Just ("OutputConfig" Lude..= outputConfig),
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("RoleArn" Lude..= roleARN)
          ]
      )

instance Lude.ToPath CreateCompilationJob where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCompilationJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateCompilationJobResponse' smart constructor.
data CreateCompilationJobResponse = CreateCompilationJobResponse'
  { -- | If the action is successful, the service sends back an HTTP 200 response. Amazon SageMaker returns the following data in JSON format:
    --
    --
    --     * @CompilationJobArn@ : The Amazon Resource Name (ARN) of the compiled job.
    compilationJobARN :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCompilationJobResponse' with the minimum fields required to make a request.
--
-- * 'compilationJobARN' - If the action is successful, the service sends back an HTTP 200 response. Amazon SageMaker returns the following data in JSON format:
--
--
--     * @CompilationJobArn@ : The Amazon Resource Name (ARN) of the compiled job.
--
--
-- * 'responseStatus' - The response status code.
mkCreateCompilationJobResponse ::
  -- | 'compilationJobARN'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateCompilationJobResponse
mkCreateCompilationJobResponse pCompilationJobARN_ pResponseStatus_ =
  CreateCompilationJobResponse'
    { compilationJobARN =
        pCompilationJobARN_,
      responseStatus = pResponseStatus_
    }

-- | If the action is successful, the service sends back an HTTP 200 response. Amazon SageMaker returns the following data in JSON format:
--
--
--     * @CompilationJobArn@ : The Amazon Resource Name (ARN) of the compiled job.
--
--
--
-- /Note:/ Consider using 'compilationJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccjrsCompilationJobARN :: Lens.Lens' CreateCompilationJobResponse Lude.Text
ccjrsCompilationJobARN = Lens.lens (compilationJobARN :: CreateCompilationJobResponse -> Lude.Text) (\s a -> s {compilationJobARN = a} :: CreateCompilationJobResponse)
{-# DEPRECATED ccjrsCompilationJobARN "Use generic-lens or generic-optics with 'compilationJobARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccjrsResponseStatus :: Lens.Lens' CreateCompilationJobResponse Lude.Int
ccjrsResponseStatus = Lens.lens (responseStatus :: CreateCompilationJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCompilationJobResponse)
{-# DEPRECATED ccjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
