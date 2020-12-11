{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    ccjTags,
    ccjCompilationJobName,
    ccjRoleARN,
    ccjInputConfig,
    ccjOutputConfig,
    ccjStoppingCondition,

    -- * Destructuring the response
    CreateCompilationJobResponse (..),
    mkCreateCompilationJobResponse,

    -- ** Response lenses
    ccjrsResponseStatus,
    ccjrsCompilationJobARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateCompilationJob' smart constructor.
data CreateCompilationJob = CreateCompilationJob'
  { tags ::
      Lude.Maybe [Tag],
    compilationJobName :: Lude.Text,
    roleARN :: Lude.Text,
    inputConfig :: InputConfig,
    outputConfig :: OutputConfig,
    stoppingCondition :: StoppingCondition
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCompilationJob' with the minimum fields required to make a request.
--
-- * 'compilationJobName' - A name for the model compilation job. The name must be unique within the AWS Region and within your AWS account.
-- * 'inputConfig' - Provides information about the location of input model artifacts, the name and shape of the expected data inputs, and the framework in which the model was trained.
-- * 'outputConfig' - Provides information about the output location for the compiled model and the target device the model runs on.
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
-- * 'stoppingCondition' - Specifies a limit to how long a model compilation job can run. When the job reaches the time limit, Amazon SageMaker ends the compilation job. Use this API to cap model training costs.
-- * 'tags' - An array of key-value pairs that you want to use to organize and track your AWS resource costs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
mkCreateCompilationJob ::
  -- | 'compilationJobName'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  -- | 'inputConfig'
  InputConfig ->
  -- | 'outputConfig'
  OutputConfig ->
  -- | 'stoppingCondition'
  StoppingCondition ->
  CreateCompilationJob
mkCreateCompilationJob
  pCompilationJobName_
  pRoleARN_
  pInputConfig_
  pOutputConfig_
  pStoppingCondition_ =
    CreateCompilationJob'
      { tags = Lude.Nothing,
        compilationJobName = pCompilationJobName_,
        roleARN = pRoleARN_,
        inputConfig = pInputConfig_,
        outputConfig = pOutputConfig_,
        stoppingCondition = pStoppingCondition_
      }

-- | An array of key-value pairs that you want to use to organize and track your AWS resource costs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccjTags :: Lens.Lens' CreateCompilationJob (Lude.Maybe [Tag])
ccjTags = Lens.lens (tags :: CreateCompilationJob -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateCompilationJob)
{-# DEPRECATED ccjTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A name for the model compilation job. The name must be unique within the AWS Region and within your AWS account.
--
-- /Note:/ Consider using 'compilationJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccjCompilationJobName :: Lens.Lens' CreateCompilationJob Lude.Text
ccjCompilationJobName = Lens.lens (compilationJobName :: CreateCompilationJob -> Lude.Text) (\s a -> s {compilationJobName = a} :: CreateCompilationJob)
{-# DEPRECATED ccjCompilationJobName "Use generic-lens or generic-optics with 'compilationJobName' instead." #-}

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

-- | Specifies a limit to how long a model compilation job can run. When the job reaches the time limit, Amazon SageMaker ends the compilation job. Use this API to cap model training costs.
--
-- /Note:/ Consider using 'stoppingCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccjStoppingCondition :: Lens.Lens' CreateCompilationJob StoppingCondition
ccjStoppingCondition = Lens.lens (stoppingCondition :: CreateCompilationJob -> StoppingCondition) (\s a -> s {stoppingCondition = a} :: CreateCompilationJob)
{-# DEPRECATED ccjStoppingCondition "Use generic-lens or generic-optics with 'stoppingCondition' instead." #-}

instance Lude.AWSRequest CreateCompilationJob where
  type Rs CreateCompilationJob = CreateCompilationJobResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateCompilationJobResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "CompilationJobArn")
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
          [ ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("CompilationJobName" Lude..= compilationJobName),
            Lude.Just ("RoleArn" Lude..= roleARN),
            Lude.Just ("InputConfig" Lude..= inputConfig),
            Lude.Just ("OutputConfig" Lude..= outputConfig),
            Lude.Just ("StoppingCondition" Lude..= stoppingCondition)
          ]
      )

instance Lude.ToPath CreateCompilationJob where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCompilationJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateCompilationJobResponse' smart constructor.
data CreateCompilationJobResponse = CreateCompilationJobResponse'
  { responseStatus ::
      Lude.Int,
    compilationJobARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
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
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'compilationJobARN'
  Lude.Text ->
  CreateCompilationJobResponse
mkCreateCompilationJobResponse pResponseStatus_ pCompilationJobARN_ =
  CreateCompilationJobResponse'
    { responseStatus = pResponseStatus_,
      compilationJobARN = pCompilationJobARN_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccjrsResponseStatus :: Lens.Lens' CreateCompilationJobResponse Lude.Int
ccjrsResponseStatus = Lens.lens (responseStatus :: CreateCompilationJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCompilationJobResponse)
{-# DEPRECATED ccjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

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
