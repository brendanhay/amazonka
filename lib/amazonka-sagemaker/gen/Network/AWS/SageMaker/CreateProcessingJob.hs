{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateProcessingJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a processing job.
module Network.AWS.SageMaker.CreateProcessingJob
  ( -- * Creating a request
    CreateProcessingJob (..),
    mkCreateProcessingJob,

    -- ** Request lenses
    cpjEnvironment,
    cpjStoppingCondition,
    cpjExperimentConfig,
    cpjProcessingInputs,
    cpjNetworkConfig,
    cpjProcessingOutputConfig,
    cpjTags,
    cpjProcessingJobName,
    cpjProcessingResources,
    cpjAppSpecification,
    cpjRoleARN,

    -- * Destructuring the response
    CreateProcessingJobResponse (..),
    mkCreateProcessingJobResponse,

    -- ** Response lenses
    cpjrsResponseStatus,
    cpjrsProcessingJobARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateProcessingJob' smart constructor.
data CreateProcessingJob = CreateProcessingJob'
  { environment ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    stoppingCondition ::
      Lude.Maybe ProcessingStoppingCondition,
    experimentConfig :: Lude.Maybe ExperimentConfig,
    processingInputs :: Lude.Maybe [ProcessingInput],
    networkConfig :: Lude.Maybe NetworkConfig,
    processingOutputConfig ::
      Lude.Maybe ProcessingOutputConfig,
    tags :: Lude.Maybe [Tag],
    processingJobName :: Lude.Text,
    processingResources :: ProcessingResources,
    appSpecification :: AppSpecification,
    roleARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateProcessingJob' with the minimum fields required to make a request.
--
-- * 'appSpecification' - Configures the processing job to run a specified Docker container image.
-- * 'environment' - Sets the environment variables in the Docker container.
-- * 'experimentConfig' - Undocumented field.
-- * 'networkConfig' - Networking options for a processing job.
-- * 'processingInputs' - For each input, data is downloaded from S3 into the processing container before the processing job begins running if "S3InputMode" is set to @File@ .
-- * 'processingJobName' - The name of the processing job. The name must be unique within an AWS Region in the AWS account.
-- * 'processingOutputConfig' - Output configuration for the processing job.
-- * 'processingResources' - Identifies the resources, ML compute instances, and ML storage volumes to deploy for a processing job. In distributed training, you specify more than one instance.
-- * 'roleARN' - The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
-- * 'stoppingCondition' - The time limit for how long the processing job is allowed to run.
-- * 'tags' - (Optional) An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
mkCreateProcessingJob ::
  -- | 'processingJobName'
  Lude.Text ->
  -- | 'processingResources'
  ProcessingResources ->
  -- | 'appSpecification'
  AppSpecification ->
  -- | 'roleARN'
  Lude.Text ->
  CreateProcessingJob
mkCreateProcessingJob
  pProcessingJobName_
  pProcessingResources_
  pAppSpecification_
  pRoleARN_ =
    CreateProcessingJob'
      { environment = Lude.Nothing,
        stoppingCondition = Lude.Nothing,
        experimentConfig = Lude.Nothing,
        processingInputs = Lude.Nothing,
        networkConfig = Lude.Nothing,
        processingOutputConfig = Lude.Nothing,
        tags = Lude.Nothing,
        processingJobName = pProcessingJobName_,
        processingResources = pProcessingResources_,
        appSpecification = pAppSpecification_,
        roleARN = pRoleARN_
      }

-- | Sets the environment variables in the Docker container.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjEnvironment :: Lens.Lens' CreateProcessingJob (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cpjEnvironment = Lens.lens (environment :: CreateProcessingJob -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {environment = a} :: CreateProcessingJob)
{-# DEPRECATED cpjEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | The time limit for how long the processing job is allowed to run.
--
-- /Note:/ Consider using 'stoppingCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjStoppingCondition :: Lens.Lens' CreateProcessingJob (Lude.Maybe ProcessingStoppingCondition)
cpjStoppingCondition = Lens.lens (stoppingCondition :: CreateProcessingJob -> Lude.Maybe ProcessingStoppingCondition) (\s a -> s {stoppingCondition = a} :: CreateProcessingJob)
{-# DEPRECATED cpjStoppingCondition "Use generic-lens or generic-optics with 'stoppingCondition' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'experimentConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjExperimentConfig :: Lens.Lens' CreateProcessingJob (Lude.Maybe ExperimentConfig)
cpjExperimentConfig = Lens.lens (experimentConfig :: CreateProcessingJob -> Lude.Maybe ExperimentConfig) (\s a -> s {experimentConfig = a} :: CreateProcessingJob)
{-# DEPRECATED cpjExperimentConfig "Use generic-lens or generic-optics with 'experimentConfig' instead." #-}

-- | For each input, data is downloaded from S3 into the processing container before the processing job begins running if "S3InputMode" is set to @File@ .
--
-- /Note:/ Consider using 'processingInputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjProcessingInputs :: Lens.Lens' CreateProcessingJob (Lude.Maybe [ProcessingInput])
cpjProcessingInputs = Lens.lens (processingInputs :: CreateProcessingJob -> Lude.Maybe [ProcessingInput]) (\s a -> s {processingInputs = a} :: CreateProcessingJob)
{-# DEPRECATED cpjProcessingInputs "Use generic-lens or generic-optics with 'processingInputs' instead." #-}

-- | Networking options for a processing job.
--
-- /Note:/ Consider using 'networkConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjNetworkConfig :: Lens.Lens' CreateProcessingJob (Lude.Maybe NetworkConfig)
cpjNetworkConfig = Lens.lens (networkConfig :: CreateProcessingJob -> Lude.Maybe NetworkConfig) (\s a -> s {networkConfig = a} :: CreateProcessingJob)
{-# DEPRECATED cpjNetworkConfig "Use generic-lens or generic-optics with 'networkConfig' instead." #-}

-- | Output configuration for the processing job.
--
-- /Note:/ Consider using 'processingOutputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjProcessingOutputConfig :: Lens.Lens' CreateProcessingJob (Lude.Maybe ProcessingOutputConfig)
cpjProcessingOutputConfig = Lens.lens (processingOutputConfig :: CreateProcessingJob -> Lude.Maybe ProcessingOutputConfig) (\s a -> s {processingOutputConfig = a} :: CreateProcessingJob)
{-# DEPRECATED cpjProcessingOutputConfig "Use generic-lens or generic-optics with 'processingOutputConfig' instead." #-}

-- | (Optional) An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjTags :: Lens.Lens' CreateProcessingJob (Lude.Maybe [Tag])
cpjTags = Lens.lens (tags :: CreateProcessingJob -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateProcessingJob)
{-# DEPRECATED cpjTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the processing job. The name must be unique within an AWS Region in the AWS account.
--
-- /Note:/ Consider using 'processingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjProcessingJobName :: Lens.Lens' CreateProcessingJob Lude.Text
cpjProcessingJobName = Lens.lens (processingJobName :: CreateProcessingJob -> Lude.Text) (\s a -> s {processingJobName = a} :: CreateProcessingJob)
{-# DEPRECATED cpjProcessingJobName "Use generic-lens or generic-optics with 'processingJobName' instead." #-}

-- | Identifies the resources, ML compute instances, and ML storage volumes to deploy for a processing job. In distributed training, you specify more than one instance.
--
-- /Note:/ Consider using 'processingResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjProcessingResources :: Lens.Lens' CreateProcessingJob ProcessingResources
cpjProcessingResources = Lens.lens (processingResources :: CreateProcessingJob -> ProcessingResources) (\s a -> s {processingResources = a} :: CreateProcessingJob)
{-# DEPRECATED cpjProcessingResources "Use generic-lens or generic-optics with 'processingResources' instead." #-}

-- | Configures the processing job to run a specified Docker container image.
--
-- /Note:/ Consider using 'appSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjAppSpecification :: Lens.Lens' CreateProcessingJob AppSpecification
cpjAppSpecification = Lens.lens (appSpecification :: CreateProcessingJob -> AppSpecification) (\s a -> s {appSpecification = a} :: CreateProcessingJob)
{-# DEPRECATED cpjAppSpecification "Use generic-lens or generic-optics with 'appSpecification' instead." #-}

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjRoleARN :: Lens.Lens' CreateProcessingJob Lude.Text
cpjRoleARN = Lens.lens (roleARN :: CreateProcessingJob -> Lude.Text) (\s a -> s {roleARN = a} :: CreateProcessingJob)
{-# DEPRECATED cpjRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest CreateProcessingJob where
  type Rs CreateProcessingJob = CreateProcessingJobResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateProcessingJobResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "ProcessingJobArn")
      )

instance Lude.ToHeaders CreateProcessingJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.CreateProcessingJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateProcessingJob where
  toJSON CreateProcessingJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Environment" Lude..=) Lude.<$> environment,
            ("StoppingCondition" Lude..=) Lude.<$> stoppingCondition,
            ("ExperimentConfig" Lude..=) Lude.<$> experimentConfig,
            ("ProcessingInputs" Lude..=) Lude.<$> processingInputs,
            ("NetworkConfig" Lude..=) Lude.<$> networkConfig,
            ("ProcessingOutputConfig" Lude..=) Lude.<$> processingOutputConfig,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("ProcessingJobName" Lude..= processingJobName),
            Lude.Just ("ProcessingResources" Lude..= processingResources),
            Lude.Just ("AppSpecification" Lude..= appSpecification),
            Lude.Just ("RoleArn" Lude..= roleARN)
          ]
      )

instance Lude.ToPath CreateProcessingJob where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateProcessingJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateProcessingJobResponse' smart constructor.
data CreateProcessingJobResponse = CreateProcessingJobResponse'
  { responseStatus ::
      Lude.Int,
    processingJobARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateProcessingJobResponse' with the minimum fields required to make a request.
--
-- * 'processingJobARN' - The Amazon Resource Name (ARN) of the processing job.
-- * 'responseStatus' - The response status code.
mkCreateProcessingJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'processingJobARN'
  Lude.Text ->
  CreateProcessingJobResponse
mkCreateProcessingJobResponse pResponseStatus_ pProcessingJobARN_ =
  CreateProcessingJobResponse'
    { responseStatus = pResponseStatus_,
      processingJobARN = pProcessingJobARN_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjrsResponseStatus :: Lens.Lens' CreateProcessingJobResponse Lude.Int
cpjrsResponseStatus = Lens.lens (responseStatus :: CreateProcessingJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateProcessingJobResponse)
{-# DEPRECATED cpjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The Amazon Resource Name (ARN) of the processing job.
--
-- /Note:/ Consider using 'processingJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjrsProcessingJobARN :: Lens.Lens' CreateProcessingJobResponse Lude.Text
cpjrsProcessingJobARN = Lens.lens (processingJobARN :: CreateProcessingJobResponse -> Lude.Text) (\s a -> s {processingJobARN = a} :: CreateProcessingJobResponse)
{-# DEPRECATED cpjrsProcessingJobARN "Use generic-lens or generic-optics with 'processingJobARN' instead." #-}
