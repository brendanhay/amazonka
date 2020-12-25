{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateTrainingJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a model training job. After training completes, Amazon SageMaker saves the resulting model artifacts to an Amazon S3 location that you specify.
--
-- If you choose to host your model using Amazon SageMaker hosting services, you can use the resulting model artifacts as part of the model. You can also use the artifacts in a machine learning service other than Amazon SageMaker, provided that you know how to use them for inferences.
-- In the request body, you provide the following:
--
--     * @AlgorithmSpecification@ - Identifies the training algorithm to use.
--
--
--     * @HyperParameters@ - Specify these algorithm-specific parameters to enable the estimation of model parameters during training. Hyperparameters can be tuned to optimize this learning process. For a list of hyperparameters for each training algorithm provided by Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> .
--
--
--     * @InputDataConfig@ - Describes the training dataset and the Amazon S3, EFS, or FSx location where it is stored.
--
--
--     * @OutputDataConfig@ - Identifies the Amazon S3 bucket where you want Amazon SageMaker to save the results of model training.
--
--
--
--     * @ResourceConfig@ - Identifies the resources, ML compute instances, and ML storage volumes to deploy for model training. In distributed training, you specify more than one instance.
--
--
--     * @EnableManagedSpotTraining@ - Optimize the cost of training machine learning models by up to 80% by using Amazon EC2 Spot instances. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/model-managed-spot-training.html Managed Spot Training> .
--
--
--     * @RoleARN@ - The Amazon Resource Number (ARN) that Amazon SageMaker assumes to perform tasks on your behalf during model training. You must grant this role the necessary permissions so that Amazon SageMaker can successfully complete model training.
--
--
--     * @StoppingCondition@ - To help cap training costs, use @MaxRuntimeInSeconds@ to set a time limit for training. Use @MaxWaitTimeInSeconds@ to specify how long you are willing to wait for a managed spot training job to complete.
--
--
-- For more information about Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/how-it-works.html How It Works> .
module Network.AWS.SageMaker.CreateTrainingJob
  ( -- * Creating a request
    CreateTrainingJob (..),
    mkCreateTrainingJob,

    -- ** Request lenses
    ctjfTrainingJobName,
    ctjfAlgorithmSpecification,
    ctjfRoleArn,
    ctjfOutputDataConfig,
    ctjfResourceConfig,
    ctjfStoppingCondition,
    ctjfCheckpointConfig,
    ctjfDebugHookConfig,
    ctjfDebugRuleConfigurations,
    ctjfEnableInterContainerTrafficEncryption,
    ctjfEnableManagedSpotTraining,
    ctjfEnableNetworkIsolation,
    ctjfExperimentConfig,
    ctjfHyperParameters,
    ctjfInputDataConfig,
    ctjfTags,
    ctjfTensorBoardOutputConfig,
    ctjfVpcConfig,

    -- * Destructuring the response
    CreateTrainingJobResponse (..),
    mkCreateTrainingJobResponse,

    -- ** Response lenses
    crsTrainingJobArn,
    crsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateTrainingJob' smart constructor.
data CreateTrainingJob = CreateTrainingJob'
  { -- | The name of the training job. The name must be unique within an AWS Region in an AWS account.
    trainingJobName :: Types.TrainingJobName,
    -- | The registry path of the Docker image that contains the training algorithm and algorithm-specific metadata, including the input mode. For more information about algorithms provided by Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . For information about providing your own algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
    algorithmSpecification :: Types.AlgorithmSpecification,
    -- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
    --
    -- During model training, Amazon SageMaker needs your permission to read input data from an S3 bucket, download a Docker image that contains training code, write model artifacts to an S3 bucket, write logs to Amazon CloudWatch Logs, and publish metrics to Amazon CloudWatch. You grant permissions for all of these tasks to an IAM role. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
    roleArn :: Types.RoleArn,
    -- | Specifies the path to the S3 location where you want to store model artifacts. Amazon SageMaker creates subfolders for the artifacts.
    outputDataConfig :: Types.OutputDataConfig,
    -- | The resources, including the ML compute instances and ML storage volumes, to use for model training.
    --
    -- ML storage volumes store model artifacts and incremental states. Training algorithms might also use ML storage volumes for scratch space. If you want Amazon SageMaker to use the ML storage volume to store the training data, choose @File@ as the @TrainingInputMode@ in the algorithm specification. For distributed training algorithms, specify an instance count greater than 1.
    resourceConfig :: Types.ResourceConfig,
    -- | Specifies a limit to how long a model training job can run. When the job reaches the time limit, Amazon SageMaker ends the training job. Use this API to cap model training costs.
    --
    -- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts, so the results of training are not lost.
    stoppingCondition :: Types.StoppingCondition,
    -- | Contains information about the output location for managed spot training checkpoint data.
    checkpointConfig :: Core.Maybe Types.CheckpointConfig,
    debugHookConfig :: Core.Maybe Types.DebugHookConfig,
    -- | Configuration information for debugging rules.
    debugRuleConfigurations :: Core.Maybe [Types.DebugRuleConfiguration],
    -- | To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithm in distributed training. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-encrypt.html Protect Communications Between ML Compute Instances in a Distributed Training Job> .
    enableInterContainerTrafficEncryption :: Core.Maybe Core.Bool,
    -- | To train models using managed spot training, choose @True@ . Managed spot training provides a fully managed and scalable infrastructure for training machine learning models. this option is useful when training jobs can be interrupted and when there is flexibility when the training job is run.
    --
    -- The complete and intermediate results of jobs are stored in an Amazon S3 bucket, and can be used as a starting point to train models incrementally. Amazon SageMaker provides metrics and logs in CloudWatch. They can be used to see when managed spot training jobs are running, interrupted, resumed, or completed.
    enableManagedSpotTraining :: Core.Maybe Core.Bool,
    -- | Isolates the training container. No inbound or outbound network calls can be made, except for calls between peers within a training cluster for distributed training. If you enable network isolation for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
    enableNetworkIsolation :: Core.Maybe Core.Bool,
    experimentConfig :: Core.Maybe Types.ExperimentConfig,
    -- | Algorithm-specific parameters that influence the quality of the model. You set hyperparameters before you start the learning process. For a list of hyperparameters for each training algorithm provided by Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> .
    --
    -- You can specify a maximum of 100 hyperparameters. Each hyperparameter is a key-value pair. Each key and value is limited to 256 characters, as specified by the @Length Constraint@ .
    hyperParameters :: Core.Maybe (Core.HashMap Types.HyperParameterKey Types.HyperParameterValue),
    -- | An array of @Channel@ objects. Each channel is a named input source. @InputDataConfig@ describes the input data and its location.
    --
    -- Algorithms can accept input data from one or more channels. For example, an algorithm might have two channels of input data, @training_data@ and @validation_data@ . The configuration for each channel provides the S3, EFS, or FSx location where the input data is stored. It also provides information about the stored data: the MIME type, compression method, and whether the data is wrapped in RecordIO format.
    -- Depending on the input mode that the algorithm supports, Amazon SageMaker either copies input data files from an S3 bucket to a local directory in the Docker container, or makes it available as input streams. For example, if you specify an EFS location, input data files will be made available as input streams. They do not need to be downloaded.
    inputDataConfig :: Core.Maybe (Core.NonEmpty Types.Channel),
    -- | An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
    tags :: Core.Maybe [Types.Tag],
    tensorBoardOutputConfig :: Core.Maybe Types.TensorBoardOutputConfig,
    -- | A 'VpcConfig' object that specifies the VPC that you want your training job to connect to. Control access to and from your training container by configuring the VPC. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
    vpcConfig :: Core.Maybe Types.VpcConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrainingJob' value with any optional fields omitted.
mkCreateTrainingJob ::
  -- | 'trainingJobName'
  Types.TrainingJobName ->
  -- | 'algorithmSpecification'
  Types.AlgorithmSpecification ->
  -- | 'roleArn'
  Types.RoleArn ->
  -- | 'outputDataConfig'
  Types.OutputDataConfig ->
  -- | 'resourceConfig'
  Types.ResourceConfig ->
  -- | 'stoppingCondition'
  Types.StoppingCondition ->
  CreateTrainingJob
mkCreateTrainingJob
  trainingJobName
  algorithmSpecification
  roleArn
  outputDataConfig
  resourceConfig
  stoppingCondition =
    CreateTrainingJob'
      { trainingJobName,
        algorithmSpecification,
        roleArn,
        outputDataConfig,
        resourceConfig,
        stoppingCondition,
        checkpointConfig = Core.Nothing,
        debugHookConfig = Core.Nothing,
        debugRuleConfigurations = Core.Nothing,
        enableInterContainerTrafficEncryption = Core.Nothing,
        enableManagedSpotTraining = Core.Nothing,
        enableNetworkIsolation = Core.Nothing,
        experimentConfig = Core.Nothing,
        hyperParameters = Core.Nothing,
        inputDataConfig = Core.Nothing,
        tags = Core.Nothing,
        tensorBoardOutputConfig = Core.Nothing,
        vpcConfig = Core.Nothing
      }

-- | The name of the training job. The name must be unique within an AWS Region in an AWS account.
--
-- /Note:/ Consider using 'trainingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjfTrainingJobName :: Lens.Lens' CreateTrainingJob Types.TrainingJobName
ctjfTrainingJobName = Lens.field @"trainingJobName"
{-# DEPRECATED ctjfTrainingJobName "Use generic-lens or generic-optics with 'trainingJobName' instead." #-}

-- | The registry path of the Docker image that contains the training algorithm and algorithm-specific metadata, including the input mode. For more information about algorithms provided by Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . For information about providing your own algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
--
-- /Note:/ Consider using 'algorithmSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjfAlgorithmSpecification :: Lens.Lens' CreateTrainingJob Types.AlgorithmSpecification
ctjfAlgorithmSpecification = Lens.field @"algorithmSpecification"
{-# DEPRECATED ctjfAlgorithmSpecification "Use generic-lens or generic-optics with 'algorithmSpecification' instead." #-}

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
--
-- During model training, Amazon SageMaker needs your permission to read input data from an S3 bucket, download a Docker image that contains training code, write model artifacts to an S3 bucket, write logs to Amazon CloudWatch Logs, and publish metrics to Amazon CloudWatch. You grant permissions for all of these tasks to an IAM role. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjfRoleArn :: Lens.Lens' CreateTrainingJob Types.RoleArn
ctjfRoleArn = Lens.field @"roleArn"
{-# DEPRECATED ctjfRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | Specifies the path to the S3 location where you want to store model artifacts. Amazon SageMaker creates subfolders for the artifacts.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjfOutputDataConfig :: Lens.Lens' CreateTrainingJob Types.OutputDataConfig
ctjfOutputDataConfig = Lens.field @"outputDataConfig"
{-# DEPRECATED ctjfOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The resources, including the ML compute instances and ML storage volumes, to use for model training.
--
-- ML storage volumes store model artifacts and incremental states. Training algorithms might also use ML storage volumes for scratch space. If you want Amazon SageMaker to use the ML storage volume to store the training data, choose @File@ as the @TrainingInputMode@ in the algorithm specification. For distributed training algorithms, specify an instance count greater than 1.
--
-- /Note:/ Consider using 'resourceConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjfResourceConfig :: Lens.Lens' CreateTrainingJob Types.ResourceConfig
ctjfResourceConfig = Lens.field @"resourceConfig"
{-# DEPRECATED ctjfResourceConfig "Use generic-lens or generic-optics with 'resourceConfig' instead." #-}

-- | Specifies a limit to how long a model training job can run. When the job reaches the time limit, Amazon SageMaker ends the training job. Use this API to cap model training costs.
--
-- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts, so the results of training are not lost.
--
-- /Note:/ Consider using 'stoppingCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjfStoppingCondition :: Lens.Lens' CreateTrainingJob Types.StoppingCondition
ctjfStoppingCondition = Lens.field @"stoppingCondition"
{-# DEPRECATED ctjfStoppingCondition "Use generic-lens or generic-optics with 'stoppingCondition' instead." #-}

-- | Contains information about the output location for managed spot training checkpoint data.
--
-- /Note:/ Consider using 'checkpointConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjfCheckpointConfig :: Lens.Lens' CreateTrainingJob (Core.Maybe Types.CheckpointConfig)
ctjfCheckpointConfig = Lens.field @"checkpointConfig"
{-# DEPRECATED ctjfCheckpointConfig "Use generic-lens or generic-optics with 'checkpointConfig' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'debugHookConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjfDebugHookConfig :: Lens.Lens' CreateTrainingJob (Core.Maybe Types.DebugHookConfig)
ctjfDebugHookConfig = Lens.field @"debugHookConfig"
{-# DEPRECATED ctjfDebugHookConfig "Use generic-lens or generic-optics with 'debugHookConfig' instead." #-}

-- | Configuration information for debugging rules.
--
-- /Note:/ Consider using 'debugRuleConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjfDebugRuleConfigurations :: Lens.Lens' CreateTrainingJob (Core.Maybe [Types.DebugRuleConfiguration])
ctjfDebugRuleConfigurations = Lens.field @"debugRuleConfigurations"
{-# DEPRECATED ctjfDebugRuleConfigurations "Use generic-lens or generic-optics with 'debugRuleConfigurations' instead." #-}

-- | To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithm in distributed training. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-encrypt.html Protect Communications Between ML Compute Instances in a Distributed Training Job> .
--
-- /Note:/ Consider using 'enableInterContainerTrafficEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjfEnableInterContainerTrafficEncryption :: Lens.Lens' CreateTrainingJob (Core.Maybe Core.Bool)
ctjfEnableInterContainerTrafficEncryption = Lens.field @"enableInterContainerTrafficEncryption"
{-# DEPRECATED ctjfEnableInterContainerTrafficEncryption "Use generic-lens or generic-optics with 'enableInterContainerTrafficEncryption' instead." #-}

-- | To train models using managed spot training, choose @True@ . Managed spot training provides a fully managed and scalable infrastructure for training machine learning models. this option is useful when training jobs can be interrupted and when there is flexibility when the training job is run.
--
-- The complete and intermediate results of jobs are stored in an Amazon S3 bucket, and can be used as a starting point to train models incrementally. Amazon SageMaker provides metrics and logs in CloudWatch. They can be used to see when managed spot training jobs are running, interrupted, resumed, or completed.
--
-- /Note:/ Consider using 'enableManagedSpotTraining' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjfEnableManagedSpotTraining :: Lens.Lens' CreateTrainingJob (Core.Maybe Core.Bool)
ctjfEnableManagedSpotTraining = Lens.field @"enableManagedSpotTraining"
{-# DEPRECATED ctjfEnableManagedSpotTraining "Use generic-lens or generic-optics with 'enableManagedSpotTraining' instead." #-}

-- | Isolates the training container. No inbound or outbound network calls can be made, except for calls between peers within a training cluster for distributed training. If you enable network isolation for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
--
-- /Note:/ Consider using 'enableNetworkIsolation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjfEnableNetworkIsolation :: Lens.Lens' CreateTrainingJob (Core.Maybe Core.Bool)
ctjfEnableNetworkIsolation = Lens.field @"enableNetworkIsolation"
{-# DEPRECATED ctjfEnableNetworkIsolation "Use generic-lens or generic-optics with 'enableNetworkIsolation' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'experimentConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjfExperimentConfig :: Lens.Lens' CreateTrainingJob (Core.Maybe Types.ExperimentConfig)
ctjfExperimentConfig = Lens.field @"experimentConfig"
{-# DEPRECATED ctjfExperimentConfig "Use generic-lens or generic-optics with 'experimentConfig' instead." #-}

-- | Algorithm-specific parameters that influence the quality of the model. You set hyperparameters before you start the learning process. For a list of hyperparameters for each training algorithm provided by Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> .
--
-- You can specify a maximum of 100 hyperparameters. Each hyperparameter is a key-value pair. Each key and value is limited to 256 characters, as specified by the @Length Constraint@ .
--
-- /Note:/ Consider using 'hyperParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjfHyperParameters :: Lens.Lens' CreateTrainingJob (Core.Maybe (Core.HashMap Types.HyperParameterKey Types.HyperParameterValue))
ctjfHyperParameters = Lens.field @"hyperParameters"
{-# DEPRECATED ctjfHyperParameters "Use generic-lens or generic-optics with 'hyperParameters' instead." #-}

-- | An array of @Channel@ objects. Each channel is a named input source. @InputDataConfig@ describes the input data and its location.
--
-- Algorithms can accept input data from one or more channels. For example, an algorithm might have two channels of input data, @training_data@ and @validation_data@ . The configuration for each channel provides the S3, EFS, or FSx location where the input data is stored. It also provides information about the stored data: the MIME type, compression method, and whether the data is wrapped in RecordIO format.
-- Depending on the input mode that the algorithm supports, Amazon SageMaker either copies input data files from an S3 bucket to a local directory in the Docker container, or makes it available as input streams. For example, if you specify an EFS location, input data files will be made available as input streams. They do not need to be downloaded.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjfInputDataConfig :: Lens.Lens' CreateTrainingJob (Core.Maybe (Core.NonEmpty Types.Channel))
ctjfInputDataConfig = Lens.field @"inputDataConfig"
{-# DEPRECATED ctjfInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjfTags :: Lens.Lens' CreateTrainingJob (Core.Maybe [Types.Tag])
ctjfTags = Lens.field @"tags"
{-# DEPRECATED ctjfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tensorBoardOutputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjfTensorBoardOutputConfig :: Lens.Lens' CreateTrainingJob (Core.Maybe Types.TensorBoardOutputConfig)
ctjfTensorBoardOutputConfig = Lens.field @"tensorBoardOutputConfig"
{-# DEPRECATED ctjfTensorBoardOutputConfig "Use generic-lens or generic-optics with 'tensorBoardOutputConfig' instead." #-}

-- | A 'VpcConfig' object that specifies the VPC that you want your training job to connect to. Control access to and from your training container by configuring the VPC. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjfVpcConfig :: Lens.Lens' CreateTrainingJob (Core.Maybe Types.VpcConfig)
ctjfVpcConfig = Lens.field @"vpcConfig"
{-# DEPRECATED ctjfVpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

instance Core.FromJSON CreateTrainingJob where
  toJSON CreateTrainingJob {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TrainingJobName" Core..= trainingJobName),
            Core.Just
              ("AlgorithmSpecification" Core..= algorithmSpecification),
            Core.Just ("RoleArn" Core..= roleArn),
            Core.Just ("OutputDataConfig" Core..= outputDataConfig),
            Core.Just ("ResourceConfig" Core..= resourceConfig),
            Core.Just ("StoppingCondition" Core..= stoppingCondition),
            ("CheckpointConfig" Core..=) Core.<$> checkpointConfig,
            ("DebugHookConfig" Core..=) Core.<$> debugHookConfig,
            ("DebugRuleConfigurations" Core..=)
              Core.<$> debugRuleConfigurations,
            ("EnableInterContainerTrafficEncryption" Core..=)
              Core.<$> enableInterContainerTrafficEncryption,
            ("EnableManagedSpotTraining" Core..=)
              Core.<$> enableManagedSpotTraining,
            ("EnableNetworkIsolation" Core..=) Core.<$> enableNetworkIsolation,
            ("ExperimentConfig" Core..=) Core.<$> experimentConfig,
            ("HyperParameters" Core..=) Core.<$> hyperParameters,
            ("InputDataConfig" Core..=) Core.<$> inputDataConfig,
            ("Tags" Core..=) Core.<$> tags,
            ("TensorBoardOutputConfig" Core..=)
              Core.<$> tensorBoardOutputConfig,
            ("VpcConfig" Core..=) Core.<$> vpcConfig
          ]
      )

instance Core.AWSRequest CreateTrainingJob where
  type Rs CreateTrainingJob = CreateTrainingJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.CreateTrainingJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTrainingJobResponse'
            Core.<$> (x Core..: "TrainingJobArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateTrainingJobResponse' smart constructor.
data CreateTrainingJobResponse = CreateTrainingJobResponse'
  { -- | The Amazon Resource Name (ARN) of the training job.
    trainingJobArn :: Types.TrainingJobArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrainingJobResponse' value with any optional fields omitted.
mkCreateTrainingJobResponse ::
  -- | 'trainingJobArn'
  Types.TrainingJobArn ->
  -- | 'responseStatus'
  Core.Int ->
  CreateTrainingJobResponse
mkCreateTrainingJobResponse trainingJobArn responseStatus =
  CreateTrainingJobResponse' {trainingJobArn, responseStatus}

-- | The Amazon Resource Name (ARN) of the training job.
--
-- /Note:/ Consider using 'trainingJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsTrainingJobArn :: Lens.Lens' CreateTrainingJobResponse Types.TrainingJobArn
crsTrainingJobArn = Lens.field @"trainingJobArn"
{-# DEPRECATED crsTrainingJobArn "Use generic-lens or generic-optics with 'trainingJobArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateTrainingJobResponse Core.Int
crsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
