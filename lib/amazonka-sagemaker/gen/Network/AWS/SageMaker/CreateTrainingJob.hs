{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    ctjtDebugHookConfig,
    ctjtCheckpointConfig,
    ctjtEnableNetworkIsolation,
    ctjtExperimentConfig,
    ctjtDebugRuleConfigurations,
    ctjtEnableManagedSpotTraining,
    ctjtHyperParameters,
    ctjtInputDataConfig,
    ctjtVPCConfig,
    ctjtEnableInterContainerTrafficEncryption,
    ctjtTensorBoardOutputConfig,
    ctjtTags,
    ctjtTrainingJobName,
    ctjtAlgorithmSpecification,
    ctjtRoleARN,
    ctjtOutputDataConfig,
    ctjtResourceConfig,
    ctjtStoppingCondition,

    -- * Destructuring the response
    CreateTrainingJobResponse (..),
    mkCreateTrainingJobResponse,

    -- ** Response lenses
    ctjtrsResponseStatus,
    ctjtrsTrainingJobARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateTrainingJob' smart constructor.
data CreateTrainingJob = CreateTrainingJob'
  { debugHookConfig ::
      Lude.Maybe DebugHookConfig,
    checkpointConfig :: Lude.Maybe CheckpointConfig,
    enableNetworkIsolation :: Lude.Maybe Lude.Bool,
    experimentConfig :: Lude.Maybe ExperimentConfig,
    debugRuleConfigurations ::
      Lude.Maybe [DebugRuleConfiguration],
    enableManagedSpotTraining :: Lude.Maybe Lude.Bool,
    hyperParameters ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    inputDataConfig :: Lude.Maybe (Lude.NonEmpty Channel),
    vpcConfig :: Lude.Maybe VPCConfig,
    enableInterContainerTrafficEncryption ::
      Lude.Maybe Lude.Bool,
    tensorBoardOutputConfig ::
      Lude.Maybe TensorBoardOutputConfig,
    tags :: Lude.Maybe [Tag],
    trainingJobName :: Lude.Text,
    algorithmSpecification :: AlgorithmSpecification,
    roleARN :: Lude.Text,
    outputDataConfig :: OutputDataConfig,
    resourceConfig :: ResourceConfig,
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

-- | Creates a value of 'CreateTrainingJob' with the minimum fields required to make a request.
--
-- * 'algorithmSpecification' - The registry path of the Docker image that contains the training algorithm and algorithm-specific metadata, including the input mode. For more information about algorithms provided by Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . For information about providing your own algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
-- * 'checkpointConfig' - Contains information about the output location for managed spot training checkpoint data.
-- * 'debugHookConfig' - Undocumented field.
-- * 'debugRuleConfigurations' - Configuration information for debugging rules.
-- * 'enableInterContainerTrafficEncryption' - To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithm in distributed training. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-encrypt.html Protect Communications Between ML Compute Instances in a Distributed Training Job> .
-- * 'enableManagedSpotTraining' - To train models using managed spot training, choose @True@ . Managed spot training provides a fully managed and scalable infrastructure for training machine learning models. this option is useful when training jobs can be interrupted and when there is flexibility when the training job is run.
--
-- The complete and intermediate results of jobs are stored in an Amazon S3 bucket, and can be used as a starting point to train models incrementally. Amazon SageMaker provides metrics and logs in CloudWatch. They can be used to see when managed spot training jobs are running, interrupted, resumed, or completed.
-- * 'enableNetworkIsolation' - Isolates the training container. No inbound or outbound network calls can be made, except for calls between peers within a training cluster for distributed training. If you enable network isolation for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
-- * 'experimentConfig' - Undocumented field.
-- * 'hyperParameters' - Algorithm-specific parameters that influence the quality of the model. You set hyperparameters before you start the learning process. For a list of hyperparameters for each training algorithm provided by Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> .
--
-- You can specify a maximum of 100 hyperparameters. Each hyperparameter is a key-value pair. Each key and value is limited to 256 characters, as specified by the @Length Constraint@ .
-- * 'inputDataConfig' - An array of @Channel@ objects. Each channel is a named input source. @InputDataConfig@ describes the input data and its location.
--
-- Algorithms can accept input data from one or more channels. For example, an algorithm might have two channels of input data, @training_data@ and @validation_data@ . The configuration for each channel provides the S3, EFS, or FSx location where the input data is stored. It also provides information about the stored data: the MIME type, compression method, and whether the data is wrapped in RecordIO format.
-- Depending on the input mode that the algorithm supports, Amazon SageMaker either copies input data files from an S3 bucket to a local directory in the Docker container, or makes it available as input streams. For example, if you specify an EFS location, input data files will be made available as input streams. They do not need to be downloaded.
-- * 'outputDataConfig' - Specifies the path to the S3 location where you want to store model artifacts. Amazon SageMaker creates subfolders for the artifacts.
-- * 'resourceConfig' - The resources, including the ML compute instances and ML storage volumes, to use for model training.
--
-- ML storage volumes store model artifacts and incremental states. Training algorithms might also use ML storage volumes for scratch space. If you want Amazon SageMaker to use the ML storage volume to store the training data, choose @File@ as the @TrainingInputMode@ in the algorithm specification. For distributed training algorithms, specify an instance count greater than 1.
-- * 'roleARN' - The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
--
-- During model training, Amazon SageMaker needs your permission to read input data from an S3 bucket, download a Docker image that contains training code, write model artifacts to an S3 bucket, write logs to Amazon CloudWatch Logs, and publish metrics to Amazon CloudWatch. You grant permissions for all of these tasks to an IAM role. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
-- * 'stoppingCondition' - Specifies a limit to how long a model training job can run. When the job reaches the time limit, Amazon SageMaker ends the training job. Use this API to cap model training costs.
--
-- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts, so the results of training are not lost.
-- * 'tags' - An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
-- * 'tensorBoardOutputConfig' - Undocumented field.
-- * 'trainingJobName' - The name of the training job. The name must be unique within an AWS Region in an AWS account.
-- * 'vpcConfig' - A 'VpcConfig' object that specifies the VPC that you want your training job to connect to. Control access to and from your training container by configuring the VPC. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
mkCreateTrainingJob ::
  -- | 'trainingJobName'
  Lude.Text ->
  -- | 'algorithmSpecification'
  AlgorithmSpecification ->
  -- | 'roleARN'
  Lude.Text ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'resourceConfig'
  ResourceConfig ->
  -- | 'stoppingCondition'
  StoppingCondition ->
  CreateTrainingJob
mkCreateTrainingJob
  pTrainingJobName_
  pAlgorithmSpecification_
  pRoleARN_
  pOutputDataConfig_
  pResourceConfig_
  pStoppingCondition_ =
    CreateTrainingJob'
      { debugHookConfig = Lude.Nothing,
        checkpointConfig = Lude.Nothing,
        enableNetworkIsolation = Lude.Nothing,
        experimentConfig = Lude.Nothing,
        debugRuleConfigurations = Lude.Nothing,
        enableManagedSpotTraining = Lude.Nothing,
        hyperParameters = Lude.Nothing,
        inputDataConfig = Lude.Nothing,
        vpcConfig = Lude.Nothing,
        enableInterContainerTrafficEncryption = Lude.Nothing,
        tensorBoardOutputConfig = Lude.Nothing,
        tags = Lude.Nothing,
        trainingJobName = pTrainingJobName_,
        algorithmSpecification = pAlgorithmSpecification_,
        roleARN = pRoleARN_,
        outputDataConfig = pOutputDataConfig_,
        resourceConfig = pResourceConfig_,
        stoppingCondition = pStoppingCondition_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'debugHookConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjtDebugHookConfig :: Lens.Lens' CreateTrainingJob (Lude.Maybe DebugHookConfig)
ctjtDebugHookConfig = Lens.lens (debugHookConfig :: CreateTrainingJob -> Lude.Maybe DebugHookConfig) (\s a -> s {debugHookConfig = a} :: CreateTrainingJob)
{-# DEPRECATED ctjtDebugHookConfig "Use generic-lens or generic-optics with 'debugHookConfig' instead." #-}

-- | Contains information about the output location for managed spot training checkpoint data.
--
-- /Note:/ Consider using 'checkpointConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjtCheckpointConfig :: Lens.Lens' CreateTrainingJob (Lude.Maybe CheckpointConfig)
ctjtCheckpointConfig = Lens.lens (checkpointConfig :: CreateTrainingJob -> Lude.Maybe CheckpointConfig) (\s a -> s {checkpointConfig = a} :: CreateTrainingJob)
{-# DEPRECATED ctjtCheckpointConfig "Use generic-lens or generic-optics with 'checkpointConfig' instead." #-}

-- | Isolates the training container. No inbound or outbound network calls can be made, except for calls between peers within a training cluster for distributed training. If you enable network isolation for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
--
-- /Note:/ Consider using 'enableNetworkIsolation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjtEnableNetworkIsolation :: Lens.Lens' CreateTrainingJob (Lude.Maybe Lude.Bool)
ctjtEnableNetworkIsolation = Lens.lens (enableNetworkIsolation :: CreateTrainingJob -> Lude.Maybe Lude.Bool) (\s a -> s {enableNetworkIsolation = a} :: CreateTrainingJob)
{-# DEPRECATED ctjtEnableNetworkIsolation "Use generic-lens or generic-optics with 'enableNetworkIsolation' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'experimentConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjtExperimentConfig :: Lens.Lens' CreateTrainingJob (Lude.Maybe ExperimentConfig)
ctjtExperimentConfig = Lens.lens (experimentConfig :: CreateTrainingJob -> Lude.Maybe ExperimentConfig) (\s a -> s {experimentConfig = a} :: CreateTrainingJob)
{-# DEPRECATED ctjtExperimentConfig "Use generic-lens or generic-optics with 'experimentConfig' instead." #-}

-- | Configuration information for debugging rules.
--
-- /Note:/ Consider using 'debugRuleConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjtDebugRuleConfigurations :: Lens.Lens' CreateTrainingJob (Lude.Maybe [DebugRuleConfiguration])
ctjtDebugRuleConfigurations = Lens.lens (debugRuleConfigurations :: CreateTrainingJob -> Lude.Maybe [DebugRuleConfiguration]) (\s a -> s {debugRuleConfigurations = a} :: CreateTrainingJob)
{-# DEPRECATED ctjtDebugRuleConfigurations "Use generic-lens or generic-optics with 'debugRuleConfigurations' instead." #-}

-- | To train models using managed spot training, choose @True@ . Managed spot training provides a fully managed and scalable infrastructure for training machine learning models. this option is useful when training jobs can be interrupted and when there is flexibility when the training job is run.
--
-- The complete and intermediate results of jobs are stored in an Amazon S3 bucket, and can be used as a starting point to train models incrementally. Amazon SageMaker provides metrics and logs in CloudWatch. They can be used to see when managed spot training jobs are running, interrupted, resumed, or completed.
--
-- /Note:/ Consider using 'enableManagedSpotTraining' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjtEnableManagedSpotTraining :: Lens.Lens' CreateTrainingJob (Lude.Maybe Lude.Bool)
ctjtEnableManagedSpotTraining = Lens.lens (enableManagedSpotTraining :: CreateTrainingJob -> Lude.Maybe Lude.Bool) (\s a -> s {enableManagedSpotTraining = a} :: CreateTrainingJob)
{-# DEPRECATED ctjtEnableManagedSpotTraining "Use generic-lens or generic-optics with 'enableManagedSpotTraining' instead." #-}

-- | Algorithm-specific parameters that influence the quality of the model. You set hyperparameters before you start the learning process. For a list of hyperparameters for each training algorithm provided by Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> .
--
-- You can specify a maximum of 100 hyperparameters. Each hyperparameter is a key-value pair. Each key and value is limited to 256 characters, as specified by the @Length Constraint@ .
--
-- /Note:/ Consider using 'hyperParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjtHyperParameters :: Lens.Lens' CreateTrainingJob (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ctjtHyperParameters = Lens.lens (hyperParameters :: CreateTrainingJob -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {hyperParameters = a} :: CreateTrainingJob)
{-# DEPRECATED ctjtHyperParameters "Use generic-lens or generic-optics with 'hyperParameters' instead." #-}

-- | An array of @Channel@ objects. Each channel is a named input source. @InputDataConfig@ describes the input data and its location.
--
-- Algorithms can accept input data from one or more channels. For example, an algorithm might have two channels of input data, @training_data@ and @validation_data@ . The configuration for each channel provides the S3, EFS, or FSx location where the input data is stored. It also provides information about the stored data: the MIME type, compression method, and whether the data is wrapped in RecordIO format.
-- Depending on the input mode that the algorithm supports, Amazon SageMaker either copies input data files from an S3 bucket to a local directory in the Docker container, or makes it available as input streams. For example, if you specify an EFS location, input data files will be made available as input streams. They do not need to be downloaded.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjtInputDataConfig :: Lens.Lens' CreateTrainingJob (Lude.Maybe (Lude.NonEmpty Channel))
ctjtInputDataConfig = Lens.lens (inputDataConfig :: CreateTrainingJob -> Lude.Maybe (Lude.NonEmpty Channel)) (\s a -> s {inputDataConfig = a} :: CreateTrainingJob)
{-# DEPRECATED ctjtInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | A 'VpcConfig' object that specifies the VPC that you want your training job to connect to. Control access to and from your training container by configuring the VPC. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjtVPCConfig :: Lens.Lens' CreateTrainingJob (Lude.Maybe VPCConfig)
ctjtVPCConfig = Lens.lens (vpcConfig :: CreateTrainingJob -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: CreateTrainingJob)
{-# DEPRECATED ctjtVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

-- | To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithm in distributed training. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-encrypt.html Protect Communications Between ML Compute Instances in a Distributed Training Job> .
--
-- /Note:/ Consider using 'enableInterContainerTrafficEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjtEnableInterContainerTrafficEncryption :: Lens.Lens' CreateTrainingJob (Lude.Maybe Lude.Bool)
ctjtEnableInterContainerTrafficEncryption = Lens.lens (enableInterContainerTrafficEncryption :: CreateTrainingJob -> Lude.Maybe Lude.Bool) (\s a -> s {enableInterContainerTrafficEncryption = a} :: CreateTrainingJob)
{-# DEPRECATED ctjtEnableInterContainerTrafficEncryption "Use generic-lens or generic-optics with 'enableInterContainerTrafficEncryption' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tensorBoardOutputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjtTensorBoardOutputConfig :: Lens.Lens' CreateTrainingJob (Lude.Maybe TensorBoardOutputConfig)
ctjtTensorBoardOutputConfig = Lens.lens (tensorBoardOutputConfig :: CreateTrainingJob -> Lude.Maybe TensorBoardOutputConfig) (\s a -> s {tensorBoardOutputConfig = a} :: CreateTrainingJob)
{-# DEPRECATED ctjtTensorBoardOutputConfig "Use generic-lens or generic-optics with 'tensorBoardOutputConfig' instead." #-}

-- | An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjtTags :: Lens.Lens' CreateTrainingJob (Lude.Maybe [Tag])
ctjtTags = Lens.lens (tags :: CreateTrainingJob -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateTrainingJob)
{-# DEPRECATED ctjtTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the training job. The name must be unique within an AWS Region in an AWS account.
--
-- /Note:/ Consider using 'trainingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjtTrainingJobName :: Lens.Lens' CreateTrainingJob Lude.Text
ctjtTrainingJobName = Lens.lens (trainingJobName :: CreateTrainingJob -> Lude.Text) (\s a -> s {trainingJobName = a} :: CreateTrainingJob)
{-# DEPRECATED ctjtTrainingJobName "Use generic-lens or generic-optics with 'trainingJobName' instead." #-}

-- | The registry path of the Docker image that contains the training algorithm and algorithm-specific metadata, including the input mode. For more information about algorithms provided by Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . For information about providing your own algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
--
-- /Note:/ Consider using 'algorithmSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjtAlgorithmSpecification :: Lens.Lens' CreateTrainingJob AlgorithmSpecification
ctjtAlgorithmSpecification = Lens.lens (algorithmSpecification :: CreateTrainingJob -> AlgorithmSpecification) (\s a -> s {algorithmSpecification = a} :: CreateTrainingJob)
{-# DEPRECATED ctjtAlgorithmSpecification "Use generic-lens or generic-optics with 'algorithmSpecification' instead." #-}

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
--
-- During model training, Amazon SageMaker needs your permission to read input data from an S3 bucket, download a Docker image that contains training code, write model artifacts to an S3 bucket, write logs to Amazon CloudWatch Logs, and publish metrics to Amazon CloudWatch. You grant permissions for all of these tasks to an IAM role. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjtRoleARN :: Lens.Lens' CreateTrainingJob Lude.Text
ctjtRoleARN = Lens.lens (roleARN :: CreateTrainingJob -> Lude.Text) (\s a -> s {roleARN = a} :: CreateTrainingJob)
{-# DEPRECATED ctjtRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | Specifies the path to the S3 location where you want to store model artifacts. Amazon SageMaker creates subfolders for the artifacts.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjtOutputDataConfig :: Lens.Lens' CreateTrainingJob OutputDataConfig
ctjtOutputDataConfig = Lens.lens (outputDataConfig :: CreateTrainingJob -> OutputDataConfig) (\s a -> s {outputDataConfig = a} :: CreateTrainingJob)
{-# DEPRECATED ctjtOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The resources, including the ML compute instances and ML storage volumes, to use for model training.
--
-- ML storage volumes store model artifacts and incremental states. Training algorithms might also use ML storage volumes for scratch space. If you want Amazon SageMaker to use the ML storage volume to store the training data, choose @File@ as the @TrainingInputMode@ in the algorithm specification. For distributed training algorithms, specify an instance count greater than 1.
--
-- /Note:/ Consider using 'resourceConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjtResourceConfig :: Lens.Lens' CreateTrainingJob ResourceConfig
ctjtResourceConfig = Lens.lens (resourceConfig :: CreateTrainingJob -> ResourceConfig) (\s a -> s {resourceConfig = a} :: CreateTrainingJob)
{-# DEPRECATED ctjtResourceConfig "Use generic-lens or generic-optics with 'resourceConfig' instead." #-}

-- | Specifies a limit to how long a model training job can run. When the job reaches the time limit, Amazon SageMaker ends the training job. Use this API to cap model training costs.
--
-- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts, so the results of training are not lost.
--
-- /Note:/ Consider using 'stoppingCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjtStoppingCondition :: Lens.Lens' CreateTrainingJob StoppingCondition
ctjtStoppingCondition = Lens.lens (stoppingCondition :: CreateTrainingJob -> StoppingCondition) (\s a -> s {stoppingCondition = a} :: CreateTrainingJob)
{-# DEPRECATED ctjtStoppingCondition "Use generic-lens or generic-optics with 'stoppingCondition' instead." #-}

instance Lude.AWSRequest CreateTrainingJob where
  type Rs CreateTrainingJob = CreateTrainingJobResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateTrainingJobResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "TrainingJobArn")
      )

instance Lude.ToHeaders CreateTrainingJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.CreateTrainingJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateTrainingJob where
  toJSON CreateTrainingJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DebugHookConfig" Lude..=) Lude.<$> debugHookConfig,
            ("CheckpointConfig" Lude..=) Lude.<$> checkpointConfig,
            ("EnableNetworkIsolation" Lude..=) Lude.<$> enableNetworkIsolation,
            ("ExperimentConfig" Lude..=) Lude.<$> experimentConfig,
            ("DebugRuleConfigurations" Lude..=)
              Lude.<$> debugRuleConfigurations,
            ("EnableManagedSpotTraining" Lude..=)
              Lude.<$> enableManagedSpotTraining,
            ("HyperParameters" Lude..=) Lude.<$> hyperParameters,
            ("InputDataConfig" Lude..=) Lude.<$> inputDataConfig,
            ("VpcConfig" Lude..=) Lude.<$> vpcConfig,
            ("EnableInterContainerTrafficEncryption" Lude..=)
              Lude.<$> enableInterContainerTrafficEncryption,
            ("TensorBoardOutputConfig" Lude..=)
              Lude.<$> tensorBoardOutputConfig,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("TrainingJobName" Lude..= trainingJobName),
            Lude.Just
              ("AlgorithmSpecification" Lude..= algorithmSpecification),
            Lude.Just ("RoleArn" Lude..= roleARN),
            Lude.Just ("OutputDataConfig" Lude..= outputDataConfig),
            Lude.Just ("ResourceConfig" Lude..= resourceConfig),
            Lude.Just ("StoppingCondition" Lude..= stoppingCondition)
          ]
      )

instance Lude.ToPath CreateTrainingJob where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTrainingJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateTrainingJobResponse' smart constructor.
data CreateTrainingJobResponse = CreateTrainingJobResponse'
  { responseStatus ::
      Lude.Int,
    trainingJobARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTrainingJobResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'trainingJobARN' - The Amazon Resource Name (ARN) of the training job.
mkCreateTrainingJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'trainingJobARN'
  Lude.Text ->
  CreateTrainingJobResponse
mkCreateTrainingJobResponse pResponseStatus_ pTrainingJobARN_ =
  CreateTrainingJobResponse'
    { responseStatus = pResponseStatus_,
      trainingJobARN = pTrainingJobARN_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjtrsResponseStatus :: Lens.Lens' CreateTrainingJobResponse Lude.Int
ctjtrsResponseStatus = Lens.lens (responseStatus :: CreateTrainingJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTrainingJobResponse)
{-# DEPRECATED ctjtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The Amazon Resource Name (ARN) of the training job.
--
-- /Note:/ Consider using 'trainingJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjtrsTrainingJobARN :: Lens.Lens' CreateTrainingJobResponse Lude.Text
ctjtrsTrainingJobARN = Lens.lens (trainingJobARN :: CreateTrainingJobResponse -> Lude.Text) (\s a -> s {trainingJobARN = a} :: CreateTrainingJobResponse)
{-# DEPRECATED ctjtrsTrainingJobARN "Use generic-lens or generic-optics with 'trainingJobARN' instead." #-}
