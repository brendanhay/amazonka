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
    ctjDebugHookConfig,
    ctjCheckpointConfig,
    ctjStoppingCondition,
    ctjEnableNetworkIsolation,
    ctjExperimentConfig,
    ctjDebugRuleConfigurations,
    ctjEnableManagedSpotTraining,
    ctjHyperParameters,
    ctjInputDataConfig,
    ctjVPCConfig,
    ctjAlgorithmSpecification,
    ctjOutputDataConfig,
    ctjTrainingJobName,
    ctjResourceConfig,
    ctjEnableInterContainerTrafficEncryption,
    ctjTensorBoardOutputConfig,
    ctjTags,
    ctjRoleARN,

    -- * Destructuring the response
    CreateTrainingJobResponse (..),
    mkCreateTrainingJobResponse,

    -- ** Response lenses
    ctjfrsTrainingJobARN,
    ctjfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateTrainingJob' smart constructor.
data CreateTrainingJob = CreateTrainingJob'
  { debugHookConfig :: Lude.Maybe DebugHookConfig,
    -- | Contains information about the output location for managed spot training checkpoint data.
    checkpointConfig :: Lude.Maybe CheckpointConfig,
    -- | Specifies a limit to how long a model training job can run. When the job reaches the time limit, Amazon SageMaker ends the training job. Use this API to cap model training costs.
    --
    -- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts, so the results of training are not lost.
    stoppingCondition :: StoppingCondition,
    -- | Isolates the training container. No inbound or outbound network calls can be made, except for calls between peers within a training cluster for distributed training. If you enable network isolation for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
    enableNetworkIsolation :: Lude.Maybe Lude.Bool,
    experimentConfig :: Lude.Maybe ExperimentConfig,
    -- | Configuration information for debugging rules.
    debugRuleConfigurations :: Lude.Maybe [DebugRuleConfiguration],
    -- | To train models using managed spot training, choose @True@ . Managed spot training provides a fully managed and scalable infrastructure for training machine learning models. this option is useful when training jobs can be interrupted and when there is flexibility when the training job is run.
    --
    -- The complete and intermediate results of jobs are stored in an Amazon S3 bucket, and can be used as a starting point to train models incrementally. Amazon SageMaker provides metrics and logs in CloudWatch. They can be used to see when managed spot training jobs are running, interrupted, resumed, or completed.
    enableManagedSpotTraining :: Lude.Maybe Lude.Bool,
    -- | Algorithm-specific parameters that influence the quality of the model. You set hyperparameters before you start the learning process. For a list of hyperparameters for each training algorithm provided by Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> .
    --
    -- You can specify a maximum of 100 hyperparameters. Each hyperparameter is a key-value pair. Each key and value is limited to 256 characters, as specified by the @Length Constraint@ .
    hyperParameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | An array of @Channel@ objects. Each channel is a named input source. @InputDataConfig@ describes the input data and its location.
    --
    -- Algorithms can accept input data from one or more channels. For example, an algorithm might have two channels of input data, @training_data@ and @validation_data@ . The configuration for each channel provides the S3, EFS, or FSx location where the input data is stored. It also provides information about the stored data: the MIME type, compression method, and whether the data is wrapped in RecordIO format.
    -- Depending on the input mode that the algorithm supports, Amazon SageMaker either copies input data files from an S3 bucket to a local directory in the Docker container, or makes it available as input streams. For example, if you specify an EFS location, input data files will be made available as input streams. They do not need to be downloaded.
    inputDataConfig :: Lude.Maybe (Lude.NonEmpty Channel),
    -- | A 'VpcConfig' object that specifies the VPC that you want your training job to connect to. Control access to and from your training container by configuring the VPC. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
    vpcConfig :: Lude.Maybe VPCConfig,
    -- | The registry path of the Docker image that contains the training algorithm and algorithm-specific metadata, including the input mode. For more information about algorithms provided by Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . For information about providing your own algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
    algorithmSpecification :: AlgorithmSpecification,
    -- | Specifies the path to the S3 location where you want to store model artifacts. Amazon SageMaker creates subfolders for the artifacts.
    outputDataConfig :: OutputDataConfig,
    -- | The name of the training job. The name must be unique within an AWS Region in an AWS account.
    trainingJobName :: Lude.Text,
    -- | The resources, including the ML compute instances and ML storage volumes, to use for model training.
    --
    -- ML storage volumes store model artifacts and incremental states. Training algorithms might also use ML storage volumes for scratch space. If you want Amazon SageMaker to use the ML storage volume to store the training data, choose @File@ as the @TrainingInputMode@ in the algorithm specification. For distributed training algorithms, specify an instance count greater than 1.
    resourceConfig :: ResourceConfig,
    -- | To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithm in distributed training. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-encrypt.html Protect Communications Between ML Compute Instances in a Distributed Training Job> .
    enableInterContainerTrafficEncryption :: Lude.Maybe Lude.Bool,
    tensorBoardOutputConfig :: Lude.Maybe TensorBoardOutputConfig,
    -- | An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
    tags :: Lude.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
    --
    -- During model training, Amazon SageMaker needs your permission to read input data from an S3 bucket, download a Docker image that contains training code, write model artifacts to an S3 bucket, write logs to Amazon CloudWatch Logs, and publish metrics to Amazon CloudWatch. You grant permissions for all of these tasks to an IAM role. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTrainingJob' with the minimum fields required to make a request.
--
-- * 'debugHookConfig' -
-- * 'checkpointConfig' - Contains information about the output location for managed spot training checkpoint data.
-- * 'stoppingCondition' - Specifies a limit to how long a model training job can run. When the job reaches the time limit, Amazon SageMaker ends the training job. Use this API to cap model training costs.
--
-- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts, so the results of training are not lost.
-- * 'enableNetworkIsolation' - Isolates the training container. No inbound or outbound network calls can be made, except for calls between peers within a training cluster for distributed training. If you enable network isolation for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
-- * 'experimentConfig' -
-- * 'debugRuleConfigurations' - Configuration information for debugging rules.
-- * 'enableManagedSpotTraining' - To train models using managed spot training, choose @True@ . Managed spot training provides a fully managed and scalable infrastructure for training machine learning models. this option is useful when training jobs can be interrupted and when there is flexibility when the training job is run.
--
-- The complete and intermediate results of jobs are stored in an Amazon S3 bucket, and can be used as a starting point to train models incrementally. Amazon SageMaker provides metrics and logs in CloudWatch. They can be used to see when managed spot training jobs are running, interrupted, resumed, or completed.
-- * 'hyperParameters' - Algorithm-specific parameters that influence the quality of the model. You set hyperparameters before you start the learning process. For a list of hyperparameters for each training algorithm provided by Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> .
--
-- You can specify a maximum of 100 hyperparameters. Each hyperparameter is a key-value pair. Each key and value is limited to 256 characters, as specified by the @Length Constraint@ .
-- * 'inputDataConfig' - An array of @Channel@ objects. Each channel is a named input source. @InputDataConfig@ describes the input data and its location.
--
-- Algorithms can accept input data from one or more channels. For example, an algorithm might have two channels of input data, @training_data@ and @validation_data@ . The configuration for each channel provides the S3, EFS, or FSx location where the input data is stored. It also provides information about the stored data: the MIME type, compression method, and whether the data is wrapped in RecordIO format.
-- Depending on the input mode that the algorithm supports, Amazon SageMaker either copies input data files from an S3 bucket to a local directory in the Docker container, or makes it available as input streams. For example, if you specify an EFS location, input data files will be made available as input streams. They do not need to be downloaded.
-- * 'vpcConfig' - A 'VpcConfig' object that specifies the VPC that you want your training job to connect to. Control access to and from your training container by configuring the VPC. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
-- * 'algorithmSpecification' - The registry path of the Docker image that contains the training algorithm and algorithm-specific metadata, including the input mode. For more information about algorithms provided by Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . For information about providing your own algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
-- * 'outputDataConfig' - Specifies the path to the S3 location where you want to store model artifacts. Amazon SageMaker creates subfolders for the artifacts.
-- * 'trainingJobName' - The name of the training job. The name must be unique within an AWS Region in an AWS account.
-- * 'resourceConfig' - The resources, including the ML compute instances and ML storage volumes, to use for model training.
--
-- ML storage volumes store model artifacts and incremental states. Training algorithms might also use ML storage volumes for scratch space. If you want Amazon SageMaker to use the ML storage volume to store the training data, choose @File@ as the @TrainingInputMode@ in the algorithm specification. For distributed training algorithms, specify an instance count greater than 1.
-- * 'enableInterContainerTrafficEncryption' - To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithm in distributed training. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-encrypt.html Protect Communications Between ML Compute Instances in a Distributed Training Job> .
-- * 'tensorBoardOutputConfig' -
-- * 'tags' - An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
-- * 'roleARN' - The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
--
-- During model training, Amazon SageMaker needs your permission to read input data from an S3 bucket, download a Docker image that contains training code, write model artifacts to an S3 bucket, write logs to Amazon CloudWatch Logs, and publish metrics to Amazon CloudWatch. You grant permissions for all of these tasks to an IAM role. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
mkCreateTrainingJob ::
  -- | 'stoppingCondition'
  StoppingCondition ->
  -- | 'algorithmSpecification'
  AlgorithmSpecification ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'trainingJobName'
  Lude.Text ->
  -- | 'resourceConfig'
  ResourceConfig ->
  -- | 'roleARN'
  Lude.Text ->
  CreateTrainingJob
mkCreateTrainingJob
  pStoppingCondition_
  pAlgorithmSpecification_
  pOutputDataConfig_
  pTrainingJobName_
  pResourceConfig_
  pRoleARN_ =
    CreateTrainingJob'
      { debugHookConfig = Lude.Nothing,
        checkpointConfig = Lude.Nothing,
        stoppingCondition = pStoppingCondition_,
        enableNetworkIsolation = Lude.Nothing,
        experimentConfig = Lude.Nothing,
        debugRuleConfigurations = Lude.Nothing,
        enableManagedSpotTraining = Lude.Nothing,
        hyperParameters = Lude.Nothing,
        inputDataConfig = Lude.Nothing,
        vpcConfig = Lude.Nothing,
        algorithmSpecification = pAlgorithmSpecification_,
        outputDataConfig = pOutputDataConfig_,
        trainingJobName = pTrainingJobName_,
        resourceConfig = pResourceConfig_,
        enableInterContainerTrafficEncryption = Lude.Nothing,
        tensorBoardOutputConfig = Lude.Nothing,
        tags = Lude.Nothing,
        roleARN = pRoleARN_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'debugHookConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjDebugHookConfig :: Lens.Lens' CreateTrainingJob (Lude.Maybe DebugHookConfig)
ctjDebugHookConfig = Lens.lens (debugHookConfig :: CreateTrainingJob -> Lude.Maybe DebugHookConfig) (\s a -> s {debugHookConfig = a} :: CreateTrainingJob)
{-# DEPRECATED ctjDebugHookConfig "Use generic-lens or generic-optics with 'debugHookConfig' instead." #-}

-- | Contains information about the output location for managed spot training checkpoint data.
--
-- /Note:/ Consider using 'checkpointConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjCheckpointConfig :: Lens.Lens' CreateTrainingJob (Lude.Maybe CheckpointConfig)
ctjCheckpointConfig = Lens.lens (checkpointConfig :: CreateTrainingJob -> Lude.Maybe CheckpointConfig) (\s a -> s {checkpointConfig = a} :: CreateTrainingJob)
{-# DEPRECATED ctjCheckpointConfig "Use generic-lens or generic-optics with 'checkpointConfig' instead." #-}

-- | Specifies a limit to how long a model training job can run. When the job reaches the time limit, Amazon SageMaker ends the training job. Use this API to cap model training costs.
--
-- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts, so the results of training are not lost.
--
-- /Note:/ Consider using 'stoppingCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjStoppingCondition :: Lens.Lens' CreateTrainingJob StoppingCondition
ctjStoppingCondition = Lens.lens (stoppingCondition :: CreateTrainingJob -> StoppingCondition) (\s a -> s {stoppingCondition = a} :: CreateTrainingJob)
{-# DEPRECATED ctjStoppingCondition "Use generic-lens or generic-optics with 'stoppingCondition' instead." #-}

-- | Isolates the training container. No inbound or outbound network calls can be made, except for calls between peers within a training cluster for distributed training. If you enable network isolation for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
--
-- /Note:/ Consider using 'enableNetworkIsolation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjEnableNetworkIsolation :: Lens.Lens' CreateTrainingJob (Lude.Maybe Lude.Bool)
ctjEnableNetworkIsolation = Lens.lens (enableNetworkIsolation :: CreateTrainingJob -> Lude.Maybe Lude.Bool) (\s a -> s {enableNetworkIsolation = a} :: CreateTrainingJob)
{-# DEPRECATED ctjEnableNetworkIsolation "Use generic-lens or generic-optics with 'enableNetworkIsolation' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'experimentConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjExperimentConfig :: Lens.Lens' CreateTrainingJob (Lude.Maybe ExperimentConfig)
ctjExperimentConfig = Lens.lens (experimentConfig :: CreateTrainingJob -> Lude.Maybe ExperimentConfig) (\s a -> s {experimentConfig = a} :: CreateTrainingJob)
{-# DEPRECATED ctjExperimentConfig "Use generic-lens or generic-optics with 'experimentConfig' instead." #-}

-- | Configuration information for debugging rules.
--
-- /Note:/ Consider using 'debugRuleConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjDebugRuleConfigurations :: Lens.Lens' CreateTrainingJob (Lude.Maybe [DebugRuleConfiguration])
ctjDebugRuleConfigurations = Lens.lens (debugRuleConfigurations :: CreateTrainingJob -> Lude.Maybe [DebugRuleConfiguration]) (\s a -> s {debugRuleConfigurations = a} :: CreateTrainingJob)
{-# DEPRECATED ctjDebugRuleConfigurations "Use generic-lens or generic-optics with 'debugRuleConfigurations' instead." #-}

-- | To train models using managed spot training, choose @True@ . Managed spot training provides a fully managed and scalable infrastructure for training machine learning models. this option is useful when training jobs can be interrupted and when there is flexibility when the training job is run.
--
-- The complete and intermediate results of jobs are stored in an Amazon S3 bucket, and can be used as a starting point to train models incrementally. Amazon SageMaker provides metrics and logs in CloudWatch. They can be used to see when managed spot training jobs are running, interrupted, resumed, or completed.
--
-- /Note:/ Consider using 'enableManagedSpotTraining' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjEnableManagedSpotTraining :: Lens.Lens' CreateTrainingJob (Lude.Maybe Lude.Bool)
ctjEnableManagedSpotTraining = Lens.lens (enableManagedSpotTraining :: CreateTrainingJob -> Lude.Maybe Lude.Bool) (\s a -> s {enableManagedSpotTraining = a} :: CreateTrainingJob)
{-# DEPRECATED ctjEnableManagedSpotTraining "Use generic-lens or generic-optics with 'enableManagedSpotTraining' instead." #-}

-- | Algorithm-specific parameters that influence the quality of the model. You set hyperparameters before you start the learning process. For a list of hyperparameters for each training algorithm provided by Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> .
--
-- You can specify a maximum of 100 hyperparameters. Each hyperparameter is a key-value pair. Each key and value is limited to 256 characters, as specified by the @Length Constraint@ .
--
-- /Note:/ Consider using 'hyperParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjHyperParameters :: Lens.Lens' CreateTrainingJob (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ctjHyperParameters = Lens.lens (hyperParameters :: CreateTrainingJob -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {hyperParameters = a} :: CreateTrainingJob)
{-# DEPRECATED ctjHyperParameters "Use generic-lens or generic-optics with 'hyperParameters' instead." #-}

-- | An array of @Channel@ objects. Each channel is a named input source. @InputDataConfig@ describes the input data and its location.
--
-- Algorithms can accept input data from one or more channels. For example, an algorithm might have two channels of input data, @training_data@ and @validation_data@ . The configuration for each channel provides the S3, EFS, or FSx location where the input data is stored. It also provides information about the stored data: the MIME type, compression method, and whether the data is wrapped in RecordIO format.
-- Depending on the input mode that the algorithm supports, Amazon SageMaker either copies input data files from an S3 bucket to a local directory in the Docker container, or makes it available as input streams. For example, if you specify an EFS location, input data files will be made available as input streams. They do not need to be downloaded.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjInputDataConfig :: Lens.Lens' CreateTrainingJob (Lude.Maybe (Lude.NonEmpty Channel))
ctjInputDataConfig = Lens.lens (inputDataConfig :: CreateTrainingJob -> Lude.Maybe (Lude.NonEmpty Channel)) (\s a -> s {inputDataConfig = a} :: CreateTrainingJob)
{-# DEPRECATED ctjInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | A 'VpcConfig' object that specifies the VPC that you want your training job to connect to. Control access to and from your training container by configuring the VPC. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjVPCConfig :: Lens.Lens' CreateTrainingJob (Lude.Maybe VPCConfig)
ctjVPCConfig = Lens.lens (vpcConfig :: CreateTrainingJob -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: CreateTrainingJob)
{-# DEPRECATED ctjVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

-- | The registry path of the Docker image that contains the training algorithm and algorithm-specific metadata, including the input mode. For more information about algorithms provided by Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . For information about providing your own algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
--
-- /Note:/ Consider using 'algorithmSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjAlgorithmSpecification :: Lens.Lens' CreateTrainingJob AlgorithmSpecification
ctjAlgorithmSpecification = Lens.lens (algorithmSpecification :: CreateTrainingJob -> AlgorithmSpecification) (\s a -> s {algorithmSpecification = a} :: CreateTrainingJob)
{-# DEPRECATED ctjAlgorithmSpecification "Use generic-lens or generic-optics with 'algorithmSpecification' instead." #-}

-- | Specifies the path to the S3 location where you want to store model artifacts. Amazon SageMaker creates subfolders for the artifacts.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjOutputDataConfig :: Lens.Lens' CreateTrainingJob OutputDataConfig
ctjOutputDataConfig = Lens.lens (outputDataConfig :: CreateTrainingJob -> OutputDataConfig) (\s a -> s {outputDataConfig = a} :: CreateTrainingJob)
{-# DEPRECATED ctjOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The name of the training job. The name must be unique within an AWS Region in an AWS account.
--
-- /Note:/ Consider using 'trainingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjTrainingJobName :: Lens.Lens' CreateTrainingJob Lude.Text
ctjTrainingJobName = Lens.lens (trainingJobName :: CreateTrainingJob -> Lude.Text) (\s a -> s {trainingJobName = a} :: CreateTrainingJob)
{-# DEPRECATED ctjTrainingJobName "Use generic-lens or generic-optics with 'trainingJobName' instead." #-}

-- | The resources, including the ML compute instances and ML storage volumes, to use for model training.
--
-- ML storage volumes store model artifacts and incremental states. Training algorithms might also use ML storage volumes for scratch space. If you want Amazon SageMaker to use the ML storage volume to store the training data, choose @File@ as the @TrainingInputMode@ in the algorithm specification. For distributed training algorithms, specify an instance count greater than 1.
--
-- /Note:/ Consider using 'resourceConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjResourceConfig :: Lens.Lens' CreateTrainingJob ResourceConfig
ctjResourceConfig = Lens.lens (resourceConfig :: CreateTrainingJob -> ResourceConfig) (\s a -> s {resourceConfig = a} :: CreateTrainingJob)
{-# DEPRECATED ctjResourceConfig "Use generic-lens or generic-optics with 'resourceConfig' instead." #-}

-- | To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithm in distributed training. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-encrypt.html Protect Communications Between ML Compute Instances in a Distributed Training Job> .
--
-- /Note:/ Consider using 'enableInterContainerTrafficEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjEnableInterContainerTrafficEncryption :: Lens.Lens' CreateTrainingJob (Lude.Maybe Lude.Bool)
ctjEnableInterContainerTrafficEncryption = Lens.lens (enableInterContainerTrafficEncryption :: CreateTrainingJob -> Lude.Maybe Lude.Bool) (\s a -> s {enableInterContainerTrafficEncryption = a} :: CreateTrainingJob)
{-# DEPRECATED ctjEnableInterContainerTrafficEncryption "Use generic-lens or generic-optics with 'enableInterContainerTrafficEncryption' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tensorBoardOutputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjTensorBoardOutputConfig :: Lens.Lens' CreateTrainingJob (Lude.Maybe TensorBoardOutputConfig)
ctjTensorBoardOutputConfig = Lens.lens (tensorBoardOutputConfig :: CreateTrainingJob -> Lude.Maybe TensorBoardOutputConfig) (\s a -> s {tensorBoardOutputConfig = a} :: CreateTrainingJob)
{-# DEPRECATED ctjTensorBoardOutputConfig "Use generic-lens or generic-optics with 'tensorBoardOutputConfig' instead." #-}

-- | An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjTags :: Lens.Lens' CreateTrainingJob (Lude.Maybe [Tag])
ctjTags = Lens.lens (tags :: CreateTrainingJob -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateTrainingJob)
{-# DEPRECATED ctjTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
--
-- During model training, Amazon SageMaker needs your permission to read input data from an S3 bucket, download a Docker image that contains training code, write model artifacts to an S3 bucket, write logs to Amazon CloudWatch Logs, and publish metrics to Amazon CloudWatch. You grant permissions for all of these tasks to an IAM role. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjRoleARN :: Lens.Lens' CreateTrainingJob Lude.Text
ctjRoleARN = Lens.lens (roleARN :: CreateTrainingJob -> Lude.Text) (\s a -> s {roleARN = a} :: CreateTrainingJob)
{-# DEPRECATED ctjRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest CreateTrainingJob where
  type Rs CreateTrainingJob = CreateTrainingJobResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateTrainingJobResponse'
            Lude.<$> (x Lude..:> "TrainingJobArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
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
            Lude.Just ("StoppingCondition" Lude..= stoppingCondition),
            ("EnableNetworkIsolation" Lude..=) Lude.<$> enableNetworkIsolation,
            ("ExperimentConfig" Lude..=) Lude.<$> experimentConfig,
            ("DebugRuleConfigurations" Lude..=)
              Lude.<$> debugRuleConfigurations,
            ("EnableManagedSpotTraining" Lude..=)
              Lude.<$> enableManagedSpotTraining,
            ("HyperParameters" Lude..=) Lude.<$> hyperParameters,
            ("InputDataConfig" Lude..=) Lude.<$> inputDataConfig,
            ("VpcConfig" Lude..=) Lude.<$> vpcConfig,
            Lude.Just
              ("AlgorithmSpecification" Lude..= algorithmSpecification),
            Lude.Just ("OutputDataConfig" Lude..= outputDataConfig),
            Lude.Just ("TrainingJobName" Lude..= trainingJobName),
            Lude.Just ("ResourceConfig" Lude..= resourceConfig),
            ("EnableInterContainerTrafficEncryption" Lude..=)
              Lude.<$> enableInterContainerTrafficEncryption,
            ("TensorBoardOutputConfig" Lude..=)
              Lude.<$> tensorBoardOutputConfig,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("RoleArn" Lude..= roleARN)
          ]
      )

instance Lude.ToPath CreateTrainingJob where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTrainingJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateTrainingJobResponse' smart constructor.
data CreateTrainingJobResponse = CreateTrainingJobResponse'
  { -- | The Amazon Resource Name (ARN) of the training job.
    trainingJobARN :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTrainingJobResponse' with the minimum fields required to make a request.
--
-- * 'trainingJobARN' - The Amazon Resource Name (ARN) of the training job.
-- * 'responseStatus' - The response status code.
mkCreateTrainingJobResponse ::
  -- | 'trainingJobARN'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateTrainingJobResponse
mkCreateTrainingJobResponse pTrainingJobARN_ pResponseStatus_ =
  CreateTrainingJobResponse'
    { trainingJobARN = pTrainingJobARN_,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the training job.
--
-- /Note:/ Consider using 'trainingJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjfrsTrainingJobARN :: Lens.Lens' CreateTrainingJobResponse Lude.Text
ctjfrsTrainingJobARN = Lens.lens (trainingJobARN :: CreateTrainingJobResponse -> Lude.Text) (\s a -> s {trainingJobARN = a} :: CreateTrainingJobResponse)
{-# DEPRECATED ctjfrsTrainingJobARN "Use generic-lens or generic-optics with 'trainingJobARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjfrsResponseStatus :: Lens.Lens' CreateTrainingJobResponse Lude.Int
ctjfrsResponseStatus = Lens.lens (responseStatus :: CreateTrainingJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTrainingJobResponse)
{-# DEPRECATED ctjfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
