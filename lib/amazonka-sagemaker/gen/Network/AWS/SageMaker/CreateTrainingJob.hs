{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
--
-- If you choose to host your model using Amazon SageMaker hosting services, you can use the resulting model artifacts as part of the model. You can also use the artifacts in a machine learning service other than Amazon SageMaker, provided that you know how to use them for inferences.
--
-- In the request body, you provide the following:
--
--     * @AlgorithmSpecification@ - Identifies the training algorithm to use.
--
--     * @HyperParameters@ - Specify these algorithm-specific parameters to enable the estimation of model parameters during training. Hyperparameters can be tuned to optimize this learning process. For a list of hyperparameters for each training algorithm provided by Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> .
--
--     * @InputDataConfig@ - Describes the training dataset and the Amazon S3, EFS, or FSx location where it is stored.
--
--     * @OutputDataConfig@ - Identifies the Amazon S3 bucket where you want Amazon SageMaker to save the results of model training.
--
--
--
--     * @ResourceConfig@ - Identifies the resources, ML compute instances, and ML storage volumes to deploy for model training. In distributed training, you specify more than one instance.
--
--     * @EnableManagedSpotTraining@ - Optimize the cost of training machine learning models by up to 80% by using Amazon EC2 Spot instances. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/model-managed-spot-training.html Managed Spot Training> .
--
--     * @RoleARN@ - The Amazon Resource Number (ARN) that Amazon SageMaker assumes to perform tasks on your behalf during model training. You must grant this role the necessary permissions so that Amazon SageMaker can successfully complete model training.
--
--     * @StoppingCondition@ - To help cap training costs, use @MaxRuntimeInSeconds@ to set a time limit for training. Use @MaxWaitTimeInSeconds@ to specify how long you are willing to wait for a managed spot training job to complete.
--
--
--
-- For more information about Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/how-it-works.html How It Works> .
module Network.AWS.SageMaker.CreateTrainingJob
  ( -- * Creating a Request
    createTrainingJob,
    CreateTrainingJob,

    -- * Request Lenses
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

    -- * Destructuring the Response
    createTrainingJobResponse,
    CreateTrainingJobResponse,

    -- * Response Lenses
    ctjtrsResponseStatus,
    ctjtrsTrainingJobARN,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'createTrainingJob' smart constructor.
data CreateTrainingJob = CreateTrainingJob'
  { _ctjtDebugHookConfig ::
      !(Maybe DebugHookConfig),
    _ctjtCheckpointConfig :: !(Maybe CheckpointConfig),
    _ctjtEnableNetworkIsolation :: !(Maybe Bool),
    _ctjtExperimentConfig :: !(Maybe ExperimentConfig),
    _ctjtDebugRuleConfigurations ::
      !(Maybe [DebugRuleConfiguration]),
    _ctjtEnableManagedSpotTraining :: !(Maybe Bool),
    _ctjtHyperParameters :: !(Maybe (Map Text (Text))),
    _ctjtInputDataConfig :: !(Maybe (List1 Channel)),
    _ctjtVPCConfig :: !(Maybe VPCConfig),
    _ctjtEnableInterContainerTrafficEncryption ::
      !(Maybe Bool),
    _ctjtTensorBoardOutputConfig ::
      !(Maybe TensorBoardOutputConfig),
    _ctjtTags :: !(Maybe [Tag]),
    _ctjtTrainingJobName :: !Text,
    _ctjtAlgorithmSpecification :: !AlgorithmSpecification,
    _ctjtRoleARN :: !Text,
    _ctjtOutputDataConfig :: !OutputDataConfig,
    _ctjtResourceConfig :: !ResourceConfig,
    _ctjtStoppingCondition :: !StoppingCondition
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTrainingJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctjtDebugHookConfig' - Undocumented member.
--
-- * 'ctjtCheckpointConfig' - Contains information about the output location for managed spot training checkpoint data.
--
-- * 'ctjtEnableNetworkIsolation' - Isolates the training container. No inbound or outbound network calls can be made, except for calls between peers within a training cluster for distributed training. If you enable network isolation for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
--
-- * 'ctjtExperimentConfig' - Undocumented member.
--
-- * 'ctjtDebugRuleConfigurations' - Configuration information for debugging rules.
--
-- * 'ctjtEnableManagedSpotTraining' - To train models using managed spot training, choose @True@ . Managed spot training provides a fully managed and scalable infrastructure for training machine learning models. this option is useful when training jobs can be interrupted and when there is flexibility when the training job is run.  The complete and intermediate results of jobs are stored in an Amazon S3 bucket, and can be used as a starting point to train models incrementally. Amazon SageMaker provides metrics and logs in CloudWatch. They can be used to see when managed spot training jobs are running, interrupted, resumed, or completed.
--
-- * 'ctjtHyperParameters' - Algorithm-specific parameters that influence the quality of the model. You set hyperparameters before you start the learning process. For a list of hyperparameters for each training algorithm provided by Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> .  You can specify a maximum of 100 hyperparameters. Each hyperparameter is a key-value pair. Each key and value is limited to 256 characters, as specified by the @Length Constraint@ .
--
-- * 'ctjtInputDataConfig' - An array of @Channel@ objects. Each channel is a named input source. @InputDataConfig@ describes the input data and its location.  Algorithms can accept input data from one or more channels. For example, an algorithm might have two channels of input data, @training_data@ and @validation_data@ . The configuration for each channel provides the S3, EFS, or FSx location where the input data is stored. It also provides information about the stored data: the MIME type, compression method, and whether the data is wrapped in RecordIO format.  Depending on the input mode that the algorithm supports, Amazon SageMaker either copies input data files from an S3 bucket to a local directory in the Docker container, or makes it available as input streams. For example, if you specify an EFS location, input data files will be made available as input streams. They do not need to be downloaded.
--
-- * 'ctjtVPCConfig' - A 'VpcConfig' object that specifies the VPC that you want your training job to connect to. Control access to and from your training container by configuring the VPC. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
--
-- * 'ctjtEnableInterContainerTrafficEncryption' - To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithm in distributed training. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-encrypt.html Protect Communications Between ML Compute Instances in a Distributed Training Job> .
--
-- * 'ctjtTensorBoardOutputConfig' - Undocumented member.
--
-- * 'ctjtTags' - An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- * 'ctjtTrainingJobName' - The name of the training job. The name must be unique within an AWS Region in an AWS account.
--
-- * 'ctjtAlgorithmSpecification' - The registry path of the Docker image that contains the training algorithm and algorithm-specific metadata, including the input mode. For more information about algorithms provided by Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . For information about providing your own algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
--
-- * 'ctjtRoleARN' - The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.  During model training, Amazon SageMaker needs your permission to read input data from an S3 bucket, download a Docker image that contains training code, write model artifacts to an S3 bucket, write logs to Amazon CloudWatch Logs, and publish metrics to Amazon CloudWatch. You grant permissions for all of these tasks to an IAM role. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
--
-- * 'ctjtOutputDataConfig' - Specifies the path to the S3 location where you want to store model artifacts. Amazon SageMaker creates subfolders for the artifacts.
--
-- * 'ctjtResourceConfig' - The resources, including the ML compute instances and ML storage volumes, to use for model training.  ML storage volumes store model artifacts and incremental states. Training algorithms might also use ML storage volumes for scratch space. If you want Amazon SageMaker to use the ML storage volume to store the training data, choose @File@ as the @TrainingInputMode@ in the algorithm specification. For distributed training algorithms, specify an instance count greater than 1.
--
-- * 'ctjtStoppingCondition' - Specifies a limit to how long a model training job can run. When the job reaches the time limit, Amazon SageMaker ends the training job. Use this API to cap model training costs. To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts, so the results of training are not lost.
createTrainingJob ::
  -- | 'ctjtTrainingJobName'
  Text ->
  -- | 'ctjtAlgorithmSpecification'
  AlgorithmSpecification ->
  -- | 'ctjtRoleARN'
  Text ->
  -- | 'ctjtOutputDataConfig'
  OutputDataConfig ->
  -- | 'ctjtResourceConfig'
  ResourceConfig ->
  -- | 'ctjtStoppingCondition'
  StoppingCondition ->
  CreateTrainingJob
createTrainingJob
  pTrainingJobName_
  pAlgorithmSpecification_
  pRoleARN_
  pOutputDataConfig_
  pResourceConfig_
  pStoppingCondition_ =
    CreateTrainingJob'
      { _ctjtDebugHookConfig = Nothing,
        _ctjtCheckpointConfig = Nothing,
        _ctjtEnableNetworkIsolation = Nothing,
        _ctjtExperimentConfig = Nothing,
        _ctjtDebugRuleConfigurations = Nothing,
        _ctjtEnableManagedSpotTraining = Nothing,
        _ctjtHyperParameters = Nothing,
        _ctjtInputDataConfig = Nothing,
        _ctjtVPCConfig = Nothing,
        _ctjtEnableInterContainerTrafficEncryption = Nothing,
        _ctjtTensorBoardOutputConfig = Nothing,
        _ctjtTags = Nothing,
        _ctjtTrainingJobName = pTrainingJobName_,
        _ctjtAlgorithmSpecification = pAlgorithmSpecification_,
        _ctjtRoleARN = pRoleARN_,
        _ctjtOutputDataConfig = pOutputDataConfig_,
        _ctjtResourceConfig = pResourceConfig_,
        _ctjtStoppingCondition = pStoppingCondition_
      }

-- | Undocumented member.
ctjtDebugHookConfig :: Lens' CreateTrainingJob (Maybe DebugHookConfig)
ctjtDebugHookConfig = lens _ctjtDebugHookConfig (\s a -> s {_ctjtDebugHookConfig = a})

-- | Contains information about the output location for managed spot training checkpoint data.
ctjtCheckpointConfig :: Lens' CreateTrainingJob (Maybe CheckpointConfig)
ctjtCheckpointConfig = lens _ctjtCheckpointConfig (\s a -> s {_ctjtCheckpointConfig = a})

-- | Isolates the training container. No inbound or outbound network calls can be made, except for calls between peers within a training cluster for distributed training. If you enable network isolation for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
ctjtEnableNetworkIsolation :: Lens' CreateTrainingJob (Maybe Bool)
ctjtEnableNetworkIsolation = lens _ctjtEnableNetworkIsolation (\s a -> s {_ctjtEnableNetworkIsolation = a})

-- | Undocumented member.
ctjtExperimentConfig :: Lens' CreateTrainingJob (Maybe ExperimentConfig)
ctjtExperimentConfig = lens _ctjtExperimentConfig (\s a -> s {_ctjtExperimentConfig = a})

-- | Configuration information for debugging rules.
ctjtDebugRuleConfigurations :: Lens' CreateTrainingJob [DebugRuleConfiguration]
ctjtDebugRuleConfigurations = lens _ctjtDebugRuleConfigurations (\s a -> s {_ctjtDebugRuleConfigurations = a}) . _Default . _Coerce

-- | To train models using managed spot training, choose @True@ . Managed spot training provides a fully managed and scalable infrastructure for training machine learning models. this option is useful when training jobs can be interrupted and when there is flexibility when the training job is run.  The complete and intermediate results of jobs are stored in an Amazon S3 bucket, and can be used as a starting point to train models incrementally. Amazon SageMaker provides metrics and logs in CloudWatch. They can be used to see when managed spot training jobs are running, interrupted, resumed, or completed.
ctjtEnableManagedSpotTraining :: Lens' CreateTrainingJob (Maybe Bool)
ctjtEnableManagedSpotTraining = lens _ctjtEnableManagedSpotTraining (\s a -> s {_ctjtEnableManagedSpotTraining = a})

-- | Algorithm-specific parameters that influence the quality of the model. You set hyperparameters before you start the learning process. For a list of hyperparameters for each training algorithm provided by Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> .  You can specify a maximum of 100 hyperparameters. Each hyperparameter is a key-value pair. Each key and value is limited to 256 characters, as specified by the @Length Constraint@ .
ctjtHyperParameters :: Lens' CreateTrainingJob (HashMap Text (Text))
ctjtHyperParameters = lens _ctjtHyperParameters (\s a -> s {_ctjtHyperParameters = a}) . _Default . _Map

-- | An array of @Channel@ objects. Each channel is a named input source. @InputDataConfig@ describes the input data and its location.  Algorithms can accept input data from one or more channels. For example, an algorithm might have two channels of input data, @training_data@ and @validation_data@ . The configuration for each channel provides the S3, EFS, or FSx location where the input data is stored. It also provides information about the stored data: the MIME type, compression method, and whether the data is wrapped in RecordIO format.  Depending on the input mode that the algorithm supports, Amazon SageMaker either copies input data files from an S3 bucket to a local directory in the Docker container, or makes it available as input streams. For example, if you specify an EFS location, input data files will be made available as input streams. They do not need to be downloaded.
ctjtInputDataConfig :: Lens' CreateTrainingJob (Maybe (NonEmpty Channel))
ctjtInputDataConfig = lens _ctjtInputDataConfig (\s a -> s {_ctjtInputDataConfig = a}) . mapping _List1

-- | A 'VpcConfig' object that specifies the VPC that you want your training job to connect to. Control access to and from your training container by configuring the VPC. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
ctjtVPCConfig :: Lens' CreateTrainingJob (Maybe VPCConfig)
ctjtVPCConfig = lens _ctjtVPCConfig (\s a -> s {_ctjtVPCConfig = a})

-- | To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithm in distributed training. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-encrypt.html Protect Communications Between ML Compute Instances in a Distributed Training Job> .
ctjtEnableInterContainerTrafficEncryption :: Lens' CreateTrainingJob (Maybe Bool)
ctjtEnableInterContainerTrafficEncryption = lens _ctjtEnableInterContainerTrafficEncryption (\s a -> s {_ctjtEnableInterContainerTrafficEncryption = a})

-- | Undocumented member.
ctjtTensorBoardOutputConfig :: Lens' CreateTrainingJob (Maybe TensorBoardOutputConfig)
ctjtTensorBoardOutputConfig = lens _ctjtTensorBoardOutputConfig (\s a -> s {_ctjtTensorBoardOutputConfig = a})

-- | An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
ctjtTags :: Lens' CreateTrainingJob [Tag]
ctjtTags = lens _ctjtTags (\s a -> s {_ctjtTags = a}) . _Default . _Coerce

-- | The name of the training job. The name must be unique within an AWS Region in an AWS account.
ctjtTrainingJobName :: Lens' CreateTrainingJob Text
ctjtTrainingJobName = lens _ctjtTrainingJobName (\s a -> s {_ctjtTrainingJobName = a})

-- | The registry path of the Docker image that contains the training algorithm and algorithm-specific metadata, including the input mode. For more information about algorithms provided by Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . For information about providing your own algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
ctjtAlgorithmSpecification :: Lens' CreateTrainingJob AlgorithmSpecification
ctjtAlgorithmSpecification = lens _ctjtAlgorithmSpecification (\s a -> s {_ctjtAlgorithmSpecification = a})

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.  During model training, Amazon SageMaker needs your permission to read input data from an S3 bucket, download a Docker image that contains training code, write model artifacts to an S3 bucket, write logs to Amazon CloudWatch Logs, and publish metrics to Amazon CloudWatch. You grant permissions for all of these tasks to an IAM role. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
ctjtRoleARN :: Lens' CreateTrainingJob Text
ctjtRoleARN = lens _ctjtRoleARN (\s a -> s {_ctjtRoleARN = a})

-- | Specifies the path to the S3 location where you want to store model artifacts. Amazon SageMaker creates subfolders for the artifacts.
ctjtOutputDataConfig :: Lens' CreateTrainingJob OutputDataConfig
ctjtOutputDataConfig = lens _ctjtOutputDataConfig (\s a -> s {_ctjtOutputDataConfig = a})

-- | The resources, including the ML compute instances and ML storage volumes, to use for model training.  ML storage volumes store model artifacts and incremental states. Training algorithms might also use ML storage volumes for scratch space. If you want Amazon SageMaker to use the ML storage volume to store the training data, choose @File@ as the @TrainingInputMode@ in the algorithm specification. For distributed training algorithms, specify an instance count greater than 1.
ctjtResourceConfig :: Lens' CreateTrainingJob ResourceConfig
ctjtResourceConfig = lens _ctjtResourceConfig (\s a -> s {_ctjtResourceConfig = a})

-- | Specifies a limit to how long a model training job can run. When the job reaches the time limit, Amazon SageMaker ends the training job. Use this API to cap model training costs. To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts, so the results of training are not lost.
ctjtStoppingCondition :: Lens' CreateTrainingJob StoppingCondition
ctjtStoppingCondition = lens _ctjtStoppingCondition (\s a -> s {_ctjtStoppingCondition = a})

instance AWSRequest CreateTrainingJob where
  type Rs CreateTrainingJob = CreateTrainingJobResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          CreateTrainingJobResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "TrainingJobArn")
      )

instance Hashable CreateTrainingJob

instance NFData CreateTrainingJob

instance ToHeaders CreateTrainingJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.CreateTrainingJob" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateTrainingJob where
  toJSON CreateTrainingJob' {..} =
    object
      ( catMaybes
          [ ("DebugHookConfig" .=) <$> _ctjtDebugHookConfig,
            ("CheckpointConfig" .=) <$> _ctjtCheckpointConfig,
            ("EnableNetworkIsolation" .=) <$> _ctjtEnableNetworkIsolation,
            ("ExperimentConfig" .=) <$> _ctjtExperimentConfig,
            ("DebugRuleConfigurations" .=) <$> _ctjtDebugRuleConfigurations,
            ("EnableManagedSpotTraining" .=)
              <$> _ctjtEnableManagedSpotTraining,
            ("HyperParameters" .=) <$> _ctjtHyperParameters,
            ("InputDataConfig" .=) <$> _ctjtInputDataConfig,
            ("VpcConfig" .=) <$> _ctjtVPCConfig,
            ("EnableInterContainerTrafficEncryption" .=)
              <$> _ctjtEnableInterContainerTrafficEncryption,
            ("TensorBoardOutputConfig" .=) <$> _ctjtTensorBoardOutputConfig,
            ("Tags" .=) <$> _ctjtTags,
            Just ("TrainingJobName" .= _ctjtTrainingJobName),
            Just ("AlgorithmSpecification" .= _ctjtAlgorithmSpecification),
            Just ("RoleArn" .= _ctjtRoleARN),
            Just ("OutputDataConfig" .= _ctjtOutputDataConfig),
            Just ("ResourceConfig" .= _ctjtResourceConfig),
            Just ("StoppingCondition" .= _ctjtStoppingCondition)
          ]
      )

instance ToPath CreateTrainingJob where
  toPath = const "/"

instance ToQuery CreateTrainingJob where
  toQuery = const mempty

-- | /See:/ 'createTrainingJobResponse' smart constructor.
data CreateTrainingJobResponse = CreateTrainingJobResponse'
  { _ctjtrsResponseStatus ::
      !Int,
    _ctjtrsTrainingJobARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTrainingJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctjtrsResponseStatus' - -- | The response status code.
--
-- * 'ctjtrsTrainingJobARN' - The Amazon Resource Name (ARN) of the training job.
createTrainingJobResponse ::
  -- | 'ctjtrsResponseStatus'
  Int ->
  -- | 'ctjtrsTrainingJobARN'
  Text ->
  CreateTrainingJobResponse
createTrainingJobResponse pResponseStatus_ pTrainingJobARN_ =
  CreateTrainingJobResponse'
    { _ctjtrsResponseStatus =
        pResponseStatus_,
      _ctjtrsTrainingJobARN = pTrainingJobARN_
    }

-- | -- | The response status code.
ctjtrsResponseStatus :: Lens' CreateTrainingJobResponse Int
ctjtrsResponseStatus = lens _ctjtrsResponseStatus (\s a -> s {_ctjtrsResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the training job.
ctjtrsTrainingJobARN :: Lens' CreateTrainingJobResponse Text
ctjtrsTrainingJobARN = lens _ctjtrsTrainingJobARN (\s a -> s {_ctjtrsTrainingJobARN = a})

instance NFData CreateTrainingJobResponse
