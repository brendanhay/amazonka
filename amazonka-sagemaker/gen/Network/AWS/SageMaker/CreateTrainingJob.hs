{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateTrainingJob
-- Copyright   : (c) 2013-2018 Brendan Hay
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
--     * @HyperParameters@ - Specify these algorithm-specific parameters to influence the quality of the final model. For a list of hyperparameters for each training algorithm provided by Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> .
--
--     * @InputDataConfig@ - Describes the training dataset and the Amazon S3 location where it is stored.
--
--     * @OutputDataConfig@ - Identifies the Amazon S3 location where you want Amazon SageMaker to save the results of model training.
--
--
--
--     * @ResourceConfig@ - Identifies the resources, ML compute instances, and ML storage volumes to deploy for model training. In distributed training, you specify more than one instance.
--
--     * @RoleARN@ - The Amazon Resource Number (ARN) that Amazon SageMaker assumes to perform tasks on your behalf during model training. You must grant this role the necessary permissions so that Amazon SageMaker can successfully complete model training.
--
--     * @StoppingCondition@ - Sets a duration for training. Use this parameter to cap model training costs.
--
--
--
-- For more information about Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/how-it-works.html How It Works> .
--
module Network.AWS.SageMaker.CreateTrainingJob
    (
    -- * Creating a Request
      createTrainingJob
    , CreateTrainingJob
    -- * Request Lenses
    , cEnableNetworkIsolation
    , cHyperParameters
    , cInputDataConfig
    , cVPCConfig
    , cEnableInterContainerTrafficEncryption
    , cTags
    , cTrainingJobName
    , cAlgorithmSpecification
    , cRoleARN
    , cOutputDataConfig
    , cResourceConfig
    , cStoppingCondition

    -- * Destructuring the Response
    , createTrainingJobResponse
    , CreateTrainingJobResponse
    -- * Response Lenses
    , crsResponseStatus
    , crsTrainingJobARN
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'createTrainingJob' smart constructor.
data CreateTrainingJob = CreateTrainingJob'
  { _cEnableNetworkIsolation                :: !(Maybe Bool)
  , _cHyperParameters                       :: !(Maybe (Map Text Text))
  , _cInputDataConfig                       :: !(Maybe (List1 Channel))
  , _cVPCConfig                             :: !(Maybe VPCConfig)
  , _cEnableInterContainerTrafficEncryption :: !(Maybe Bool)
  , _cTags                                  :: !(Maybe [Tag])
  , _cTrainingJobName                       :: !Text
  , _cAlgorithmSpecification                :: !AlgorithmSpecification
  , _cRoleARN                               :: !Text
  , _cOutputDataConfig                      :: !OutputDataConfig
  , _cResourceConfig                        :: !ResourceConfig
  , _cStoppingCondition                     :: !StoppingCondition
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTrainingJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cEnableNetworkIsolation' - Isolates the training container. No inbound or outbound network calls can be made, except for calls between peers within a training cluster for distributed training. If you enable network isolation for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
--
-- * 'cHyperParameters' - Algorithm-specific parameters that influence the quality of the model. You set hyperparameters before you start the learning process. For a list of hyperparameters for each training algorithm provided by Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> .  You can specify a maximum of 100 hyperparameters. Each hyperparameter is a key-value pair. Each key and value is limited to 256 characters, as specified by the @Length Constraint@ .
--
-- * 'cInputDataConfig' - An array of @Channel@ objects. Each channel is a named input source. @InputDataConfig@ describes the input data and its location.  Algorithms can accept input data from one or more channels. For example, an algorithm might have two channels of input data, @training_data@ and @validation_data@ . The configuration for each channel provides the S3 location where the input data is stored. It also provides information about the stored data: the MIME type, compression method, and whether the data is wrapped in RecordIO format.  Depending on the input mode that the algorithm supports, Amazon SageMaker either copies input data files from an S3 bucket to a local directory in the Docker container, or makes it available as input streams.
--
-- * 'cVPCConfig' - A 'VpcConfig' object that specifies the VPC that you want your training job to connect to. Control access to and from your training container by configuring the VPC. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
--
-- * 'cEnableInterContainerTrafficEncryption' - To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithm in distributed training. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-encrypt.html Protect Communications Between ML Compute Instances in a Distributed Training Job> .
--
-- * 'cTags' - An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- * 'cTrainingJobName' - The name of the training job. The name must be unique within an AWS Region in an AWS account.
--
-- * 'cAlgorithmSpecification' - The registry path of the Docker image that contains the training algorithm and algorithm-specific metadata, including the input mode. For more information about algorithms provided by Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . For information about providing your own algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
--
-- * 'cRoleARN' - The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.  During model training, Amazon SageMaker needs your permission to read input data from an S3 bucket, download a Docker image that contains training code, write model artifacts to an S3 bucket, write logs to Amazon CloudWatch Logs, and publish metrics to Amazon CloudWatch. You grant permissions for all of these tasks to an IAM role. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
--
-- * 'cOutputDataConfig' - Specifies the path to the S3 bucket where you want to store model artifacts. Amazon SageMaker creates subfolders for the artifacts.
--
-- * 'cResourceConfig' - The resources, including the ML compute instances and ML storage volumes, to use for model training.  ML storage volumes store model artifacts and incremental states. Training algorithms might also use ML storage volumes for scratch space. If you want Amazon SageMaker to use the ML storage volume to store the training data, choose @File@ as the @TrainingInputMode@ in the algorithm specification. For distributed training algorithms, specify an instance count greater than 1.
--
-- * 'cStoppingCondition' - Sets a duration for training. Use this parameter to cap model training costs. To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms might use this 120-second window to save the model artifacts.  When Amazon SageMaker terminates a job because the stopping condition has been met, training algorithms provided by Amazon SageMaker save the intermediate results of the job. This intermediate data is a valid model artifact. You can use it to create a model using the @CreateModel@ API.
createTrainingJob
    :: Text -- ^ 'cTrainingJobName'
    -> AlgorithmSpecification -- ^ 'cAlgorithmSpecification'
    -> Text -- ^ 'cRoleARN'
    -> OutputDataConfig -- ^ 'cOutputDataConfig'
    -> ResourceConfig -- ^ 'cResourceConfig'
    -> StoppingCondition -- ^ 'cStoppingCondition'
    -> CreateTrainingJob
createTrainingJob pTrainingJobName_ pAlgorithmSpecification_ pRoleARN_ pOutputDataConfig_ pResourceConfig_ pStoppingCondition_ =
  CreateTrainingJob'
    { _cEnableNetworkIsolation = Nothing
    , _cHyperParameters = Nothing
    , _cInputDataConfig = Nothing
    , _cVPCConfig = Nothing
    , _cEnableInterContainerTrafficEncryption = Nothing
    , _cTags = Nothing
    , _cTrainingJobName = pTrainingJobName_
    , _cAlgorithmSpecification = pAlgorithmSpecification_
    , _cRoleARN = pRoleARN_
    , _cOutputDataConfig = pOutputDataConfig_
    , _cResourceConfig = pResourceConfig_
    , _cStoppingCondition = pStoppingCondition_
    }


-- | Isolates the training container. No inbound or outbound network calls can be made, except for calls between peers within a training cluster for distributed training. If you enable network isolation for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
cEnableNetworkIsolation :: Lens' CreateTrainingJob (Maybe Bool)
cEnableNetworkIsolation = lens _cEnableNetworkIsolation (\ s a -> s{_cEnableNetworkIsolation = a})

-- | Algorithm-specific parameters that influence the quality of the model. You set hyperparameters before you start the learning process. For a list of hyperparameters for each training algorithm provided by Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> .  You can specify a maximum of 100 hyperparameters. Each hyperparameter is a key-value pair. Each key and value is limited to 256 characters, as specified by the @Length Constraint@ .
cHyperParameters :: Lens' CreateTrainingJob (HashMap Text Text)
cHyperParameters = lens _cHyperParameters (\ s a -> s{_cHyperParameters = a}) . _Default . _Map

-- | An array of @Channel@ objects. Each channel is a named input source. @InputDataConfig@ describes the input data and its location.  Algorithms can accept input data from one or more channels. For example, an algorithm might have two channels of input data, @training_data@ and @validation_data@ . The configuration for each channel provides the S3 location where the input data is stored. It also provides information about the stored data: the MIME type, compression method, and whether the data is wrapped in RecordIO format.  Depending on the input mode that the algorithm supports, Amazon SageMaker either copies input data files from an S3 bucket to a local directory in the Docker container, or makes it available as input streams.
cInputDataConfig :: Lens' CreateTrainingJob (Maybe (NonEmpty Channel))
cInputDataConfig = lens _cInputDataConfig (\ s a -> s{_cInputDataConfig = a}) . mapping _List1

-- | A 'VpcConfig' object that specifies the VPC that you want your training job to connect to. Control access to and from your training container by configuring the VPC. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
cVPCConfig :: Lens' CreateTrainingJob (Maybe VPCConfig)
cVPCConfig = lens _cVPCConfig (\ s a -> s{_cVPCConfig = a})

-- | To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithm in distributed training. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-encrypt.html Protect Communications Between ML Compute Instances in a Distributed Training Job> .
cEnableInterContainerTrafficEncryption :: Lens' CreateTrainingJob (Maybe Bool)
cEnableInterContainerTrafficEncryption = lens _cEnableInterContainerTrafficEncryption (\ s a -> s{_cEnableInterContainerTrafficEncryption = a})

-- | An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
cTags :: Lens' CreateTrainingJob [Tag]
cTags = lens _cTags (\ s a -> s{_cTags = a}) . _Default . _Coerce

-- | The name of the training job. The name must be unique within an AWS Region in an AWS account.
cTrainingJobName :: Lens' CreateTrainingJob Text
cTrainingJobName = lens _cTrainingJobName (\ s a -> s{_cTrainingJobName = a})

-- | The registry path of the Docker image that contains the training algorithm and algorithm-specific metadata, including the input mode. For more information about algorithms provided by Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . For information about providing your own algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
cAlgorithmSpecification :: Lens' CreateTrainingJob AlgorithmSpecification
cAlgorithmSpecification = lens _cAlgorithmSpecification (\ s a -> s{_cAlgorithmSpecification = a})

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.  During model training, Amazon SageMaker needs your permission to read input data from an S3 bucket, download a Docker image that contains training code, write model artifacts to an S3 bucket, write logs to Amazon CloudWatch Logs, and publish metrics to Amazon CloudWatch. You grant permissions for all of these tasks to an IAM role. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
cRoleARN :: Lens' CreateTrainingJob Text
cRoleARN = lens _cRoleARN (\ s a -> s{_cRoleARN = a})

-- | Specifies the path to the S3 bucket where you want to store model artifacts. Amazon SageMaker creates subfolders for the artifacts.
cOutputDataConfig :: Lens' CreateTrainingJob OutputDataConfig
cOutputDataConfig = lens _cOutputDataConfig (\ s a -> s{_cOutputDataConfig = a})

-- | The resources, including the ML compute instances and ML storage volumes, to use for model training.  ML storage volumes store model artifacts and incremental states. Training algorithms might also use ML storage volumes for scratch space. If you want Amazon SageMaker to use the ML storage volume to store the training data, choose @File@ as the @TrainingInputMode@ in the algorithm specification. For distributed training algorithms, specify an instance count greater than 1.
cResourceConfig :: Lens' CreateTrainingJob ResourceConfig
cResourceConfig = lens _cResourceConfig (\ s a -> s{_cResourceConfig = a})

-- | Sets a duration for training. Use this parameter to cap model training costs. To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms might use this 120-second window to save the model artifacts.  When Amazon SageMaker terminates a job because the stopping condition has been met, training algorithms provided by Amazon SageMaker save the intermediate results of the job. This intermediate data is a valid model artifact. You can use it to create a model using the @CreateModel@ API.
cStoppingCondition :: Lens' CreateTrainingJob StoppingCondition
cStoppingCondition = lens _cStoppingCondition (\ s a -> s{_cStoppingCondition = a})

instance AWSRequest CreateTrainingJob where
        type Rs CreateTrainingJob = CreateTrainingJobResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 CreateTrainingJobResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "TrainingJobArn"))

instance Hashable CreateTrainingJob where

instance NFData CreateTrainingJob where

instance ToHeaders CreateTrainingJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.CreateTrainingJob" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateTrainingJob where
        toJSON CreateTrainingJob'{..}
          = object
              (catMaybes
                 [("EnableNetworkIsolation" .=) <$>
                    _cEnableNetworkIsolation,
                  ("HyperParameters" .=) <$> _cHyperParameters,
                  ("InputDataConfig" .=) <$> _cInputDataConfig,
                  ("VpcConfig" .=) <$> _cVPCConfig,
                  ("EnableInterContainerTrafficEncryption" .=) <$>
                    _cEnableInterContainerTrafficEncryption,
                  ("Tags" .=) <$> _cTags,
                  Just ("TrainingJobName" .= _cTrainingJobName),
                  Just
                    ("AlgorithmSpecification" .=
                       _cAlgorithmSpecification),
                  Just ("RoleArn" .= _cRoleARN),
                  Just ("OutputDataConfig" .= _cOutputDataConfig),
                  Just ("ResourceConfig" .= _cResourceConfig),
                  Just ("StoppingCondition" .= _cStoppingCondition)])

instance ToPath CreateTrainingJob where
        toPath = const "/"

instance ToQuery CreateTrainingJob where
        toQuery = const mempty

-- | /See:/ 'createTrainingJobResponse' smart constructor.
data CreateTrainingJobResponse = CreateTrainingJobResponse'
  { _crsResponseStatus :: !Int
  , _crsTrainingJobARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTrainingJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsResponseStatus' - -- | The response status code.
--
-- * 'crsTrainingJobARN' - The Amazon Resource Name (ARN) of the training job.
createTrainingJobResponse
    :: Int -- ^ 'crsResponseStatus'
    -> Text -- ^ 'crsTrainingJobARN'
    -> CreateTrainingJobResponse
createTrainingJobResponse pResponseStatus_ pTrainingJobARN_ =
  CreateTrainingJobResponse'
    { _crsResponseStatus = pResponseStatus_
    , _crsTrainingJobARN = pTrainingJobARN_
    }


-- | -- | The response status code.
crsResponseStatus :: Lens' CreateTrainingJobResponse Int
crsResponseStatus = lens _crsResponseStatus (\ s a -> s{_crsResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the training job.
crsTrainingJobARN :: Lens' CreateTrainingJobResponse Text
crsTrainingJobARN = lens _crsTrainingJobARN (\ s a -> s{_crsTrainingJobARN = a})

instance NFData CreateTrainingJobResponse where
