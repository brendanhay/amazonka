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
-- If you choose to host your model using Amazon SageMaker hosting services, you can use the resulting model artifacts as part of the model. You can also use the artifacts in a deep learning service other than Amazon SageMaker, provided that you know how to use them for inferences.
--
-- In the request body, you provide the following:
--
--     * @AlgorithmSpecification@ - Identifies the training algorithm to use.
--
--     * @HyperParameters@ - Specify these algorithm-specific parameters to influence the quality of the final model. For a list of hyperparameters for each training algorithm provided by Amazon SageMaker, see <http://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> .
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
-- For more information about Amazon SageMaker, see <http://docs.aws.amazon.com/sagemaker/latest/dg/how-it-works.html How It Works> .
--
module Network.AWS.SageMaker.CreateTrainingJob
    (
    -- * Creating a Request
      createTrainingJob
    , CreateTrainingJob
    -- * Request Lenses
    , ctjHyperParameters
    , ctjVPCConfig
    , ctjTags
    , ctjTrainingJobName
    , ctjAlgorithmSpecification
    , ctjRoleARN
    , ctjInputDataConfig
    , ctjOutputDataConfig
    , ctjResourceConfig
    , ctjStoppingCondition

    -- * Destructuring the Response
    , createTrainingJobResponse
    , CreateTrainingJobResponse
    -- * Response Lenses
    , ctjrsResponseStatus
    , ctjrsTrainingJobARN
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'createTrainingJob' smart constructor.
data CreateTrainingJob = CreateTrainingJob'
  { _ctjHyperParameters        :: !(Maybe (Map Text Text))
  , _ctjVPCConfig              :: !(Maybe VPCConfig)
  , _ctjTags                   :: !(Maybe [Tag])
  , _ctjTrainingJobName        :: !Text
  , _ctjAlgorithmSpecification :: !AlgorithmSpecification
  , _ctjRoleARN                :: !Text
  , _ctjInputDataConfig        :: !(List1 Channel)
  , _ctjOutputDataConfig       :: !OutputDataConfig
  , _ctjResourceConfig         :: !ResourceConfig
  , _ctjStoppingCondition      :: !StoppingCondition
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTrainingJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctjHyperParameters' - Algorithm-specific parameters. You set hyperparameters before you start the learning process. Hyperparameters influence the quality of the model. For a list of hyperparameters for each training algorithm provided by Amazon SageMaker, see <http://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> .  You can specify a maximum of 100 hyperparameters. Each hyperparameter is a key-value pair. Each key and value is limited to 256 characters, as specified by the @Length Constraint@ .
--
-- * 'ctjVPCConfig' - A object that specifies the VPC that you want your training job to connect to. Control access to and from your training container by configuring the VPC. For more information, see 'train-vpc'
--
-- * 'ctjTags' - An array of key-value pairs. For more information, see <http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- * 'ctjTrainingJobName' - The name of the training job. The name must be unique within an AWS Region in an AWS account. It appears in the Amazon SageMaker console.
--
-- * 'ctjAlgorithmSpecification' - The registry path of the Docker image that contains the training algorithm and algorithm-specific metadata, including the input mode. For more information about algorithms provided by Amazon SageMaker, see <http://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . For information about providing your own algorithms, see 'your-algorithms' .
--
-- * 'ctjRoleARN' - The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.  During model training, Amazon SageMaker needs your permission to read input data from an S3 bucket, download a Docker image that contains training code, write model artifacts to an S3 bucket, write logs to Amazon CloudWatch Logs, and publish metrics to Amazon CloudWatch. You grant permissions for all of these tasks to an IAM role. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
--
-- * 'ctjInputDataConfig' - An array of @Channel@ objects. Each channel is a named input source. @InputDataConfig@ describes the input data and its location.  Algorithms can accept input data from one or more channels. For example, an algorithm might have two channels of input data, @training_data@ and @validation_data@ . The configuration for each channel provides the S3 location where the input data is stored. It also provides information about the stored data: the MIME type, compression method, and whether the data is wrapped in RecordIO format.  Depending on the input mode that the algorithm supports, Amazon SageMaker either copies input data files from an S3 bucket to a local directory in the Docker container, or makes it available as input streams.
--
-- * 'ctjOutputDataConfig' - Specifies the path to the S3 bucket where you want to store model artifacts. Amazon SageMaker creates subfolders for the artifacts.
--
-- * 'ctjResourceConfig' - The resources, including the ML compute instances and ML storage volumes, to use for model training.  ML storage volumes store model artifacts and incremental states. Training algorithms might also use ML storage volumes for scratch space. If you want Amazon SageMaker to use the ML storage volume to store the training data, choose @File@ as the @TrainingInputMode@ in the algorithm specification. For distributed training algorithms, specify an instance count greater than 1.
--
-- * 'ctjStoppingCondition' - Sets a duration for training. Use this parameter to cap model training costs. To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms might use this 120-second window to save the model artifacts.  When Amazon SageMaker terminates a job because the stopping condition has been met, training algorithms provided by Amazon SageMaker save the intermediate results of the job. This intermediate data is a valid model artifact. You can use it to create a model using the @CreateModel@ API.
createTrainingJob
    :: Text -- ^ 'ctjTrainingJobName'
    -> AlgorithmSpecification -- ^ 'ctjAlgorithmSpecification'
    -> Text -- ^ 'ctjRoleARN'
    -> NonEmpty Channel -- ^ 'ctjInputDataConfig'
    -> OutputDataConfig -- ^ 'ctjOutputDataConfig'
    -> ResourceConfig -- ^ 'ctjResourceConfig'
    -> StoppingCondition -- ^ 'ctjStoppingCondition'
    -> CreateTrainingJob
createTrainingJob pTrainingJobName_ pAlgorithmSpecification_ pRoleARN_ pInputDataConfig_ pOutputDataConfig_ pResourceConfig_ pStoppingCondition_ =
  CreateTrainingJob'
    { _ctjHyperParameters = Nothing
    , _ctjVPCConfig = Nothing
    , _ctjTags = Nothing
    , _ctjTrainingJobName = pTrainingJobName_
    , _ctjAlgorithmSpecification = pAlgorithmSpecification_
    , _ctjRoleARN = pRoleARN_
    , _ctjInputDataConfig = _List1 # pInputDataConfig_
    , _ctjOutputDataConfig = pOutputDataConfig_
    , _ctjResourceConfig = pResourceConfig_
    , _ctjStoppingCondition = pStoppingCondition_
    }


-- | Algorithm-specific parameters. You set hyperparameters before you start the learning process. Hyperparameters influence the quality of the model. For a list of hyperparameters for each training algorithm provided by Amazon SageMaker, see <http://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> .  You can specify a maximum of 100 hyperparameters. Each hyperparameter is a key-value pair. Each key and value is limited to 256 characters, as specified by the @Length Constraint@ .
ctjHyperParameters :: Lens' CreateTrainingJob (HashMap Text Text)
ctjHyperParameters = lens _ctjHyperParameters (\ s a -> s{_ctjHyperParameters = a}) . _Default . _Map

-- | A object that specifies the VPC that you want your training job to connect to. Control access to and from your training container by configuring the VPC. For more information, see 'train-vpc'
ctjVPCConfig :: Lens' CreateTrainingJob (Maybe VPCConfig)
ctjVPCConfig = lens _ctjVPCConfig (\ s a -> s{_ctjVPCConfig = a})

-- | An array of key-value pairs. For more information, see <http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
ctjTags :: Lens' CreateTrainingJob [Tag]
ctjTags = lens _ctjTags (\ s a -> s{_ctjTags = a}) . _Default . _Coerce

-- | The name of the training job. The name must be unique within an AWS Region in an AWS account. It appears in the Amazon SageMaker console.
ctjTrainingJobName :: Lens' CreateTrainingJob Text
ctjTrainingJobName = lens _ctjTrainingJobName (\ s a -> s{_ctjTrainingJobName = a})

-- | The registry path of the Docker image that contains the training algorithm and algorithm-specific metadata, including the input mode. For more information about algorithms provided by Amazon SageMaker, see <http://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . For information about providing your own algorithms, see 'your-algorithms' .
ctjAlgorithmSpecification :: Lens' CreateTrainingJob AlgorithmSpecification
ctjAlgorithmSpecification = lens _ctjAlgorithmSpecification (\ s a -> s{_ctjAlgorithmSpecification = a})

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.  During model training, Amazon SageMaker needs your permission to read input data from an S3 bucket, download a Docker image that contains training code, write model artifacts to an S3 bucket, write logs to Amazon CloudWatch Logs, and publish metrics to Amazon CloudWatch. You grant permissions for all of these tasks to an IAM role. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
ctjRoleARN :: Lens' CreateTrainingJob Text
ctjRoleARN = lens _ctjRoleARN (\ s a -> s{_ctjRoleARN = a})

-- | An array of @Channel@ objects. Each channel is a named input source. @InputDataConfig@ describes the input data and its location.  Algorithms can accept input data from one or more channels. For example, an algorithm might have two channels of input data, @training_data@ and @validation_data@ . The configuration for each channel provides the S3 location where the input data is stored. It also provides information about the stored data: the MIME type, compression method, and whether the data is wrapped in RecordIO format.  Depending on the input mode that the algorithm supports, Amazon SageMaker either copies input data files from an S3 bucket to a local directory in the Docker container, or makes it available as input streams.
ctjInputDataConfig :: Lens' CreateTrainingJob (NonEmpty Channel)
ctjInputDataConfig = lens _ctjInputDataConfig (\ s a -> s{_ctjInputDataConfig = a}) . _List1

-- | Specifies the path to the S3 bucket where you want to store model artifacts. Amazon SageMaker creates subfolders for the artifacts.
ctjOutputDataConfig :: Lens' CreateTrainingJob OutputDataConfig
ctjOutputDataConfig = lens _ctjOutputDataConfig (\ s a -> s{_ctjOutputDataConfig = a})

-- | The resources, including the ML compute instances and ML storage volumes, to use for model training.  ML storage volumes store model artifacts and incremental states. Training algorithms might also use ML storage volumes for scratch space. If you want Amazon SageMaker to use the ML storage volume to store the training data, choose @File@ as the @TrainingInputMode@ in the algorithm specification. For distributed training algorithms, specify an instance count greater than 1.
ctjResourceConfig :: Lens' CreateTrainingJob ResourceConfig
ctjResourceConfig = lens _ctjResourceConfig (\ s a -> s{_ctjResourceConfig = a})

-- | Sets a duration for training. Use this parameter to cap model training costs. To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms might use this 120-second window to save the model artifacts.  When Amazon SageMaker terminates a job because the stopping condition has been met, training algorithms provided by Amazon SageMaker save the intermediate results of the job. This intermediate data is a valid model artifact. You can use it to create a model using the @CreateModel@ API.
ctjStoppingCondition :: Lens' CreateTrainingJob StoppingCondition
ctjStoppingCondition = lens _ctjStoppingCondition (\ s a -> s{_ctjStoppingCondition = a})

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
                 [("HyperParameters" .=) <$> _ctjHyperParameters,
                  ("VpcConfig" .=) <$> _ctjVPCConfig,
                  ("Tags" .=) <$> _ctjTags,
                  Just ("TrainingJobName" .= _ctjTrainingJobName),
                  Just
                    ("AlgorithmSpecification" .=
                       _ctjAlgorithmSpecification),
                  Just ("RoleArn" .= _ctjRoleARN),
                  Just ("InputDataConfig" .= _ctjInputDataConfig),
                  Just ("OutputDataConfig" .= _ctjOutputDataConfig),
                  Just ("ResourceConfig" .= _ctjResourceConfig),
                  Just ("StoppingCondition" .= _ctjStoppingCondition)])

instance ToPath CreateTrainingJob where
        toPath = const "/"

instance ToQuery CreateTrainingJob where
        toQuery = const mempty

-- | /See:/ 'createTrainingJobResponse' smart constructor.
data CreateTrainingJobResponse = CreateTrainingJobResponse'
  { _ctjrsResponseStatus :: !Int
  , _ctjrsTrainingJobARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTrainingJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctjrsResponseStatus' - -- | The response status code.
--
-- * 'ctjrsTrainingJobARN' - The Amazon Resource Name (ARN) of the training job.
createTrainingJobResponse
    :: Int -- ^ 'ctjrsResponseStatus'
    -> Text -- ^ 'ctjrsTrainingJobARN'
    -> CreateTrainingJobResponse
createTrainingJobResponse pResponseStatus_ pTrainingJobARN_ =
  CreateTrainingJobResponse'
    { _ctjrsResponseStatus = pResponseStatus_
    , _ctjrsTrainingJobARN = pTrainingJobARN_
    }


-- | -- | The response status code.
ctjrsResponseStatus :: Lens' CreateTrainingJobResponse Int
ctjrsResponseStatus = lens _ctjrsResponseStatus (\ s a -> s{_ctjrsResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the training job.
ctjrsTrainingJobARN :: Lens' CreateTrainingJobResponse Text
ctjrsTrainingJobARN = lens _ctjrsTrainingJobARN (\ s a -> s{_ctjrsTrainingJobARN = a})

instance NFData CreateTrainingJobResponse where
