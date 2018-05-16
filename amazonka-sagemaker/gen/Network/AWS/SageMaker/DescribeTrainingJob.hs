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
-- Module      : Network.AWS.SageMaker.DescribeTrainingJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a training job.
--
--
module Network.AWS.SageMaker.DescribeTrainingJob
    (
    -- * Creating a Request
      describeTrainingJob
    , DescribeTrainingJob
    -- * Request Lenses
    , dtjTrainingJobName

    -- * Destructuring the Response
    , describeTrainingJobResponse
    , DescribeTrainingJobResponse
    -- * Response Lenses
    , dtjrsFailureReason
    , dtjrsTrainingEndTime
    , dtjrsLastModifiedTime
    , dtjrsHyperParameters
    , dtjrsVPCConfig
    , dtjrsOutputDataConfig
    , dtjrsTrainingStartTime
    , dtjrsRoleARN
    , dtjrsResponseStatus
    , dtjrsTrainingJobName
    , dtjrsTrainingJobARN
    , dtjrsModelArtifacts
    , dtjrsTrainingJobStatus
    , dtjrsSecondaryStatus
    , dtjrsAlgorithmSpecification
    , dtjrsInputDataConfig
    , dtjrsResourceConfig
    , dtjrsStoppingCondition
    , dtjrsCreationTime
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'describeTrainingJob' smart constructor.
newtype DescribeTrainingJob = DescribeTrainingJob'
  { _dtjTrainingJobName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTrainingJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtjTrainingJobName' - The name of the training job.
describeTrainingJob
    :: Text -- ^ 'dtjTrainingJobName'
    -> DescribeTrainingJob
describeTrainingJob pTrainingJobName_ =
  DescribeTrainingJob' {_dtjTrainingJobName = pTrainingJobName_}


-- | The name of the training job.
dtjTrainingJobName :: Lens' DescribeTrainingJob Text
dtjTrainingJobName = lens _dtjTrainingJobName (\ s a -> s{_dtjTrainingJobName = a})

instance AWSRequest DescribeTrainingJob where
        type Rs DescribeTrainingJob =
             DescribeTrainingJobResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTrainingJobResponse' <$>
                   (x .?> "FailureReason") <*> (x .?> "TrainingEndTime")
                     <*> (x .?> "LastModifiedTime")
                     <*> (x .?> "HyperParameters" .!@ mempty)
                     <*> (x .?> "VpcConfig")
                     <*> (x .?> "OutputDataConfig")
                     <*> (x .?> "TrainingStartTime")
                     <*> (x .?> "RoleArn")
                     <*> (pure (fromEnum s))
                     <*> (x .:> "TrainingJobName")
                     <*> (x .:> "TrainingJobArn")
                     <*> (x .:> "ModelArtifacts")
                     <*> (x .:> "TrainingJobStatus")
                     <*> (x .:> "SecondaryStatus")
                     <*> (x .:> "AlgorithmSpecification")
                     <*> (x .:> "InputDataConfig")
                     <*> (x .:> "ResourceConfig")
                     <*> (x .:> "StoppingCondition")
                     <*> (x .:> "CreationTime"))

instance Hashable DescribeTrainingJob where

instance NFData DescribeTrainingJob where

instance ToHeaders DescribeTrainingJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.DescribeTrainingJob" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeTrainingJob where
        toJSON DescribeTrainingJob'{..}
          = object
              (catMaybes
                 [Just ("TrainingJobName" .= _dtjTrainingJobName)])

instance ToPath DescribeTrainingJob where
        toPath = const "/"

instance ToQuery DescribeTrainingJob where
        toQuery = const mempty

-- | /See:/ 'describeTrainingJobResponse' smart constructor.
data DescribeTrainingJobResponse = DescribeTrainingJobResponse'
  { _dtjrsFailureReason          :: !(Maybe Text)
  , _dtjrsTrainingEndTime        :: !(Maybe POSIX)
  , _dtjrsLastModifiedTime       :: !(Maybe POSIX)
  , _dtjrsHyperParameters        :: !(Maybe (Map Text Text))
  , _dtjrsVPCConfig              :: !(Maybe VPCConfig)
  , _dtjrsOutputDataConfig       :: !(Maybe OutputDataConfig)
  , _dtjrsTrainingStartTime      :: !(Maybe POSIX)
  , _dtjrsRoleARN                :: !(Maybe Text)
  , _dtjrsResponseStatus         :: !Int
  , _dtjrsTrainingJobName        :: !Text
  , _dtjrsTrainingJobARN         :: !Text
  , _dtjrsModelArtifacts         :: !ModelArtifacts
  , _dtjrsTrainingJobStatus      :: !TrainingJobStatus
  , _dtjrsSecondaryStatus        :: !SecondaryStatus
  , _dtjrsAlgorithmSpecification :: !AlgorithmSpecification
  , _dtjrsInputDataConfig        :: !(List1 Channel)
  , _dtjrsResourceConfig         :: !ResourceConfig
  , _dtjrsStoppingCondition      :: !StoppingCondition
  , _dtjrsCreationTime           :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTrainingJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtjrsFailureReason' - If the training job failed, the reason it failed.
--
-- * 'dtjrsTrainingEndTime' - A timestamp that indicates when model training ended.
--
-- * 'dtjrsLastModifiedTime' - A timestamp that indicates when the status of the training job was last modified.
--
-- * 'dtjrsHyperParameters' - Algorithm-specific parameters.
--
-- * 'dtjrsVPCConfig' - A object that specifies the VPC that this training job has access to. For more information, see 'train-vpc' .
--
-- * 'dtjrsOutputDataConfig' - The S3 path where model artifacts that you configured when creating the job are stored. Amazon SageMaker creates subfolders for model artifacts.
--
-- * 'dtjrsTrainingStartTime' - A timestamp that indicates when training started.
--
-- * 'dtjrsRoleARN' - The AWS Identity and Access Management (IAM) role configured for the training job.
--
-- * 'dtjrsResponseStatus' - -- | The response status code.
--
-- * 'dtjrsTrainingJobName' - Name of the model training job.
--
-- * 'dtjrsTrainingJobARN' - The Amazon Resource Name (ARN) of the training job.
--
-- * 'dtjrsModelArtifacts' - Information about the Amazon S3 location that is configured for storing model artifacts.
--
-- * 'dtjrsTrainingJobStatus' - The status of the training job.  For the @InProgress@ status, Amazon SageMaker can return these secondary statuses:     * Starting - Preparing for training.     * Downloading - Optional stage for algorithms that support File training input mode. It indicates data is being downloaded to ML storage volumes.     * Training - Training is in progress.     * Uploading - Training is complete and model upload is in progress. For the @Stopped@ training status, Amazon SageMaker can return these secondary statuses:     * MaxRuntimeExceeded - Job stopped as a result of maximum allowed runtime exceeded.
--
-- * 'dtjrsSecondaryStatus' - Provides granular information about the system state. For more information, see @TrainingJobStatus@ .
--
-- * 'dtjrsAlgorithmSpecification' - Information about the algorithm used for training, and algorithm metadata.
--
-- * 'dtjrsInputDataConfig' - An array of @Channel@ objects that describes each data input channel.
--
-- * 'dtjrsResourceConfig' - Resources, including ML compute instances and ML storage volumes, that are configured for model training.
--
-- * 'dtjrsStoppingCondition' - The condition under which to stop the training job.
--
-- * 'dtjrsCreationTime' - A timestamp that indicates when the training job was created.
describeTrainingJobResponse
    :: Int -- ^ 'dtjrsResponseStatus'
    -> Text -- ^ 'dtjrsTrainingJobName'
    -> Text -- ^ 'dtjrsTrainingJobARN'
    -> ModelArtifacts -- ^ 'dtjrsModelArtifacts'
    -> TrainingJobStatus -- ^ 'dtjrsTrainingJobStatus'
    -> SecondaryStatus -- ^ 'dtjrsSecondaryStatus'
    -> AlgorithmSpecification -- ^ 'dtjrsAlgorithmSpecification'
    -> NonEmpty Channel -- ^ 'dtjrsInputDataConfig'
    -> ResourceConfig -- ^ 'dtjrsResourceConfig'
    -> StoppingCondition -- ^ 'dtjrsStoppingCondition'
    -> UTCTime -- ^ 'dtjrsCreationTime'
    -> DescribeTrainingJobResponse
describeTrainingJobResponse pResponseStatus_ pTrainingJobName_ pTrainingJobARN_ pModelArtifacts_ pTrainingJobStatus_ pSecondaryStatus_ pAlgorithmSpecification_ pInputDataConfig_ pResourceConfig_ pStoppingCondition_ pCreationTime_ =
  DescribeTrainingJobResponse'
    { _dtjrsFailureReason = Nothing
    , _dtjrsTrainingEndTime = Nothing
    , _dtjrsLastModifiedTime = Nothing
    , _dtjrsHyperParameters = Nothing
    , _dtjrsVPCConfig = Nothing
    , _dtjrsOutputDataConfig = Nothing
    , _dtjrsTrainingStartTime = Nothing
    , _dtjrsRoleARN = Nothing
    , _dtjrsResponseStatus = pResponseStatus_
    , _dtjrsTrainingJobName = pTrainingJobName_
    , _dtjrsTrainingJobARN = pTrainingJobARN_
    , _dtjrsModelArtifacts = pModelArtifacts_
    , _dtjrsTrainingJobStatus = pTrainingJobStatus_
    , _dtjrsSecondaryStatus = pSecondaryStatus_
    , _dtjrsAlgorithmSpecification = pAlgorithmSpecification_
    , _dtjrsInputDataConfig = _List1 # pInputDataConfig_
    , _dtjrsResourceConfig = pResourceConfig_
    , _dtjrsStoppingCondition = pStoppingCondition_
    , _dtjrsCreationTime = _Time # pCreationTime_
    }


-- | If the training job failed, the reason it failed.
dtjrsFailureReason :: Lens' DescribeTrainingJobResponse (Maybe Text)
dtjrsFailureReason = lens _dtjrsFailureReason (\ s a -> s{_dtjrsFailureReason = a})

-- | A timestamp that indicates when model training ended.
dtjrsTrainingEndTime :: Lens' DescribeTrainingJobResponse (Maybe UTCTime)
dtjrsTrainingEndTime = lens _dtjrsTrainingEndTime (\ s a -> s{_dtjrsTrainingEndTime = a}) . mapping _Time

-- | A timestamp that indicates when the status of the training job was last modified.
dtjrsLastModifiedTime :: Lens' DescribeTrainingJobResponse (Maybe UTCTime)
dtjrsLastModifiedTime = lens _dtjrsLastModifiedTime (\ s a -> s{_dtjrsLastModifiedTime = a}) . mapping _Time

-- | Algorithm-specific parameters.
dtjrsHyperParameters :: Lens' DescribeTrainingJobResponse (HashMap Text Text)
dtjrsHyperParameters = lens _dtjrsHyperParameters (\ s a -> s{_dtjrsHyperParameters = a}) . _Default . _Map

-- | A object that specifies the VPC that this training job has access to. For more information, see 'train-vpc' .
dtjrsVPCConfig :: Lens' DescribeTrainingJobResponse (Maybe VPCConfig)
dtjrsVPCConfig = lens _dtjrsVPCConfig (\ s a -> s{_dtjrsVPCConfig = a})

-- | The S3 path where model artifacts that you configured when creating the job are stored. Amazon SageMaker creates subfolders for model artifacts.
dtjrsOutputDataConfig :: Lens' DescribeTrainingJobResponse (Maybe OutputDataConfig)
dtjrsOutputDataConfig = lens _dtjrsOutputDataConfig (\ s a -> s{_dtjrsOutputDataConfig = a})

-- | A timestamp that indicates when training started.
dtjrsTrainingStartTime :: Lens' DescribeTrainingJobResponse (Maybe UTCTime)
dtjrsTrainingStartTime = lens _dtjrsTrainingStartTime (\ s a -> s{_dtjrsTrainingStartTime = a}) . mapping _Time

-- | The AWS Identity and Access Management (IAM) role configured for the training job.
dtjrsRoleARN :: Lens' DescribeTrainingJobResponse (Maybe Text)
dtjrsRoleARN = lens _dtjrsRoleARN (\ s a -> s{_dtjrsRoleARN = a})

-- | -- | The response status code.
dtjrsResponseStatus :: Lens' DescribeTrainingJobResponse Int
dtjrsResponseStatus = lens _dtjrsResponseStatus (\ s a -> s{_dtjrsResponseStatus = a})

-- | Name of the model training job.
dtjrsTrainingJobName :: Lens' DescribeTrainingJobResponse Text
dtjrsTrainingJobName = lens _dtjrsTrainingJobName (\ s a -> s{_dtjrsTrainingJobName = a})

-- | The Amazon Resource Name (ARN) of the training job.
dtjrsTrainingJobARN :: Lens' DescribeTrainingJobResponse Text
dtjrsTrainingJobARN = lens _dtjrsTrainingJobARN (\ s a -> s{_dtjrsTrainingJobARN = a})

-- | Information about the Amazon S3 location that is configured for storing model artifacts.
dtjrsModelArtifacts :: Lens' DescribeTrainingJobResponse ModelArtifacts
dtjrsModelArtifacts = lens _dtjrsModelArtifacts (\ s a -> s{_dtjrsModelArtifacts = a})

-- | The status of the training job.  For the @InProgress@ status, Amazon SageMaker can return these secondary statuses:     * Starting - Preparing for training.     * Downloading - Optional stage for algorithms that support File training input mode. It indicates data is being downloaded to ML storage volumes.     * Training - Training is in progress.     * Uploading - Training is complete and model upload is in progress. For the @Stopped@ training status, Amazon SageMaker can return these secondary statuses:     * MaxRuntimeExceeded - Job stopped as a result of maximum allowed runtime exceeded.
dtjrsTrainingJobStatus :: Lens' DescribeTrainingJobResponse TrainingJobStatus
dtjrsTrainingJobStatus = lens _dtjrsTrainingJobStatus (\ s a -> s{_dtjrsTrainingJobStatus = a})

-- | Provides granular information about the system state. For more information, see @TrainingJobStatus@ .
dtjrsSecondaryStatus :: Lens' DescribeTrainingJobResponse SecondaryStatus
dtjrsSecondaryStatus = lens _dtjrsSecondaryStatus (\ s a -> s{_dtjrsSecondaryStatus = a})

-- | Information about the algorithm used for training, and algorithm metadata.
dtjrsAlgorithmSpecification :: Lens' DescribeTrainingJobResponse AlgorithmSpecification
dtjrsAlgorithmSpecification = lens _dtjrsAlgorithmSpecification (\ s a -> s{_dtjrsAlgorithmSpecification = a})

-- | An array of @Channel@ objects that describes each data input channel.
dtjrsInputDataConfig :: Lens' DescribeTrainingJobResponse (NonEmpty Channel)
dtjrsInputDataConfig = lens _dtjrsInputDataConfig (\ s a -> s{_dtjrsInputDataConfig = a}) . _List1

-- | Resources, including ML compute instances and ML storage volumes, that are configured for model training.
dtjrsResourceConfig :: Lens' DescribeTrainingJobResponse ResourceConfig
dtjrsResourceConfig = lens _dtjrsResourceConfig (\ s a -> s{_dtjrsResourceConfig = a})

-- | The condition under which to stop the training job.
dtjrsStoppingCondition :: Lens' DescribeTrainingJobResponse StoppingCondition
dtjrsStoppingCondition = lens _dtjrsStoppingCondition (\ s a -> s{_dtjrsStoppingCondition = a})

-- | A timestamp that indicates when the training job was created.
dtjrsCreationTime :: Lens' DescribeTrainingJobResponse UTCTime
dtjrsCreationTime = lens _dtjrsCreationTime (\ s a -> s{_dtjrsCreationTime = a}) . _Time

instance NFData DescribeTrainingJobResponse where
