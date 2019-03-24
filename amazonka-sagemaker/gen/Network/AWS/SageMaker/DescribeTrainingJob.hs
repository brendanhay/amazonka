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
    , drsLabelingJobARN
    , drsFailureReason
    , drsSecondaryStatusTransitions
    , drsTrainingEndTime
    , drsEnableNetworkIsolation
    , drsLastModifiedTime
    , drsHyperParameters
    , drsInputDataConfig
    , drsVPCConfig
    , drsFinalMetricDataList
    , drsOutputDataConfig
    , drsTrainingStartTime
    , drsTuningJobARN
    , drsEnableInterContainerTrafficEncryption
    , drsRoleARN
    , drsResponseStatus
    , drsTrainingJobName
    , drsTrainingJobARN
    , drsModelArtifacts
    , drsTrainingJobStatus
    , drsSecondaryStatus
    , drsAlgorithmSpecification
    , drsResourceConfig
    , drsStoppingCondition
    , drsCreationTime
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
                   (x .?> "LabelingJobArn") <*> (x .?> "FailureReason")
                     <*> (x .?> "SecondaryStatusTransitions" .!@ mempty)
                     <*> (x .?> "TrainingEndTime")
                     <*> (x .?> "EnableNetworkIsolation")
                     <*> (x .?> "LastModifiedTime")
                     <*> (x .?> "HyperParameters" .!@ mempty)
                     <*> (x .?> "InputDataConfig")
                     <*> (x .?> "VpcConfig")
                     <*> (x .?> "FinalMetricDataList" .!@ mempty)
                     <*> (x .?> "OutputDataConfig")
                     <*> (x .?> "TrainingStartTime")
                     <*> (x .?> "TuningJobArn")
                     <*> (x .?> "EnableInterContainerTrafficEncryption")
                     <*> (x .?> "RoleArn")
                     <*> (pure (fromEnum s))
                     <*> (x .:> "TrainingJobName")
                     <*> (x .:> "TrainingJobArn")
                     <*> (x .:> "ModelArtifacts")
                     <*> (x .:> "TrainingJobStatus")
                     <*> (x .:> "SecondaryStatus")
                     <*> (x .:> "AlgorithmSpecification")
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
  { _drsLabelingJobARN :: !(Maybe Text)
  , _drsFailureReason :: !(Maybe Text)
  , _drsSecondaryStatusTransitions :: !(Maybe [SecondaryStatusTransition])
  , _drsTrainingEndTime :: !(Maybe POSIX)
  , _drsEnableNetworkIsolation :: !(Maybe Bool)
  , _drsLastModifiedTime :: !(Maybe POSIX)
  , _drsHyperParameters :: !(Maybe (Map Text Text))
  , _drsInputDataConfig :: !(Maybe (List1 Channel))
  , _drsVPCConfig :: !(Maybe VPCConfig)
  , _drsFinalMetricDataList :: !(Maybe [MetricData])
  , _drsOutputDataConfig :: !(Maybe OutputDataConfig)
  , _drsTrainingStartTime :: !(Maybe POSIX)
  , _drsTuningJobARN :: !(Maybe Text)
  , _drsEnableInterContainerTrafficEncryption :: !(Maybe Bool)
  , _drsRoleARN :: !(Maybe Text)
  , _drsResponseStatus :: !Int
  , _drsTrainingJobName :: !Text
  , _drsTrainingJobARN :: !Text
  , _drsModelArtifacts :: !ModelArtifacts
  , _drsTrainingJobStatus :: !TrainingJobStatus
  , _drsSecondaryStatus :: !SecondaryStatus
  , _drsAlgorithmSpecification :: !AlgorithmSpecification
  , _drsResourceConfig :: !ResourceConfig
  , _drsStoppingCondition :: !StoppingCondition
  , _drsCreationTime :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTrainingJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsLabelingJobARN' - The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth labeling job that created the transform or training job.
--
-- * 'drsFailureReason' - If the training job failed, the reason it failed.
--
-- * 'drsSecondaryStatusTransitions' - A history of all of the secondary statuses that the training job has transitioned through.
--
-- * 'drsTrainingEndTime' - Indicates the time when the training job ends on training instances. You are billed for the time interval between the value of @TrainingStartTime@ and this time. For successful jobs and stopped jobs, this is the time after model artifacts are uploaded. For failed jobs, this is the time when Amazon SageMaker detects a job failure.
--
-- * 'drsEnableNetworkIsolation' - If you want to allow inbound or outbound network calls, except for calls between peers within a training cluster for distributed training, choose @True@ . If you enable network isolation for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
--
-- * 'drsLastModifiedTime' - A timestamp that indicates when the status of the training job was last modified.
--
-- * 'drsHyperParameters' - Algorithm-specific parameters.
--
-- * 'drsInputDataConfig' - An array of @Channel@ objects that describes each data input channel.
--
-- * 'drsVPCConfig' - A 'VpcConfig' object that specifies the VPC that this training job has access to. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
--
-- * 'drsFinalMetricDataList' - A collection of @MetricData@ objects that specify the names, values, and dates and times that the training algorithm emitted to Amazon CloudWatch.
--
-- * 'drsOutputDataConfig' - The S3 path where model artifacts that you configured when creating the job are stored. Amazon SageMaker creates subfolders for model artifacts.
--
-- * 'drsTrainingStartTime' - Indicates the time when the training job starts on training instances. You are billed for the time interval between this time and the value of @TrainingEndTime@ . The start time in CloudWatch Logs might be later than this time. The difference is due to the time it takes to download the training data and to the size of the training container.
--
-- * 'drsTuningJobARN' - The Amazon Resource Name (ARN) of the associated hyperparameter tuning job if the training job was launched by a hyperparameter tuning job.
--
-- * 'drsEnableInterContainerTrafficEncryption' - To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithm in distributed training.
--
-- * 'drsRoleARN' - The AWS Identity and Access Management (IAM) role configured for the training job.
--
-- * 'drsResponseStatus' - -- | The response status code.
--
-- * 'drsTrainingJobName' - Name of the model training job.
--
-- * 'drsTrainingJobARN' - The Amazon Resource Name (ARN) of the training job.
--
-- * 'drsModelArtifacts' - Information about the Amazon S3 location that is configured for storing model artifacts.
--
-- * 'drsTrainingJobStatus' - The status of the training job. Amazon SageMaker provides the following training job statuses:     * @InProgress@ - The training is in progress.     * @Completed@ - The training job has completed.     * @Failed@ - The training job has failed. To see the reason for the failure, see the @FailureReason@ field in the response to a @DescribeTrainingJobResponse@ call.     * @Stopping@ - The training job is stopping.     * @Stopped@ - The training job has stopped. For more detailed information, see @SecondaryStatus@ .
--
-- * 'drsSecondaryStatus' - Provides detailed information about the state of the training job. For detailed information on the secondary status of the training job, see @StatusMessage@ under 'SecondaryStatusTransition' . Amazon SageMaker provides primary statuses and secondary statuses that apply to each of them:     * InProgress    *     * @Starting@ - Starting the training job.     * @Downloading@ - An optional stage for algorithms that support @File@ training input mode. It indicates that data is being downloaded to the ML storage volumes.     * @Training@ - Training is in progress.     * @Uploading@ - Training is complete and the model artifacts are being uploaded to the S3 location.     * Completed    *     * @Completed@ - The training job has completed.     * Failed    *     * @Failed@ - The training job has failed. The reason for the failure is returned in the @FailureReason@ field of @DescribeTrainingJobResponse@ .     * Stopped    *     * @MaxRuntimeExceeded@ - The job stopped because it exceeded the maximum allowed runtime.     * @Stopped@ - The training job has stopped.     * Stopping    *     * @Stopping@ - Stopping the training job. /Important:/ Valid values for @SecondaryStatus@ are subject to change.  We no longer support the following secondary statuses:     * @LaunchingMLInstances@      * @PreparingTrainingStack@      * @DownloadingTrainingImage@
--
-- * 'drsAlgorithmSpecification' - Information about the algorithm used for training, and algorithm metadata.
--
-- * 'drsResourceConfig' - Resources, including ML compute instances and ML storage volumes, that are configured for model training.
--
-- * 'drsStoppingCondition' - The condition under which to stop the training job.
--
-- * 'drsCreationTime' - A timestamp that indicates when the training job was created.
describeTrainingJobResponse
    :: Int -- ^ 'drsResponseStatus'
    -> Text -- ^ 'drsTrainingJobName'
    -> Text -- ^ 'drsTrainingJobARN'
    -> ModelArtifacts -- ^ 'drsModelArtifacts'
    -> TrainingJobStatus -- ^ 'drsTrainingJobStatus'
    -> SecondaryStatus -- ^ 'drsSecondaryStatus'
    -> AlgorithmSpecification -- ^ 'drsAlgorithmSpecification'
    -> ResourceConfig -- ^ 'drsResourceConfig'
    -> StoppingCondition -- ^ 'drsStoppingCondition'
    -> UTCTime -- ^ 'drsCreationTime'
    -> DescribeTrainingJobResponse
describeTrainingJobResponse pResponseStatus_ pTrainingJobName_ pTrainingJobARN_ pModelArtifacts_ pTrainingJobStatus_ pSecondaryStatus_ pAlgorithmSpecification_ pResourceConfig_ pStoppingCondition_ pCreationTime_ =
  DescribeTrainingJobResponse'
    { _drsLabelingJobARN = Nothing
    , _drsFailureReason = Nothing
    , _drsSecondaryStatusTransitions = Nothing
    , _drsTrainingEndTime = Nothing
    , _drsEnableNetworkIsolation = Nothing
    , _drsLastModifiedTime = Nothing
    , _drsHyperParameters = Nothing
    , _drsInputDataConfig = Nothing
    , _drsVPCConfig = Nothing
    , _drsFinalMetricDataList = Nothing
    , _drsOutputDataConfig = Nothing
    , _drsTrainingStartTime = Nothing
    , _drsTuningJobARN = Nothing
    , _drsEnableInterContainerTrafficEncryption = Nothing
    , _drsRoleARN = Nothing
    , _drsResponseStatus = pResponseStatus_
    , _drsTrainingJobName = pTrainingJobName_
    , _drsTrainingJobARN = pTrainingJobARN_
    , _drsModelArtifacts = pModelArtifacts_
    , _drsTrainingJobStatus = pTrainingJobStatus_
    , _drsSecondaryStatus = pSecondaryStatus_
    , _drsAlgorithmSpecification = pAlgorithmSpecification_
    , _drsResourceConfig = pResourceConfig_
    , _drsStoppingCondition = pStoppingCondition_
    , _drsCreationTime = _Time # pCreationTime_
    }


-- | The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth labeling job that created the transform or training job.
drsLabelingJobARN :: Lens' DescribeTrainingJobResponse (Maybe Text)
drsLabelingJobARN = lens _drsLabelingJobARN (\ s a -> s{_drsLabelingJobARN = a})

-- | If the training job failed, the reason it failed.
drsFailureReason :: Lens' DescribeTrainingJobResponse (Maybe Text)
drsFailureReason = lens _drsFailureReason (\ s a -> s{_drsFailureReason = a})

-- | A history of all of the secondary statuses that the training job has transitioned through.
drsSecondaryStatusTransitions :: Lens' DescribeTrainingJobResponse [SecondaryStatusTransition]
drsSecondaryStatusTransitions = lens _drsSecondaryStatusTransitions (\ s a -> s{_drsSecondaryStatusTransitions = a}) . _Default . _Coerce

-- | Indicates the time when the training job ends on training instances. You are billed for the time interval between the value of @TrainingStartTime@ and this time. For successful jobs and stopped jobs, this is the time after model artifacts are uploaded. For failed jobs, this is the time when Amazon SageMaker detects a job failure.
drsTrainingEndTime :: Lens' DescribeTrainingJobResponse (Maybe UTCTime)
drsTrainingEndTime = lens _drsTrainingEndTime (\ s a -> s{_drsTrainingEndTime = a}) . mapping _Time

-- | If you want to allow inbound or outbound network calls, except for calls between peers within a training cluster for distributed training, choose @True@ . If you enable network isolation for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
drsEnableNetworkIsolation :: Lens' DescribeTrainingJobResponse (Maybe Bool)
drsEnableNetworkIsolation = lens _drsEnableNetworkIsolation (\ s a -> s{_drsEnableNetworkIsolation = a})

-- | A timestamp that indicates when the status of the training job was last modified.
drsLastModifiedTime :: Lens' DescribeTrainingJobResponse (Maybe UTCTime)
drsLastModifiedTime = lens _drsLastModifiedTime (\ s a -> s{_drsLastModifiedTime = a}) . mapping _Time

-- | Algorithm-specific parameters.
drsHyperParameters :: Lens' DescribeTrainingJobResponse (HashMap Text Text)
drsHyperParameters = lens _drsHyperParameters (\ s a -> s{_drsHyperParameters = a}) . _Default . _Map

-- | An array of @Channel@ objects that describes each data input channel.
drsInputDataConfig :: Lens' DescribeTrainingJobResponse (Maybe (NonEmpty Channel))
drsInputDataConfig = lens _drsInputDataConfig (\ s a -> s{_drsInputDataConfig = a}) . mapping _List1

-- | A 'VpcConfig' object that specifies the VPC that this training job has access to. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
drsVPCConfig :: Lens' DescribeTrainingJobResponse (Maybe VPCConfig)
drsVPCConfig = lens _drsVPCConfig (\ s a -> s{_drsVPCConfig = a})

-- | A collection of @MetricData@ objects that specify the names, values, and dates and times that the training algorithm emitted to Amazon CloudWatch.
drsFinalMetricDataList :: Lens' DescribeTrainingJobResponse [MetricData]
drsFinalMetricDataList = lens _drsFinalMetricDataList (\ s a -> s{_drsFinalMetricDataList = a}) . _Default . _Coerce

-- | The S3 path where model artifacts that you configured when creating the job are stored. Amazon SageMaker creates subfolders for model artifacts.
drsOutputDataConfig :: Lens' DescribeTrainingJobResponse (Maybe OutputDataConfig)
drsOutputDataConfig = lens _drsOutputDataConfig (\ s a -> s{_drsOutputDataConfig = a})

-- | Indicates the time when the training job starts on training instances. You are billed for the time interval between this time and the value of @TrainingEndTime@ . The start time in CloudWatch Logs might be later than this time. The difference is due to the time it takes to download the training data and to the size of the training container.
drsTrainingStartTime :: Lens' DescribeTrainingJobResponse (Maybe UTCTime)
drsTrainingStartTime = lens _drsTrainingStartTime (\ s a -> s{_drsTrainingStartTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the associated hyperparameter tuning job if the training job was launched by a hyperparameter tuning job.
drsTuningJobARN :: Lens' DescribeTrainingJobResponse (Maybe Text)
drsTuningJobARN = lens _drsTuningJobARN (\ s a -> s{_drsTuningJobARN = a})

-- | To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithm in distributed training.
drsEnableInterContainerTrafficEncryption :: Lens' DescribeTrainingJobResponse (Maybe Bool)
drsEnableInterContainerTrafficEncryption = lens _drsEnableInterContainerTrafficEncryption (\ s a -> s{_drsEnableInterContainerTrafficEncryption = a})

-- | The AWS Identity and Access Management (IAM) role configured for the training job.
drsRoleARN :: Lens' DescribeTrainingJobResponse (Maybe Text)
drsRoleARN = lens _drsRoleARN (\ s a -> s{_drsRoleARN = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeTrainingJobResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

-- | Name of the model training job.
drsTrainingJobName :: Lens' DescribeTrainingJobResponse Text
drsTrainingJobName = lens _drsTrainingJobName (\ s a -> s{_drsTrainingJobName = a})

-- | The Amazon Resource Name (ARN) of the training job.
drsTrainingJobARN :: Lens' DescribeTrainingJobResponse Text
drsTrainingJobARN = lens _drsTrainingJobARN (\ s a -> s{_drsTrainingJobARN = a})

-- | Information about the Amazon S3 location that is configured for storing model artifacts.
drsModelArtifacts :: Lens' DescribeTrainingJobResponse ModelArtifacts
drsModelArtifacts = lens _drsModelArtifacts (\ s a -> s{_drsModelArtifacts = a})

-- | The status of the training job. Amazon SageMaker provides the following training job statuses:     * @InProgress@ - The training is in progress.     * @Completed@ - The training job has completed.     * @Failed@ - The training job has failed. To see the reason for the failure, see the @FailureReason@ field in the response to a @DescribeTrainingJobResponse@ call.     * @Stopping@ - The training job is stopping.     * @Stopped@ - The training job has stopped. For more detailed information, see @SecondaryStatus@ .
drsTrainingJobStatus :: Lens' DescribeTrainingJobResponse TrainingJobStatus
drsTrainingJobStatus = lens _drsTrainingJobStatus (\ s a -> s{_drsTrainingJobStatus = a})

-- | Provides detailed information about the state of the training job. For detailed information on the secondary status of the training job, see @StatusMessage@ under 'SecondaryStatusTransition' . Amazon SageMaker provides primary statuses and secondary statuses that apply to each of them:     * InProgress    *     * @Starting@ - Starting the training job.     * @Downloading@ - An optional stage for algorithms that support @File@ training input mode. It indicates that data is being downloaded to the ML storage volumes.     * @Training@ - Training is in progress.     * @Uploading@ - Training is complete and the model artifacts are being uploaded to the S3 location.     * Completed    *     * @Completed@ - The training job has completed.     * Failed    *     * @Failed@ - The training job has failed. The reason for the failure is returned in the @FailureReason@ field of @DescribeTrainingJobResponse@ .     * Stopped    *     * @MaxRuntimeExceeded@ - The job stopped because it exceeded the maximum allowed runtime.     * @Stopped@ - The training job has stopped.     * Stopping    *     * @Stopping@ - Stopping the training job. /Important:/ Valid values for @SecondaryStatus@ are subject to change.  We no longer support the following secondary statuses:     * @LaunchingMLInstances@      * @PreparingTrainingStack@      * @DownloadingTrainingImage@
drsSecondaryStatus :: Lens' DescribeTrainingJobResponse SecondaryStatus
drsSecondaryStatus = lens _drsSecondaryStatus (\ s a -> s{_drsSecondaryStatus = a})

-- | Information about the algorithm used for training, and algorithm metadata.
drsAlgorithmSpecification :: Lens' DescribeTrainingJobResponse AlgorithmSpecification
drsAlgorithmSpecification = lens _drsAlgorithmSpecification (\ s a -> s{_drsAlgorithmSpecification = a})

-- | Resources, including ML compute instances and ML storage volumes, that are configured for model training.
drsResourceConfig :: Lens' DescribeTrainingJobResponse ResourceConfig
drsResourceConfig = lens _drsResourceConfig (\ s a -> s{_drsResourceConfig = a})

-- | The condition under which to stop the training job.
drsStoppingCondition :: Lens' DescribeTrainingJobResponse StoppingCondition
drsStoppingCondition = lens _drsStoppingCondition (\ s a -> s{_drsStoppingCondition = a})

-- | A timestamp that indicates when the training job was created.
drsCreationTime :: Lens' DescribeTrainingJobResponse UTCTime
drsCreationTime = lens _drsCreationTime (\ s a -> s{_drsCreationTime = a}) . _Time

instance NFData DescribeTrainingJobResponse where
