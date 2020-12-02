{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingJob where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.AppSpecification
import Network.AWS.SageMaker.Types.ExperimentConfig
import Network.AWS.SageMaker.Types.NetworkConfig
import Network.AWS.SageMaker.Types.ProcessingInput
import Network.AWS.SageMaker.Types.ProcessingJobStatus
import Network.AWS.SageMaker.Types.ProcessingOutputConfig
import Network.AWS.SageMaker.Types.ProcessingResources
import Network.AWS.SageMaker.Types.ProcessingStoppingCondition
import Network.AWS.SageMaker.Types.Tag

-- | An Amazon SageMaker processing job that is used to analyze data and evaluate models. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/processing-job.html Process Data and Evaluate Models> .
--
--
--
-- /See:/ 'processingJob' smart constructor.
data ProcessingJob = ProcessingJob'
  { _pjCreationTime ::
      !(Maybe POSIX),
    _pjFailureReason :: !(Maybe Text),
    _pjMonitoringScheduleARN :: !(Maybe Text),
    _pjAppSpecification :: !(Maybe AppSpecification),
    _pjProcessingResources :: !(Maybe ProcessingResources),
    _pjEnvironment :: !(Maybe (Map Text (Text))),
    _pjProcessingJobName :: !(Maybe Text),
    _pjStoppingCondition :: !(Maybe ProcessingStoppingCondition),
    _pjExperimentConfig :: !(Maybe ExperimentConfig),
    _pjLastModifiedTime :: !(Maybe POSIX),
    _pjProcessingInputs :: !(Maybe [ProcessingInput]),
    _pjNetworkConfig :: !(Maybe NetworkConfig),
    _pjAutoMLJobARN :: !(Maybe Text),
    _pjTrainingJobARN :: !(Maybe Text),
    _pjProcessingJobStatus :: !(Maybe ProcessingJobStatus),
    _pjExitMessage :: !(Maybe Text),
    _pjProcessingOutputConfig :: !(Maybe ProcessingOutputConfig),
    _pjProcessingStartTime :: !(Maybe POSIX),
    _pjProcessingEndTime :: !(Maybe POSIX),
    _pjTags :: !(Maybe [Tag]),
    _pjProcessingJobARN :: !(Maybe Text),
    _pjRoleARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProcessingJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pjCreationTime' - The time the processing job was created.
--
-- * 'pjFailureReason' - A string, up to one KB in size, that contains the reason a processing job failed, if it failed.
--
-- * 'pjMonitoringScheduleARN' - The ARN of a monitoring schedule for an endpoint associated with this processing job.
--
-- * 'pjAppSpecification' - Undocumented member.
--
-- * 'pjProcessingResources' - Undocumented member.
--
-- * 'pjEnvironment' - Sets the environment variables in the Docker container.
--
-- * 'pjProcessingJobName' - The name of the processing job.
--
-- * 'pjStoppingCondition' - Undocumented member.
--
-- * 'pjExperimentConfig' - Undocumented member.
--
-- * 'pjLastModifiedTime' - The time the processing job was last modified.
--
-- * 'pjProcessingInputs' - For each input, data is downloaded from S3 into the processing container before the processing job begins running if "S3InputMode" is set to @File@ .
--
-- * 'pjNetworkConfig' - Undocumented member.
--
-- * 'pjAutoMLJobARN' - The Amazon Resource Name (ARN) of the AutoML job associated with this processing job.
--
-- * 'pjTrainingJobARN' - The ARN of the training job associated with this processing job.
--
-- * 'pjProcessingJobStatus' - The status of the processing job.
--
-- * 'pjExitMessage' - A string, up to one KB in size, that contains metadata from the processing container when the processing job exits.
--
-- * 'pjProcessingOutputConfig' - Undocumented member.
--
-- * 'pjProcessingStartTime' - The time that the processing job started.
--
-- * 'pjProcessingEndTime' - The time that the processing job ended.
--
-- * 'pjTags' - An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- * 'pjProcessingJobARN' - The ARN of the processing job.
--
-- * 'pjRoleARN' - The ARN of the role used to create the processing job.
processingJob ::
  ProcessingJob
processingJob =
  ProcessingJob'
    { _pjCreationTime = Nothing,
      _pjFailureReason = Nothing,
      _pjMonitoringScheduleARN = Nothing,
      _pjAppSpecification = Nothing,
      _pjProcessingResources = Nothing,
      _pjEnvironment = Nothing,
      _pjProcessingJobName = Nothing,
      _pjStoppingCondition = Nothing,
      _pjExperimentConfig = Nothing,
      _pjLastModifiedTime = Nothing,
      _pjProcessingInputs = Nothing,
      _pjNetworkConfig = Nothing,
      _pjAutoMLJobARN = Nothing,
      _pjTrainingJobARN = Nothing,
      _pjProcessingJobStatus = Nothing,
      _pjExitMessage = Nothing,
      _pjProcessingOutputConfig = Nothing,
      _pjProcessingStartTime = Nothing,
      _pjProcessingEndTime = Nothing,
      _pjTags = Nothing,
      _pjProcessingJobARN = Nothing,
      _pjRoleARN = Nothing
    }

-- | The time the processing job was created.
pjCreationTime :: Lens' ProcessingJob (Maybe UTCTime)
pjCreationTime = lens _pjCreationTime (\s a -> s {_pjCreationTime = a}) . mapping _Time

-- | A string, up to one KB in size, that contains the reason a processing job failed, if it failed.
pjFailureReason :: Lens' ProcessingJob (Maybe Text)
pjFailureReason = lens _pjFailureReason (\s a -> s {_pjFailureReason = a})

-- | The ARN of a monitoring schedule for an endpoint associated with this processing job.
pjMonitoringScheduleARN :: Lens' ProcessingJob (Maybe Text)
pjMonitoringScheduleARN = lens _pjMonitoringScheduleARN (\s a -> s {_pjMonitoringScheduleARN = a})

-- | Undocumented member.
pjAppSpecification :: Lens' ProcessingJob (Maybe AppSpecification)
pjAppSpecification = lens _pjAppSpecification (\s a -> s {_pjAppSpecification = a})

-- | Undocumented member.
pjProcessingResources :: Lens' ProcessingJob (Maybe ProcessingResources)
pjProcessingResources = lens _pjProcessingResources (\s a -> s {_pjProcessingResources = a})

-- | Sets the environment variables in the Docker container.
pjEnvironment :: Lens' ProcessingJob (HashMap Text (Text))
pjEnvironment = lens _pjEnvironment (\s a -> s {_pjEnvironment = a}) . _Default . _Map

-- | The name of the processing job.
pjProcessingJobName :: Lens' ProcessingJob (Maybe Text)
pjProcessingJobName = lens _pjProcessingJobName (\s a -> s {_pjProcessingJobName = a})

-- | Undocumented member.
pjStoppingCondition :: Lens' ProcessingJob (Maybe ProcessingStoppingCondition)
pjStoppingCondition = lens _pjStoppingCondition (\s a -> s {_pjStoppingCondition = a})

-- | Undocumented member.
pjExperimentConfig :: Lens' ProcessingJob (Maybe ExperimentConfig)
pjExperimentConfig = lens _pjExperimentConfig (\s a -> s {_pjExperimentConfig = a})

-- | The time the processing job was last modified.
pjLastModifiedTime :: Lens' ProcessingJob (Maybe UTCTime)
pjLastModifiedTime = lens _pjLastModifiedTime (\s a -> s {_pjLastModifiedTime = a}) . mapping _Time

-- | For each input, data is downloaded from S3 into the processing container before the processing job begins running if "S3InputMode" is set to @File@ .
pjProcessingInputs :: Lens' ProcessingJob [ProcessingInput]
pjProcessingInputs = lens _pjProcessingInputs (\s a -> s {_pjProcessingInputs = a}) . _Default . _Coerce

-- | Undocumented member.
pjNetworkConfig :: Lens' ProcessingJob (Maybe NetworkConfig)
pjNetworkConfig = lens _pjNetworkConfig (\s a -> s {_pjNetworkConfig = a})

-- | The Amazon Resource Name (ARN) of the AutoML job associated with this processing job.
pjAutoMLJobARN :: Lens' ProcessingJob (Maybe Text)
pjAutoMLJobARN = lens _pjAutoMLJobARN (\s a -> s {_pjAutoMLJobARN = a})

-- | The ARN of the training job associated with this processing job.
pjTrainingJobARN :: Lens' ProcessingJob (Maybe Text)
pjTrainingJobARN = lens _pjTrainingJobARN (\s a -> s {_pjTrainingJobARN = a})

-- | The status of the processing job.
pjProcessingJobStatus :: Lens' ProcessingJob (Maybe ProcessingJobStatus)
pjProcessingJobStatus = lens _pjProcessingJobStatus (\s a -> s {_pjProcessingJobStatus = a})

-- | A string, up to one KB in size, that contains metadata from the processing container when the processing job exits.
pjExitMessage :: Lens' ProcessingJob (Maybe Text)
pjExitMessage = lens _pjExitMessage (\s a -> s {_pjExitMessage = a})

-- | Undocumented member.
pjProcessingOutputConfig :: Lens' ProcessingJob (Maybe ProcessingOutputConfig)
pjProcessingOutputConfig = lens _pjProcessingOutputConfig (\s a -> s {_pjProcessingOutputConfig = a})

-- | The time that the processing job started.
pjProcessingStartTime :: Lens' ProcessingJob (Maybe UTCTime)
pjProcessingStartTime = lens _pjProcessingStartTime (\s a -> s {_pjProcessingStartTime = a}) . mapping _Time

-- | The time that the processing job ended.
pjProcessingEndTime :: Lens' ProcessingJob (Maybe UTCTime)
pjProcessingEndTime = lens _pjProcessingEndTime (\s a -> s {_pjProcessingEndTime = a}) . mapping _Time

-- | An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
pjTags :: Lens' ProcessingJob [Tag]
pjTags = lens _pjTags (\s a -> s {_pjTags = a}) . _Default . _Coerce

-- | The ARN of the processing job.
pjProcessingJobARN :: Lens' ProcessingJob (Maybe Text)
pjProcessingJobARN = lens _pjProcessingJobARN (\s a -> s {_pjProcessingJobARN = a})

-- | The ARN of the role used to create the processing job.
pjRoleARN :: Lens' ProcessingJob (Maybe Text)
pjRoleARN = lens _pjRoleARN (\s a -> s {_pjRoleARN = a})

instance FromJSON ProcessingJob where
  parseJSON =
    withObject
      "ProcessingJob"
      ( \x ->
          ProcessingJob'
            <$> (x .:? "CreationTime")
            <*> (x .:? "FailureReason")
            <*> (x .:? "MonitoringScheduleArn")
            <*> (x .:? "AppSpecification")
            <*> (x .:? "ProcessingResources")
            <*> (x .:? "Environment" .!= mempty)
            <*> (x .:? "ProcessingJobName")
            <*> (x .:? "StoppingCondition")
            <*> (x .:? "ExperimentConfig")
            <*> (x .:? "LastModifiedTime")
            <*> (x .:? "ProcessingInputs" .!= mempty)
            <*> (x .:? "NetworkConfig")
            <*> (x .:? "AutoMLJobArn")
            <*> (x .:? "TrainingJobArn")
            <*> (x .:? "ProcessingJobStatus")
            <*> (x .:? "ExitMessage")
            <*> (x .:? "ProcessingOutputConfig")
            <*> (x .:? "ProcessingStartTime")
            <*> (x .:? "ProcessingEndTime")
            <*> (x .:? "Tags" .!= mempty)
            <*> (x .:? "ProcessingJobArn")
            <*> (x .:? "RoleArn")
      )

instance Hashable ProcessingJob

instance NFData ProcessingJob
