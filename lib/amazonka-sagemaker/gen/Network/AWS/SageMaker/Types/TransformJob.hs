{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TransformJob where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.BatchStrategy
import Network.AWS.SageMaker.Types.DataProcessing
import Network.AWS.SageMaker.Types.ExperimentConfig
import Network.AWS.SageMaker.Types.ModelClientConfig
import Network.AWS.SageMaker.Types.Tag
import Network.AWS.SageMaker.Types.TransformInput
import Network.AWS.SageMaker.Types.TransformJobStatus
import Network.AWS.SageMaker.Types.TransformOutput
import Network.AWS.SageMaker.Types.TransformResources

-- | A batch transform job. For information about SageMaker batch transform, see <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform.html Use Batch Transform> .
--
--
--
-- /See:/ 'transformJob' smart constructor.
data TransformJob = TransformJob'
  { _traCreationTime ::
      !(Maybe POSIX),
    _traLabelingJobARN :: !(Maybe Text),
    _traTransformJobName :: !(Maybe Text),
    _traFailureReason :: !(Maybe Text),
    _traModelClientConfig :: !(Maybe ModelClientConfig),
    _traBatchStrategy :: !(Maybe BatchStrategy),
    _traMaxPayloadInMB :: !(Maybe Nat),
    _traEnvironment :: !(Maybe (Map Text (Text))),
    _traTransformResources :: !(Maybe TransformResources),
    _traModelName :: !(Maybe Text),
    _traExperimentConfig :: !(Maybe ExperimentConfig),
    _traTransformEndTime :: !(Maybe POSIX),
    _traTransformStartTime :: !(Maybe POSIX),
    _traAutoMLJobARN :: !(Maybe Text),
    _traTransformJobStatus :: !(Maybe TransformJobStatus),
    _traTransformInput :: !(Maybe TransformInput),
    _traMaxConcurrentTransforms :: !(Maybe Nat),
    _traTransformOutput :: !(Maybe TransformOutput),
    _traDataProcessing :: !(Maybe DataProcessing),
    _traTransformJobARN :: !(Maybe Text),
    _traTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransformJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'traCreationTime' - A timestamp that shows when the transform Job was created.
--
-- * 'traLabelingJobARN' - The Amazon Resource Name (ARN) of the labeling job that created the transform job.
--
-- * 'traTransformJobName' - The name of the transform job.
--
-- * 'traFailureReason' - If the transform job failed, the reason it failed.
--
-- * 'traModelClientConfig' - Undocumented member.
--
-- * 'traBatchStrategy' - Specifies the number of records to include in a mini-batch for an HTTP inference request. A record is a single unit of input data that inference can be made on. For example, a single line in a CSV file is a record.
--
-- * 'traMaxPayloadInMB' - The maximum allowed size of the payload, in MB. A payload is the data portion of a record (without metadata). The value in @MaxPayloadInMB@ must be greater than, or equal to, the size of a single record. To estimate the size of a record in MB, divide the size of your dataset by the number of records. To ensure that the records fit within the maximum payload size, we recommend using a slightly larger value. The default value is 6 MB. For cases where the payload might be arbitrarily large and is transmitted using HTTP chunked encoding, set the value to 0. This feature works only in supported algorithms. Currently, SageMaker built-in algorithms do not support HTTP chunked encoding.
--
-- * 'traEnvironment' - The environment variables to set in the Docker container. We support up to 16 key and values entries in the map.
--
-- * 'traTransformResources' - Undocumented member.
--
-- * 'traModelName' - The name of the model associated with the transform job.
--
-- * 'traExperimentConfig' - Undocumented member.
--
-- * 'traTransformEndTime' - Indicates when the transform job has been completed, or has stopped or failed. You are billed for the time interval between this time and the value of @TransformStartTime@ .
--
-- * 'traTransformStartTime' - Indicates when the transform job starts on ML instances. You are billed for the time interval between this time and the value of @TransformEndTime@ .
--
-- * 'traAutoMLJobARN' - The Amazon Resource Name (ARN) of the AutoML job that created the transform job.
--
-- * 'traTransformJobStatus' - The status of the transform job. Transform job statuses are:     * @InProgress@ - The job is in progress.     * @Completed@ - The job has completed.     * @Failed@ - The transform job has failed. To see the reason for the failure, see the @FailureReason@ field in the response to a @DescribeTransformJob@ call.     * @Stopping@ - The transform job is stopping.     * @Stopped@ - The transform job has stopped.
--
-- * 'traTransformInput' - Undocumented member.
--
-- * 'traMaxConcurrentTransforms' - The maximum number of parallel requests that can be sent to each instance in a transform job. If @MaxConcurrentTransforms@ is set to 0 or left unset, SageMaker checks the optional execution-parameters to determine the settings for your chosen algorithm. If the execution-parameters endpoint is not enabled, the default value is 1. For built-in algorithms, you don't need to set a value for @MaxConcurrentTransforms@ .
--
-- * 'traTransformOutput' - Undocumented member.
--
-- * 'traDataProcessing' - Undocumented member.
--
-- * 'traTransformJobARN' - The Amazon Resource Name (ARN) of the transform job.
--
-- * 'traTags' - A list of tags associated with the transform job.
transformJob ::
  TransformJob
transformJob =
  TransformJob'
    { _traCreationTime = Nothing,
      _traLabelingJobARN = Nothing,
      _traTransformJobName = Nothing,
      _traFailureReason = Nothing,
      _traModelClientConfig = Nothing,
      _traBatchStrategy = Nothing,
      _traMaxPayloadInMB = Nothing,
      _traEnvironment = Nothing,
      _traTransformResources = Nothing,
      _traModelName = Nothing,
      _traExperimentConfig = Nothing,
      _traTransformEndTime = Nothing,
      _traTransformStartTime = Nothing,
      _traAutoMLJobARN = Nothing,
      _traTransformJobStatus = Nothing,
      _traTransformInput = Nothing,
      _traMaxConcurrentTransforms = Nothing,
      _traTransformOutput = Nothing,
      _traDataProcessing = Nothing,
      _traTransformJobARN = Nothing,
      _traTags = Nothing
    }

-- | A timestamp that shows when the transform Job was created.
traCreationTime :: Lens' TransformJob (Maybe UTCTime)
traCreationTime = lens _traCreationTime (\s a -> s {_traCreationTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the labeling job that created the transform job.
traLabelingJobARN :: Lens' TransformJob (Maybe Text)
traLabelingJobARN = lens _traLabelingJobARN (\s a -> s {_traLabelingJobARN = a})

-- | The name of the transform job.
traTransformJobName :: Lens' TransformJob (Maybe Text)
traTransformJobName = lens _traTransformJobName (\s a -> s {_traTransformJobName = a})

-- | If the transform job failed, the reason it failed.
traFailureReason :: Lens' TransformJob (Maybe Text)
traFailureReason = lens _traFailureReason (\s a -> s {_traFailureReason = a})

-- | Undocumented member.
traModelClientConfig :: Lens' TransformJob (Maybe ModelClientConfig)
traModelClientConfig = lens _traModelClientConfig (\s a -> s {_traModelClientConfig = a})

-- | Specifies the number of records to include in a mini-batch for an HTTP inference request. A record is a single unit of input data that inference can be made on. For example, a single line in a CSV file is a record.
traBatchStrategy :: Lens' TransformJob (Maybe BatchStrategy)
traBatchStrategy = lens _traBatchStrategy (\s a -> s {_traBatchStrategy = a})

-- | The maximum allowed size of the payload, in MB. A payload is the data portion of a record (without metadata). The value in @MaxPayloadInMB@ must be greater than, or equal to, the size of a single record. To estimate the size of a record in MB, divide the size of your dataset by the number of records. To ensure that the records fit within the maximum payload size, we recommend using a slightly larger value. The default value is 6 MB. For cases where the payload might be arbitrarily large and is transmitted using HTTP chunked encoding, set the value to 0. This feature works only in supported algorithms. Currently, SageMaker built-in algorithms do not support HTTP chunked encoding.
traMaxPayloadInMB :: Lens' TransformJob (Maybe Natural)
traMaxPayloadInMB = lens _traMaxPayloadInMB (\s a -> s {_traMaxPayloadInMB = a}) . mapping _Nat

-- | The environment variables to set in the Docker container. We support up to 16 key and values entries in the map.
traEnvironment :: Lens' TransformJob (HashMap Text (Text))
traEnvironment = lens _traEnvironment (\s a -> s {_traEnvironment = a}) . _Default . _Map

-- | Undocumented member.
traTransformResources :: Lens' TransformJob (Maybe TransformResources)
traTransformResources = lens _traTransformResources (\s a -> s {_traTransformResources = a})

-- | The name of the model associated with the transform job.
traModelName :: Lens' TransformJob (Maybe Text)
traModelName = lens _traModelName (\s a -> s {_traModelName = a})

-- | Undocumented member.
traExperimentConfig :: Lens' TransformJob (Maybe ExperimentConfig)
traExperimentConfig = lens _traExperimentConfig (\s a -> s {_traExperimentConfig = a})

-- | Indicates when the transform job has been completed, or has stopped or failed. You are billed for the time interval between this time and the value of @TransformStartTime@ .
traTransformEndTime :: Lens' TransformJob (Maybe UTCTime)
traTransformEndTime = lens _traTransformEndTime (\s a -> s {_traTransformEndTime = a}) . mapping _Time

-- | Indicates when the transform job starts on ML instances. You are billed for the time interval between this time and the value of @TransformEndTime@ .
traTransformStartTime :: Lens' TransformJob (Maybe UTCTime)
traTransformStartTime = lens _traTransformStartTime (\s a -> s {_traTransformStartTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the AutoML job that created the transform job.
traAutoMLJobARN :: Lens' TransformJob (Maybe Text)
traAutoMLJobARN = lens _traAutoMLJobARN (\s a -> s {_traAutoMLJobARN = a})

-- | The status of the transform job. Transform job statuses are:     * @InProgress@ - The job is in progress.     * @Completed@ - The job has completed.     * @Failed@ - The transform job has failed. To see the reason for the failure, see the @FailureReason@ field in the response to a @DescribeTransformJob@ call.     * @Stopping@ - The transform job is stopping.     * @Stopped@ - The transform job has stopped.
traTransformJobStatus :: Lens' TransformJob (Maybe TransformJobStatus)
traTransformJobStatus = lens _traTransformJobStatus (\s a -> s {_traTransformJobStatus = a})

-- | Undocumented member.
traTransformInput :: Lens' TransformJob (Maybe TransformInput)
traTransformInput = lens _traTransformInput (\s a -> s {_traTransformInput = a})

-- | The maximum number of parallel requests that can be sent to each instance in a transform job. If @MaxConcurrentTransforms@ is set to 0 or left unset, SageMaker checks the optional execution-parameters to determine the settings for your chosen algorithm. If the execution-parameters endpoint is not enabled, the default value is 1. For built-in algorithms, you don't need to set a value for @MaxConcurrentTransforms@ .
traMaxConcurrentTransforms :: Lens' TransformJob (Maybe Natural)
traMaxConcurrentTransforms = lens _traMaxConcurrentTransforms (\s a -> s {_traMaxConcurrentTransforms = a}) . mapping _Nat

-- | Undocumented member.
traTransformOutput :: Lens' TransformJob (Maybe TransformOutput)
traTransformOutput = lens _traTransformOutput (\s a -> s {_traTransformOutput = a})

-- | Undocumented member.
traDataProcessing :: Lens' TransformJob (Maybe DataProcessing)
traDataProcessing = lens _traDataProcessing (\s a -> s {_traDataProcessing = a})

-- | The Amazon Resource Name (ARN) of the transform job.
traTransformJobARN :: Lens' TransformJob (Maybe Text)
traTransformJobARN = lens _traTransformJobARN (\s a -> s {_traTransformJobARN = a})

-- | A list of tags associated with the transform job.
traTags :: Lens' TransformJob [Tag]
traTags = lens _traTags (\s a -> s {_traTags = a}) . _Default . _Coerce

instance FromJSON TransformJob where
  parseJSON =
    withObject
      "TransformJob"
      ( \x ->
          TransformJob'
            <$> (x .:? "CreationTime")
            <*> (x .:? "LabelingJobArn")
            <*> (x .:? "TransformJobName")
            <*> (x .:? "FailureReason")
            <*> (x .:? "ModelClientConfig")
            <*> (x .:? "BatchStrategy")
            <*> (x .:? "MaxPayloadInMB")
            <*> (x .:? "Environment" .!= mempty)
            <*> (x .:? "TransformResources")
            <*> (x .:? "ModelName")
            <*> (x .:? "ExperimentConfig")
            <*> (x .:? "TransformEndTime")
            <*> (x .:? "TransformStartTime")
            <*> (x .:? "AutoMLJobArn")
            <*> (x .:? "TransformJobStatus")
            <*> (x .:? "TransformInput")
            <*> (x .:? "MaxConcurrentTransforms")
            <*> (x .:? "TransformOutput")
            <*> (x .:? "DataProcessing")
            <*> (x .:? "TransformJobArn")
            <*> (x .:? "Tags" .!= mempty)
      )

instance Hashable TransformJob

instance NFData TransformJob
