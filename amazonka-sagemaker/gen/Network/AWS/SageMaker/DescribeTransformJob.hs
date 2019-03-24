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
-- Module      : Network.AWS.SageMaker.DescribeTransformJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a transform job.
--
--
module Network.AWS.SageMaker.DescribeTransformJob
    (
    -- * Creating a Request
      describeTransformJob
    , DescribeTransformJob
    -- * Request Lenses
    , dtjTransformJobName

    -- * Destructuring the Response
    , describeTransformJobResponse
    , DescribeTransformJobResponse
    -- * Response Lenses
    , dtjrsLabelingJobARN
    , dtjrsFailureReason
    , dtjrsBatchStrategy
    , dtjrsMaxPayloadInMB
    , dtjrsEnvironment
    , dtjrsTransformEndTime
    , dtjrsTransformStartTime
    , dtjrsMaxConcurrentTransforms
    , dtjrsTransformOutput
    , dtjrsResponseStatus
    , dtjrsTransformJobName
    , dtjrsTransformJobARN
    , dtjrsTransformJobStatus
    , dtjrsModelName
    , dtjrsTransformInput
    , dtjrsTransformResources
    , dtjrsCreationTime
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'describeTransformJob' smart constructor.
newtype DescribeTransformJob = DescribeTransformJob'
  { _dtjTransformJobName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTransformJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtjTransformJobName' - The name of the transform job that you want to view details of.
describeTransformJob
    :: Text -- ^ 'dtjTransformJobName'
    -> DescribeTransformJob
describeTransformJob pTransformJobName_ =
  DescribeTransformJob' {_dtjTransformJobName = pTransformJobName_}


-- | The name of the transform job that you want to view details of.
dtjTransformJobName :: Lens' DescribeTransformJob Text
dtjTransformJobName = lens _dtjTransformJobName (\ s a -> s{_dtjTransformJobName = a})

instance AWSRequest DescribeTransformJob where
        type Rs DescribeTransformJob =
             DescribeTransformJobResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTransformJobResponse' <$>
                   (x .?> "LabelingJobArn") <*> (x .?> "FailureReason")
                     <*> (x .?> "BatchStrategy")
                     <*> (x .?> "MaxPayloadInMB")
                     <*> (x .?> "Environment" .!@ mempty)
                     <*> (x .?> "TransformEndTime")
                     <*> (x .?> "TransformStartTime")
                     <*> (x .?> "MaxConcurrentTransforms")
                     <*> (x .?> "TransformOutput")
                     <*> (pure (fromEnum s))
                     <*> (x .:> "TransformJobName")
                     <*> (x .:> "TransformJobArn")
                     <*> (x .:> "TransformJobStatus")
                     <*> (x .:> "ModelName")
                     <*> (x .:> "TransformInput")
                     <*> (x .:> "TransformResources")
                     <*> (x .:> "CreationTime"))

instance Hashable DescribeTransformJob where

instance NFData DescribeTransformJob where

instance ToHeaders DescribeTransformJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.DescribeTransformJob" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeTransformJob where
        toJSON DescribeTransformJob'{..}
          = object
              (catMaybes
                 [Just ("TransformJobName" .= _dtjTransformJobName)])

instance ToPath DescribeTransformJob where
        toPath = const "/"

instance ToQuery DescribeTransformJob where
        toQuery = const mempty

-- | /See:/ 'describeTransformJobResponse' smart constructor.
data DescribeTransformJobResponse = DescribeTransformJobResponse'
  { _dtjrsLabelingJobARN          :: !(Maybe Text)
  , _dtjrsFailureReason           :: !(Maybe Text)
  , _dtjrsBatchStrategy           :: !(Maybe BatchStrategy)
  , _dtjrsMaxPayloadInMB          :: !(Maybe Nat)
  , _dtjrsEnvironment             :: !(Maybe (Map Text Text))
  , _dtjrsTransformEndTime        :: !(Maybe POSIX)
  , _dtjrsTransformStartTime      :: !(Maybe POSIX)
  , _dtjrsMaxConcurrentTransforms :: !(Maybe Nat)
  , _dtjrsTransformOutput         :: !(Maybe TransformOutput)
  , _dtjrsResponseStatus          :: !Int
  , _dtjrsTransformJobName        :: !Text
  , _dtjrsTransformJobARN         :: !Text
  , _dtjrsTransformJobStatus      :: !TransformJobStatus
  , _dtjrsModelName               :: !Text
  , _dtjrsTransformInput          :: !TransformInput
  , _dtjrsTransformResources      :: !TransformResources
  , _dtjrsCreationTime            :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTransformJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtjrsLabelingJobARN' - The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth labeling job that created the transform or training job.
--
-- * 'dtjrsFailureReason' - If the transform job failed, @FailureReason@ describes why it failed. A transform job creates a log file, which includes error messages, and stores it as an Amazon S3 object. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/logging-cloudwatch.html Log Amazon SageMaker Events with Amazon CloudWatch> .
--
-- * 'dtjrsBatchStrategy' - Specifies the number of records to include in a mini-batch for an HTTP inference request. A /record/ // is a single unit of input data that inference can be made on. For example, a single line in a CSV file is a record.  To enable the batch strategy, you must set @SplitType@ to @Line@ , @RecordIO@ , or @TFRecord@ .
--
-- * 'dtjrsMaxPayloadInMB' - The maximum payload size, in MB, used in the transform job.
--
-- * 'dtjrsEnvironment' - The environment variables to set in the Docker container. We support up to 16 key and values entries in the map.
--
-- * 'dtjrsTransformEndTime' - Indicates when the transform job has been completed, or has stopped or failed. You are billed for the time interval between this time and the value of @TransformStartTime@ .
--
-- * 'dtjrsTransformStartTime' - Indicates when the transform job starts on ML instances. You are billed for the time interval between this time and the value of @TransformEndTime@ .
--
-- * 'dtjrsMaxConcurrentTransforms' - The maximum number of parallel requests on each instance node that can be launched in a transform job. The default value is 1.
--
-- * 'dtjrsTransformOutput' - Identifies the Amazon S3 location where you want Amazon SageMaker to save the results from the transform job.
--
-- * 'dtjrsResponseStatus' - -- | The response status code.
--
-- * 'dtjrsTransformJobName' - The name of the transform job.
--
-- * 'dtjrsTransformJobARN' - The Amazon Resource Name (ARN) of the transform job.
--
-- * 'dtjrsTransformJobStatus' - The status of the transform job. If the transform job failed, the reason is returned in the @FailureReason@ field.
--
-- * 'dtjrsModelName' - The name of the model used in the transform job.
--
-- * 'dtjrsTransformInput' - Describes the dataset to be transformed and the Amazon S3 location where it is stored.
--
-- * 'dtjrsTransformResources' - Describes the resources, including ML instance types and ML instance count, to use for the transform job.
--
-- * 'dtjrsCreationTime' - A timestamp that shows when the transform Job was created.
describeTransformJobResponse
    :: Int -- ^ 'dtjrsResponseStatus'
    -> Text -- ^ 'dtjrsTransformJobName'
    -> Text -- ^ 'dtjrsTransformJobARN'
    -> TransformJobStatus -- ^ 'dtjrsTransformJobStatus'
    -> Text -- ^ 'dtjrsModelName'
    -> TransformInput -- ^ 'dtjrsTransformInput'
    -> TransformResources -- ^ 'dtjrsTransformResources'
    -> UTCTime -- ^ 'dtjrsCreationTime'
    -> DescribeTransformJobResponse
describeTransformJobResponse pResponseStatus_ pTransformJobName_ pTransformJobARN_ pTransformJobStatus_ pModelName_ pTransformInput_ pTransformResources_ pCreationTime_ =
  DescribeTransformJobResponse'
    { _dtjrsLabelingJobARN = Nothing
    , _dtjrsFailureReason = Nothing
    , _dtjrsBatchStrategy = Nothing
    , _dtjrsMaxPayloadInMB = Nothing
    , _dtjrsEnvironment = Nothing
    , _dtjrsTransformEndTime = Nothing
    , _dtjrsTransformStartTime = Nothing
    , _dtjrsMaxConcurrentTransforms = Nothing
    , _dtjrsTransformOutput = Nothing
    , _dtjrsResponseStatus = pResponseStatus_
    , _dtjrsTransformJobName = pTransformJobName_
    , _dtjrsTransformJobARN = pTransformJobARN_
    , _dtjrsTransformJobStatus = pTransformJobStatus_
    , _dtjrsModelName = pModelName_
    , _dtjrsTransformInput = pTransformInput_
    , _dtjrsTransformResources = pTransformResources_
    , _dtjrsCreationTime = _Time # pCreationTime_
    }


-- | The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth labeling job that created the transform or training job.
dtjrsLabelingJobARN :: Lens' DescribeTransformJobResponse (Maybe Text)
dtjrsLabelingJobARN = lens _dtjrsLabelingJobARN (\ s a -> s{_dtjrsLabelingJobARN = a})

-- | If the transform job failed, @FailureReason@ describes why it failed. A transform job creates a log file, which includes error messages, and stores it as an Amazon S3 object. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/logging-cloudwatch.html Log Amazon SageMaker Events with Amazon CloudWatch> .
dtjrsFailureReason :: Lens' DescribeTransformJobResponse (Maybe Text)
dtjrsFailureReason = lens _dtjrsFailureReason (\ s a -> s{_dtjrsFailureReason = a})

-- | Specifies the number of records to include in a mini-batch for an HTTP inference request. A /record/ // is a single unit of input data that inference can be made on. For example, a single line in a CSV file is a record.  To enable the batch strategy, you must set @SplitType@ to @Line@ , @RecordIO@ , or @TFRecord@ .
dtjrsBatchStrategy :: Lens' DescribeTransformJobResponse (Maybe BatchStrategy)
dtjrsBatchStrategy = lens _dtjrsBatchStrategy (\ s a -> s{_dtjrsBatchStrategy = a})

-- | The maximum payload size, in MB, used in the transform job.
dtjrsMaxPayloadInMB :: Lens' DescribeTransformJobResponse (Maybe Natural)
dtjrsMaxPayloadInMB = lens _dtjrsMaxPayloadInMB (\ s a -> s{_dtjrsMaxPayloadInMB = a}) . mapping _Nat

-- | The environment variables to set in the Docker container. We support up to 16 key and values entries in the map.
dtjrsEnvironment :: Lens' DescribeTransformJobResponse (HashMap Text Text)
dtjrsEnvironment = lens _dtjrsEnvironment (\ s a -> s{_dtjrsEnvironment = a}) . _Default . _Map

-- | Indicates when the transform job has been completed, or has stopped or failed. You are billed for the time interval between this time and the value of @TransformStartTime@ .
dtjrsTransformEndTime :: Lens' DescribeTransformJobResponse (Maybe UTCTime)
dtjrsTransformEndTime = lens _dtjrsTransformEndTime (\ s a -> s{_dtjrsTransformEndTime = a}) . mapping _Time

-- | Indicates when the transform job starts on ML instances. You are billed for the time interval between this time and the value of @TransformEndTime@ .
dtjrsTransformStartTime :: Lens' DescribeTransformJobResponse (Maybe UTCTime)
dtjrsTransformStartTime = lens _dtjrsTransformStartTime (\ s a -> s{_dtjrsTransformStartTime = a}) . mapping _Time

-- | The maximum number of parallel requests on each instance node that can be launched in a transform job. The default value is 1.
dtjrsMaxConcurrentTransforms :: Lens' DescribeTransformJobResponse (Maybe Natural)
dtjrsMaxConcurrentTransforms = lens _dtjrsMaxConcurrentTransforms (\ s a -> s{_dtjrsMaxConcurrentTransforms = a}) . mapping _Nat

-- | Identifies the Amazon S3 location where you want Amazon SageMaker to save the results from the transform job.
dtjrsTransformOutput :: Lens' DescribeTransformJobResponse (Maybe TransformOutput)
dtjrsTransformOutput = lens _dtjrsTransformOutput (\ s a -> s{_dtjrsTransformOutput = a})

-- | -- | The response status code.
dtjrsResponseStatus :: Lens' DescribeTransformJobResponse Int
dtjrsResponseStatus = lens _dtjrsResponseStatus (\ s a -> s{_dtjrsResponseStatus = a})

-- | The name of the transform job.
dtjrsTransformJobName :: Lens' DescribeTransformJobResponse Text
dtjrsTransformJobName = lens _dtjrsTransformJobName (\ s a -> s{_dtjrsTransformJobName = a})

-- | The Amazon Resource Name (ARN) of the transform job.
dtjrsTransformJobARN :: Lens' DescribeTransformJobResponse Text
dtjrsTransformJobARN = lens _dtjrsTransformJobARN (\ s a -> s{_dtjrsTransformJobARN = a})

-- | The status of the transform job. If the transform job failed, the reason is returned in the @FailureReason@ field.
dtjrsTransformJobStatus :: Lens' DescribeTransformJobResponse TransformJobStatus
dtjrsTransformJobStatus = lens _dtjrsTransformJobStatus (\ s a -> s{_dtjrsTransformJobStatus = a})

-- | The name of the model used in the transform job.
dtjrsModelName :: Lens' DescribeTransformJobResponse Text
dtjrsModelName = lens _dtjrsModelName (\ s a -> s{_dtjrsModelName = a})

-- | Describes the dataset to be transformed and the Amazon S3 location where it is stored.
dtjrsTransformInput :: Lens' DescribeTransformJobResponse TransformInput
dtjrsTransformInput = lens _dtjrsTransformInput (\ s a -> s{_dtjrsTransformInput = a})

-- | Describes the resources, including ML instance types and ML instance count, to use for the transform job.
dtjrsTransformResources :: Lens' DescribeTransformJobResponse TransformResources
dtjrsTransformResources = lens _dtjrsTransformResources (\ s a -> s{_dtjrsTransformResources = a})

-- | A timestamp that shows when the transform Job was created.
dtjrsCreationTime :: Lens' DescribeTransformJobResponse UTCTime
dtjrsCreationTime = lens _dtjrsCreationTime (\ s a -> s{_dtjrsCreationTime = a}) . _Time

instance NFData DescribeTransformJobResponse where
