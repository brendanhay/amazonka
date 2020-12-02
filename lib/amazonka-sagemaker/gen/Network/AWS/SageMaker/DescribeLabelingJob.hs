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
-- Module      : Network.AWS.SageMaker.DescribeLabelingJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a labeling job.
module Network.AWS.SageMaker.DescribeLabelingJob
  ( -- * Creating a Request
    describeLabelingJob,
    DescribeLabelingJob,

    -- * Request Lenses
    dljLabelingJobName,

    -- * Destructuring the Response
    describeLabelingJobResponse,
    DescribeLabelingJobResponse,

    -- * Response Lenses
    dljrsFailureReason,
    dljrsLabelingJobAlgorithmsConfig,
    dljrsLabelCategoryConfigS3URI,
    dljrsStoppingConditions,
    dljrsLabelAttributeName,
    dljrsLabelingJobOutput,
    dljrsTags,
    dljrsResponseStatus,
    dljrsLabelingJobStatus,
    dljrsLabelCounters,
    dljrsCreationTime,
    dljrsLastModifiedTime,
    dljrsJobReferenceCode,
    dljrsLabelingJobName,
    dljrsLabelingJobARN,
    dljrsInputConfig,
    dljrsOutputConfig,
    dljrsRoleARN,
    dljrsHumanTaskConfig,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'describeLabelingJob' smart constructor.
newtype DescribeLabelingJob = DescribeLabelingJob'
  { _dljLabelingJobName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeLabelingJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dljLabelingJobName' - The name of the labeling job to return information for.
describeLabelingJob ::
  -- | 'dljLabelingJobName'
  Text ->
  DescribeLabelingJob
describeLabelingJob pLabelingJobName_ =
  DescribeLabelingJob' {_dljLabelingJobName = pLabelingJobName_}

-- | The name of the labeling job to return information for.
dljLabelingJobName :: Lens' DescribeLabelingJob Text
dljLabelingJobName = lens _dljLabelingJobName (\s a -> s {_dljLabelingJobName = a})

instance AWSRequest DescribeLabelingJob where
  type Rs DescribeLabelingJob = DescribeLabelingJobResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          DescribeLabelingJobResponse'
            <$> (x .?> "FailureReason")
            <*> (x .?> "LabelingJobAlgorithmsConfig")
            <*> (x .?> "LabelCategoryConfigS3Uri")
            <*> (x .?> "StoppingConditions")
            <*> (x .?> "LabelAttributeName")
            <*> (x .?> "LabelingJobOutput")
            <*> (x .?> "Tags" .!@ mempty)
            <*> (pure (fromEnum s))
            <*> (x .:> "LabelingJobStatus")
            <*> (x .:> "LabelCounters")
            <*> (x .:> "CreationTime")
            <*> (x .:> "LastModifiedTime")
            <*> (x .:> "JobReferenceCode")
            <*> (x .:> "LabelingJobName")
            <*> (x .:> "LabelingJobArn")
            <*> (x .:> "InputConfig")
            <*> (x .:> "OutputConfig")
            <*> (x .:> "RoleArn")
            <*> (x .:> "HumanTaskConfig")
      )

instance Hashable DescribeLabelingJob

instance NFData DescribeLabelingJob

instance ToHeaders DescribeLabelingJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.DescribeLabelingJob" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeLabelingJob where
  toJSON DescribeLabelingJob' {..} =
    object
      (catMaybes [Just ("LabelingJobName" .= _dljLabelingJobName)])

instance ToPath DescribeLabelingJob where
  toPath = const "/"

instance ToQuery DescribeLabelingJob where
  toQuery = const mempty

-- | /See:/ 'describeLabelingJobResponse' smart constructor.
data DescribeLabelingJobResponse = DescribeLabelingJobResponse'
  { _dljrsFailureReason ::
      !(Maybe Text),
    _dljrsLabelingJobAlgorithmsConfig ::
      !( Maybe
           LabelingJobAlgorithmsConfig
       ),
    _dljrsLabelCategoryConfigS3URI ::
      !(Maybe Text),
    _dljrsStoppingConditions ::
      !( Maybe
           LabelingJobStoppingConditions
       ),
    _dljrsLabelAttributeName ::
      !(Maybe Text),
    _dljrsLabelingJobOutput ::
      !(Maybe LabelingJobOutput),
    _dljrsTags :: !(Maybe [Tag]),
    _dljrsResponseStatus :: !Int,
    _dljrsLabelingJobStatus ::
      !LabelingJobStatus,
    _dljrsLabelCounters ::
      !LabelCounters,
    _dljrsCreationTime :: !POSIX,
    _dljrsLastModifiedTime :: !POSIX,
    _dljrsJobReferenceCode :: !Text,
    _dljrsLabelingJobName :: !Text,
    _dljrsLabelingJobARN :: !Text,
    _dljrsInputConfig ::
      !LabelingJobInputConfig,
    _dljrsOutputConfig ::
      !LabelingJobOutputConfig,
    _dljrsRoleARN :: !Text,
    _dljrsHumanTaskConfig ::
      !HumanTaskConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeLabelingJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dljrsFailureReason' - If the job failed, the reason that it failed.
--
-- * 'dljrsLabelingJobAlgorithmsConfig' - Configuration information for automated data labeling.
--
-- * 'dljrsLabelCategoryConfigS3URI' - The S3 location of the JSON file that defines the categories used to label data objects. Please note the following label-category limits:     * Semantic segmentation labeling jobs using automated labeling: 20 labels     * Box bounding labeling jobs (all): 10 labels The file is a JSON structure in the following format: @{@  @"document-version": "2018-11-28"@  @"labels": [@  @{@  @"label": "/label 1/ "@  @},@  @{@  @"label": "/label 2/ "@  @},@  @...@  @{@  @"label": "/label n/ "@  @}@  @]@  @}@
--
-- * 'dljrsStoppingConditions' - A set of conditions for stopping a labeling job. If any of the conditions are met, the job is automatically stopped.
--
-- * 'dljrsLabelAttributeName' - The attribute used as the label in the output manifest file.
--
-- * 'dljrsLabelingJobOutput' - The location of the output produced by the labeling job.
--
-- * 'dljrsTags' - An array of key/value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- * 'dljrsResponseStatus' - -- | The response status code.
--
-- * 'dljrsLabelingJobStatus' - The processing status of the labeling job.
--
-- * 'dljrsLabelCounters' - Provides a breakdown of the number of data objects labeled by humans, the number of objects labeled by machine, the number of objects than couldn't be labeled, and the total number of objects labeled.
--
-- * 'dljrsCreationTime' - The date and time that the labeling job was created.
--
-- * 'dljrsLastModifiedTime' - The date and time that the labeling job was last updated.
--
-- * 'dljrsJobReferenceCode' - A unique identifier for work done as part of a labeling job.
--
-- * 'dljrsLabelingJobName' - The name assigned to the labeling job when it was created.
--
-- * 'dljrsLabelingJobARN' - The Amazon Resource Name (ARN) of the labeling job.
--
-- * 'dljrsInputConfig' - Input configuration information for the labeling job, such as the Amazon S3 location of the data objects and the location of the manifest file that describes the data objects.
--
-- * 'dljrsOutputConfig' - The location of the job's output data and the AWS Key Management Service key ID for the key used to encrypt the output data, if any.
--
-- * 'dljrsRoleARN' - The Amazon Resource Name (ARN) that Amazon SageMaker assumes to perform tasks on your behalf during data labeling.
--
-- * 'dljrsHumanTaskConfig' - Configuration information required for human workers to complete a labeling task.
describeLabelingJobResponse ::
  -- | 'dljrsResponseStatus'
  Int ->
  -- | 'dljrsLabelingJobStatus'
  LabelingJobStatus ->
  -- | 'dljrsLabelCounters'
  LabelCounters ->
  -- | 'dljrsCreationTime'
  UTCTime ->
  -- | 'dljrsLastModifiedTime'
  UTCTime ->
  -- | 'dljrsJobReferenceCode'
  Text ->
  -- | 'dljrsLabelingJobName'
  Text ->
  -- | 'dljrsLabelingJobARN'
  Text ->
  -- | 'dljrsInputConfig'
  LabelingJobInputConfig ->
  -- | 'dljrsOutputConfig'
  LabelingJobOutputConfig ->
  -- | 'dljrsRoleARN'
  Text ->
  -- | 'dljrsHumanTaskConfig'
  HumanTaskConfig ->
  DescribeLabelingJobResponse
describeLabelingJobResponse
  pResponseStatus_
  pLabelingJobStatus_
  pLabelCounters_
  pCreationTime_
  pLastModifiedTime_
  pJobReferenceCode_
  pLabelingJobName_
  pLabelingJobARN_
  pInputConfig_
  pOutputConfig_
  pRoleARN_
  pHumanTaskConfig_ =
    DescribeLabelingJobResponse'
      { _dljrsFailureReason = Nothing,
        _dljrsLabelingJobAlgorithmsConfig = Nothing,
        _dljrsLabelCategoryConfigS3URI = Nothing,
        _dljrsStoppingConditions = Nothing,
        _dljrsLabelAttributeName = Nothing,
        _dljrsLabelingJobOutput = Nothing,
        _dljrsTags = Nothing,
        _dljrsResponseStatus = pResponseStatus_,
        _dljrsLabelingJobStatus = pLabelingJobStatus_,
        _dljrsLabelCounters = pLabelCounters_,
        _dljrsCreationTime = _Time # pCreationTime_,
        _dljrsLastModifiedTime = _Time # pLastModifiedTime_,
        _dljrsJobReferenceCode = pJobReferenceCode_,
        _dljrsLabelingJobName = pLabelingJobName_,
        _dljrsLabelingJobARN = pLabelingJobARN_,
        _dljrsInputConfig = pInputConfig_,
        _dljrsOutputConfig = pOutputConfig_,
        _dljrsRoleARN = pRoleARN_,
        _dljrsHumanTaskConfig = pHumanTaskConfig_
      }

-- | If the job failed, the reason that it failed.
dljrsFailureReason :: Lens' DescribeLabelingJobResponse (Maybe Text)
dljrsFailureReason = lens _dljrsFailureReason (\s a -> s {_dljrsFailureReason = a})

-- | Configuration information for automated data labeling.
dljrsLabelingJobAlgorithmsConfig :: Lens' DescribeLabelingJobResponse (Maybe LabelingJobAlgorithmsConfig)
dljrsLabelingJobAlgorithmsConfig = lens _dljrsLabelingJobAlgorithmsConfig (\s a -> s {_dljrsLabelingJobAlgorithmsConfig = a})

-- | The S3 location of the JSON file that defines the categories used to label data objects. Please note the following label-category limits:     * Semantic segmentation labeling jobs using automated labeling: 20 labels     * Box bounding labeling jobs (all): 10 labels The file is a JSON structure in the following format: @{@  @"document-version": "2018-11-28"@  @"labels": [@  @{@  @"label": "/label 1/ "@  @},@  @{@  @"label": "/label 2/ "@  @},@  @...@  @{@  @"label": "/label n/ "@  @}@  @]@  @}@
dljrsLabelCategoryConfigS3URI :: Lens' DescribeLabelingJobResponse (Maybe Text)
dljrsLabelCategoryConfigS3URI = lens _dljrsLabelCategoryConfigS3URI (\s a -> s {_dljrsLabelCategoryConfigS3URI = a})

-- | A set of conditions for stopping a labeling job. If any of the conditions are met, the job is automatically stopped.
dljrsStoppingConditions :: Lens' DescribeLabelingJobResponse (Maybe LabelingJobStoppingConditions)
dljrsStoppingConditions = lens _dljrsStoppingConditions (\s a -> s {_dljrsStoppingConditions = a})

-- | The attribute used as the label in the output manifest file.
dljrsLabelAttributeName :: Lens' DescribeLabelingJobResponse (Maybe Text)
dljrsLabelAttributeName = lens _dljrsLabelAttributeName (\s a -> s {_dljrsLabelAttributeName = a})

-- | The location of the output produced by the labeling job.
dljrsLabelingJobOutput :: Lens' DescribeLabelingJobResponse (Maybe LabelingJobOutput)
dljrsLabelingJobOutput = lens _dljrsLabelingJobOutput (\s a -> s {_dljrsLabelingJobOutput = a})

-- | An array of key/value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
dljrsTags :: Lens' DescribeLabelingJobResponse [Tag]
dljrsTags = lens _dljrsTags (\s a -> s {_dljrsTags = a}) . _Default . _Coerce

-- | -- | The response status code.
dljrsResponseStatus :: Lens' DescribeLabelingJobResponse Int
dljrsResponseStatus = lens _dljrsResponseStatus (\s a -> s {_dljrsResponseStatus = a})

-- | The processing status of the labeling job.
dljrsLabelingJobStatus :: Lens' DescribeLabelingJobResponse LabelingJobStatus
dljrsLabelingJobStatus = lens _dljrsLabelingJobStatus (\s a -> s {_dljrsLabelingJobStatus = a})

-- | Provides a breakdown of the number of data objects labeled by humans, the number of objects labeled by machine, the number of objects than couldn't be labeled, and the total number of objects labeled.
dljrsLabelCounters :: Lens' DescribeLabelingJobResponse LabelCounters
dljrsLabelCounters = lens _dljrsLabelCounters (\s a -> s {_dljrsLabelCounters = a})

-- | The date and time that the labeling job was created.
dljrsCreationTime :: Lens' DescribeLabelingJobResponse UTCTime
dljrsCreationTime = lens _dljrsCreationTime (\s a -> s {_dljrsCreationTime = a}) . _Time

-- | The date and time that the labeling job was last updated.
dljrsLastModifiedTime :: Lens' DescribeLabelingJobResponse UTCTime
dljrsLastModifiedTime = lens _dljrsLastModifiedTime (\s a -> s {_dljrsLastModifiedTime = a}) . _Time

-- | A unique identifier for work done as part of a labeling job.
dljrsJobReferenceCode :: Lens' DescribeLabelingJobResponse Text
dljrsJobReferenceCode = lens _dljrsJobReferenceCode (\s a -> s {_dljrsJobReferenceCode = a})

-- | The name assigned to the labeling job when it was created.
dljrsLabelingJobName :: Lens' DescribeLabelingJobResponse Text
dljrsLabelingJobName = lens _dljrsLabelingJobName (\s a -> s {_dljrsLabelingJobName = a})

-- | The Amazon Resource Name (ARN) of the labeling job.
dljrsLabelingJobARN :: Lens' DescribeLabelingJobResponse Text
dljrsLabelingJobARN = lens _dljrsLabelingJobARN (\s a -> s {_dljrsLabelingJobARN = a})

-- | Input configuration information for the labeling job, such as the Amazon S3 location of the data objects and the location of the manifest file that describes the data objects.
dljrsInputConfig :: Lens' DescribeLabelingJobResponse LabelingJobInputConfig
dljrsInputConfig = lens _dljrsInputConfig (\s a -> s {_dljrsInputConfig = a})

-- | The location of the job's output data and the AWS Key Management Service key ID for the key used to encrypt the output data, if any.
dljrsOutputConfig :: Lens' DescribeLabelingJobResponse LabelingJobOutputConfig
dljrsOutputConfig = lens _dljrsOutputConfig (\s a -> s {_dljrsOutputConfig = a})

-- | The Amazon Resource Name (ARN) that Amazon SageMaker assumes to perform tasks on your behalf during data labeling.
dljrsRoleARN :: Lens' DescribeLabelingJobResponse Text
dljrsRoleARN = lens _dljrsRoleARN (\s a -> s {_dljrsRoleARN = a})

-- | Configuration information required for human workers to complete a labeling task.
dljrsHumanTaskConfig :: Lens' DescribeLabelingJobResponse HumanTaskConfig
dljrsHumanTaskConfig = lens _dljrsHumanTaskConfig (\s a -> s {_dljrsHumanTaskConfig = a})

instance NFData DescribeLabelingJobResponse
