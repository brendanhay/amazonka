{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
  ( -- * Creating a request
    DescribeLabelingJob (..),
    mkDescribeLabelingJob,

    -- ** Request lenses
    dljLabelingJobName,

    -- * Destructuring the response
    DescribeLabelingJobResponse (..),
    mkDescribeLabelingJobResponse,

    -- ** Response lenses
    dljrsCreationTime,
    dljrsLabelingJobARN,
    dljrsFailureReason,
    dljrsLabelingJobAlgorithmsConfig,
    dljrsLabelCategoryConfigS3URI,
    dljrsStoppingConditions,
    dljrsHumanTaskConfig,
    dljrsLastModifiedTime,
    dljrsLabelCounters,
    dljrsInputConfig,
    dljrsLabelAttributeName,
    dljrsLabelingJobStatus,
    dljrsOutputConfig,
    dljrsLabelingJobName,
    dljrsLabelingJobOutput,
    dljrsTags,
    dljrsJobReferenceCode,
    dljrsRoleARN,
    dljrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeLabelingJob' smart constructor.
newtype DescribeLabelingJob = DescribeLabelingJob'
  { -- | The name of the labeling job to return information for.
    labelingJobName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLabelingJob' with the minimum fields required to make a request.
--
-- * 'labelingJobName' - The name of the labeling job to return information for.
mkDescribeLabelingJob ::
  -- | 'labelingJobName'
  Lude.Text ->
  DescribeLabelingJob
mkDescribeLabelingJob pLabelingJobName_ =
  DescribeLabelingJob' {labelingJobName = pLabelingJobName_}

-- | The name of the labeling job to return information for.
--
-- /Note:/ Consider using 'labelingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljLabelingJobName :: Lens.Lens' DescribeLabelingJob Lude.Text
dljLabelingJobName = Lens.lens (labelingJobName :: DescribeLabelingJob -> Lude.Text) (\s a -> s {labelingJobName = a} :: DescribeLabelingJob)
{-# DEPRECATED dljLabelingJobName "Use generic-lens or generic-optics with 'labelingJobName' instead." #-}

instance Lude.AWSRequest DescribeLabelingJob where
  type Rs DescribeLabelingJob = DescribeLabelingJobResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeLabelingJobResponse'
            Lude.<$> (x Lude..:> "CreationTime")
            Lude.<*> (x Lude..:> "LabelingJobArn")
            Lude.<*> (x Lude..?> "FailureReason")
            Lude.<*> (x Lude..?> "LabelingJobAlgorithmsConfig")
            Lude.<*> (x Lude..?> "LabelCategoryConfigS3Uri")
            Lude.<*> (x Lude..?> "StoppingConditions")
            Lude.<*> (x Lude..:> "HumanTaskConfig")
            Lude.<*> (x Lude..:> "LastModifiedTime")
            Lude.<*> (x Lude..:> "LabelCounters")
            Lude.<*> (x Lude..:> "InputConfig")
            Lude.<*> (x Lude..?> "LabelAttributeName")
            Lude.<*> (x Lude..:> "LabelingJobStatus")
            Lude.<*> (x Lude..:> "OutputConfig")
            Lude.<*> (x Lude..:> "LabelingJobName")
            Lude.<*> (x Lude..?> "LabelingJobOutput")
            Lude.<*> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..:> "JobReferenceCode")
            Lude.<*> (x Lude..:> "RoleArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeLabelingJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DescribeLabelingJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeLabelingJob where
  toJSON DescribeLabelingJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("LabelingJobName" Lude..= labelingJobName)]
      )

instance Lude.ToPath DescribeLabelingJob where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLabelingJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeLabelingJobResponse' smart constructor.
data DescribeLabelingJobResponse = DescribeLabelingJobResponse'
  { -- | The date and time that the labeling job was created.
    creationTime :: Lude.Timestamp,
    -- | The Amazon Resource Name (ARN) of the labeling job.
    labelingJobARN :: Lude.Text,
    -- | If the job failed, the reason that it failed.
    failureReason :: Lude.Maybe Lude.Text,
    -- | Configuration information for automated data labeling.
    labelingJobAlgorithmsConfig :: Lude.Maybe LabelingJobAlgorithmsConfig,
    -- | The S3 location of the JSON file that defines the categories used to label data objects. Please note the following label-category limits:
    --
    --
    --     * Semantic segmentation labeling jobs using automated labeling: 20 labels
    --
    --
    --     * Box bounding labeling jobs (all): 10 labels
    --
    --
    -- The file is a JSON structure in the following format:
    -- @{@
    -- @"document-version": "2018-11-28"@
    -- @"labels": [@
    -- @{@
    -- @"label": "/label 1/ "@
    -- @},@
    -- @{@
    -- @"label": "/label 2/ "@
    -- @},@
    -- @...@
    -- @{@
    -- @"label": "/label n/ "@
    -- @}@
    -- @]@
    -- @}@
    labelCategoryConfigS3URI :: Lude.Maybe Lude.Text,
    -- | A set of conditions for stopping a labeling job. If any of the conditions are met, the job is automatically stopped.
    stoppingConditions :: Lude.Maybe LabelingJobStoppingConditions,
    -- | Configuration information required for human workers to complete a labeling task.
    humanTaskConfig :: HumanTaskConfig,
    -- | The date and time that the labeling job was last updated.
    lastModifiedTime :: Lude.Timestamp,
    -- | Provides a breakdown of the number of data objects labeled by humans, the number of objects labeled by machine, the number of objects than couldn't be labeled, and the total number of objects labeled.
    labelCounters :: LabelCounters,
    -- | Input configuration information for the labeling job, such as the Amazon S3 location of the data objects and the location of the manifest file that describes the data objects.
    inputConfig :: LabelingJobInputConfig,
    -- | The attribute used as the label in the output manifest file.
    labelAttributeName :: Lude.Maybe Lude.Text,
    -- | The processing status of the labeling job.
    labelingJobStatus :: LabelingJobStatus,
    -- | The location of the job's output data and the AWS Key Management Service key ID for the key used to encrypt the output data, if any.
    outputConfig :: LabelingJobOutputConfig,
    -- | The name assigned to the labeling job when it was created.
    labelingJobName :: Lude.Text,
    -- | The location of the output produced by the labeling job.
    labelingJobOutput :: Lude.Maybe LabelingJobOutput,
    -- | An array of key/value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
    tags :: Lude.Maybe [Tag],
    -- | A unique identifier for work done as part of a labeling job.
    jobReferenceCode :: Lude.Text,
    -- | The Amazon Resource Name (ARN) that Amazon SageMaker assumes to perform tasks on your behalf during data labeling.
    roleARN :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLabelingJobResponse' with the minimum fields required to make a request.
--
-- * 'creationTime' - The date and time that the labeling job was created.
-- * 'labelingJobARN' - The Amazon Resource Name (ARN) of the labeling job.
-- * 'failureReason' - If the job failed, the reason that it failed.
-- * 'labelingJobAlgorithmsConfig' - Configuration information for automated data labeling.
-- * 'labelCategoryConfigS3URI' - The S3 location of the JSON file that defines the categories used to label data objects. Please note the following label-category limits:
--
--
--     * Semantic segmentation labeling jobs using automated labeling: 20 labels
--
--
--     * Box bounding labeling jobs (all): 10 labels
--
--
-- The file is a JSON structure in the following format:
-- @{@
-- @"document-version": "2018-11-28"@
-- @"labels": [@
-- @{@
-- @"label": "/label 1/ "@
-- @},@
-- @{@
-- @"label": "/label 2/ "@
-- @},@
-- @...@
-- @{@
-- @"label": "/label n/ "@
-- @}@
-- @]@
-- @}@
-- * 'stoppingConditions' - A set of conditions for stopping a labeling job. If any of the conditions are met, the job is automatically stopped.
-- * 'humanTaskConfig' - Configuration information required for human workers to complete a labeling task.
-- * 'lastModifiedTime' - The date and time that the labeling job was last updated.
-- * 'labelCounters' - Provides a breakdown of the number of data objects labeled by humans, the number of objects labeled by machine, the number of objects than couldn't be labeled, and the total number of objects labeled.
-- * 'inputConfig' - Input configuration information for the labeling job, such as the Amazon S3 location of the data objects and the location of the manifest file that describes the data objects.
-- * 'labelAttributeName' - The attribute used as the label in the output manifest file.
-- * 'labelingJobStatus' - The processing status of the labeling job.
-- * 'outputConfig' - The location of the job's output data and the AWS Key Management Service key ID for the key used to encrypt the output data, if any.
-- * 'labelingJobName' - The name assigned to the labeling job when it was created.
-- * 'labelingJobOutput' - The location of the output produced by the labeling job.
-- * 'tags' - An array of key/value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
-- * 'jobReferenceCode' - A unique identifier for work done as part of a labeling job.
-- * 'roleARN' - The Amazon Resource Name (ARN) that Amazon SageMaker assumes to perform tasks on your behalf during data labeling.
-- * 'responseStatus' - The response status code.
mkDescribeLabelingJobResponse ::
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'labelingJobARN'
  Lude.Text ->
  -- | 'humanTaskConfig'
  HumanTaskConfig ->
  -- | 'lastModifiedTime'
  Lude.Timestamp ->
  -- | 'labelCounters'
  LabelCounters ->
  -- | 'inputConfig'
  LabelingJobInputConfig ->
  -- | 'labelingJobStatus'
  LabelingJobStatus ->
  -- | 'outputConfig'
  LabelingJobOutputConfig ->
  -- | 'labelingJobName'
  Lude.Text ->
  -- | 'jobReferenceCode'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLabelingJobResponse
mkDescribeLabelingJobResponse
  pCreationTime_
  pLabelingJobARN_
  pHumanTaskConfig_
  pLastModifiedTime_
  pLabelCounters_
  pInputConfig_
  pLabelingJobStatus_
  pOutputConfig_
  pLabelingJobName_
  pJobReferenceCode_
  pRoleARN_
  pResponseStatus_ =
    DescribeLabelingJobResponse'
      { creationTime = pCreationTime_,
        labelingJobARN = pLabelingJobARN_,
        failureReason = Lude.Nothing,
        labelingJobAlgorithmsConfig = Lude.Nothing,
        labelCategoryConfigS3URI = Lude.Nothing,
        stoppingConditions = Lude.Nothing,
        humanTaskConfig = pHumanTaskConfig_,
        lastModifiedTime = pLastModifiedTime_,
        labelCounters = pLabelCounters_,
        inputConfig = pInputConfig_,
        labelAttributeName = Lude.Nothing,
        labelingJobStatus = pLabelingJobStatus_,
        outputConfig = pOutputConfig_,
        labelingJobName = pLabelingJobName_,
        labelingJobOutput = Lude.Nothing,
        tags = Lude.Nothing,
        jobReferenceCode = pJobReferenceCode_,
        roleARN = pRoleARN_,
        responseStatus = pResponseStatus_
      }

-- | The date and time that the labeling job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrsCreationTime :: Lens.Lens' DescribeLabelingJobResponse Lude.Timestamp
dljrsCreationTime = Lens.lens (creationTime :: DescribeLabelingJobResponse -> Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeLabelingJobResponse)
{-# DEPRECATED dljrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the labeling job.
--
-- /Note:/ Consider using 'labelingJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrsLabelingJobARN :: Lens.Lens' DescribeLabelingJobResponse Lude.Text
dljrsLabelingJobARN = Lens.lens (labelingJobARN :: DescribeLabelingJobResponse -> Lude.Text) (\s a -> s {labelingJobARN = a} :: DescribeLabelingJobResponse)
{-# DEPRECATED dljrsLabelingJobARN "Use generic-lens or generic-optics with 'labelingJobARN' instead." #-}

-- | If the job failed, the reason that it failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrsFailureReason :: Lens.Lens' DescribeLabelingJobResponse (Lude.Maybe Lude.Text)
dljrsFailureReason = Lens.lens (failureReason :: DescribeLabelingJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: DescribeLabelingJobResponse)
{-# DEPRECATED dljrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | Configuration information for automated data labeling.
--
-- /Note:/ Consider using 'labelingJobAlgorithmsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrsLabelingJobAlgorithmsConfig :: Lens.Lens' DescribeLabelingJobResponse (Lude.Maybe LabelingJobAlgorithmsConfig)
dljrsLabelingJobAlgorithmsConfig = Lens.lens (labelingJobAlgorithmsConfig :: DescribeLabelingJobResponse -> Lude.Maybe LabelingJobAlgorithmsConfig) (\s a -> s {labelingJobAlgorithmsConfig = a} :: DescribeLabelingJobResponse)
{-# DEPRECATED dljrsLabelingJobAlgorithmsConfig "Use generic-lens or generic-optics with 'labelingJobAlgorithmsConfig' instead." #-}

-- | The S3 location of the JSON file that defines the categories used to label data objects. Please note the following label-category limits:
--
--
--     * Semantic segmentation labeling jobs using automated labeling: 20 labels
--
--
--     * Box bounding labeling jobs (all): 10 labels
--
--
-- The file is a JSON structure in the following format:
-- @{@
-- @"document-version": "2018-11-28"@
-- @"labels": [@
-- @{@
-- @"label": "/label 1/ "@
-- @},@
-- @{@
-- @"label": "/label 2/ "@
-- @},@
-- @...@
-- @{@
-- @"label": "/label n/ "@
-- @}@
-- @]@
-- @}@
--
-- /Note:/ Consider using 'labelCategoryConfigS3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrsLabelCategoryConfigS3URI :: Lens.Lens' DescribeLabelingJobResponse (Lude.Maybe Lude.Text)
dljrsLabelCategoryConfigS3URI = Lens.lens (labelCategoryConfigS3URI :: DescribeLabelingJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {labelCategoryConfigS3URI = a} :: DescribeLabelingJobResponse)
{-# DEPRECATED dljrsLabelCategoryConfigS3URI "Use generic-lens or generic-optics with 'labelCategoryConfigS3URI' instead." #-}

-- | A set of conditions for stopping a labeling job. If any of the conditions are met, the job is automatically stopped.
--
-- /Note:/ Consider using 'stoppingConditions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrsStoppingConditions :: Lens.Lens' DescribeLabelingJobResponse (Lude.Maybe LabelingJobStoppingConditions)
dljrsStoppingConditions = Lens.lens (stoppingConditions :: DescribeLabelingJobResponse -> Lude.Maybe LabelingJobStoppingConditions) (\s a -> s {stoppingConditions = a} :: DescribeLabelingJobResponse)
{-# DEPRECATED dljrsStoppingConditions "Use generic-lens or generic-optics with 'stoppingConditions' instead." #-}

-- | Configuration information required for human workers to complete a labeling task.
--
-- /Note:/ Consider using 'humanTaskConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrsHumanTaskConfig :: Lens.Lens' DescribeLabelingJobResponse HumanTaskConfig
dljrsHumanTaskConfig = Lens.lens (humanTaskConfig :: DescribeLabelingJobResponse -> HumanTaskConfig) (\s a -> s {humanTaskConfig = a} :: DescribeLabelingJobResponse)
{-# DEPRECATED dljrsHumanTaskConfig "Use generic-lens or generic-optics with 'humanTaskConfig' instead." #-}

-- | The date and time that the labeling job was last updated.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrsLastModifiedTime :: Lens.Lens' DescribeLabelingJobResponse Lude.Timestamp
dljrsLastModifiedTime = Lens.lens (lastModifiedTime :: DescribeLabelingJobResponse -> Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: DescribeLabelingJobResponse)
{-# DEPRECATED dljrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | Provides a breakdown of the number of data objects labeled by humans, the number of objects labeled by machine, the number of objects than couldn't be labeled, and the total number of objects labeled.
--
-- /Note:/ Consider using 'labelCounters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrsLabelCounters :: Lens.Lens' DescribeLabelingJobResponse LabelCounters
dljrsLabelCounters = Lens.lens (labelCounters :: DescribeLabelingJobResponse -> LabelCounters) (\s a -> s {labelCounters = a} :: DescribeLabelingJobResponse)
{-# DEPRECATED dljrsLabelCounters "Use generic-lens or generic-optics with 'labelCounters' instead." #-}

-- | Input configuration information for the labeling job, such as the Amazon S3 location of the data objects and the location of the manifest file that describes the data objects.
--
-- /Note:/ Consider using 'inputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrsInputConfig :: Lens.Lens' DescribeLabelingJobResponse LabelingJobInputConfig
dljrsInputConfig = Lens.lens (inputConfig :: DescribeLabelingJobResponse -> LabelingJobInputConfig) (\s a -> s {inputConfig = a} :: DescribeLabelingJobResponse)
{-# DEPRECATED dljrsInputConfig "Use generic-lens or generic-optics with 'inputConfig' instead." #-}

-- | The attribute used as the label in the output manifest file.
--
-- /Note:/ Consider using 'labelAttributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrsLabelAttributeName :: Lens.Lens' DescribeLabelingJobResponse (Lude.Maybe Lude.Text)
dljrsLabelAttributeName = Lens.lens (labelAttributeName :: DescribeLabelingJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {labelAttributeName = a} :: DescribeLabelingJobResponse)
{-# DEPRECATED dljrsLabelAttributeName "Use generic-lens or generic-optics with 'labelAttributeName' instead." #-}

-- | The processing status of the labeling job.
--
-- /Note:/ Consider using 'labelingJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrsLabelingJobStatus :: Lens.Lens' DescribeLabelingJobResponse LabelingJobStatus
dljrsLabelingJobStatus = Lens.lens (labelingJobStatus :: DescribeLabelingJobResponse -> LabelingJobStatus) (\s a -> s {labelingJobStatus = a} :: DescribeLabelingJobResponse)
{-# DEPRECATED dljrsLabelingJobStatus "Use generic-lens or generic-optics with 'labelingJobStatus' instead." #-}

-- | The location of the job's output data and the AWS Key Management Service key ID for the key used to encrypt the output data, if any.
--
-- /Note:/ Consider using 'outputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrsOutputConfig :: Lens.Lens' DescribeLabelingJobResponse LabelingJobOutputConfig
dljrsOutputConfig = Lens.lens (outputConfig :: DescribeLabelingJobResponse -> LabelingJobOutputConfig) (\s a -> s {outputConfig = a} :: DescribeLabelingJobResponse)
{-# DEPRECATED dljrsOutputConfig "Use generic-lens or generic-optics with 'outputConfig' instead." #-}

-- | The name assigned to the labeling job when it was created.
--
-- /Note:/ Consider using 'labelingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrsLabelingJobName :: Lens.Lens' DescribeLabelingJobResponse Lude.Text
dljrsLabelingJobName = Lens.lens (labelingJobName :: DescribeLabelingJobResponse -> Lude.Text) (\s a -> s {labelingJobName = a} :: DescribeLabelingJobResponse)
{-# DEPRECATED dljrsLabelingJobName "Use generic-lens or generic-optics with 'labelingJobName' instead." #-}

-- | The location of the output produced by the labeling job.
--
-- /Note:/ Consider using 'labelingJobOutput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrsLabelingJobOutput :: Lens.Lens' DescribeLabelingJobResponse (Lude.Maybe LabelingJobOutput)
dljrsLabelingJobOutput = Lens.lens (labelingJobOutput :: DescribeLabelingJobResponse -> Lude.Maybe LabelingJobOutput) (\s a -> s {labelingJobOutput = a} :: DescribeLabelingJobResponse)
{-# DEPRECATED dljrsLabelingJobOutput "Use generic-lens or generic-optics with 'labelingJobOutput' instead." #-}

-- | An array of key/value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrsTags :: Lens.Lens' DescribeLabelingJobResponse (Lude.Maybe [Tag])
dljrsTags = Lens.lens (tags :: DescribeLabelingJobResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: DescribeLabelingJobResponse)
{-# DEPRECATED dljrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A unique identifier for work done as part of a labeling job.
--
-- /Note:/ Consider using 'jobReferenceCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrsJobReferenceCode :: Lens.Lens' DescribeLabelingJobResponse Lude.Text
dljrsJobReferenceCode = Lens.lens (jobReferenceCode :: DescribeLabelingJobResponse -> Lude.Text) (\s a -> s {jobReferenceCode = a} :: DescribeLabelingJobResponse)
{-# DEPRECATED dljrsJobReferenceCode "Use generic-lens or generic-optics with 'jobReferenceCode' instead." #-}

-- | The Amazon Resource Name (ARN) that Amazon SageMaker assumes to perform tasks on your behalf during data labeling.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrsRoleARN :: Lens.Lens' DescribeLabelingJobResponse Lude.Text
dljrsRoleARN = Lens.lens (roleARN :: DescribeLabelingJobResponse -> Lude.Text) (\s a -> s {roleARN = a} :: DescribeLabelingJobResponse)
{-# DEPRECATED dljrsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrsResponseStatus :: Lens.Lens' DescribeLabelingJobResponse Lude.Int
dljrsResponseStatus = Lens.lens (responseStatus :: DescribeLabelingJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLabelingJobResponse)
{-# DEPRECATED dljrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
