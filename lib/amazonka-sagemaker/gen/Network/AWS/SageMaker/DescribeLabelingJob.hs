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
    dljrrsLabelingJobStatus,
    dljrrsLabelCounters,
    dljrrsCreationTime,
    dljrrsLastModifiedTime,
    dljrrsJobReferenceCode,
    dljrrsLabelingJobName,
    dljrrsLabelingJobArn,
    dljrrsInputConfig,
    dljrrsOutputConfig,
    dljrrsRoleArn,
    dljrrsHumanTaskConfig,
    dljrrsFailureReason,
    dljrrsLabelAttributeName,
    dljrrsLabelCategoryConfigS3Uri,
    dljrrsLabelingJobAlgorithmsConfig,
    dljrrsLabelingJobOutput,
    dljrrsStoppingConditions,
    dljrrsTags,
    dljrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeLabelingJob' smart constructor.
newtype DescribeLabelingJob = DescribeLabelingJob'
  { -- | The name of the labeling job to return information for.
    labelingJobName :: Types.LabelingJobName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLabelingJob' value with any optional fields omitted.
mkDescribeLabelingJob ::
  -- | 'labelingJobName'
  Types.LabelingJobName ->
  DescribeLabelingJob
mkDescribeLabelingJob labelingJobName =
  DescribeLabelingJob' {labelingJobName}

-- | The name of the labeling job to return information for.
--
-- /Note:/ Consider using 'labelingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljLabelingJobName :: Lens.Lens' DescribeLabelingJob Types.LabelingJobName
dljLabelingJobName = Lens.field @"labelingJobName"
{-# DEPRECATED dljLabelingJobName "Use generic-lens or generic-optics with 'labelingJobName' instead." #-}

instance Core.FromJSON DescribeLabelingJob where
  toJSON DescribeLabelingJob {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("LabelingJobName" Core..= labelingJobName)]
      )

instance Core.AWSRequest DescribeLabelingJob where
  type Rs DescribeLabelingJob = DescribeLabelingJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DescribeLabelingJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLabelingJobResponse'
            Core.<$> (x Core..: "LabelingJobStatus")
            Core.<*> (x Core..: "LabelCounters")
            Core.<*> (x Core..: "CreationTime")
            Core.<*> (x Core..: "LastModifiedTime")
            Core.<*> (x Core..: "JobReferenceCode")
            Core.<*> (x Core..: "LabelingJobName")
            Core.<*> (x Core..: "LabelingJobArn")
            Core.<*> (x Core..: "InputConfig")
            Core.<*> (x Core..: "OutputConfig")
            Core.<*> (x Core..: "RoleArn")
            Core.<*> (x Core..: "HumanTaskConfig")
            Core.<*> (x Core..:? "FailureReason")
            Core.<*> (x Core..:? "LabelAttributeName")
            Core.<*> (x Core..:? "LabelCategoryConfigS3Uri")
            Core.<*> (x Core..:? "LabelingJobAlgorithmsConfig")
            Core.<*> (x Core..:? "LabelingJobOutput")
            Core.<*> (x Core..:? "StoppingConditions")
            Core.<*> (x Core..:? "Tags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeLabelingJobResponse' smart constructor.
data DescribeLabelingJobResponse = DescribeLabelingJobResponse'
  { -- | The processing status of the labeling job.
    labelingJobStatus :: Types.LabelingJobStatus,
    -- | Provides a breakdown of the number of data objects labeled by humans, the number of objects labeled by machine, the number of objects than couldn't be labeled, and the total number of objects labeled.
    labelCounters :: Types.LabelCounters,
    -- | The date and time that the labeling job was created.
    creationTime :: Core.NominalDiffTime,
    -- | The date and time that the labeling job was last updated.
    lastModifiedTime :: Core.NominalDiffTime,
    -- | A unique identifier for work done as part of a labeling job.
    jobReferenceCode :: Types.JobReferenceCode,
    -- | The name assigned to the labeling job when it was created.
    labelingJobName :: Types.LabelingJobName,
    -- | The Amazon Resource Name (ARN) of the labeling job.
    labelingJobArn :: Types.LabelingJobArn,
    -- | Input configuration information for the labeling job, such as the Amazon S3 location of the data objects and the location of the manifest file that describes the data objects.
    inputConfig :: Types.LabelingJobInputConfig,
    -- | The location of the job's output data and the AWS Key Management Service key ID for the key used to encrypt the output data, if any.
    outputConfig :: Types.LabelingJobOutputConfig,
    -- | The Amazon Resource Name (ARN) that Amazon SageMaker assumes to perform tasks on your behalf during data labeling.
    roleArn :: Types.RoleArn,
    -- | Configuration information required for human workers to complete a labeling task.
    humanTaskConfig :: Types.HumanTaskConfig,
    -- | If the job failed, the reason that it failed.
    failureReason :: Core.Maybe Types.FailureReason,
    -- | The attribute used as the label in the output manifest file.
    labelAttributeName :: Core.Maybe Types.LabelAttributeName,
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
    labelCategoryConfigS3Uri :: Core.Maybe Types.LabelCategoryConfigS3Uri,
    -- | Configuration information for automated data labeling.
    labelingJobAlgorithmsConfig :: Core.Maybe Types.LabelingJobAlgorithmsConfig,
    -- | The location of the output produced by the labeling job.
    labelingJobOutput :: Core.Maybe Types.LabelingJobOutput,
    -- | A set of conditions for stopping a labeling job. If any of the conditions are met, the job is automatically stopped.
    stoppingConditions :: Core.Maybe Types.LabelingJobStoppingConditions,
    -- | An array of key/value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
    tags :: Core.Maybe [Types.Tag],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeLabelingJobResponse' value with any optional fields omitted.
mkDescribeLabelingJobResponse ::
  -- | 'labelingJobStatus'
  Types.LabelingJobStatus ->
  -- | 'labelCounters'
  Types.LabelCounters ->
  -- | 'creationTime'
  Core.NominalDiffTime ->
  -- | 'lastModifiedTime'
  Core.NominalDiffTime ->
  -- | 'jobReferenceCode'
  Types.JobReferenceCode ->
  -- | 'labelingJobName'
  Types.LabelingJobName ->
  -- | 'labelingJobArn'
  Types.LabelingJobArn ->
  -- | 'inputConfig'
  Types.LabelingJobInputConfig ->
  -- | 'outputConfig'
  Types.LabelingJobOutputConfig ->
  -- | 'roleArn'
  Types.RoleArn ->
  -- | 'humanTaskConfig'
  Types.HumanTaskConfig ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeLabelingJobResponse
mkDescribeLabelingJobResponse
  labelingJobStatus
  labelCounters
  creationTime
  lastModifiedTime
  jobReferenceCode
  labelingJobName
  labelingJobArn
  inputConfig
  outputConfig
  roleArn
  humanTaskConfig
  responseStatus =
    DescribeLabelingJobResponse'
      { labelingJobStatus,
        labelCounters,
        creationTime,
        lastModifiedTime,
        jobReferenceCode,
        labelingJobName,
        labelingJobArn,
        inputConfig,
        outputConfig,
        roleArn,
        humanTaskConfig,
        failureReason = Core.Nothing,
        labelAttributeName = Core.Nothing,
        labelCategoryConfigS3Uri = Core.Nothing,
        labelingJobAlgorithmsConfig = Core.Nothing,
        labelingJobOutput = Core.Nothing,
        stoppingConditions = Core.Nothing,
        tags = Core.Nothing,
        responseStatus
      }

-- | The processing status of the labeling job.
--
-- /Note:/ Consider using 'labelingJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrrsLabelingJobStatus :: Lens.Lens' DescribeLabelingJobResponse Types.LabelingJobStatus
dljrrsLabelingJobStatus = Lens.field @"labelingJobStatus"
{-# DEPRECATED dljrrsLabelingJobStatus "Use generic-lens or generic-optics with 'labelingJobStatus' instead." #-}

-- | Provides a breakdown of the number of data objects labeled by humans, the number of objects labeled by machine, the number of objects than couldn't be labeled, and the total number of objects labeled.
--
-- /Note:/ Consider using 'labelCounters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrrsLabelCounters :: Lens.Lens' DescribeLabelingJobResponse Types.LabelCounters
dljrrsLabelCounters = Lens.field @"labelCounters"
{-# DEPRECATED dljrrsLabelCounters "Use generic-lens or generic-optics with 'labelCounters' instead." #-}

-- | The date and time that the labeling job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrrsCreationTime :: Lens.Lens' DescribeLabelingJobResponse Core.NominalDiffTime
dljrrsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED dljrrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The date and time that the labeling job was last updated.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrrsLastModifiedTime :: Lens.Lens' DescribeLabelingJobResponse Core.NominalDiffTime
dljrrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED dljrrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | A unique identifier for work done as part of a labeling job.
--
-- /Note:/ Consider using 'jobReferenceCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrrsJobReferenceCode :: Lens.Lens' DescribeLabelingJobResponse Types.JobReferenceCode
dljrrsJobReferenceCode = Lens.field @"jobReferenceCode"
{-# DEPRECATED dljrrsJobReferenceCode "Use generic-lens or generic-optics with 'jobReferenceCode' instead." #-}

-- | The name assigned to the labeling job when it was created.
--
-- /Note:/ Consider using 'labelingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrrsLabelingJobName :: Lens.Lens' DescribeLabelingJobResponse Types.LabelingJobName
dljrrsLabelingJobName = Lens.field @"labelingJobName"
{-# DEPRECATED dljrrsLabelingJobName "Use generic-lens or generic-optics with 'labelingJobName' instead." #-}

-- | The Amazon Resource Name (ARN) of the labeling job.
--
-- /Note:/ Consider using 'labelingJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrrsLabelingJobArn :: Lens.Lens' DescribeLabelingJobResponse Types.LabelingJobArn
dljrrsLabelingJobArn = Lens.field @"labelingJobArn"
{-# DEPRECATED dljrrsLabelingJobArn "Use generic-lens or generic-optics with 'labelingJobArn' instead." #-}

-- | Input configuration information for the labeling job, such as the Amazon S3 location of the data objects and the location of the manifest file that describes the data objects.
--
-- /Note:/ Consider using 'inputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrrsInputConfig :: Lens.Lens' DescribeLabelingJobResponse Types.LabelingJobInputConfig
dljrrsInputConfig = Lens.field @"inputConfig"
{-# DEPRECATED dljrrsInputConfig "Use generic-lens or generic-optics with 'inputConfig' instead." #-}

-- | The location of the job's output data and the AWS Key Management Service key ID for the key used to encrypt the output data, if any.
--
-- /Note:/ Consider using 'outputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrrsOutputConfig :: Lens.Lens' DescribeLabelingJobResponse Types.LabelingJobOutputConfig
dljrrsOutputConfig = Lens.field @"outputConfig"
{-# DEPRECATED dljrrsOutputConfig "Use generic-lens or generic-optics with 'outputConfig' instead." #-}

-- | The Amazon Resource Name (ARN) that Amazon SageMaker assumes to perform tasks on your behalf during data labeling.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrrsRoleArn :: Lens.Lens' DescribeLabelingJobResponse Types.RoleArn
dljrrsRoleArn = Lens.field @"roleArn"
{-# DEPRECATED dljrrsRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | Configuration information required for human workers to complete a labeling task.
--
-- /Note:/ Consider using 'humanTaskConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrrsHumanTaskConfig :: Lens.Lens' DescribeLabelingJobResponse Types.HumanTaskConfig
dljrrsHumanTaskConfig = Lens.field @"humanTaskConfig"
{-# DEPRECATED dljrrsHumanTaskConfig "Use generic-lens or generic-optics with 'humanTaskConfig' instead." #-}

-- | If the job failed, the reason that it failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrrsFailureReason :: Lens.Lens' DescribeLabelingJobResponse (Core.Maybe Types.FailureReason)
dljrrsFailureReason = Lens.field @"failureReason"
{-# DEPRECATED dljrrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The attribute used as the label in the output manifest file.
--
-- /Note:/ Consider using 'labelAttributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrrsLabelAttributeName :: Lens.Lens' DescribeLabelingJobResponse (Core.Maybe Types.LabelAttributeName)
dljrrsLabelAttributeName = Lens.field @"labelAttributeName"
{-# DEPRECATED dljrrsLabelAttributeName "Use generic-lens or generic-optics with 'labelAttributeName' instead." #-}

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
-- /Note:/ Consider using 'labelCategoryConfigS3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrrsLabelCategoryConfigS3Uri :: Lens.Lens' DescribeLabelingJobResponse (Core.Maybe Types.LabelCategoryConfigS3Uri)
dljrrsLabelCategoryConfigS3Uri = Lens.field @"labelCategoryConfigS3Uri"
{-# DEPRECATED dljrrsLabelCategoryConfigS3Uri "Use generic-lens or generic-optics with 'labelCategoryConfigS3Uri' instead." #-}

-- | Configuration information for automated data labeling.
--
-- /Note:/ Consider using 'labelingJobAlgorithmsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrrsLabelingJobAlgorithmsConfig :: Lens.Lens' DescribeLabelingJobResponse (Core.Maybe Types.LabelingJobAlgorithmsConfig)
dljrrsLabelingJobAlgorithmsConfig = Lens.field @"labelingJobAlgorithmsConfig"
{-# DEPRECATED dljrrsLabelingJobAlgorithmsConfig "Use generic-lens or generic-optics with 'labelingJobAlgorithmsConfig' instead." #-}

-- | The location of the output produced by the labeling job.
--
-- /Note:/ Consider using 'labelingJobOutput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrrsLabelingJobOutput :: Lens.Lens' DescribeLabelingJobResponse (Core.Maybe Types.LabelingJobOutput)
dljrrsLabelingJobOutput = Lens.field @"labelingJobOutput"
{-# DEPRECATED dljrrsLabelingJobOutput "Use generic-lens or generic-optics with 'labelingJobOutput' instead." #-}

-- | A set of conditions for stopping a labeling job. If any of the conditions are met, the job is automatically stopped.
--
-- /Note:/ Consider using 'stoppingConditions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrrsStoppingConditions :: Lens.Lens' DescribeLabelingJobResponse (Core.Maybe Types.LabelingJobStoppingConditions)
dljrrsStoppingConditions = Lens.field @"stoppingConditions"
{-# DEPRECATED dljrrsStoppingConditions "Use generic-lens or generic-optics with 'stoppingConditions' instead." #-}

-- | An array of key/value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrrsTags :: Lens.Lens' DescribeLabelingJobResponse (Core.Maybe [Types.Tag])
dljrrsTags = Lens.field @"tags"
{-# DEPRECATED dljrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dljrrsResponseStatus :: Lens.Lens' DescribeLabelingJobResponse Core.Int
dljrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dljrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
