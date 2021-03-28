{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateLabelingJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a job that uses workers to label the data objects in your input dataset. You can use the labeled data to train machine learning models.
--
-- You can select your workforce from one of three providers:
--
--     * A private workforce that you create. It can include employees, contractors, and outside experts. Use a private workforce when want the data to stay within your organization or when a specific set of skills is required.
--
--
--     * One or more vendors that you select from the AWS Marketplace. Vendors provide expertise in specific areas. 
--
--
--     * The Amazon Mechanical Turk workforce. This is the largest workforce, but it should only be used for public data or data that has been stripped of any personally identifiable information.
--
--
-- You can also use /automated data labeling/ to reduce the number of data objects that need to be labeled by a human. Automated data labeling uses /active learning/ to determine if a data object can be labeled by machine or if it needs to be sent to a human worker. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-automated-labeling.html Using Automated Data Labeling> .
-- The data objects to be labeled are contained in an Amazon S3 bucket. You create a /manifest file/ that describes the location of each object. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-data.html Using Input and Output Data> .
-- The output can be used as the manifest file for another labeling job or as training data for your machine learning models.
module Network.AWS.SageMaker.CreateLabelingJob
    (
    -- * Creating a request
      CreateLabelingJob (..)
    , mkCreateLabelingJob
    -- ** Request lenses
    , cljLabelingJobName
    , cljLabelAttributeName
    , cljInputConfig
    , cljOutputConfig
    , cljRoleArn
    , cljHumanTaskConfig
    , cljLabelCategoryConfigS3Uri
    , cljLabelingJobAlgorithmsConfig
    , cljStoppingConditions
    , cljTags

    -- * Destructuring the response
    , CreateLabelingJobResponse (..)
    , mkCreateLabelingJobResponse
    -- ** Response lenses
    , cljrrsLabelingJobArn
    , cljrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateLabelingJob' smart constructor.
data CreateLabelingJob = CreateLabelingJob'
  { labelingJobName :: Types.LabelingJobName
    -- ^ The name of the labeling job. This name is used to identify the job in a list of labeling jobs.
  , labelAttributeName :: Types.LabelAttributeName
    -- ^ The attribute name to use for the label in the output manifest file. This is the key for the key/value pair formed with the label that a worker assigns to the object. The name can't end with "-metadata". If you are running a semantic segmentation labeling job, the attribute name must end with "-ref". If you are running any other kind of labeling job, the attribute name must not end with "-ref".
  , inputConfig :: Types.LabelingJobInputConfig
    -- ^ Input data for the labeling job, such as the Amazon S3 location of the data objects and the location of the manifest file that describes the data objects.
  , outputConfig :: Types.LabelingJobOutputConfig
    -- ^ The location of the output data and the AWS Key Management Service key ID for the key used to encrypt the output data, if any.
  , roleArn :: Types.RoleArn
    -- ^ The Amazon Resource Number (ARN) that Amazon SageMaker assumes to perform tasks on your behalf during data labeling. You must grant this role the necessary permissions so that Amazon SageMaker can successfully complete data labeling.
  , humanTaskConfig :: Types.HumanTaskConfig
    -- ^ Configures the labeling task and how it is presented to workers; including, but not limited to price, keywords, and batch size (task count).
  , labelCategoryConfigS3Uri :: Core.Maybe Types.LabelCategoryConfigS3Uri
    -- ^ The S3 URI of the file that defines the categories used to label the data objects.
--
-- For 3D point cloud task types, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-point-cloud-label-category-config.html Create a Labeling Category Configuration File for 3D Point Cloud Labeling Jobs> . 
-- For all other <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-task-types.html built-in task types> and <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-custom-templates.html custom tasks> , your label category configuration file must be a JSON file in the following format. Identify the labels you want to use by replacing @label_1@ , @label_2@ ,@...@ ,@label_n@ with your label categories.
-- @{@ 
-- @"document-version": "2018-11-28"@ 
-- @"labels": [@ 
-- @{@ 
-- @"label": "/label_1/ "@ 
-- @},@ 
-- @{@ 
-- @"label": "/label_2/ "@ 
-- @},@ 
-- @...@ 
-- @{@ 
-- @"label": "/label_n/ "@ 
-- @}@ 
-- @]@ 
-- @}@ 
  , labelingJobAlgorithmsConfig :: Core.Maybe Types.LabelingJobAlgorithmsConfig
    -- ^ Configures the information required to perform automated data labeling.
  , stoppingConditions :: Core.Maybe Types.LabelingJobStoppingConditions
    -- ^ A set of conditions for stopping the labeling job. If any of the conditions are met, the job is automatically stopped. You can use these conditions to control the cost of data labeling.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ An array of key/value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLabelingJob' value with any optional fields omitted.
mkCreateLabelingJob
    :: Types.LabelingJobName -- ^ 'labelingJobName'
    -> Types.LabelAttributeName -- ^ 'labelAttributeName'
    -> Types.LabelingJobInputConfig -- ^ 'inputConfig'
    -> Types.LabelingJobOutputConfig -- ^ 'outputConfig'
    -> Types.RoleArn -- ^ 'roleArn'
    -> Types.HumanTaskConfig -- ^ 'humanTaskConfig'
    -> CreateLabelingJob
mkCreateLabelingJob labelingJobName labelAttributeName inputConfig
  outputConfig roleArn humanTaskConfig
  = CreateLabelingJob'{labelingJobName, labelAttributeName,
                       inputConfig, outputConfig, roleArn, humanTaskConfig,
                       labelCategoryConfigS3Uri = Core.Nothing,
                       labelingJobAlgorithmsConfig = Core.Nothing,
                       stoppingConditions = Core.Nothing, tags = Core.Nothing}

-- | The name of the labeling job. This name is used to identify the job in a list of labeling jobs.
--
-- /Note:/ Consider using 'labelingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cljLabelingJobName :: Lens.Lens' CreateLabelingJob Types.LabelingJobName
cljLabelingJobName = Lens.field @"labelingJobName"
{-# INLINEABLE cljLabelingJobName #-}
{-# DEPRECATED labelingJobName "Use generic-lens or generic-optics with 'labelingJobName' instead"  #-}

-- | The attribute name to use for the label in the output manifest file. This is the key for the key/value pair formed with the label that a worker assigns to the object. The name can't end with "-metadata". If you are running a semantic segmentation labeling job, the attribute name must end with "-ref". If you are running any other kind of labeling job, the attribute name must not end with "-ref".
--
-- /Note:/ Consider using 'labelAttributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cljLabelAttributeName :: Lens.Lens' CreateLabelingJob Types.LabelAttributeName
cljLabelAttributeName = Lens.field @"labelAttributeName"
{-# INLINEABLE cljLabelAttributeName #-}
{-# DEPRECATED labelAttributeName "Use generic-lens or generic-optics with 'labelAttributeName' instead"  #-}

-- | Input data for the labeling job, such as the Amazon S3 location of the data objects and the location of the manifest file that describes the data objects.
--
-- /Note:/ Consider using 'inputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cljInputConfig :: Lens.Lens' CreateLabelingJob Types.LabelingJobInputConfig
cljInputConfig = Lens.field @"inputConfig"
{-# INLINEABLE cljInputConfig #-}
{-# DEPRECATED inputConfig "Use generic-lens or generic-optics with 'inputConfig' instead"  #-}

-- | The location of the output data and the AWS Key Management Service key ID for the key used to encrypt the output data, if any.
--
-- /Note:/ Consider using 'outputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cljOutputConfig :: Lens.Lens' CreateLabelingJob Types.LabelingJobOutputConfig
cljOutputConfig = Lens.field @"outputConfig"
{-# INLINEABLE cljOutputConfig #-}
{-# DEPRECATED outputConfig "Use generic-lens or generic-optics with 'outputConfig' instead"  #-}

-- | The Amazon Resource Number (ARN) that Amazon SageMaker assumes to perform tasks on your behalf during data labeling. You must grant this role the necessary permissions so that Amazon SageMaker can successfully complete data labeling.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cljRoleArn :: Lens.Lens' CreateLabelingJob Types.RoleArn
cljRoleArn = Lens.field @"roleArn"
{-# INLINEABLE cljRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | Configures the labeling task and how it is presented to workers; including, but not limited to price, keywords, and batch size (task count).
--
-- /Note:/ Consider using 'humanTaskConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cljHumanTaskConfig :: Lens.Lens' CreateLabelingJob Types.HumanTaskConfig
cljHumanTaskConfig = Lens.field @"humanTaskConfig"
{-# INLINEABLE cljHumanTaskConfig #-}
{-# DEPRECATED humanTaskConfig "Use generic-lens or generic-optics with 'humanTaskConfig' instead"  #-}

-- | The S3 URI of the file that defines the categories used to label the data objects.
--
-- For 3D point cloud task types, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-point-cloud-label-category-config.html Create a Labeling Category Configuration File for 3D Point Cloud Labeling Jobs> . 
-- For all other <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-task-types.html built-in task types> and <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-custom-templates.html custom tasks> , your label category configuration file must be a JSON file in the following format. Identify the labels you want to use by replacing @label_1@ , @label_2@ ,@...@ ,@label_n@ with your label categories.
-- @{@ 
-- @"document-version": "2018-11-28"@ 
-- @"labels": [@ 
-- @{@ 
-- @"label": "/label_1/ "@ 
-- @},@ 
-- @{@ 
-- @"label": "/label_2/ "@ 
-- @},@ 
-- @...@ 
-- @{@ 
-- @"label": "/label_n/ "@ 
-- @}@ 
-- @]@ 
-- @}@ 
--
-- /Note:/ Consider using 'labelCategoryConfigS3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cljLabelCategoryConfigS3Uri :: Lens.Lens' CreateLabelingJob (Core.Maybe Types.LabelCategoryConfigS3Uri)
cljLabelCategoryConfigS3Uri = Lens.field @"labelCategoryConfigS3Uri"
{-# INLINEABLE cljLabelCategoryConfigS3Uri #-}
{-# DEPRECATED labelCategoryConfigS3Uri "Use generic-lens or generic-optics with 'labelCategoryConfigS3Uri' instead"  #-}

-- | Configures the information required to perform automated data labeling.
--
-- /Note:/ Consider using 'labelingJobAlgorithmsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cljLabelingJobAlgorithmsConfig :: Lens.Lens' CreateLabelingJob (Core.Maybe Types.LabelingJobAlgorithmsConfig)
cljLabelingJobAlgorithmsConfig = Lens.field @"labelingJobAlgorithmsConfig"
{-# INLINEABLE cljLabelingJobAlgorithmsConfig #-}
{-# DEPRECATED labelingJobAlgorithmsConfig "Use generic-lens or generic-optics with 'labelingJobAlgorithmsConfig' instead"  #-}

-- | A set of conditions for stopping the labeling job. If any of the conditions are met, the job is automatically stopped. You can use these conditions to control the cost of data labeling.
--
-- /Note:/ Consider using 'stoppingConditions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cljStoppingConditions :: Lens.Lens' CreateLabelingJob (Core.Maybe Types.LabelingJobStoppingConditions)
cljStoppingConditions = Lens.field @"stoppingConditions"
{-# INLINEABLE cljStoppingConditions #-}
{-# DEPRECATED stoppingConditions "Use generic-lens or generic-optics with 'stoppingConditions' instead"  #-}

-- | An array of key/value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cljTags :: Lens.Lens' CreateLabelingJob (Core.Maybe [Types.Tag])
cljTags = Lens.field @"tags"
{-# INLINEABLE cljTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateLabelingJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateLabelingJob where
        toHeaders CreateLabelingJob{..}
          = Core.pure ("X-Amz-Target", "SageMaker.CreateLabelingJob") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateLabelingJob where
        toJSON CreateLabelingJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("LabelingJobName" Core..= labelingJobName),
                  Core.Just ("LabelAttributeName" Core..= labelAttributeName),
                  Core.Just ("InputConfig" Core..= inputConfig),
                  Core.Just ("OutputConfig" Core..= outputConfig),
                  Core.Just ("RoleArn" Core..= roleArn),
                  Core.Just ("HumanTaskConfig" Core..= humanTaskConfig),
                  ("LabelCategoryConfigS3Uri" Core..=) Core.<$>
                    labelCategoryConfigS3Uri,
                  ("LabelingJobAlgorithmsConfig" Core..=) Core.<$>
                    labelingJobAlgorithmsConfig,
                  ("StoppingConditions" Core..=) Core.<$> stoppingConditions,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateLabelingJob where
        type Rs CreateLabelingJob = CreateLabelingJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateLabelingJobResponse' Core.<$>
                   (x Core..: "LabelingJobArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateLabelingJobResponse' smart constructor.
data CreateLabelingJobResponse = CreateLabelingJobResponse'
  { labelingJobArn :: Types.LabelingJobArn
    -- ^ The Amazon Resource Name (ARN) of the labeling job. You use this ARN to identify the labeling job.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLabelingJobResponse' value with any optional fields omitted.
mkCreateLabelingJobResponse
    :: Types.LabelingJobArn -- ^ 'labelingJobArn'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateLabelingJobResponse
mkCreateLabelingJobResponse labelingJobArn responseStatus
  = CreateLabelingJobResponse'{labelingJobArn, responseStatus}

-- | The Amazon Resource Name (ARN) of the labeling job. You use this ARN to identify the labeling job.
--
-- /Note:/ Consider using 'labelingJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cljrrsLabelingJobArn :: Lens.Lens' CreateLabelingJobResponse Types.LabelingJobArn
cljrrsLabelingJobArn = Lens.field @"labelingJobArn"
{-# INLINEABLE cljrrsLabelingJobArn #-}
{-# DEPRECATED labelingJobArn "Use generic-lens or generic-optics with 'labelingJobArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cljrrsResponseStatus :: Lens.Lens' CreateLabelingJobResponse Core.Int
cljrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cljrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
