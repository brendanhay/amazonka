{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateLabelingJob (..),
    mkCreateLabelingJob,

    -- ** Request lenses
    cljLabelingJobAlgorithmsConfig,
    cljLabelCategoryConfigS3URI,
    cljStoppingConditions,
    cljTags,
    cljLabelingJobName,
    cljLabelAttributeName,
    cljInputConfig,
    cljOutputConfig,
    cljRoleARN,
    cljHumanTaskConfig,

    -- * Destructuring the response
    CreateLabelingJobResponse (..),
    mkCreateLabelingJobResponse,

    -- ** Response lenses
    cljrsResponseStatus,
    cljrsLabelingJobARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateLabelingJob' smart constructor.
data CreateLabelingJob = CreateLabelingJob'
  { labelingJobAlgorithmsConfig ::
      Lude.Maybe LabelingJobAlgorithmsConfig,
    labelCategoryConfigS3URI :: Lude.Maybe Lude.Text,
    stoppingConditions ::
      Lude.Maybe LabelingJobStoppingConditions,
    tags :: Lude.Maybe [Tag],
    labelingJobName :: Lude.Text,
    labelAttributeName :: Lude.Text,
    inputConfig :: LabelingJobInputConfig,
    outputConfig :: LabelingJobOutputConfig,
    roleARN :: Lude.Text,
    humanTaskConfig :: HumanTaskConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLabelingJob' with the minimum fields required to make a request.
--
-- * 'humanTaskConfig' - Configures the labeling task and how it is presented to workers; including, but not limited to price, keywords, and batch size (task count).
-- * 'inputConfig' - Input data for the labeling job, such as the Amazon S3 location of the data objects and the location of the manifest file that describes the data objects.
-- * 'labelAttributeName' - The attribute name to use for the label in the output manifest file. This is the key for the key/value pair formed with the label that a worker assigns to the object. The name can't end with "-metadata". If you are running a semantic segmentation labeling job, the attribute name must end with "-ref". If you are running any other kind of labeling job, the attribute name must not end with "-ref".
-- * 'labelCategoryConfigS3URI' - The S3 URI of the file that defines the categories used to label the data objects.
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
-- * 'labelingJobAlgorithmsConfig' - Configures the information required to perform automated data labeling.
-- * 'labelingJobName' - The name of the labeling job. This name is used to identify the job in a list of labeling jobs.
-- * 'outputConfig' - The location of the output data and the AWS Key Management Service key ID for the key used to encrypt the output data, if any.
-- * 'roleARN' - The Amazon Resource Number (ARN) that Amazon SageMaker assumes to perform tasks on your behalf during data labeling. You must grant this role the necessary permissions so that Amazon SageMaker can successfully complete data labeling.
-- * 'stoppingConditions' - A set of conditions for stopping the labeling job. If any of the conditions are met, the job is automatically stopped. You can use these conditions to control the cost of data labeling.
-- * 'tags' - An array of key/value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
mkCreateLabelingJob ::
  -- | 'labelingJobName'
  Lude.Text ->
  -- | 'labelAttributeName'
  Lude.Text ->
  -- | 'inputConfig'
  LabelingJobInputConfig ->
  -- | 'outputConfig'
  LabelingJobOutputConfig ->
  -- | 'roleARN'
  Lude.Text ->
  -- | 'humanTaskConfig'
  HumanTaskConfig ->
  CreateLabelingJob
mkCreateLabelingJob
  pLabelingJobName_
  pLabelAttributeName_
  pInputConfig_
  pOutputConfig_
  pRoleARN_
  pHumanTaskConfig_ =
    CreateLabelingJob'
      { labelingJobAlgorithmsConfig = Lude.Nothing,
        labelCategoryConfigS3URI = Lude.Nothing,
        stoppingConditions = Lude.Nothing,
        tags = Lude.Nothing,
        labelingJobName = pLabelingJobName_,
        labelAttributeName = pLabelAttributeName_,
        inputConfig = pInputConfig_,
        outputConfig = pOutputConfig_,
        roleARN = pRoleARN_,
        humanTaskConfig = pHumanTaskConfig_
      }

-- | Configures the information required to perform automated data labeling.
--
-- /Note:/ Consider using 'labelingJobAlgorithmsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cljLabelingJobAlgorithmsConfig :: Lens.Lens' CreateLabelingJob (Lude.Maybe LabelingJobAlgorithmsConfig)
cljLabelingJobAlgorithmsConfig = Lens.lens (labelingJobAlgorithmsConfig :: CreateLabelingJob -> Lude.Maybe LabelingJobAlgorithmsConfig) (\s a -> s {labelingJobAlgorithmsConfig = a} :: CreateLabelingJob)
{-# DEPRECATED cljLabelingJobAlgorithmsConfig "Use generic-lens or generic-optics with 'labelingJobAlgorithmsConfig' instead." #-}

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
-- /Note:/ Consider using 'labelCategoryConfigS3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cljLabelCategoryConfigS3URI :: Lens.Lens' CreateLabelingJob (Lude.Maybe Lude.Text)
cljLabelCategoryConfigS3URI = Lens.lens (labelCategoryConfigS3URI :: CreateLabelingJob -> Lude.Maybe Lude.Text) (\s a -> s {labelCategoryConfigS3URI = a} :: CreateLabelingJob)
{-# DEPRECATED cljLabelCategoryConfigS3URI "Use generic-lens or generic-optics with 'labelCategoryConfigS3URI' instead." #-}

-- | A set of conditions for stopping the labeling job. If any of the conditions are met, the job is automatically stopped. You can use these conditions to control the cost of data labeling.
--
-- /Note:/ Consider using 'stoppingConditions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cljStoppingConditions :: Lens.Lens' CreateLabelingJob (Lude.Maybe LabelingJobStoppingConditions)
cljStoppingConditions = Lens.lens (stoppingConditions :: CreateLabelingJob -> Lude.Maybe LabelingJobStoppingConditions) (\s a -> s {stoppingConditions = a} :: CreateLabelingJob)
{-# DEPRECATED cljStoppingConditions "Use generic-lens or generic-optics with 'stoppingConditions' instead." #-}

-- | An array of key/value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cljTags :: Lens.Lens' CreateLabelingJob (Lude.Maybe [Tag])
cljTags = Lens.lens (tags :: CreateLabelingJob -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateLabelingJob)
{-# DEPRECATED cljTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the labeling job. This name is used to identify the job in a list of labeling jobs.
--
-- /Note:/ Consider using 'labelingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cljLabelingJobName :: Lens.Lens' CreateLabelingJob Lude.Text
cljLabelingJobName = Lens.lens (labelingJobName :: CreateLabelingJob -> Lude.Text) (\s a -> s {labelingJobName = a} :: CreateLabelingJob)
{-# DEPRECATED cljLabelingJobName "Use generic-lens or generic-optics with 'labelingJobName' instead." #-}

-- | The attribute name to use for the label in the output manifest file. This is the key for the key/value pair formed with the label that a worker assigns to the object. The name can't end with "-metadata". If you are running a semantic segmentation labeling job, the attribute name must end with "-ref". If you are running any other kind of labeling job, the attribute name must not end with "-ref".
--
-- /Note:/ Consider using 'labelAttributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cljLabelAttributeName :: Lens.Lens' CreateLabelingJob Lude.Text
cljLabelAttributeName = Lens.lens (labelAttributeName :: CreateLabelingJob -> Lude.Text) (\s a -> s {labelAttributeName = a} :: CreateLabelingJob)
{-# DEPRECATED cljLabelAttributeName "Use generic-lens or generic-optics with 'labelAttributeName' instead." #-}

-- | Input data for the labeling job, such as the Amazon S3 location of the data objects and the location of the manifest file that describes the data objects.
--
-- /Note:/ Consider using 'inputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cljInputConfig :: Lens.Lens' CreateLabelingJob LabelingJobInputConfig
cljInputConfig = Lens.lens (inputConfig :: CreateLabelingJob -> LabelingJobInputConfig) (\s a -> s {inputConfig = a} :: CreateLabelingJob)
{-# DEPRECATED cljInputConfig "Use generic-lens or generic-optics with 'inputConfig' instead." #-}

-- | The location of the output data and the AWS Key Management Service key ID for the key used to encrypt the output data, if any.
--
-- /Note:/ Consider using 'outputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cljOutputConfig :: Lens.Lens' CreateLabelingJob LabelingJobOutputConfig
cljOutputConfig = Lens.lens (outputConfig :: CreateLabelingJob -> LabelingJobOutputConfig) (\s a -> s {outputConfig = a} :: CreateLabelingJob)
{-# DEPRECATED cljOutputConfig "Use generic-lens or generic-optics with 'outputConfig' instead." #-}

-- | The Amazon Resource Number (ARN) that Amazon SageMaker assumes to perform tasks on your behalf during data labeling. You must grant this role the necessary permissions so that Amazon SageMaker can successfully complete data labeling.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cljRoleARN :: Lens.Lens' CreateLabelingJob Lude.Text
cljRoleARN = Lens.lens (roleARN :: CreateLabelingJob -> Lude.Text) (\s a -> s {roleARN = a} :: CreateLabelingJob)
{-# DEPRECATED cljRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | Configures the labeling task and how it is presented to workers; including, but not limited to price, keywords, and batch size (task count).
--
-- /Note:/ Consider using 'humanTaskConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cljHumanTaskConfig :: Lens.Lens' CreateLabelingJob HumanTaskConfig
cljHumanTaskConfig = Lens.lens (humanTaskConfig :: CreateLabelingJob -> HumanTaskConfig) (\s a -> s {humanTaskConfig = a} :: CreateLabelingJob)
{-# DEPRECATED cljHumanTaskConfig "Use generic-lens or generic-optics with 'humanTaskConfig' instead." #-}

instance Lude.AWSRequest CreateLabelingJob where
  type Rs CreateLabelingJob = CreateLabelingJobResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateLabelingJobResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "LabelingJobArn")
      )

instance Lude.ToHeaders CreateLabelingJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.CreateLabelingJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateLabelingJob where
  toJSON CreateLabelingJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("LabelingJobAlgorithmsConfig" Lude..=)
              Lude.<$> labelingJobAlgorithmsConfig,
            ("LabelCategoryConfigS3Uri" Lude..=)
              Lude.<$> labelCategoryConfigS3URI,
            ("StoppingConditions" Lude..=) Lude.<$> stoppingConditions,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("LabelingJobName" Lude..= labelingJobName),
            Lude.Just ("LabelAttributeName" Lude..= labelAttributeName),
            Lude.Just ("InputConfig" Lude..= inputConfig),
            Lude.Just ("OutputConfig" Lude..= outputConfig),
            Lude.Just ("RoleArn" Lude..= roleARN),
            Lude.Just ("HumanTaskConfig" Lude..= humanTaskConfig)
          ]
      )

instance Lude.ToPath CreateLabelingJob where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateLabelingJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateLabelingJobResponse' smart constructor.
data CreateLabelingJobResponse = CreateLabelingJobResponse'
  { responseStatus ::
      Lude.Int,
    labelingJobARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLabelingJobResponse' with the minimum fields required to make a request.
--
-- * 'labelingJobARN' - The Amazon Resource Name (ARN) of the labeling job. You use this ARN to identify the labeling job.
-- * 'responseStatus' - The response status code.
mkCreateLabelingJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'labelingJobARN'
  Lude.Text ->
  CreateLabelingJobResponse
mkCreateLabelingJobResponse pResponseStatus_ pLabelingJobARN_ =
  CreateLabelingJobResponse'
    { responseStatus = pResponseStatus_,
      labelingJobARN = pLabelingJobARN_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cljrsResponseStatus :: Lens.Lens' CreateLabelingJobResponse Lude.Int
cljrsResponseStatus = Lens.lens (responseStatus :: CreateLabelingJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateLabelingJobResponse)
{-# DEPRECATED cljrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The Amazon Resource Name (ARN) of the labeling job. You use this ARN to identify the labeling job.
--
-- /Note:/ Consider using 'labelingJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cljrsLabelingJobARN :: Lens.Lens' CreateLabelingJobResponse Lude.Text
cljrsLabelingJobARN = Lens.lens (labelingJobARN :: CreateLabelingJobResponse -> Lude.Text) (\s a -> s {labelingJobARN = a} :: CreateLabelingJobResponse)
{-# DEPRECATED cljrsLabelingJobARN "Use generic-lens or generic-optics with 'labelingJobARN' instead." #-}
