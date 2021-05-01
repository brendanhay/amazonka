{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateLabelingJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a job that uses workers to label the data objects in your input
-- dataset. You can use the labeled data to train machine learning models.
--
-- You can select your workforce from one of three providers:
--
-- -   A private workforce that you create. It can include employees,
--     contractors, and outside experts. Use a private workforce when want
--     the data to stay within your organization or when a specific set of
--     skills is required.
--
-- -   One or more vendors that you select from the AWS Marketplace.
--     Vendors provide expertise in specific areas.
--
-- -   The Amazon Mechanical Turk workforce. This is the largest workforce,
--     but it should only be used for public data or data that has been
--     stripped of any personally identifiable information.
--
-- You can also use /automated data labeling/ to reduce the number of data
-- objects that need to be labeled by a human. Automated data labeling uses
-- /active learning/ to determine if a data object can be labeled by
-- machine or if it needs to be sent to a human worker. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-automated-labeling.html Using Automated Data Labeling>.
--
-- The data objects to be labeled are contained in an Amazon S3 bucket. You
-- create a /manifest file/ that describes the location of each object. For
-- more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-data.html Using Input and Output Data>.
--
-- The output can be used as the manifest file for another labeling job or
-- as training data for your machine learning models.
--
-- You can use this operation to create a static labeling job or a
-- streaming labeling job. A static labeling job stops if all data objects
-- in the input manifest file identified in @ManifestS3Uri@ have been
-- labeled. A streaming labeling job runs perpetually until it is manually
-- stopped, or remains idle for 10 days. You can send new data objects to
-- an active (@InProgress@) streaming labeling job in real time. To learn
-- how to create a static labeling job, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-create-labeling-job-api.html Create a Labeling Job (API)>
-- in the Amazon SageMaker Developer Guide. To learn how to create a
-- streaming labeling job, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-streaming-create-job.html Create a Streaming Labeling Job>.
module Network.AWS.SageMaker.CreateLabelingJob
  ( -- * Creating a Request
    CreateLabelingJob (..),
    newCreateLabelingJob,

    -- * Request Lenses
    createLabelingJob_stoppingConditions,
    createLabelingJob_labelCategoryConfigS3Uri,
    createLabelingJob_labelingJobAlgorithmsConfig,
    createLabelingJob_tags,
    createLabelingJob_labelingJobName,
    createLabelingJob_labelAttributeName,
    createLabelingJob_inputConfig,
    createLabelingJob_outputConfig,
    createLabelingJob_roleArn,
    createLabelingJob_humanTaskConfig,

    -- * Destructuring the Response
    CreateLabelingJobResponse (..),
    newCreateLabelingJobResponse,

    -- * Response Lenses
    createLabelingJobResponse_httpStatus,
    createLabelingJobResponse_labelingJobArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateLabelingJob' smart constructor.
data CreateLabelingJob = CreateLabelingJob'
  { -- | A set of conditions for stopping the labeling job. If any of the
    -- conditions are met, the job is automatically stopped. You can use these
    -- conditions to control the cost of data labeling.
    stoppingConditions :: Prelude.Maybe LabelingJobStoppingConditions,
    -- | The S3 URI of the file, referred to as a /label category configuration
    -- file/, that defines the categories used to label the data objects.
    --
    -- For 3D point cloud and video frame task types, you can add label
    -- category attributes and frame attributes to your label category
    -- configuration file. To learn how, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-point-cloud-label-category-config.html Create a Labeling Category Configuration File for 3D Point Cloud Labeling Jobs>.
    --
    -- For all other
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-task-types.html built-in task types>
    -- and
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-custom-templates.html custom tasks>,
    -- your label category configuration file must be a JSON file in the
    -- following format. Identify the labels you want to use by replacing
    -- @label_1@, @label_2@,@...@,@label_n@ with your label categories.
    --
    -- @{ @
    --
    -- @\"document-version\": \"2018-11-28\",@
    --
    -- @\"labels\": [{\"label\": \"label_1\"},{\"label\": \"label_2\"},...{\"label\": \"label_n\"}]@
    --
    -- @}@
    --
    -- Note the following about the label category configuration file:
    --
    -- -   For image classification and text classification (single and
    --     multi-label) you must specify at least two label categories. For all
    --     other task types, the minimum number of label categories required is
    --     one.
    --
    -- -   Each label category must be unique, you cannot specify duplicate
    --     label categories.
    --
    -- -   If you create a 3D point cloud or video frame adjustment or
    --     verification labeling job, you must include
    --     @auditLabelAttributeName@ in the label category configuration. Use
    --     this parameter to enter the
    --     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateLabelingJob.html#sagemaker-CreateLabelingJob-request-LabelAttributeName LabelAttributeName>
    --     of the labeling job you want to adjust or verify annotations of.
    labelCategoryConfigS3Uri :: Prelude.Maybe Prelude.Text,
    -- | Configures the information required to perform automated data labeling.
    labelingJobAlgorithmsConfig :: Prelude.Maybe LabelingJobAlgorithmsConfig,
    -- | An array of key\/value pairs. For more information, see
    -- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags>
    -- in the /AWS Billing and Cost Management User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the labeling job. This name is used to identify the job in a
    -- list of labeling jobs. Labeling job names must be unique within an AWS
    -- account and region. @LabelingJobName@ is not case sensitive. For
    -- example, Example-job and example-job are considered the same labeling
    -- job name by Ground Truth.
    labelingJobName :: Prelude.Text,
    -- | The attribute name to use for the label in the output manifest file.
    -- This is the key for the key\/value pair formed with the label that a
    -- worker assigns to the object. The @LabelAttributeName@ must meet the
    -- following requirements.
    --
    -- -   The name can\'t end with \"-metadata\".
    --
    -- -   If you are using one of the following
    --     <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-task-types.html built-in task types>,
    --     the attribute name /must/ end with \"-ref\". If the task type you
    --     are using is not listed below, the attribute name /must not/ end
    --     with \"-ref\".
    --
    --     -   Image semantic segmentation (@SemanticSegmentation)@, and
    --         adjustment (@AdjustmentSemanticSegmentation@) and verification
    --         (@VerificationSemanticSegmentation@) labeling jobs for this task
    --         type.
    --
    --     -   Video frame object detection (@VideoObjectDetection@), and
    --         adjustment and verification (@AdjustmentVideoObjectDetection@)
    --         labeling jobs for this task type.
    --
    --     -   Video frame object tracking (@VideoObjectTracking@), and
    --         adjustment and verification (@AdjustmentVideoObjectTracking@)
    --         labeling jobs for this task type.
    --
    --     -   3D point cloud semantic segmentation
    --         (@3DPointCloudSemanticSegmentation@), and adjustment and
    --         verification (@Adjustment3DPointCloudSemanticSegmentation@)
    --         labeling jobs for this task type.
    --
    --     -   3D point cloud object tracking (@3DPointCloudObjectTracking@),
    --         and adjustment and verification
    --         (@Adjustment3DPointCloudObjectTracking@) labeling jobs for this
    --         task type.
    --
    -- If you are creating an adjustment or verification labeling job, you must
    -- use a /different/ @LabelAttributeName@ than the one used in the original
    -- labeling job. The original labeling job is the Ground Truth labeling job
    -- that produced the labels that you want verified or adjusted. To learn
    -- more about adjustment and verification labeling jobs, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-verification-data.html Verify and Adjust Labels>.
    labelAttributeName :: Prelude.Text,
    -- | Input data for the labeling job, such as the Amazon S3 location of the
    -- data objects and the location of the manifest file that describes the
    -- data objects.
    --
    -- You must specify at least one of the following: @S3DataSource@ or
    -- @SnsDataSource@.
    --
    -- -   Use @SnsDataSource@ to specify an SNS input topic for a streaming
    --     labeling job. If you do not specify and SNS input topic ARN, Ground
    --     Truth will create a one-time labeling job that stops after all data
    --     objects in the input manifest file have been labeled.
    --
    -- -   Use @S3DataSource@ to specify an input manifest file for both
    --     streaming and one-time labeling jobs. Adding an @S3DataSource@ is
    --     optional if you use @SnsDataSource@ to create a streaming labeling
    --     job.
    --
    -- If you use the Amazon Mechanical Turk workforce, your input data should
    -- not include confidential information, personal information or protected
    -- health information. Use @ContentClassifiers@ to specify that your data
    -- is free of personally identifiable information and adult content.
    inputConfig :: LabelingJobInputConfig,
    -- | The location of the output data and the AWS Key Management Service key
    -- ID for the key used to encrypt the output data, if any.
    outputConfig :: LabelingJobOutputConfig,
    -- | The Amazon Resource Number (ARN) that Amazon SageMaker assumes to
    -- perform tasks on your behalf during data labeling. You must grant this
    -- role the necessary permissions so that Amazon SageMaker can successfully
    -- complete data labeling.
    roleArn :: Prelude.Text,
    -- | Configures the labeling task and how it is presented to workers;
    -- including, but not limited to price, keywords, and batch size (task
    -- count).
    humanTaskConfig :: HumanTaskConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateLabelingJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stoppingConditions', 'createLabelingJob_stoppingConditions' - A set of conditions for stopping the labeling job. If any of the
-- conditions are met, the job is automatically stopped. You can use these
-- conditions to control the cost of data labeling.
--
-- 'labelCategoryConfigS3Uri', 'createLabelingJob_labelCategoryConfigS3Uri' - The S3 URI of the file, referred to as a /label category configuration
-- file/, that defines the categories used to label the data objects.
--
-- For 3D point cloud and video frame task types, you can add label
-- category attributes and frame attributes to your label category
-- configuration file. To learn how, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-point-cloud-label-category-config.html Create a Labeling Category Configuration File for 3D Point Cloud Labeling Jobs>.
--
-- For all other
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-task-types.html built-in task types>
-- and
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-custom-templates.html custom tasks>,
-- your label category configuration file must be a JSON file in the
-- following format. Identify the labels you want to use by replacing
-- @label_1@, @label_2@,@...@,@label_n@ with your label categories.
--
-- @{ @
--
-- @\"document-version\": \"2018-11-28\",@
--
-- @\"labels\": [{\"label\": \"label_1\"},{\"label\": \"label_2\"},...{\"label\": \"label_n\"}]@
--
-- @}@
--
-- Note the following about the label category configuration file:
--
-- -   For image classification and text classification (single and
--     multi-label) you must specify at least two label categories. For all
--     other task types, the minimum number of label categories required is
--     one.
--
-- -   Each label category must be unique, you cannot specify duplicate
--     label categories.
--
-- -   If you create a 3D point cloud or video frame adjustment or
--     verification labeling job, you must include
--     @auditLabelAttributeName@ in the label category configuration. Use
--     this parameter to enter the
--     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateLabelingJob.html#sagemaker-CreateLabelingJob-request-LabelAttributeName LabelAttributeName>
--     of the labeling job you want to adjust or verify annotations of.
--
-- 'labelingJobAlgorithmsConfig', 'createLabelingJob_labelingJobAlgorithmsConfig' - Configures the information required to perform automated data labeling.
--
-- 'tags', 'createLabelingJob_tags' - An array of key\/value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags>
-- in the /AWS Billing and Cost Management User Guide/.
--
-- 'labelingJobName', 'createLabelingJob_labelingJobName' - The name of the labeling job. This name is used to identify the job in a
-- list of labeling jobs. Labeling job names must be unique within an AWS
-- account and region. @LabelingJobName@ is not case sensitive. For
-- example, Example-job and example-job are considered the same labeling
-- job name by Ground Truth.
--
-- 'labelAttributeName', 'createLabelingJob_labelAttributeName' - The attribute name to use for the label in the output manifest file.
-- This is the key for the key\/value pair formed with the label that a
-- worker assigns to the object. The @LabelAttributeName@ must meet the
-- following requirements.
--
-- -   The name can\'t end with \"-metadata\".
--
-- -   If you are using one of the following
--     <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-task-types.html built-in task types>,
--     the attribute name /must/ end with \"-ref\". If the task type you
--     are using is not listed below, the attribute name /must not/ end
--     with \"-ref\".
--
--     -   Image semantic segmentation (@SemanticSegmentation)@, and
--         adjustment (@AdjustmentSemanticSegmentation@) and verification
--         (@VerificationSemanticSegmentation@) labeling jobs for this task
--         type.
--
--     -   Video frame object detection (@VideoObjectDetection@), and
--         adjustment and verification (@AdjustmentVideoObjectDetection@)
--         labeling jobs for this task type.
--
--     -   Video frame object tracking (@VideoObjectTracking@), and
--         adjustment and verification (@AdjustmentVideoObjectTracking@)
--         labeling jobs for this task type.
--
--     -   3D point cloud semantic segmentation
--         (@3DPointCloudSemanticSegmentation@), and adjustment and
--         verification (@Adjustment3DPointCloudSemanticSegmentation@)
--         labeling jobs for this task type.
--
--     -   3D point cloud object tracking (@3DPointCloudObjectTracking@),
--         and adjustment and verification
--         (@Adjustment3DPointCloudObjectTracking@) labeling jobs for this
--         task type.
--
-- If you are creating an adjustment or verification labeling job, you must
-- use a /different/ @LabelAttributeName@ than the one used in the original
-- labeling job. The original labeling job is the Ground Truth labeling job
-- that produced the labels that you want verified or adjusted. To learn
-- more about adjustment and verification labeling jobs, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-verification-data.html Verify and Adjust Labels>.
--
-- 'inputConfig', 'createLabelingJob_inputConfig' - Input data for the labeling job, such as the Amazon S3 location of the
-- data objects and the location of the manifest file that describes the
-- data objects.
--
-- You must specify at least one of the following: @S3DataSource@ or
-- @SnsDataSource@.
--
-- -   Use @SnsDataSource@ to specify an SNS input topic for a streaming
--     labeling job. If you do not specify and SNS input topic ARN, Ground
--     Truth will create a one-time labeling job that stops after all data
--     objects in the input manifest file have been labeled.
--
-- -   Use @S3DataSource@ to specify an input manifest file for both
--     streaming and one-time labeling jobs. Adding an @S3DataSource@ is
--     optional if you use @SnsDataSource@ to create a streaming labeling
--     job.
--
-- If you use the Amazon Mechanical Turk workforce, your input data should
-- not include confidential information, personal information or protected
-- health information. Use @ContentClassifiers@ to specify that your data
-- is free of personally identifiable information and adult content.
--
-- 'outputConfig', 'createLabelingJob_outputConfig' - The location of the output data and the AWS Key Management Service key
-- ID for the key used to encrypt the output data, if any.
--
-- 'roleArn', 'createLabelingJob_roleArn' - The Amazon Resource Number (ARN) that Amazon SageMaker assumes to
-- perform tasks on your behalf during data labeling. You must grant this
-- role the necessary permissions so that Amazon SageMaker can successfully
-- complete data labeling.
--
-- 'humanTaskConfig', 'createLabelingJob_humanTaskConfig' - Configures the labeling task and how it is presented to workers;
-- including, but not limited to price, keywords, and batch size (task
-- count).
newCreateLabelingJob ::
  -- | 'labelingJobName'
  Prelude.Text ->
  -- | 'labelAttributeName'
  Prelude.Text ->
  -- | 'inputConfig'
  LabelingJobInputConfig ->
  -- | 'outputConfig'
  LabelingJobOutputConfig ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'humanTaskConfig'
  HumanTaskConfig ->
  CreateLabelingJob
newCreateLabelingJob
  pLabelingJobName_
  pLabelAttributeName_
  pInputConfig_
  pOutputConfig_
  pRoleArn_
  pHumanTaskConfig_ =
    CreateLabelingJob'
      { stoppingConditions =
          Prelude.Nothing,
        labelCategoryConfigS3Uri = Prelude.Nothing,
        labelingJobAlgorithmsConfig = Prelude.Nothing,
        tags = Prelude.Nothing,
        labelingJobName = pLabelingJobName_,
        labelAttributeName = pLabelAttributeName_,
        inputConfig = pInputConfig_,
        outputConfig = pOutputConfig_,
        roleArn = pRoleArn_,
        humanTaskConfig = pHumanTaskConfig_
      }

-- | A set of conditions for stopping the labeling job. If any of the
-- conditions are met, the job is automatically stopped. You can use these
-- conditions to control the cost of data labeling.
createLabelingJob_stoppingConditions :: Lens.Lens' CreateLabelingJob (Prelude.Maybe LabelingJobStoppingConditions)
createLabelingJob_stoppingConditions = Lens.lens (\CreateLabelingJob' {stoppingConditions} -> stoppingConditions) (\s@CreateLabelingJob' {} a -> s {stoppingConditions = a} :: CreateLabelingJob)

-- | The S3 URI of the file, referred to as a /label category configuration
-- file/, that defines the categories used to label the data objects.
--
-- For 3D point cloud and video frame task types, you can add label
-- category attributes and frame attributes to your label category
-- configuration file. To learn how, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-point-cloud-label-category-config.html Create a Labeling Category Configuration File for 3D Point Cloud Labeling Jobs>.
--
-- For all other
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-task-types.html built-in task types>
-- and
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-custom-templates.html custom tasks>,
-- your label category configuration file must be a JSON file in the
-- following format. Identify the labels you want to use by replacing
-- @label_1@, @label_2@,@...@,@label_n@ with your label categories.
--
-- @{ @
--
-- @\"document-version\": \"2018-11-28\",@
--
-- @\"labels\": [{\"label\": \"label_1\"},{\"label\": \"label_2\"},...{\"label\": \"label_n\"}]@
--
-- @}@
--
-- Note the following about the label category configuration file:
--
-- -   For image classification and text classification (single and
--     multi-label) you must specify at least two label categories. For all
--     other task types, the minimum number of label categories required is
--     one.
--
-- -   Each label category must be unique, you cannot specify duplicate
--     label categories.
--
-- -   If you create a 3D point cloud or video frame adjustment or
--     verification labeling job, you must include
--     @auditLabelAttributeName@ in the label category configuration. Use
--     this parameter to enter the
--     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateLabelingJob.html#sagemaker-CreateLabelingJob-request-LabelAttributeName LabelAttributeName>
--     of the labeling job you want to adjust or verify annotations of.
createLabelingJob_labelCategoryConfigS3Uri :: Lens.Lens' CreateLabelingJob (Prelude.Maybe Prelude.Text)
createLabelingJob_labelCategoryConfigS3Uri = Lens.lens (\CreateLabelingJob' {labelCategoryConfigS3Uri} -> labelCategoryConfigS3Uri) (\s@CreateLabelingJob' {} a -> s {labelCategoryConfigS3Uri = a} :: CreateLabelingJob)

-- | Configures the information required to perform automated data labeling.
createLabelingJob_labelingJobAlgorithmsConfig :: Lens.Lens' CreateLabelingJob (Prelude.Maybe LabelingJobAlgorithmsConfig)
createLabelingJob_labelingJobAlgorithmsConfig = Lens.lens (\CreateLabelingJob' {labelingJobAlgorithmsConfig} -> labelingJobAlgorithmsConfig) (\s@CreateLabelingJob' {} a -> s {labelingJobAlgorithmsConfig = a} :: CreateLabelingJob)

-- | An array of key\/value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags>
-- in the /AWS Billing and Cost Management User Guide/.
createLabelingJob_tags :: Lens.Lens' CreateLabelingJob (Prelude.Maybe [Tag])
createLabelingJob_tags = Lens.lens (\CreateLabelingJob' {tags} -> tags) (\s@CreateLabelingJob' {} a -> s {tags = a} :: CreateLabelingJob) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the labeling job. This name is used to identify the job in a
-- list of labeling jobs. Labeling job names must be unique within an AWS
-- account and region. @LabelingJobName@ is not case sensitive. For
-- example, Example-job and example-job are considered the same labeling
-- job name by Ground Truth.
createLabelingJob_labelingJobName :: Lens.Lens' CreateLabelingJob Prelude.Text
createLabelingJob_labelingJobName = Lens.lens (\CreateLabelingJob' {labelingJobName} -> labelingJobName) (\s@CreateLabelingJob' {} a -> s {labelingJobName = a} :: CreateLabelingJob)

-- | The attribute name to use for the label in the output manifest file.
-- This is the key for the key\/value pair formed with the label that a
-- worker assigns to the object. The @LabelAttributeName@ must meet the
-- following requirements.
--
-- -   The name can\'t end with \"-metadata\".
--
-- -   If you are using one of the following
--     <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-task-types.html built-in task types>,
--     the attribute name /must/ end with \"-ref\". If the task type you
--     are using is not listed below, the attribute name /must not/ end
--     with \"-ref\".
--
--     -   Image semantic segmentation (@SemanticSegmentation)@, and
--         adjustment (@AdjustmentSemanticSegmentation@) and verification
--         (@VerificationSemanticSegmentation@) labeling jobs for this task
--         type.
--
--     -   Video frame object detection (@VideoObjectDetection@), and
--         adjustment and verification (@AdjustmentVideoObjectDetection@)
--         labeling jobs for this task type.
--
--     -   Video frame object tracking (@VideoObjectTracking@), and
--         adjustment and verification (@AdjustmentVideoObjectTracking@)
--         labeling jobs for this task type.
--
--     -   3D point cloud semantic segmentation
--         (@3DPointCloudSemanticSegmentation@), and adjustment and
--         verification (@Adjustment3DPointCloudSemanticSegmentation@)
--         labeling jobs for this task type.
--
--     -   3D point cloud object tracking (@3DPointCloudObjectTracking@),
--         and adjustment and verification
--         (@Adjustment3DPointCloudObjectTracking@) labeling jobs for this
--         task type.
--
-- If you are creating an adjustment or verification labeling job, you must
-- use a /different/ @LabelAttributeName@ than the one used in the original
-- labeling job. The original labeling job is the Ground Truth labeling job
-- that produced the labels that you want verified or adjusted. To learn
-- more about adjustment and verification labeling jobs, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-verification-data.html Verify and Adjust Labels>.
createLabelingJob_labelAttributeName :: Lens.Lens' CreateLabelingJob Prelude.Text
createLabelingJob_labelAttributeName = Lens.lens (\CreateLabelingJob' {labelAttributeName} -> labelAttributeName) (\s@CreateLabelingJob' {} a -> s {labelAttributeName = a} :: CreateLabelingJob)

-- | Input data for the labeling job, such as the Amazon S3 location of the
-- data objects and the location of the manifest file that describes the
-- data objects.
--
-- You must specify at least one of the following: @S3DataSource@ or
-- @SnsDataSource@.
--
-- -   Use @SnsDataSource@ to specify an SNS input topic for a streaming
--     labeling job. If you do not specify and SNS input topic ARN, Ground
--     Truth will create a one-time labeling job that stops after all data
--     objects in the input manifest file have been labeled.
--
-- -   Use @S3DataSource@ to specify an input manifest file for both
--     streaming and one-time labeling jobs. Adding an @S3DataSource@ is
--     optional if you use @SnsDataSource@ to create a streaming labeling
--     job.
--
-- If you use the Amazon Mechanical Turk workforce, your input data should
-- not include confidential information, personal information or protected
-- health information. Use @ContentClassifiers@ to specify that your data
-- is free of personally identifiable information and adult content.
createLabelingJob_inputConfig :: Lens.Lens' CreateLabelingJob LabelingJobInputConfig
createLabelingJob_inputConfig = Lens.lens (\CreateLabelingJob' {inputConfig} -> inputConfig) (\s@CreateLabelingJob' {} a -> s {inputConfig = a} :: CreateLabelingJob)

-- | The location of the output data and the AWS Key Management Service key
-- ID for the key used to encrypt the output data, if any.
createLabelingJob_outputConfig :: Lens.Lens' CreateLabelingJob LabelingJobOutputConfig
createLabelingJob_outputConfig = Lens.lens (\CreateLabelingJob' {outputConfig} -> outputConfig) (\s@CreateLabelingJob' {} a -> s {outputConfig = a} :: CreateLabelingJob)

-- | The Amazon Resource Number (ARN) that Amazon SageMaker assumes to
-- perform tasks on your behalf during data labeling. You must grant this
-- role the necessary permissions so that Amazon SageMaker can successfully
-- complete data labeling.
createLabelingJob_roleArn :: Lens.Lens' CreateLabelingJob Prelude.Text
createLabelingJob_roleArn = Lens.lens (\CreateLabelingJob' {roleArn} -> roleArn) (\s@CreateLabelingJob' {} a -> s {roleArn = a} :: CreateLabelingJob)

-- | Configures the labeling task and how it is presented to workers;
-- including, but not limited to price, keywords, and batch size (task
-- count).
createLabelingJob_humanTaskConfig :: Lens.Lens' CreateLabelingJob HumanTaskConfig
createLabelingJob_humanTaskConfig = Lens.lens (\CreateLabelingJob' {humanTaskConfig} -> humanTaskConfig) (\s@CreateLabelingJob' {} a -> s {humanTaskConfig = a} :: CreateLabelingJob)

instance Prelude.AWSRequest CreateLabelingJob where
  type Rs CreateLabelingJob = CreateLabelingJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLabelingJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "LabelingJobArn")
      )

instance Prelude.Hashable CreateLabelingJob

instance Prelude.NFData CreateLabelingJob

instance Prelude.ToHeaders CreateLabelingJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.CreateLabelingJob" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateLabelingJob where
  toJSON CreateLabelingJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("StoppingConditions" Prelude..=)
              Prelude.<$> stoppingConditions,
            ("LabelCategoryConfigS3Uri" Prelude..=)
              Prelude.<$> labelCategoryConfigS3Uri,
            ("LabelingJobAlgorithmsConfig" Prelude..=)
              Prelude.<$> labelingJobAlgorithmsConfig,
            ("Tags" Prelude..=) Prelude.<$> tags,
            Prelude.Just
              ("LabelingJobName" Prelude..= labelingJobName),
            Prelude.Just
              ("LabelAttributeName" Prelude..= labelAttributeName),
            Prelude.Just ("InputConfig" Prelude..= inputConfig),
            Prelude.Just
              ("OutputConfig" Prelude..= outputConfig),
            Prelude.Just ("RoleArn" Prelude..= roleArn),
            Prelude.Just
              ("HumanTaskConfig" Prelude..= humanTaskConfig)
          ]
      )

instance Prelude.ToPath CreateLabelingJob where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateLabelingJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLabelingJobResponse' smart constructor.
data CreateLabelingJobResponse = CreateLabelingJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the labeling job. You use this ARN to
    -- identify the labeling job.
    labelingJobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateLabelingJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createLabelingJobResponse_httpStatus' - The response's http status code.
--
-- 'labelingJobArn', 'createLabelingJobResponse_labelingJobArn' - The Amazon Resource Name (ARN) of the labeling job. You use this ARN to
-- identify the labeling job.
newCreateLabelingJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'labelingJobArn'
  Prelude.Text ->
  CreateLabelingJobResponse
newCreateLabelingJobResponse
  pHttpStatus_
  pLabelingJobArn_ =
    CreateLabelingJobResponse'
      { httpStatus =
          pHttpStatus_,
        labelingJobArn = pLabelingJobArn_
      }

-- | The response's http status code.
createLabelingJobResponse_httpStatus :: Lens.Lens' CreateLabelingJobResponse Prelude.Int
createLabelingJobResponse_httpStatus = Lens.lens (\CreateLabelingJobResponse' {httpStatus} -> httpStatus) (\s@CreateLabelingJobResponse' {} a -> s {httpStatus = a} :: CreateLabelingJobResponse)

-- | The Amazon Resource Name (ARN) of the labeling job. You use this ARN to
-- identify the labeling job.
createLabelingJobResponse_labelingJobArn :: Lens.Lens' CreateLabelingJobResponse Prelude.Text
createLabelingJobResponse_labelingJobArn = Lens.lens (\CreateLabelingJobResponse' {labelingJobArn} -> labelingJobArn) (\s@CreateLabelingJobResponse' {} a -> s {labelingJobArn = a} :: CreateLabelingJobResponse)

instance Prelude.NFData CreateLabelingJobResponse
