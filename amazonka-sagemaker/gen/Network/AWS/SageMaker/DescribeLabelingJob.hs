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
-- Module      : Network.AWS.SageMaker.DescribeLabelingJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a labeling job.
module Network.AWS.SageMaker.DescribeLabelingJob
  ( -- * Creating a Request
    DescribeLabelingJob (..),
    newDescribeLabelingJob,

    -- * Request Lenses
    describeLabelingJob_labelingJobName,

    -- * Destructuring the Response
    DescribeLabelingJobResponse (..),
    newDescribeLabelingJobResponse,

    -- * Response Lenses
    describeLabelingJobResponse_stoppingConditions,
    describeLabelingJobResponse_labelAttributeName,
    describeLabelingJobResponse_labelCategoryConfigS3Uri,
    describeLabelingJobResponse_labelingJobAlgorithmsConfig,
    describeLabelingJobResponse_failureReason,
    describeLabelingJobResponse_tags,
    describeLabelingJobResponse_labelingJobOutput,
    describeLabelingJobResponse_httpStatus,
    describeLabelingJobResponse_labelingJobStatus,
    describeLabelingJobResponse_labelCounters,
    describeLabelingJobResponse_creationTime,
    describeLabelingJobResponse_lastModifiedTime,
    describeLabelingJobResponse_jobReferenceCode,
    describeLabelingJobResponse_labelingJobName,
    describeLabelingJobResponse_labelingJobArn,
    describeLabelingJobResponse_inputConfig,
    describeLabelingJobResponse_outputConfig,
    describeLabelingJobResponse_roleArn,
    describeLabelingJobResponse_humanTaskConfig,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeLabelingJob' smart constructor.
data DescribeLabelingJob = DescribeLabelingJob'
  { -- | The name of the labeling job to return information for.
    labelingJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeLabelingJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labelingJobName', 'describeLabelingJob_labelingJobName' - The name of the labeling job to return information for.
newDescribeLabelingJob ::
  -- | 'labelingJobName'
  Prelude.Text ->
  DescribeLabelingJob
newDescribeLabelingJob pLabelingJobName_ =
  DescribeLabelingJob'
    { labelingJobName =
        pLabelingJobName_
    }

-- | The name of the labeling job to return information for.
describeLabelingJob_labelingJobName :: Lens.Lens' DescribeLabelingJob Prelude.Text
describeLabelingJob_labelingJobName = Lens.lens (\DescribeLabelingJob' {labelingJobName} -> labelingJobName) (\s@DescribeLabelingJob' {} a -> s {labelingJobName = a} :: DescribeLabelingJob)

instance Prelude.AWSRequest DescribeLabelingJob where
  type
    Rs DescribeLabelingJob =
      DescribeLabelingJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLabelingJobResponse'
            Prelude.<$> (x Prelude..?> "StoppingConditions")
            Prelude.<*> (x Prelude..?> "LabelAttributeName")
            Prelude.<*> (x Prelude..?> "LabelCategoryConfigS3Uri")
            Prelude.<*> (x Prelude..?> "LabelingJobAlgorithmsConfig")
            Prelude.<*> (x Prelude..?> "FailureReason")
            Prelude.<*> (x Prelude..?> "Tags" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "LabelingJobOutput")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "LabelingJobStatus")
            Prelude.<*> (x Prelude..:> "LabelCounters")
            Prelude.<*> (x Prelude..:> "CreationTime")
            Prelude.<*> (x Prelude..:> "LastModifiedTime")
            Prelude.<*> (x Prelude..:> "JobReferenceCode")
            Prelude.<*> (x Prelude..:> "LabelingJobName")
            Prelude.<*> (x Prelude..:> "LabelingJobArn")
            Prelude.<*> (x Prelude..:> "InputConfig")
            Prelude.<*> (x Prelude..:> "OutputConfig")
            Prelude.<*> (x Prelude..:> "RoleArn")
            Prelude.<*> (x Prelude..:> "HumanTaskConfig")
      )

instance Prelude.Hashable DescribeLabelingJob

instance Prelude.NFData DescribeLabelingJob

instance Prelude.ToHeaders DescribeLabelingJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.DescribeLabelingJob" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeLabelingJob where
  toJSON DescribeLabelingJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("LabelingJobName" Prelude..= labelingJobName)
          ]
      )

instance Prelude.ToPath DescribeLabelingJob where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeLabelingJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeLabelingJobResponse' smart constructor.
data DescribeLabelingJobResponse = DescribeLabelingJobResponse'
  { -- | A set of conditions for stopping a labeling job. If any of the
    -- conditions are met, the job is automatically stopped.
    stoppingConditions :: Prelude.Maybe LabelingJobStoppingConditions,
    -- | The attribute used as the label in the output manifest file.
    labelAttributeName :: Prelude.Maybe Prelude.Text,
    -- | The S3 location of the JSON file that defines the categories used to
    -- label data objects. Please note the following label-category limits:
    --
    -- -   Semantic segmentation labeling jobs using automated labeling: 20
    --     labels
    --
    -- -   Box bounding labeling jobs (all): 10 labels
    --
    -- The file is a JSON structure in the following format:
    --
    -- @{@
    --
    -- @ \"document-version\": \"2018-11-28\"@
    --
    -- @ \"labels\": [@
    --
    -- @ {@
    --
    -- @ \"label\": \"label 1\"@
    --
    -- @ },@
    --
    -- @ {@
    --
    -- @ \"label\": \"label 2\"@
    --
    -- @ },@
    --
    -- @ ...@
    --
    -- @ {@
    --
    -- @ \"label\": \"label n\"@
    --
    -- @ }@
    --
    -- @ ]@
    --
    -- @}@
    labelCategoryConfigS3Uri :: Prelude.Maybe Prelude.Text,
    -- | Configuration information for automated data labeling.
    labelingJobAlgorithmsConfig :: Prelude.Maybe LabelingJobAlgorithmsConfig,
    -- | If the job failed, the reason that it failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | An array of key-value pairs. You can use tags to categorize your AWS
    -- resources in different ways, for example, by purpose, owner, or
    -- environment. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>.
    tags :: Prelude.Maybe [Tag],
    -- | The location of the output produced by the labeling job.
    labelingJobOutput :: Prelude.Maybe LabelingJobOutput,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The processing status of the labeling job.
    labelingJobStatus :: LabelingJobStatus,
    -- | Provides a breakdown of the number of data objects labeled by humans,
    -- the number of objects labeled by machine, the number of objects than
    -- couldn\'t be labeled, and the total number of objects labeled.
    labelCounters :: LabelCounters,
    -- | The date and time that the labeling job was created.
    creationTime :: Prelude.POSIX,
    -- | The date and time that the labeling job was last updated.
    lastModifiedTime :: Prelude.POSIX,
    -- | A unique identifier for work done as part of a labeling job.
    jobReferenceCode :: Prelude.Text,
    -- | The name assigned to the labeling job when it was created.
    labelingJobName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the labeling job.
    labelingJobArn :: Prelude.Text,
    -- | Input configuration information for the labeling job, such as the Amazon
    -- S3 location of the data objects and the location of the manifest file
    -- that describes the data objects.
    inputConfig :: LabelingJobInputConfig,
    -- | The location of the job\'s output data and the AWS Key Management
    -- Service key ID for the key used to encrypt the output data, if any.
    outputConfig :: LabelingJobOutputConfig,
    -- | The Amazon Resource Name (ARN) that Amazon SageMaker assumes to perform
    -- tasks on your behalf during data labeling.
    roleArn :: Prelude.Text,
    -- | Configuration information required for human workers to complete a
    -- labeling task.
    humanTaskConfig :: HumanTaskConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeLabelingJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stoppingConditions', 'describeLabelingJobResponse_stoppingConditions' - A set of conditions for stopping a labeling job. If any of the
-- conditions are met, the job is automatically stopped.
--
-- 'labelAttributeName', 'describeLabelingJobResponse_labelAttributeName' - The attribute used as the label in the output manifest file.
--
-- 'labelCategoryConfigS3Uri', 'describeLabelingJobResponse_labelCategoryConfigS3Uri' - The S3 location of the JSON file that defines the categories used to
-- label data objects. Please note the following label-category limits:
--
-- -   Semantic segmentation labeling jobs using automated labeling: 20
--     labels
--
-- -   Box bounding labeling jobs (all): 10 labels
--
-- The file is a JSON structure in the following format:
--
-- @{@
--
-- @ \"document-version\": \"2018-11-28\"@
--
-- @ \"labels\": [@
--
-- @ {@
--
-- @ \"label\": \"label 1\"@
--
-- @ },@
--
-- @ {@
--
-- @ \"label\": \"label 2\"@
--
-- @ },@
--
-- @ ...@
--
-- @ {@
--
-- @ \"label\": \"label n\"@
--
-- @ }@
--
-- @ ]@
--
-- @}@
--
-- 'labelingJobAlgorithmsConfig', 'describeLabelingJobResponse_labelingJobAlgorithmsConfig' - Configuration information for automated data labeling.
--
-- 'failureReason', 'describeLabelingJobResponse_failureReason' - If the job failed, the reason that it failed.
--
-- 'tags', 'describeLabelingJobResponse_tags' - An array of key-value pairs. You can use tags to categorize your AWS
-- resources in different ways, for example, by purpose, owner, or
-- environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>.
--
-- 'labelingJobOutput', 'describeLabelingJobResponse_labelingJobOutput' - The location of the output produced by the labeling job.
--
-- 'httpStatus', 'describeLabelingJobResponse_httpStatus' - The response's http status code.
--
-- 'labelingJobStatus', 'describeLabelingJobResponse_labelingJobStatus' - The processing status of the labeling job.
--
-- 'labelCounters', 'describeLabelingJobResponse_labelCounters' - Provides a breakdown of the number of data objects labeled by humans,
-- the number of objects labeled by machine, the number of objects than
-- couldn\'t be labeled, and the total number of objects labeled.
--
-- 'creationTime', 'describeLabelingJobResponse_creationTime' - The date and time that the labeling job was created.
--
-- 'lastModifiedTime', 'describeLabelingJobResponse_lastModifiedTime' - The date and time that the labeling job was last updated.
--
-- 'jobReferenceCode', 'describeLabelingJobResponse_jobReferenceCode' - A unique identifier for work done as part of a labeling job.
--
-- 'labelingJobName', 'describeLabelingJobResponse_labelingJobName' - The name assigned to the labeling job when it was created.
--
-- 'labelingJobArn', 'describeLabelingJobResponse_labelingJobArn' - The Amazon Resource Name (ARN) of the labeling job.
--
-- 'inputConfig', 'describeLabelingJobResponse_inputConfig' - Input configuration information for the labeling job, such as the Amazon
-- S3 location of the data objects and the location of the manifest file
-- that describes the data objects.
--
-- 'outputConfig', 'describeLabelingJobResponse_outputConfig' - The location of the job\'s output data and the AWS Key Management
-- Service key ID for the key used to encrypt the output data, if any.
--
-- 'roleArn', 'describeLabelingJobResponse_roleArn' - The Amazon Resource Name (ARN) that Amazon SageMaker assumes to perform
-- tasks on your behalf during data labeling.
--
-- 'humanTaskConfig', 'describeLabelingJobResponse_humanTaskConfig' - Configuration information required for human workers to complete a
-- labeling task.
newDescribeLabelingJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'labelingJobStatus'
  LabelingJobStatus ->
  -- | 'labelCounters'
  LabelCounters ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  -- | 'jobReferenceCode'
  Prelude.Text ->
  -- | 'labelingJobName'
  Prelude.Text ->
  -- | 'labelingJobArn'
  Prelude.Text ->
  -- | 'inputConfig'
  LabelingJobInputConfig ->
  -- | 'outputConfig'
  LabelingJobOutputConfig ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'humanTaskConfig'
  HumanTaskConfig ->
  DescribeLabelingJobResponse
newDescribeLabelingJobResponse
  pHttpStatus_
  pLabelingJobStatus_
  pLabelCounters_
  pCreationTime_
  pLastModifiedTime_
  pJobReferenceCode_
  pLabelingJobName_
  pLabelingJobArn_
  pInputConfig_
  pOutputConfig_
  pRoleArn_
  pHumanTaskConfig_ =
    DescribeLabelingJobResponse'
      { stoppingConditions =
          Prelude.Nothing,
        labelAttributeName = Prelude.Nothing,
        labelCategoryConfigS3Uri = Prelude.Nothing,
        labelingJobAlgorithmsConfig = Prelude.Nothing,
        failureReason = Prelude.Nothing,
        tags = Prelude.Nothing,
        labelingJobOutput = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        labelingJobStatus = pLabelingJobStatus_,
        labelCounters = pLabelCounters_,
        creationTime =
          Prelude._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Prelude._Time Lens.# pLastModifiedTime_,
        jobReferenceCode = pJobReferenceCode_,
        labelingJobName = pLabelingJobName_,
        labelingJobArn = pLabelingJobArn_,
        inputConfig = pInputConfig_,
        outputConfig = pOutputConfig_,
        roleArn = pRoleArn_,
        humanTaskConfig = pHumanTaskConfig_
      }

-- | A set of conditions for stopping a labeling job. If any of the
-- conditions are met, the job is automatically stopped.
describeLabelingJobResponse_stoppingConditions :: Lens.Lens' DescribeLabelingJobResponse (Prelude.Maybe LabelingJobStoppingConditions)
describeLabelingJobResponse_stoppingConditions = Lens.lens (\DescribeLabelingJobResponse' {stoppingConditions} -> stoppingConditions) (\s@DescribeLabelingJobResponse' {} a -> s {stoppingConditions = a} :: DescribeLabelingJobResponse)

-- | The attribute used as the label in the output manifest file.
describeLabelingJobResponse_labelAttributeName :: Lens.Lens' DescribeLabelingJobResponse (Prelude.Maybe Prelude.Text)
describeLabelingJobResponse_labelAttributeName = Lens.lens (\DescribeLabelingJobResponse' {labelAttributeName} -> labelAttributeName) (\s@DescribeLabelingJobResponse' {} a -> s {labelAttributeName = a} :: DescribeLabelingJobResponse)

-- | The S3 location of the JSON file that defines the categories used to
-- label data objects. Please note the following label-category limits:
--
-- -   Semantic segmentation labeling jobs using automated labeling: 20
--     labels
--
-- -   Box bounding labeling jobs (all): 10 labels
--
-- The file is a JSON structure in the following format:
--
-- @{@
--
-- @ \"document-version\": \"2018-11-28\"@
--
-- @ \"labels\": [@
--
-- @ {@
--
-- @ \"label\": \"label 1\"@
--
-- @ },@
--
-- @ {@
--
-- @ \"label\": \"label 2\"@
--
-- @ },@
--
-- @ ...@
--
-- @ {@
--
-- @ \"label\": \"label n\"@
--
-- @ }@
--
-- @ ]@
--
-- @}@
describeLabelingJobResponse_labelCategoryConfigS3Uri :: Lens.Lens' DescribeLabelingJobResponse (Prelude.Maybe Prelude.Text)
describeLabelingJobResponse_labelCategoryConfigS3Uri = Lens.lens (\DescribeLabelingJobResponse' {labelCategoryConfigS3Uri} -> labelCategoryConfigS3Uri) (\s@DescribeLabelingJobResponse' {} a -> s {labelCategoryConfigS3Uri = a} :: DescribeLabelingJobResponse)

-- | Configuration information for automated data labeling.
describeLabelingJobResponse_labelingJobAlgorithmsConfig :: Lens.Lens' DescribeLabelingJobResponse (Prelude.Maybe LabelingJobAlgorithmsConfig)
describeLabelingJobResponse_labelingJobAlgorithmsConfig = Lens.lens (\DescribeLabelingJobResponse' {labelingJobAlgorithmsConfig} -> labelingJobAlgorithmsConfig) (\s@DescribeLabelingJobResponse' {} a -> s {labelingJobAlgorithmsConfig = a} :: DescribeLabelingJobResponse)

-- | If the job failed, the reason that it failed.
describeLabelingJobResponse_failureReason :: Lens.Lens' DescribeLabelingJobResponse (Prelude.Maybe Prelude.Text)
describeLabelingJobResponse_failureReason = Lens.lens (\DescribeLabelingJobResponse' {failureReason} -> failureReason) (\s@DescribeLabelingJobResponse' {} a -> s {failureReason = a} :: DescribeLabelingJobResponse)

-- | An array of key-value pairs. You can use tags to categorize your AWS
-- resources in different ways, for example, by purpose, owner, or
-- environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>.
describeLabelingJobResponse_tags :: Lens.Lens' DescribeLabelingJobResponse (Prelude.Maybe [Tag])
describeLabelingJobResponse_tags = Lens.lens (\DescribeLabelingJobResponse' {tags} -> tags) (\s@DescribeLabelingJobResponse' {} a -> s {tags = a} :: DescribeLabelingJobResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The location of the output produced by the labeling job.
describeLabelingJobResponse_labelingJobOutput :: Lens.Lens' DescribeLabelingJobResponse (Prelude.Maybe LabelingJobOutput)
describeLabelingJobResponse_labelingJobOutput = Lens.lens (\DescribeLabelingJobResponse' {labelingJobOutput} -> labelingJobOutput) (\s@DescribeLabelingJobResponse' {} a -> s {labelingJobOutput = a} :: DescribeLabelingJobResponse)

-- | The response's http status code.
describeLabelingJobResponse_httpStatus :: Lens.Lens' DescribeLabelingJobResponse Prelude.Int
describeLabelingJobResponse_httpStatus = Lens.lens (\DescribeLabelingJobResponse' {httpStatus} -> httpStatus) (\s@DescribeLabelingJobResponse' {} a -> s {httpStatus = a} :: DescribeLabelingJobResponse)

-- | The processing status of the labeling job.
describeLabelingJobResponse_labelingJobStatus :: Lens.Lens' DescribeLabelingJobResponse LabelingJobStatus
describeLabelingJobResponse_labelingJobStatus = Lens.lens (\DescribeLabelingJobResponse' {labelingJobStatus} -> labelingJobStatus) (\s@DescribeLabelingJobResponse' {} a -> s {labelingJobStatus = a} :: DescribeLabelingJobResponse)

-- | Provides a breakdown of the number of data objects labeled by humans,
-- the number of objects labeled by machine, the number of objects than
-- couldn\'t be labeled, and the total number of objects labeled.
describeLabelingJobResponse_labelCounters :: Lens.Lens' DescribeLabelingJobResponse LabelCounters
describeLabelingJobResponse_labelCounters = Lens.lens (\DescribeLabelingJobResponse' {labelCounters} -> labelCounters) (\s@DescribeLabelingJobResponse' {} a -> s {labelCounters = a} :: DescribeLabelingJobResponse)

-- | The date and time that the labeling job was created.
describeLabelingJobResponse_creationTime :: Lens.Lens' DescribeLabelingJobResponse Prelude.UTCTime
describeLabelingJobResponse_creationTime = Lens.lens (\DescribeLabelingJobResponse' {creationTime} -> creationTime) (\s@DescribeLabelingJobResponse' {} a -> s {creationTime = a} :: DescribeLabelingJobResponse) Prelude.. Prelude._Time

-- | The date and time that the labeling job was last updated.
describeLabelingJobResponse_lastModifiedTime :: Lens.Lens' DescribeLabelingJobResponse Prelude.UTCTime
describeLabelingJobResponse_lastModifiedTime = Lens.lens (\DescribeLabelingJobResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeLabelingJobResponse' {} a -> s {lastModifiedTime = a} :: DescribeLabelingJobResponse) Prelude.. Prelude._Time

-- | A unique identifier for work done as part of a labeling job.
describeLabelingJobResponse_jobReferenceCode :: Lens.Lens' DescribeLabelingJobResponse Prelude.Text
describeLabelingJobResponse_jobReferenceCode = Lens.lens (\DescribeLabelingJobResponse' {jobReferenceCode} -> jobReferenceCode) (\s@DescribeLabelingJobResponse' {} a -> s {jobReferenceCode = a} :: DescribeLabelingJobResponse)

-- | The name assigned to the labeling job when it was created.
describeLabelingJobResponse_labelingJobName :: Lens.Lens' DescribeLabelingJobResponse Prelude.Text
describeLabelingJobResponse_labelingJobName = Lens.lens (\DescribeLabelingJobResponse' {labelingJobName} -> labelingJobName) (\s@DescribeLabelingJobResponse' {} a -> s {labelingJobName = a} :: DescribeLabelingJobResponse)

-- | The Amazon Resource Name (ARN) of the labeling job.
describeLabelingJobResponse_labelingJobArn :: Lens.Lens' DescribeLabelingJobResponse Prelude.Text
describeLabelingJobResponse_labelingJobArn = Lens.lens (\DescribeLabelingJobResponse' {labelingJobArn} -> labelingJobArn) (\s@DescribeLabelingJobResponse' {} a -> s {labelingJobArn = a} :: DescribeLabelingJobResponse)

-- | Input configuration information for the labeling job, such as the Amazon
-- S3 location of the data objects and the location of the manifest file
-- that describes the data objects.
describeLabelingJobResponse_inputConfig :: Lens.Lens' DescribeLabelingJobResponse LabelingJobInputConfig
describeLabelingJobResponse_inputConfig = Lens.lens (\DescribeLabelingJobResponse' {inputConfig} -> inputConfig) (\s@DescribeLabelingJobResponse' {} a -> s {inputConfig = a} :: DescribeLabelingJobResponse)

-- | The location of the job\'s output data and the AWS Key Management
-- Service key ID for the key used to encrypt the output data, if any.
describeLabelingJobResponse_outputConfig :: Lens.Lens' DescribeLabelingJobResponse LabelingJobOutputConfig
describeLabelingJobResponse_outputConfig = Lens.lens (\DescribeLabelingJobResponse' {outputConfig} -> outputConfig) (\s@DescribeLabelingJobResponse' {} a -> s {outputConfig = a} :: DescribeLabelingJobResponse)

-- | The Amazon Resource Name (ARN) that Amazon SageMaker assumes to perform
-- tasks on your behalf during data labeling.
describeLabelingJobResponse_roleArn :: Lens.Lens' DescribeLabelingJobResponse Prelude.Text
describeLabelingJobResponse_roleArn = Lens.lens (\DescribeLabelingJobResponse' {roleArn} -> roleArn) (\s@DescribeLabelingJobResponse' {} a -> s {roleArn = a} :: DescribeLabelingJobResponse)

-- | Configuration information required for human workers to complete a
-- labeling task.
describeLabelingJobResponse_humanTaskConfig :: Lens.Lens' DescribeLabelingJobResponse HumanTaskConfig
describeLabelingJobResponse_humanTaskConfig = Lens.lens (\DescribeLabelingJobResponse' {humanTaskConfig} -> humanTaskConfig) (\s@DescribeLabelingJobResponse' {} a -> s {humanTaskConfig = a} :: DescribeLabelingJobResponse)

instance Prelude.NFData DescribeLabelingJobResponse
