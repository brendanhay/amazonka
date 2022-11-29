{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.Types.TransformJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TransformJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.BatchStrategy
import Amazonka.SageMaker.Types.DataProcessing
import Amazonka.SageMaker.Types.ExperimentConfig
import Amazonka.SageMaker.Types.ModelClientConfig
import Amazonka.SageMaker.Types.Tag
import Amazonka.SageMaker.Types.TransformInput
import Amazonka.SageMaker.Types.TransformJobStatus
import Amazonka.SageMaker.Types.TransformOutput
import Amazonka.SageMaker.Types.TransformResources

-- | A batch transform job. For information about SageMaker batch transform,
-- see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform.html Use Batch Transform>.
--
-- /See:/ 'newTransformJob' smart constructor.
data TransformJob = TransformJob'
  { -- | A list of tags associated with the transform job.
    tags :: Prelude.Maybe [Tag],
    -- | The maximum number of parallel requests that can be sent to each
    -- instance in a transform job. If @MaxConcurrentTransforms@ is set to 0 or
    -- left unset, SageMaker checks the optional execution-parameters to
    -- determine the settings for your chosen algorithm. If the
    -- execution-parameters endpoint is not enabled, the default value is 1.
    -- For built-in algorithms, you don\'t need to set a value for
    -- @MaxConcurrentTransforms@.
    maxConcurrentTransforms :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the transform job.
    transformJobArn :: Prelude.Maybe Prelude.Text,
    modelClientConfig :: Prelude.Maybe ModelClientConfig,
    -- | The environment variables to set in the Docker container. We support up
    -- to 16 key and values entries in the map.
    environment :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    transformOutput :: Prelude.Maybe TransformOutput,
    experimentConfig :: Prelude.Maybe ExperimentConfig,
    transformInput :: Prelude.Maybe TransformInput,
    -- | The name of the transform job.
    transformJobName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the AutoML job that created the
    -- transform job.
    autoMLJobArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the transform job.
    --
    -- Transform job statuses are:
    --
    -- -   @InProgress@ - The job is in progress.
    --
    -- -   @Completed@ - The job has completed.
    --
    -- -   @Failed@ - The transform job has failed. To see the reason for the
    --     failure, see the @FailureReason@ field in the response to a
    --     @DescribeTransformJob@ call.
    --
    -- -   @Stopping@ - The transform job is stopping.
    --
    -- -   @Stopped@ - The transform job has stopped.
    transformJobStatus :: Prelude.Maybe TransformJobStatus,
    -- | Indicates when the transform job has been completed, or has stopped or
    -- failed. You are billed for the time interval between this time and the
    -- value of @TransformStartTime@.
    transformEndTime :: Prelude.Maybe Core.POSIX,
    -- | The maximum allowed size of the payload, in MB. A payload is the data
    -- portion of a record (without metadata). The value in @MaxPayloadInMB@
    -- must be greater than, or equal to, the size of a single record. To
    -- estimate the size of a record in MB, divide the size of your dataset by
    -- the number of records. To ensure that the records fit within the maximum
    -- payload size, we recommend using a slightly larger value. The default
    -- value is 6 MB. For cases where the payload might be arbitrarily large
    -- and is transmitted using HTTP chunked encoding, set the value to 0. This
    -- feature works only in supported algorithms. Currently, SageMaker
    -- built-in algorithms do not support HTTP chunked encoding.
    maxPayloadInMB :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the number of records to include in a mini-batch for an HTTP
    -- inference request. A record is a single unit of input data that
    -- inference can be made on. For example, a single line in a CSV file is a
    -- record.
    batchStrategy :: Prelude.Maybe BatchStrategy,
    -- | The Amazon Resource Name (ARN) of the labeling job that created the
    -- transform job.
    labelingJobArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the model associated with the transform job.
    modelName :: Prelude.Maybe Prelude.Text,
    transformResources :: Prelude.Maybe TransformResources,
    -- | A timestamp that shows when the transform Job was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    dataProcessing :: Prelude.Maybe DataProcessing,
    -- | Indicates when the transform job starts on ML instances. You are billed
    -- for the time interval between this time and the value of
    -- @TransformEndTime@.
    transformStartTime :: Prelude.Maybe Core.POSIX,
    -- | If the transform job failed, the reason it failed.
    failureReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransformJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'transformJob_tags' - A list of tags associated with the transform job.
--
-- 'maxConcurrentTransforms', 'transformJob_maxConcurrentTransforms' - The maximum number of parallel requests that can be sent to each
-- instance in a transform job. If @MaxConcurrentTransforms@ is set to 0 or
-- left unset, SageMaker checks the optional execution-parameters to
-- determine the settings for your chosen algorithm. If the
-- execution-parameters endpoint is not enabled, the default value is 1.
-- For built-in algorithms, you don\'t need to set a value for
-- @MaxConcurrentTransforms@.
--
-- 'transformJobArn', 'transformJob_transformJobArn' - The Amazon Resource Name (ARN) of the transform job.
--
-- 'modelClientConfig', 'transformJob_modelClientConfig' - Undocumented member.
--
-- 'environment', 'transformJob_environment' - The environment variables to set in the Docker container. We support up
-- to 16 key and values entries in the map.
--
-- 'transformOutput', 'transformJob_transformOutput' - Undocumented member.
--
-- 'experimentConfig', 'transformJob_experimentConfig' - Undocumented member.
--
-- 'transformInput', 'transformJob_transformInput' - Undocumented member.
--
-- 'transformJobName', 'transformJob_transformJobName' - The name of the transform job.
--
-- 'autoMLJobArn', 'transformJob_autoMLJobArn' - The Amazon Resource Name (ARN) of the AutoML job that created the
-- transform job.
--
-- 'transformJobStatus', 'transformJob_transformJobStatus' - The status of the transform job.
--
-- Transform job statuses are:
--
-- -   @InProgress@ - The job is in progress.
--
-- -   @Completed@ - The job has completed.
--
-- -   @Failed@ - The transform job has failed. To see the reason for the
--     failure, see the @FailureReason@ field in the response to a
--     @DescribeTransformJob@ call.
--
-- -   @Stopping@ - The transform job is stopping.
--
-- -   @Stopped@ - The transform job has stopped.
--
-- 'transformEndTime', 'transformJob_transformEndTime' - Indicates when the transform job has been completed, or has stopped or
-- failed. You are billed for the time interval between this time and the
-- value of @TransformStartTime@.
--
-- 'maxPayloadInMB', 'transformJob_maxPayloadInMB' - The maximum allowed size of the payload, in MB. A payload is the data
-- portion of a record (without metadata). The value in @MaxPayloadInMB@
-- must be greater than, or equal to, the size of a single record. To
-- estimate the size of a record in MB, divide the size of your dataset by
-- the number of records. To ensure that the records fit within the maximum
-- payload size, we recommend using a slightly larger value. The default
-- value is 6 MB. For cases where the payload might be arbitrarily large
-- and is transmitted using HTTP chunked encoding, set the value to 0. This
-- feature works only in supported algorithms. Currently, SageMaker
-- built-in algorithms do not support HTTP chunked encoding.
--
-- 'batchStrategy', 'transformJob_batchStrategy' - Specifies the number of records to include in a mini-batch for an HTTP
-- inference request. A record is a single unit of input data that
-- inference can be made on. For example, a single line in a CSV file is a
-- record.
--
-- 'labelingJobArn', 'transformJob_labelingJobArn' - The Amazon Resource Name (ARN) of the labeling job that created the
-- transform job.
--
-- 'modelName', 'transformJob_modelName' - The name of the model associated with the transform job.
--
-- 'transformResources', 'transformJob_transformResources' - Undocumented member.
--
-- 'creationTime', 'transformJob_creationTime' - A timestamp that shows when the transform Job was created.
--
-- 'dataProcessing', 'transformJob_dataProcessing' - Undocumented member.
--
-- 'transformStartTime', 'transformJob_transformStartTime' - Indicates when the transform job starts on ML instances. You are billed
-- for the time interval between this time and the value of
-- @TransformEndTime@.
--
-- 'failureReason', 'transformJob_failureReason' - If the transform job failed, the reason it failed.
newTransformJob ::
  TransformJob
newTransformJob =
  TransformJob'
    { tags = Prelude.Nothing,
      maxConcurrentTransforms = Prelude.Nothing,
      transformJobArn = Prelude.Nothing,
      modelClientConfig = Prelude.Nothing,
      environment = Prelude.Nothing,
      transformOutput = Prelude.Nothing,
      experimentConfig = Prelude.Nothing,
      transformInput = Prelude.Nothing,
      transformJobName = Prelude.Nothing,
      autoMLJobArn = Prelude.Nothing,
      transformJobStatus = Prelude.Nothing,
      transformEndTime = Prelude.Nothing,
      maxPayloadInMB = Prelude.Nothing,
      batchStrategy = Prelude.Nothing,
      labelingJobArn = Prelude.Nothing,
      modelName = Prelude.Nothing,
      transformResources = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      dataProcessing = Prelude.Nothing,
      transformStartTime = Prelude.Nothing,
      failureReason = Prelude.Nothing
    }

-- | A list of tags associated with the transform job.
transformJob_tags :: Lens.Lens' TransformJob (Prelude.Maybe [Tag])
transformJob_tags = Lens.lens (\TransformJob' {tags} -> tags) (\s@TransformJob' {} a -> s {tags = a} :: TransformJob) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of parallel requests that can be sent to each
-- instance in a transform job. If @MaxConcurrentTransforms@ is set to 0 or
-- left unset, SageMaker checks the optional execution-parameters to
-- determine the settings for your chosen algorithm. If the
-- execution-parameters endpoint is not enabled, the default value is 1.
-- For built-in algorithms, you don\'t need to set a value for
-- @MaxConcurrentTransforms@.
transformJob_maxConcurrentTransforms :: Lens.Lens' TransformJob (Prelude.Maybe Prelude.Natural)
transformJob_maxConcurrentTransforms = Lens.lens (\TransformJob' {maxConcurrentTransforms} -> maxConcurrentTransforms) (\s@TransformJob' {} a -> s {maxConcurrentTransforms = a} :: TransformJob)

-- | The Amazon Resource Name (ARN) of the transform job.
transformJob_transformJobArn :: Lens.Lens' TransformJob (Prelude.Maybe Prelude.Text)
transformJob_transformJobArn = Lens.lens (\TransformJob' {transformJobArn} -> transformJobArn) (\s@TransformJob' {} a -> s {transformJobArn = a} :: TransformJob)

-- | Undocumented member.
transformJob_modelClientConfig :: Lens.Lens' TransformJob (Prelude.Maybe ModelClientConfig)
transformJob_modelClientConfig = Lens.lens (\TransformJob' {modelClientConfig} -> modelClientConfig) (\s@TransformJob' {} a -> s {modelClientConfig = a} :: TransformJob)

-- | The environment variables to set in the Docker container. We support up
-- to 16 key and values entries in the map.
transformJob_environment :: Lens.Lens' TransformJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
transformJob_environment = Lens.lens (\TransformJob' {environment} -> environment) (\s@TransformJob' {} a -> s {environment = a} :: TransformJob) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
transformJob_transformOutput :: Lens.Lens' TransformJob (Prelude.Maybe TransformOutput)
transformJob_transformOutput = Lens.lens (\TransformJob' {transformOutput} -> transformOutput) (\s@TransformJob' {} a -> s {transformOutput = a} :: TransformJob)

-- | Undocumented member.
transformJob_experimentConfig :: Lens.Lens' TransformJob (Prelude.Maybe ExperimentConfig)
transformJob_experimentConfig = Lens.lens (\TransformJob' {experimentConfig} -> experimentConfig) (\s@TransformJob' {} a -> s {experimentConfig = a} :: TransformJob)

-- | Undocumented member.
transformJob_transformInput :: Lens.Lens' TransformJob (Prelude.Maybe TransformInput)
transformJob_transformInput = Lens.lens (\TransformJob' {transformInput} -> transformInput) (\s@TransformJob' {} a -> s {transformInput = a} :: TransformJob)

-- | The name of the transform job.
transformJob_transformJobName :: Lens.Lens' TransformJob (Prelude.Maybe Prelude.Text)
transformJob_transformJobName = Lens.lens (\TransformJob' {transformJobName} -> transformJobName) (\s@TransformJob' {} a -> s {transformJobName = a} :: TransformJob)

-- | The Amazon Resource Name (ARN) of the AutoML job that created the
-- transform job.
transformJob_autoMLJobArn :: Lens.Lens' TransformJob (Prelude.Maybe Prelude.Text)
transformJob_autoMLJobArn = Lens.lens (\TransformJob' {autoMLJobArn} -> autoMLJobArn) (\s@TransformJob' {} a -> s {autoMLJobArn = a} :: TransformJob)

-- | The status of the transform job.
--
-- Transform job statuses are:
--
-- -   @InProgress@ - The job is in progress.
--
-- -   @Completed@ - The job has completed.
--
-- -   @Failed@ - The transform job has failed. To see the reason for the
--     failure, see the @FailureReason@ field in the response to a
--     @DescribeTransformJob@ call.
--
-- -   @Stopping@ - The transform job is stopping.
--
-- -   @Stopped@ - The transform job has stopped.
transformJob_transformJobStatus :: Lens.Lens' TransformJob (Prelude.Maybe TransformJobStatus)
transformJob_transformJobStatus = Lens.lens (\TransformJob' {transformJobStatus} -> transformJobStatus) (\s@TransformJob' {} a -> s {transformJobStatus = a} :: TransformJob)

-- | Indicates when the transform job has been completed, or has stopped or
-- failed. You are billed for the time interval between this time and the
-- value of @TransformStartTime@.
transformJob_transformEndTime :: Lens.Lens' TransformJob (Prelude.Maybe Prelude.UTCTime)
transformJob_transformEndTime = Lens.lens (\TransformJob' {transformEndTime} -> transformEndTime) (\s@TransformJob' {} a -> s {transformEndTime = a} :: TransformJob) Prelude.. Lens.mapping Core._Time

-- | The maximum allowed size of the payload, in MB. A payload is the data
-- portion of a record (without metadata). The value in @MaxPayloadInMB@
-- must be greater than, or equal to, the size of a single record. To
-- estimate the size of a record in MB, divide the size of your dataset by
-- the number of records. To ensure that the records fit within the maximum
-- payload size, we recommend using a slightly larger value. The default
-- value is 6 MB. For cases where the payload might be arbitrarily large
-- and is transmitted using HTTP chunked encoding, set the value to 0. This
-- feature works only in supported algorithms. Currently, SageMaker
-- built-in algorithms do not support HTTP chunked encoding.
transformJob_maxPayloadInMB :: Lens.Lens' TransformJob (Prelude.Maybe Prelude.Natural)
transformJob_maxPayloadInMB = Lens.lens (\TransformJob' {maxPayloadInMB} -> maxPayloadInMB) (\s@TransformJob' {} a -> s {maxPayloadInMB = a} :: TransformJob)

-- | Specifies the number of records to include in a mini-batch for an HTTP
-- inference request. A record is a single unit of input data that
-- inference can be made on. For example, a single line in a CSV file is a
-- record.
transformJob_batchStrategy :: Lens.Lens' TransformJob (Prelude.Maybe BatchStrategy)
transformJob_batchStrategy = Lens.lens (\TransformJob' {batchStrategy} -> batchStrategy) (\s@TransformJob' {} a -> s {batchStrategy = a} :: TransformJob)

-- | The Amazon Resource Name (ARN) of the labeling job that created the
-- transform job.
transformJob_labelingJobArn :: Lens.Lens' TransformJob (Prelude.Maybe Prelude.Text)
transformJob_labelingJobArn = Lens.lens (\TransformJob' {labelingJobArn} -> labelingJobArn) (\s@TransformJob' {} a -> s {labelingJobArn = a} :: TransformJob)

-- | The name of the model associated with the transform job.
transformJob_modelName :: Lens.Lens' TransformJob (Prelude.Maybe Prelude.Text)
transformJob_modelName = Lens.lens (\TransformJob' {modelName} -> modelName) (\s@TransformJob' {} a -> s {modelName = a} :: TransformJob)

-- | Undocumented member.
transformJob_transformResources :: Lens.Lens' TransformJob (Prelude.Maybe TransformResources)
transformJob_transformResources = Lens.lens (\TransformJob' {transformResources} -> transformResources) (\s@TransformJob' {} a -> s {transformResources = a} :: TransformJob)

-- | A timestamp that shows when the transform Job was created.
transformJob_creationTime :: Lens.Lens' TransformJob (Prelude.Maybe Prelude.UTCTime)
transformJob_creationTime = Lens.lens (\TransformJob' {creationTime} -> creationTime) (\s@TransformJob' {} a -> s {creationTime = a} :: TransformJob) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
transformJob_dataProcessing :: Lens.Lens' TransformJob (Prelude.Maybe DataProcessing)
transformJob_dataProcessing = Lens.lens (\TransformJob' {dataProcessing} -> dataProcessing) (\s@TransformJob' {} a -> s {dataProcessing = a} :: TransformJob)

-- | Indicates when the transform job starts on ML instances. You are billed
-- for the time interval between this time and the value of
-- @TransformEndTime@.
transformJob_transformStartTime :: Lens.Lens' TransformJob (Prelude.Maybe Prelude.UTCTime)
transformJob_transformStartTime = Lens.lens (\TransformJob' {transformStartTime} -> transformStartTime) (\s@TransformJob' {} a -> s {transformStartTime = a} :: TransformJob) Prelude.. Lens.mapping Core._Time

-- | If the transform job failed, the reason it failed.
transformJob_failureReason :: Lens.Lens' TransformJob (Prelude.Maybe Prelude.Text)
transformJob_failureReason = Lens.lens (\TransformJob' {failureReason} -> failureReason) (\s@TransformJob' {} a -> s {failureReason = a} :: TransformJob)

instance Core.FromJSON TransformJob where
  parseJSON =
    Core.withObject
      "TransformJob"
      ( \x ->
          TransformJob'
            Prelude.<$> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "MaxConcurrentTransforms")
            Prelude.<*> (x Core..:? "TransformJobArn")
            Prelude.<*> (x Core..:? "ModelClientConfig")
            Prelude.<*> (x Core..:? "Environment" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "TransformOutput")
            Prelude.<*> (x Core..:? "ExperimentConfig")
            Prelude.<*> (x Core..:? "TransformInput")
            Prelude.<*> (x Core..:? "TransformJobName")
            Prelude.<*> (x Core..:? "AutoMLJobArn")
            Prelude.<*> (x Core..:? "TransformJobStatus")
            Prelude.<*> (x Core..:? "TransformEndTime")
            Prelude.<*> (x Core..:? "MaxPayloadInMB")
            Prelude.<*> (x Core..:? "BatchStrategy")
            Prelude.<*> (x Core..:? "LabelingJobArn")
            Prelude.<*> (x Core..:? "ModelName")
            Prelude.<*> (x Core..:? "TransformResources")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "DataProcessing")
            Prelude.<*> (x Core..:? "TransformStartTime")
            Prelude.<*> (x Core..:? "FailureReason")
      )

instance Prelude.Hashable TransformJob where
  hashWithSalt _salt TransformJob' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` maxConcurrentTransforms
      `Prelude.hashWithSalt` transformJobArn
      `Prelude.hashWithSalt` modelClientConfig
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` transformOutput
      `Prelude.hashWithSalt` experimentConfig
      `Prelude.hashWithSalt` transformInput
      `Prelude.hashWithSalt` transformJobName
      `Prelude.hashWithSalt` autoMLJobArn
      `Prelude.hashWithSalt` transformJobStatus
      `Prelude.hashWithSalt` transformEndTime
      `Prelude.hashWithSalt` maxPayloadInMB
      `Prelude.hashWithSalt` batchStrategy
      `Prelude.hashWithSalt` labelingJobArn
      `Prelude.hashWithSalt` modelName
      `Prelude.hashWithSalt` transformResources
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` dataProcessing
      `Prelude.hashWithSalt` transformStartTime
      `Prelude.hashWithSalt` failureReason

instance Prelude.NFData TransformJob where
  rnf TransformJob' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf maxConcurrentTransforms
      `Prelude.seq` Prelude.rnf transformJobArn
      `Prelude.seq` Prelude.rnf modelClientConfig
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf transformOutput
      `Prelude.seq` Prelude.rnf experimentConfig
      `Prelude.seq` Prelude.rnf transformInput
      `Prelude.seq` Prelude.rnf transformJobName
      `Prelude.seq` Prelude.rnf autoMLJobArn
      `Prelude.seq` Prelude.rnf transformJobStatus
      `Prelude.seq` Prelude.rnf transformEndTime
      `Prelude.seq` Prelude.rnf maxPayloadInMB
      `Prelude.seq` Prelude.rnf batchStrategy
      `Prelude.seq` Prelude.rnf labelingJobArn
      `Prelude.seq` Prelude.rnf modelName
      `Prelude.seq` Prelude.rnf transformResources
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf dataProcessing
      `Prelude.seq` Prelude.rnf transformStartTime
      `Prelude.seq` Prelude.rnf failureReason
