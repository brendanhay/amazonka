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
-- Module      : Amazonka.SageMaker.Types.ProcessingJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ProcessingJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AppSpecification
import Amazonka.SageMaker.Types.ExperimentConfig
import Amazonka.SageMaker.Types.NetworkConfig
import Amazonka.SageMaker.Types.ProcessingInput
import Amazonka.SageMaker.Types.ProcessingJobStatus
import Amazonka.SageMaker.Types.ProcessingOutputConfig
import Amazonka.SageMaker.Types.ProcessingResources
import Amazonka.SageMaker.Types.ProcessingStoppingCondition
import Amazonka.SageMaker.Types.Tag

-- | An Amazon SageMaker processing job that is used to analyze data and
-- evaluate models. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/processing-job.html Process Data and Evaluate Models>.
--
-- /See:/ 'newProcessingJob' smart constructor.
data ProcessingJob = ProcessingJob'
  { -- | An array of key-value pairs. For more information, see
    -- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
    -- in the /Amazon Web Services Billing and Cost Management User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the processing job.
    processingJobName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the role used to create the processing job.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Sets the environment variables in the Docker container.
    environment :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The status of the processing job.
    processingJobStatus :: Prelude.Maybe ProcessingJobStatus,
    -- | The ARN of a monitoring schedule for an endpoint associated with this
    -- processing job.
    monitoringScheduleArn :: Prelude.Maybe Prelude.Text,
    networkConfig :: Prelude.Maybe NetworkConfig,
    experimentConfig :: Prelude.Maybe ExperimentConfig,
    appSpecification :: Prelude.Maybe AppSpecification,
    -- | The Amazon Resource Name (ARN) of the AutoML job associated with this
    -- processing job.
    autoMLJobArn :: Prelude.Maybe Prelude.Text,
    -- | List of input configurations for the processing job.
    processingInputs :: Prelude.Maybe [ProcessingInput],
    -- | The time the processing job was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    stoppingCondition :: Prelude.Maybe ProcessingStoppingCondition,
    -- | The ARN of the processing job.
    processingJobArn :: Prelude.Maybe Prelude.Text,
    -- | The time that the processing job started.
    processingStartTime :: Prelude.Maybe Data.POSIX,
    -- | The time the processing job was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The time that the processing job ended.
    processingEndTime :: Prelude.Maybe Data.POSIX,
    processingResources :: Prelude.Maybe ProcessingResources,
    -- | The ARN of the training job associated with this processing job.
    trainingJobArn :: Prelude.Maybe Prelude.Text,
    -- | A string, up to one KB in size, that contains metadata from the
    -- processing container when the processing job exits.
    exitMessage :: Prelude.Maybe Prelude.Text,
    -- | A string, up to one KB in size, that contains the reason a processing
    -- job failed, if it failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    processingOutputConfig :: Prelude.Maybe ProcessingOutputConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProcessingJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'processingJob_tags' - An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
-- in the /Amazon Web Services Billing and Cost Management User Guide/.
--
-- 'processingJobName', 'processingJob_processingJobName' - The name of the processing job.
--
-- 'roleArn', 'processingJob_roleArn' - The ARN of the role used to create the processing job.
--
-- 'environment', 'processingJob_environment' - Sets the environment variables in the Docker container.
--
-- 'processingJobStatus', 'processingJob_processingJobStatus' - The status of the processing job.
--
-- 'monitoringScheduleArn', 'processingJob_monitoringScheduleArn' - The ARN of a monitoring schedule for an endpoint associated with this
-- processing job.
--
-- 'networkConfig', 'processingJob_networkConfig' - Undocumented member.
--
-- 'experimentConfig', 'processingJob_experimentConfig' - Undocumented member.
--
-- 'appSpecification', 'processingJob_appSpecification' - Undocumented member.
--
-- 'autoMLJobArn', 'processingJob_autoMLJobArn' - The Amazon Resource Name (ARN) of the AutoML job associated with this
-- processing job.
--
-- 'processingInputs', 'processingJob_processingInputs' - List of input configurations for the processing job.
--
-- 'lastModifiedTime', 'processingJob_lastModifiedTime' - The time the processing job was last modified.
--
-- 'stoppingCondition', 'processingJob_stoppingCondition' - Undocumented member.
--
-- 'processingJobArn', 'processingJob_processingJobArn' - The ARN of the processing job.
--
-- 'processingStartTime', 'processingJob_processingStartTime' - The time that the processing job started.
--
-- 'creationTime', 'processingJob_creationTime' - The time the processing job was created.
--
-- 'processingEndTime', 'processingJob_processingEndTime' - The time that the processing job ended.
--
-- 'processingResources', 'processingJob_processingResources' - Undocumented member.
--
-- 'trainingJobArn', 'processingJob_trainingJobArn' - The ARN of the training job associated with this processing job.
--
-- 'exitMessage', 'processingJob_exitMessage' - A string, up to one KB in size, that contains metadata from the
-- processing container when the processing job exits.
--
-- 'failureReason', 'processingJob_failureReason' - A string, up to one KB in size, that contains the reason a processing
-- job failed, if it failed.
--
-- 'processingOutputConfig', 'processingJob_processingOutputConfig' - Undocumented member.
newProcessingJob ::
  ProcessingJob
newProcessingJob =
  ProcessingJob'
    { tags = Prelude.Nothing,
      processingJobName = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      environment = Prelude.Nothing,
      processingJobStatus = Prelude.Nothing,
      monitoringScheduleArn = Prelude.Nothing,
      networkConfig = Prelude.Nothing,
      experimentConfig = Prelude.Nothing,
      appSpecification = Prelude.Nothing,
      autoMLJobArn = Prelude.Nothing,
      processingInputs = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      stoppingCondition = Prelude.Nothing,
      processingJobArn = Prelude.Nothing,
      processingStartTime = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      processingEndTime = Prelude.Nothing,
      processingResources = Prelude.Nothing,
      trainingJobArn = Prelude.Nothing,
      exitMessage = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      processingOutputConfig = Prelude.Nothing
    }

-- | An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
-- in the /Amazon Web Services Billing and Cost Management User Guide/.
processingJob_tags :: Lens.Lens' ProcessingJob (Prelude.Maybe [Tag])
processingJob_tags = Lens.lens (\ProcessingJob' {tags} -> tags) (\s@ProcessingJob' {} a -> s {tags = a} :: ProcessingJob) Prelude.. Lens.mapping Lens.coerced

-- | The name of the processing job.
processingJob_processingJobName :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.Text)
processingJob_processingJobName = Lens.lens (\ProcessingJob' {processingJobName} -> processingJobName) (\s@ProcessingJob' {} a -> s {processingJobName = a} :: ProcessingJob)

-- | The ARN of the role used to create the processing job.
processingJob_roleArn :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.Text)
processingJob_roleArn = Lens.lens (\ProcessingJob' {roleArn} -> roleArn) (\s@ProcessingJob' {} a -> s {roleArn = a} :: ProcessingJob)

-- | Sets the environment variables in the Docker container.
processingJob_environment :: Lens.Lens' ProcessingJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
processingJob_environment = Lens.lens (\ProcessingJob' {environment} -> environment) (\s@ProcessingJob' {} a -> s {environment = a} :: ProcessingJob) Prelude.. Lens.mapping Lens.coerced

-- | The status of the processing job.
processingJob_processingJobStatus :: Lens.Lens' ProcessingJob (Prelude.Maybe ProcessingJobStatus)
processingJob_processingJobStatus = Lens.lens (\ProcessingJob' {processingJobStatus} -> processingJobStatus) (\s@ProcessingJob' {} a -> s {processingJobStatus = a} :: ProcessingJob)

-- | The ARN of a monitoring schedule for an endpoint associated with this
-- processing job.
processingJob_monitoringScheduleArn :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.Text)
processingJob_monitoringScheduleArn = Lens.lens (\ProcessingJob' {monitoringScheduleArn} -> monitoringScheduleArn) (\s@ProcessingJob' {} a -> s {monitoringScheduleArn = a} :: ProcessingJob)

-- | Undocumented member.
processingJob_networkConfig :: Lens.Lens' ProcessingJob (Prelude.Maybe NetworkConfig)
processingJob_networkConfig = Lens.lens (\ProcessingJob' {networkConfig} -> networkConfig) (\s@ProcessingJob' {} a -> s {networkConfig = a} :: ProcessingJob)

-- | Undocumented member.
processingJob_experimentConfig :: Lens.Lens' ProcessingJob (Prelude.Maybe ExperimentConfig)
processingJob_experimentConfig = Lens.lens (\ProcessingJob' {experimentConfig} -> experimentConfig) (\s@ProcessingJob' {} a -> s {experimentConfig = a} :: ProcessingJob)

-- | Undocumented member.
processingJob_appSpecification :: Lens.Lens' ProcessingJob (Prelude.Maybe AppSpecification)
processingJob_appSpecification = Lens.lens (\ProcessingJob' {appSpecification} -> appSpecification) (\s@ProcessingJob' {} a -> s {appSpecification = a} :: ProcessingJob)

-- | The Amazon Resource Name (ARN) of the AutoML job associated with this
-- processing job.
processingJob_autoMLJobArn :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.Text)
processingJob_autoMLJobArn = Lens.lens (\ProcessingJob' {autoMLJobArn} -> autoMLJobArn) (\s@ProcessingJob' {} a -> s {autoMLJobArn = a} :: ProcessingJob)

-- | List of input configurations for the processing job.
processingJob_processingInputs :: Lens.Lens' ProcessingJob (Prelude.Maybe [ProcessingInput])
processingJob_processingInputs = Lens.lens (\ProcessingJob' {processingInputs} -> processingInputs) (\s@ProcessingJob' {} a -> s {processingInputs = a} :: ProcessingJob) Prelude.. Lens.mapping Lens.coerced

-- | The time the processing job was last modified.
processingJob_lastModifiedTime :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.UTCTime)
processingJob_lastModifiedTime = Lens.lens (\ProcessingJob' {lastModifiedTime} -> lastModifiedTime) (\s@ProcessingJob' {} a -> s {lastModifiedTime = a} :: ProcessingJob) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
processingJob_stoppingCondition :: Lens.Lens' ProcessingJob (Prelude.Maybe ProcessingStoppingCondition)
processingJob_stoppingCondition = Lens.lens (\ProcessingJob' {stoppingCondition} -> stoppingCondition) (\s@ProcessingJob' {} a -> s {stoppingCondition = a} :: ProcessingJob)

-- | The ARN of the processing job.
processingJob_processingJobArn :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.Text)
processingJob_processingJobArn = Lens.lens (\ProcessingJob' {processingJobArn} -> processingJobArn) (\s@ProcessingJob' {} a -> s {processingJobArn = a} :: ProcessingJob)

-- | The time that the processing job started.
processingJob_processingStartTime :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.UTCTime)
processingJob_processingStartTime = Lens.lens (\ProcessingJob' {processingStartTime} -> processingStartTime) (\s@ProcessingJob' {} a -> s {processingStartTime = a} :: ProcessingJob) Prelude.. Lens.mapping Data._Time

-- | The time the processing job was created.
processingJob_creationTime :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.UTCTime)
processingJob_creationTime = Lens.lens (\ProcessingJob' {creationTime} -> creationTime) (\s@ProcessingJob' {} a -> s {creationTime = a} :: ProcessingJob) Prelude.. Lens.mapping Data._Time

-- | The time that the processing job ended.
processingJob_processingEndTime :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.UTCTime)
processingJob_processingEndTime = Lens.lens (\ProcessingJob' {processingEndTime} -> processingEndTime) (\s@ProcessingJob' {} a -> s {processingEndTime = a} :: ProcessingJob) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
processingJob_processingResources :: Lens.Lens' ProcessingJob (Prelude.Maybe ProcessingResources)
processingJob_processingResources = Lens.lens (\ProcessingJob' {processingResources} -> processingResources) (\s@ProcessingJob' {} a -> s {processingResources = a} :: ProcessingJob)

-- | The ARN of the training job associated with this processing job.
processingJob_trainingJobArn :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.Text)
processingJob_trainingJobArn = Lens.lens (\ProcessingJob' {trainingJobArn} -> trainingJobArn) (\s@ProcessingJob' {} a -> s {trainingJobArn = a} :: ProcessingJob)

-- | A string, up to one KB in size, that contains metadata from the
-- processing container when the processing job exits.
processingJob_exitMessage :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.Text)
processingJob_exitMessage = Lens.lens (\ProcessingJob' {exitMessage} -> exitMessage) (\s@ProcessingJob' {} a -> s {exitMessage = a} :: ProcessingJob)

-- | A string, up to one KB in size, that contains the reason a processing
-- job failed, if it failed.
processingJob_failureReason :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.Text)
processingJob_failureReason = Lens.lens (\ProcessingJob' {failureReason} -> failureReason) (\s@ProcessingJob' {} a -> s {failureReason = a} :: ProcessingJob)

-- | Undocumented member.
processingJob_processingOutputConfig :: Lens.Lens' ProcessingJob (Prelude.Maybe ProcessingOutputConfig)
processingJob_processingOutputConfig = Lens.lens (\ProcessingJob' {processingOutputConfig} -> processingOutputConfig) (\s@ProcessingJob' {} a -> s {processingOutputConfig = a} :: ProcessingJob)

instance Data.FromJSON ProcessingJob where
  parseJSON =
    Data.withObject
      "ProcessingJob"
      ( \x ->
          ProcessingJob'
            Prelude.<$> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ProcessingJobName")
            Prelude.<*> (x Data..:? "RoleArn")
            Prelude.<*> (x Data..:? "Environment" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ProcessingJobStatus")
            Prelude.<*> (x Data..:? "MonitoringScheduleArn")
            Prelude.<*> (x Data..:? "NetworkConfig")
            Prelude.<*> (x Data..:? "ExperimentConfig")
            Prelude.<*> (x Data..:? "AppSpecification")
            Prelude.<*> (x Data..:? "AutoMLJobArn")
            Prelude.<*> ( x Data..:? "ProcessingInputs"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "StoppingCondition")
            Prelude.<*> (x Data..:? "ProcessingJobArn")
            Prelude.<*> (x Data..:? "ProcessingStartTime")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "ProcessingEndTime")
            Prelude.<*> (x Data..:? "ProcessingResources")
            Prelude.<*> (x Data..:? "TrainingJobArn")
            Prelude.<*> (x Data..:? "ExitMessage")
            Prelude.<*> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..:? "ProcessingOutputConfig")
      )

instance Prelude.Hashable ProcessingJob where
  hashWithSalt _salt ProcessingJob' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` processingJobName
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` processingJobStatus
      `Prelude.hashWithSalt` monitoringScheduleArn
      `Prelude.hashWithSalt` networkConfig
      `Prelude.hashWithSalt` experimentConfig
      `Prelude.hashWithSalt` appSpecification
      `Prelude.hashWithSalt` autoMLJobArn
      `Prelude.hashWithSalt` processingInputs
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` stoppingCondition
      `Prelude.hashWithSalt` processingJobArn
      `Prelude.hashWithSalt` processingStartTime
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` processingEndTime
      `Prelude.hashWithSalt` processingResources
      `Prelude.hashWithSalt` trainingJobArn
      `Prelude.hashWithSalt` exitMessage
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` processingOutputConfig

instance Prelude.NFData ProcessingJob where
  rnf ProcessingJob' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf processingJobName
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf processingJobStatus
      `Prelude.seq` Prelude.rnf monitoringScheduleArn
      `Prelude.seq` Prelude.rnf networkConfig
      `Prelude.seq` Prelude.rnf experimentConfig
      `Prelude.seq` Prelude.rnf appSpecification
      `Prelude.seq` Prelude.rnf autoMLJobArn
      `Prelude.seq` Prelude.rnf processingInputs
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf stoppingCondition
      `Prelude.seq` Prelude.rnf processingJobArn
      `Prelude.seq` Prelude.rnf processingStartTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf processingEndTime
      `Prelude.seq` Prelude.rnf processingResources
      `Prelude.seq` Prelude.rnf trainingJobArn
      `Prelude.seq` Prelude.rnf exitMessage
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf
        processingOutputConfig
