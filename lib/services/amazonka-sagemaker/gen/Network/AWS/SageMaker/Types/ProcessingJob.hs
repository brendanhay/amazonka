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
-- Module      : Network.AWS.SageMaker.Types.ProcessingJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingJob where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.AppSpecification
import Network.AWS.SageMaker.Types.ExperimentConfig
import Network.AWS.SageMaker.Types.NetworkConfig
import Network.AWS.SageMaker.Types.ProcessingInput
import Network.AWS.SageMaker.Types.ProcessingJobStatus
import Network.AWS.SageMaker.Types.ProcessingOutputConfig
import Network.AWS.SageMaker.Types.ProcessingResources
import Network.AWS.SageMaker.Types.ProcessingStoppingCondition
import Network.AWS.SageMaker.Types.Tag

-- | An Amazon SageMaker processing job that is used to analyze data and
-- evaluate models. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/processing-job.html Process Data and Evaluate Models>.
--
-- /See:/ 'newProcessingJob' smart constructor.
data ProcessingJob = ProcessingJob'
  { -- | The time the processing job was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | A string, up to one KB in size, that contains the reason a processing
    -- job failed, if it failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a monitoring schedule for an endpoint associated with this
    -- processing job.
    monitoringScheduleArn :: Prelude.Maybe Prelude.Text,
    appSpecification :: Prelude.Maybe AppSpecification,
    processingResources :: Prelude.Maybe ProcessingResources,
    -- | Sets the environment variables in the Docker container.
    environment :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the processing job.
    processingJobName :: Prelude.Maybe Prelude.Text,
    stoppingCondition :: Prelude.Maybe ProcessingStoppingCondition,
    experimentConfig :: Prelude.Maybe ExperimentConfig,
    -- | The time the processing job was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | List of input configurations for the processing job.
    processingInputs :: Prelude.Maybe [ProcessingInput],
    networkConfig :: Prelude.Maybe NetworkConfig,
    -- | The Amazon Resource Name (ARN) of the AutoML job associated with this
    -- processing job.
    autoMLJobArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the training job associated with this processing job.
    trainingJobArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the processing job.
    processingJobStatus :: Prelude.Maybe ProcessingJobStatus,
    -- | A string, up to one KB in size, that contains metadata from the
    -- processing container when the processing job exits.
    exitMessage :: Prelude.Maybe Prelude.Text,
    processingOutputConfig :: Prelude.Maybe ProcessingOutputConfig,
    -- | The time that the processing job started.
    processingStartTime :: Prelude.Maybe Core.POSIX,
    -- | The time that the processing job ended.
    processingEndTime :: Prelude.Maybe Core.POSIX,
    -- | An array of key-value pairs. For more information, see
    -- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
    -- in the /Amazon Web Services Billing and Cost Management User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The ARN of the processing job.
    processingJobArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the role used to create the processing job.
    roleArn :: Prelude.Maybe Prelude.Text
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
-- 'creationTime', 'processingJob_creationTime' - The time the processing job was created.
--
-- 'failureReason', 'processingJob_failureReason' - A string, up to one KB in size, that contains the reason a processing
-- job failed, if it failed.
--
-- 'monitoringScheduleArn', 'processingJob_monitoringScheduleArn' - The ARN of a monitoring schedule for an endpoint associated with this
-- processing job.
--
-- 'appSpecification', 'processingJob_appSpecification' - Undocumented member.
--
-- 'processingResources', 'processingJob_processingResources' - Undocumented member.
--
-- 'environment', 'processingJob_environment' - Sets the environment variables in the Docker container.
--
-- 'processingJobName', 'processingJob_processingJobName' - The name of the processing job.
--
-- 'stoppingCondition', 'processingJob_stoppingCondition' - Undocumented member.
--
-- 'experimentConfig', 'processingJob_experimentConfig' - Undocumented member.
--
-- 'lastModifiedTime', 'processingJob_lastModifiedTime' - The time the processing job was last modified.
--
-- 'processingInputs', 'processingJob_processingInputs' - List of input configurations for the processing job.
--
-- 'networkConfig', 'processingJob_networkConfig' - Undocumented member.
--
-- 'autoMLJobArn', 'processingJob_autoMLJobArn' - The Amazon Resource Name (ARN) of the AutoML job associated with this
-- processing job.
--
-- 'trainingJobArn', 'processingJob_trainingJobArn' - The ARN of the training job associated with this processing job.
--
-- 'processingJobStatus', 'processingJob_processingJobStatus' - The status of the processing job.
--
-- 'exitMessage', 'processingJob_exitMessage' - A string, up to one KB in size, that contains metadata from the
-- processing container when the processing job exits.
--
-- 'processingOutputConfig', 'processingJob_processingOutputConfig' - Undocumented member.
--
-- 'processingStartTime', 'processingJob_processingStartTime' - The time that the processing job started.
--
-- 'processingEndTime', 'processingJob_processingEndTime' - The time that the processing job ended.
--
-- 'tags', 'processingJob_tags' - An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
-- in the /Amazon Web Services Billing and Cost Management User Guide/.
--
-- 'processingJobArn', 'processingJob_processingJobArn' - The ARN of the processing job.
--
-- 'roleArn', 'processingJob_roleArn' - The ARN of the role used to create the processing job.
newProcessingJob ::
  ProcessingJob
newProcessingJob =
  ProcessingJob'
    { creationTime = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      monitoringScheduleArn = Prelude.Nothing,
      appSpecification = Prelude.Nothing,
      processingResources = Prelude.Nothing,
      environment = Prelude.Nothing,
      processingJobName = Prelude.Nothing,
      stoppingCondition = Prelude.Nothing,
      experimentConfig = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      processingInputs = Prelude.Nothing,
      networkConfig = Prelude.Nothing,
      autoMLJobArn = Prelude.Nothing,
      trainingJobArn = Prelude.Nothing,
      processingJobStatus = Prelude.Nothing,
      exitMessage = Prelude.Nothing,
      processingOutputConfig = Prelude.Nothing,
      processingStartTime = Prelude.Nothing,
      processingEndTime = Prelude.Nothing,
      tags = Prelude.Nothing,
      processingJobArn = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | The time the processing job was created.
processingJob_creationTime :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.UTCTime)
processingJob_creationTime = Lens.lens (\ProcessingJob' {creationTime} -> creationTime) (\s@ProcessingJob' {} a -> s {creationTime = a} :: ProcessingJob) Prelude.. Lens.mapping Core._Time

-- | A string, up to one KB in size, that contains the reason a processing
-- job failed, if it failed.
processingJob_failureReason :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.Text)
processingJob_failureReason = Lens.lens (\ProcessingJob' {failureReason} -> failureReason) (\s@ProcessingJob' {} a -> s {failureReason = a} :: ProcessingJob)

-- | The ARN of a monitoring schedule for an endpoint associated with this
-- processing job.
processingJob_monitoringScheduleArn :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.Text)
processingJob_monitoringScheduleArn = Lens.lens (\ProcessingJob' {monitoringScheduleArn} -> monitoringScheduleArn) (\s@ProcessingJob' {} a -> s {monitoringScheduleArn = a} :: ProcessingJob)

-- | Undocumented member.
processingJob_appSpecification :: Lens.Lens' ProcessingJob (Prelude.Maybe AppSpecification)
processingJob_appSpecification = Lens.lens (\ProcessingJob' {appSpecification} -> appSpecification) (\s@ProcessingJob' {} a -> s {appSpecification = a} :: ProcessingJob)

-- | Undocumented member.
processingJob_processingResources :: Lens.Lens' ProcessingJob (Prelude.Maybe ProcessingResources)
processingJob_processingResources = Lens.lens (\ProcessingJob' {processingResources} -> processingResources) (\s@ProcessingJob' {} a -> s {processingResources = a} :: ProcessingJob)

-- | Sets the environment variables in the Docker container.
processingJob_environment :: Lens.Lens' ProcessingJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
processingJob_environment = Lens.lens (\ProcessingJob' {environment} -> environment) (\s@ProcessingJob' {} a -> s {environment = a} :: ProcessingJob) Prelude.. Lens.mapping Lens.coerced

-- | The name of the processing job.
processingJob_processingJobName :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.Text)
processingJob_processingJobName = Lens.lens (\ProcessingJob' {processingJobName} -> processingJobName) (\s@ProcessingJob' {} a -> s {processingJobName = a} :: ProcessingJob)

-- | Undocumented member.
processingJob_stoppingCondition :: Lens.Lens' ProcessingJob (Prelude.Maybe ProcessingStoppingCondition)
processingJob_stoppingCondition = Lens.lens (\ProcessingJob' {stoppingCondition} -> stoppingCondition) (\s@ProcessingJob' {} a -> s {stoppingCondition = a} :: ProcessingJob)

-- | Undocumented member.
processingJob_experimentConfig :: Lens.Lens' ProcessingJob (Prelude.Maybe ExperimentConfig)
processingJob_experimentConfig = Lens.lens (\ProcessingJob' {experimentConfig} -> experimentConfig) (\s@ProcessingJob' {} a -> s {experimentConfig = a} :: ProcessingJob)

-- | The time the processing job was last modified.
processingJob_lastModifiedTime :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.UTCTime)
processingJob_lastModifiedTime = Lens.lens (\ProcessingJob' {lastModifiedTime} -> lastModifiedTime) (\s@ProcessingJob' {} a -> s {lastModifiedTime = a} :: ProcessingJob) Prelude.. Lens.mapping Core._Time

-- | List of input configurations for the processing job.
processingJob_processingInputs :: Lens.Lens' ProcessingJob (Prelude.Maybe [ProcessingInput])
processingJob_processingInputs = Lens.lens (\ProcessingJob' {processingInputs} -> processingInputs) (\s@ProcessingJob' {} a -> s {processingInputs = a} :: ProcessingJob) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
processingJob_networkConfig :: Lens.Lens' ProcessingJob (Prelude.Maybe NetworkConfig)
processingJob_networkConfig = Lens.lens (\ProcessingJob' {networkConfig} -> networkConfig) (\s@ProcessingJob' {} a -> s {networkConfig = a} :: ProcessingJob)

-- | The Amazon Resource Name (ARN) of the AutoML job associated with this
-- processing job.
processingJob_autoMLJobArn :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.Text)
processingJob_autoMLJobArn = Lens.lens (\ProcessingJob' {autoMLJobArn} -> autoMLJobArn) (\s@ProcessingJob' {} a -> s {autoMLJobArn = a} :: ProcessingJob)

-- | The ARN of the training job associated with this processing job.
processingJob_trainingJobArn :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.Text)
processingJob_trainingJobArn = Lens.lens (\ProcessingJob' {trainingJobArn} -> trainingJobArn) (\s@ProcessingJob' {} a -> s {trainingJobArn = a} :: ProcessingJob)

-- | The status of the processing job.
processingJob_processingJobStatus :: Lens.Lens' ProcessingJob (Prelude.Maybe ProcessingJobStatus)
processingJob_processingJobStatus = Lens.lens (\ProcessingJob' {processingJobStatus} -> processingJobStatus) (\s@ProcessingJob' {} a -> s {processingJobStatus = a} :: ProcessingJob)

-- | A string, up to one KB in size, that contains metadata from the
-- processing container when the processing job exits.
processingJob_exitMessage :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.Text)
processingJob_exitMessage = Lens.lens (\ProcessingJob' {exitMessage} -> exitMessage) (\s@ProcessingJob' {} a -> s {exitMessage = a} :: ProcessingJob)

-- | Undocumented member.
processingJob_processingOutputConfig :: Lens.Lens' ProcessingJob (Prelude.Maybe ProcessingOutputConfig)
processingJob_processingOutputConfig = Lens.lens (\ProcessingJob' {processingOutputConfig} -> processingOutputConfig) (\s@ProcessingJob' {} a -> s {processingOutputConfig = a} :: ProcessingJob)

-- | The time that the processing job started.
processingJob_processingStartTime :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.UTCTime)
processingJob_processingStartTime = Lens.lens (\ProcessingJob' {processingStartTime} -> processingStartTime) (\s@ProcessingJob' {} a -> s {processingStartTime = a} :: ProcessingJob) Prelude.. Lens.mapping Core._Time

-- | The time that the processing job ended.
processingJob_processingEndTime :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.UTCTime)
processingJob_processingEndTime = Lens.lens (\ProcessingJob' {processingEndTime} -> processingEndTime) (\s@ProcessingJob' {} a -> s {processingEndTime = a} :: ProcessingJob) Prelude.. Lens.mapping Core._Time

-- | An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
-- in the /Amazon Web Services Billing and Cost Management User Guide/.
processingJob_tags :: Lens.Lens' ProcessingJob (Prelude.Maybe [Tag])
processingJob_tags = Lens.lens (\ProcessingJob' {tags} -> tags) (\s@ProcessingJob' {} a -> s {tags = a} :: ProcessingJob) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the processing job.
processingJob_processingJobArn :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.Text)
processingJob_processingJobArn = Lens.lens (\ProcessingJob' {processingJobArn} -> processingJobArn) (\s@ProcessingJob' {} a -> s {processingJobArn = a} :: ProcessingJob)

-- | The ARN of the role used to create the processing job.
processingJob_roleArn :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.Text)
processingJob_roleArn = Lens.lens (\ProcessingJob' {roleArn} -> roleArn) (\s@ProcessingJob' {} a -> s {roleArn = a} :: ProcessingJob)

instance Core.FromJSON ProcessingJob where
  parseJSON =
    Core.withObject
      "ProcessingJob"
      ( \x ->
          ProcessingJob'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "FailureReason")
            Prelude.<*> (x Core..:? "MonitoringScheduleArn")
            Prelude.<*> (x Core..:? "AppSpecification")
            Prelude.<*> (x Core..:? "ProcessingResources")
            Prelude.<*> (x Core..:? "Environment" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ProcessingJobName")
            Prelude.<*> (x Core..:? "StoppingCondition")
            Prelude.<*> (x Core..:? "ExperimentConfig")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> ( x Core..:? "ProcessingInputs"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "NetworkConfig")
            Prelude.<*> (x Core..:? "AutoMLJobArn")
            Prelude.<*> (x Core..:? "TrainingJobArn")
            Prelude.<*> (x Core..:? "ProcessingJobStatus")
            Prelude.<*> (x Core..:? "ExitMessage")
            Prelude.<*> (x Core..:? "ProcessingOutputConfig")
            Prelude.<*> (x Core..:? "ProcessingStartTime")
            Prelude.<*> (x Core..:? "ProcessingEndTime")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ProcessingJobArn")
            Prelude.<*> (x Core..:? "RoleArn")
      )

instance Prelude.Hashable ProcessingJob

instance Prelude.NFData ProcessingJob
