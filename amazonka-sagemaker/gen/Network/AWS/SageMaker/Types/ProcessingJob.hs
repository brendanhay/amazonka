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
  { networkConfig :: Core.Maybe NetworkConfig,
    -- | The time the processing job was created.
    creationTime :: Core.Maybe Core.POSIX,
    appSpecification :: Core.Maybe AppSpecification,
    -- | The time that the processing job ended.
    processingEndTime :: Core.Maybe Core.POSIX,
    -- | The ARN of the role used to create the processing job.
    roleArn :: Core.Maybe Core.Text,
    processingOutputConfig :: Core.Maybe ProcessingOutputConfig,
    -- | A string, up to one KB in size, that contains metadata from the
    -- processing container when the processing job exits.
    exitMessage :: Core.Maybe Core.Text,
    experimentConfig :: Core.Maybe ExperimentConfig,
    -- | The status of the processing job.
    processingJobStatus :: Core.Maybe ProcessingJobStatus,
    -- | Sets the environment variables in the Docker container.
    environment :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The Amazon Resource Name (ARN) of the AutoML job associated with this
    -- processing job.
    autoMLJobArn :: Core.Maybe Core.Text,
    -- | A string, up to one KB in size, that contains the reason a processing
    -- job failed, if it failed.
    failureReason :: Core.Maybe Core.Text,
    -- | The ARN of a monitoring schedule for an endpoint associated with this
    -- processing job.
    monitoringScheduleArn :: Core.Maybe Core.Text,
    -- | The ARN of the processing job.
    processingJobArn :: Core.Maybe Core.Text,
    -- | An array of key-value pairs. For more information, see
    -- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
    -- in the /AWS Billing and Cost Management User Guide/.
    tags :: Core.Maybe [Tag],
    -- | The time the processing job was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | List of input configurations for the processing job.
    processingInputs :: Core.Maybe [ProcessingInput],
    -- | The time that the processing job started.
    processingStartTime :: Core.Maybe Core.POSIX,
    stoppingCondition :: Core.Maybe ProcessingStoppingCondition,
    -- | The name of the processing job.
    processingJobName :: Core.Maybe Core.Text,
    processingResources :: Core.Maybe ProcessingResources,
    -- | The ARN of the training job associated with this processing job.
    trainingJobArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProcessingJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkConfig', 'processingJob_networkConfig' - Undocumented member.
--
-- 'creationTime', 'processingJob_creationTime' - The time the processing job was created.
--
-- 'appSpecification', 'processingJob_appSpecification' - Undocumented member.
--
-- 'processingEndTime', 'processingJob_processingEndTime' - The time that the processing job ended.
--
-- 'roleArn', 'processingJob_roleArn' - The ARN of the role used to create the processing job.
--
-- 'processingOutputConfig', 'processingJob_processingOutputConfig' - Undocumented member.
--
-- 'exitMessage', 'processingJob_exitMessage' - A string, up to one KB in size, that contains metadata from the
-- processing container when the processing job exits.
--
-- 'experimentConfig', 'processingJob_experimentConfig' - Undocumented member.
--
-- 'processingJobStatus', 'processingJob_processingJobStatus' - The status of the processing job.
--
-- 'environment', 'processingJob_environment' - Sets the environment variables in the Docker container.
--
-- 'autoMLJobArn', 'processingJob_autoMLJobArn' - The Amazon Resource Name (ARN) of the AutoML job associated with this
-- processing job.
--
-- 'failureReason', 'processingJob_failureReason' - A string, up to one KB in size, that contains the reason a processing
-- job failed, if it failed.
--
-- 'monitoringScheduleArn', 'processingJob_monitoringScheduleArn' - The ARN of a monitoring schedule for an endpoint associated with this
-- processing job.
--
-- 'processingJobArn', 'processingJob_processingJobArn' - The ARN of the processing job.
--
-- 'tags', 'processingJob_tags' - An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
-- in the /AWS Billing and Cost Management User Guide/.
--
-- 'lastModifiedTime', 'processingJob_lastModifiedTime' - The time the processing job was last modified.
--
-- 'processingInputs', 'processingJob_processingInputs' - List of input configurations for the processing job.
--
-- 'processingStartTime', 'processingJob_processingStartTime' - The time that the processing job started.
--
-- 'stoppingCondition', 'processingJob_stoppingCondition' - Undocumented member.
--
-- 'processingJobName', 'processingJob_processingJobName' - The name of the processing job.
--
-- 'processingResources', 'processingJob_processingResources' - Undocumented member.
--
-- 'trainingJobArn', 'processingJob_trainingJobArn' - The ARN of the training job associated with this processing job.
newProcessingJob ::
  ProcessingJob
newProcessingJob =
  ProcessingJob'
    { networkConfig = Core.Nothing,
      creationTime = Core.Nothing,
      appSpecification = Core.Nothing,
      processingEndTime = Core.Nothing,
      roleArn = Core.Nothing,
      processingOutputConfig = Core.Nothing,
      exitMessage = Core.Nothing,
      experimentConfig = Core.Nothing,
      processingJobStatus = Core.Nothing,
      environment = Core.Nothing,
      autoMLJobArn = Core.Nothing,
      failureReason = Core.Nothing,
      monitoringScheduleArn = Core.Nothing,
      processingJobArn = Core.Nothing,
      tags = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      processingInputs = Core.Nothing,
      processingStartTime = Core.Nothing,
      stoppingCondition = Core.Nothing,
      processingJobName = Core.Nothing,
      processingResources = Core.Nothing,
      trainingJobArn = Core.Nothing
    }

-- | Undocumented member.
processingJob_networkConfig :: Lens.Lens' ProcessingJob (Core.Maybe NetworkConfig)
processingJob_networkConfig = Lens.lens (\ProcessingJob' {networkConfig} -> networkConfig) (\s@ProcessingJob' {} a -> s {networkConfig = a} :: ProcessingJob)

-- | The time the processing job was created.
processingJob_creationTime :: Lens.Lens' ProcessingJob (Core.Maybe Core.UTCTime)
processingJob_creationTime = Lens.lens (\ProcessingJob' {creationTime} -> creationTime) (\s@ProcessingJob' {} a -> s {creationTime = a} :: ProcessingJob) Core.. Lens.mapping Core._Time

-- | Undocumented member.
processingJob_appSpecification :: Lens.Lens' ProcessingJob (Core.Maybe AppSpecification)
processingJob_appSpecification = Lens.lens (\ProcessingJob' {appSpecification} -> appSpecification) (\s@ProcessingJob' {} a -> s {appSpecification = a} :: ProcessingJob)

-- | The time that the processing job ended.
processingJob_processingEndTime :: Lens.Lens' ProcessingJob (Core.Maybe Core.UTCTime)
processingJob_processingEndTime = Lens.lens (\ProcessingJob' {processingEndTime} -> processingEndTime) (\s@ProcessingJob' {} a -> s {processingEndTime = a} :: ProcessingJob) Core.. Lens.mapping Core._Time

-- | The ARN of the role used to create the processing job.
processingJob_roleArn :: Lens.Lens' ProcessingJob (Core.Maybe Core.Text)
processingJob_roleArn = Lens.lens (\ProcessingJob' {roleArn} -> roleArn) (\s@ProcessingJob' {} a -> s {roleArn = a} :: ProcessingJob)

-- | Undocumented member.
processingJob_processingOutputConfig :: Lens.Lens' ProcessingJob (Core.Maybe ProcessingOutputConfig)
processingJob_processingOutputConfig = Lens.lens (\ProcessingJob' {processingOutputConfig} -> processingOutputConfig) (\s@ProcessingJob' {} a -> s {processingOutputConfig = a} :: ProcessingJob)

-- | A string, up to one KB in size, that contains metadata from the
-- processing container when the processing job exits.
processingJob_exitMessage :: Lens.Lens' ProcessingJob (Core.Maybe Core.Text)
processingJob_exitMessage = Lens.lens (\ProcessingJob' {exitMessage} -> exitMessage) (\s@ProcessingJob' {} a -> s {exitMessage = a} :: ProcessingJob)

-- | Undocumented member.
processingJob_experimentConfig :: Lens.Lens' ProcessingJob (Core.Maybe ExperimentConfig)
processingJob_experimentConfig = Lens.lens (\ProcessingJob' {experimentConfig} -> experimentConfig) (\s@ProcessingJob' {} a -> s {experimentConfig = a} :: ProcessingJob)

-- | The status of the processing job.
processingJob_processingJobStatus :: Lens.Lens' ProcessingJob (Core.Maybe ProcessingJobStatus)
processingJob_processingJobStatus = Lens.lens (\ProcessingJob' {processingJobStatus} -> processingJobStatus) (\s@ProcessingJob' {} a -> s {processingJobStatus = a} :: ProcessingJob)

-- | Sets the environment variables in the Docker container.
processingJob_environment :: Lens.Lens' ProcessingJob (Core.Maybe (Core.HashMap Core.Text Core.Text))
processingJob_environment = Lens.lens (\ProcessingJob' {environment} -> environment) (\s@ProcessingJob' {} a -> s {environment = a} :: ProcessingJob) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the AutoML job associated with this
-- processing job.
processingJob_autoMLJobArn :: Lens.Lens' ProcessingJob (Core.Maybe Core.Text)
processingJob_autoMLJobArn = Lens.lens (\ProcessingJob' {autoMLJobArn} -> autoMLJobArn) (\s@ProcessingJob' {} a -> s {autoMLJobArn = a} :: ProcessingJob)

-- | A string, up to one KB in size, that contains the reason a processing
-- job failed, if it failed.
processingJob_failureReason :: Lens.Lens' ProcessingJob (Core.Maybe Core.Text)
processingJob_failureReason = Lens.lens (\ProcessingJob' {failureReason} -> failureReason) (\s@ProcessingJob' {} a -> s {failureReason = a} :: ProcessingJob)

-- | The ARN of a monitoring schedule for an endpoint associated with this
-- processing job.
processingJob_monitoringScheduleArn :: Lens.Lens' ProcessingJob (Core.Maybe Core.Text)
processingJob_monitoringScheduleArn = Lens.lens (\ProcessingJob' {monitoringScheduleArn} -> monitoringScheduleArn) (\s@ProcessingJob' {} a -> s {monitoringScheduleArn = a} :: ProcessingJob)

-- | The ARN of the processing job.
processingJob_processingJobArn :: Lens.Lens' ProcessingJob (Core.Maybe Core.Text)
processingJob_processingJobArn = Lens.lens (\ProcessingJob' {processingJobArn} -> processingJobArn) (\s@ProcessingJob' {} a -> s {processingJobArn = a} :: ProcessingJob)

-- | An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
-- in the /AWS Billing and Cost Management User Guide/.
processingJob_tags :: Lens.Lens' ProcessingJob (Core.Maybe [Tag])
processingJob_tags = Lens.lens (\ProcessingJob' {tags} -> tags) (\s@ProcessingJob' {} a -> s {tags = a} :: ProcessingJob) Core.. Lens.mapping Lens._Coerce

-- | The time the processing job was last modified.
processingJob_lastModifiedTime :: Lens.Lens' ProcessingJob (Core.Maybe Core.UTCTime)
processingJob_lastModifiedTime = Lens.lens (\ProcessingJob' {lastModifiedTime} -> lastModifiedTime) (\s@ProcessingJob' {} a -> s {lastModifiedTime = a} :: ProcessingJob) Core.. Lens.mapping Core._Time

-- | List of input configurations for the processing job.
processingJob_processingInputs :: Lens.Lens' ProcessingJob (Core.Maybe [ProcessingInput])
processingJob_processingInputs = Lens.lens (\ProcessingJob' {processingInputs} -> processingInputs) (\s@ProcessingJob' {} a -> s {processingInputs = a} :: ProcessingJob) Core.. Lens.mapping Lens._Coerce

-- | The time that the processing job started.
processingJob_processingStartTime :: Lens.Lens' ProcessingJob (Core.Maybe Core.UTCTime)
processingJob_processingStartTime = Lens.lens (\ProcessingJob' {processingStartTime} -> processingStartTime) (\s@ProcessingJob' {} a -> s {processingStartTime = a} :: ProcessingJob) Core.. Lens.mapping Core._Time

-- | Undocumented member.
processingJob_stoppingCondition :: Lens.Lens' ProcessingJob (Core.Maybe ProcessingStoppingCondition)
processingJob_stoppingCondition = Lens.lens (\ProcessingJob' {stoppingCondition} -> stoppingCondition) (\s@ProcessingJob' {} a -> s {stoppingCondition = a} :: ProcessingJob)

-- | The name of the processing job.
processingJob_processingJobName :: Lens.Lens' ProcessingJob (Core.Maybe Core.Text)
processingJob_processingJobName = Lens.lens (\ProcessingJob' {processingJobName} -> processingJobName) (\s@ProcessingJob' {} a -> s {processingJobName = a} :: ProcessingJob)

-- | Undocumented member.
processingJob_processingResources :: Lens.Lens' ProcessingJob (Core.Maybe ProcessingResources)
processingJob_processingResources = Lens.lens (\ProcessingJob' {processingResources} -> processingResources) (\s@ProcessingJob' {} a -> s {processingResources = a} :: ProcessingJob)

-- | The ARN of the training job associated with this processing job.
processingJob_trainingJobArn :: Lens.Lens' ProcessingJob (Core.Maybe Core.Text)
processingJob_trainingJobArn = Lens.lens (\ProcessingJob' {trainingJobArn} -> trainingJobArn) (\s@ProcessingJob' {} a -> s {trainingJobArn = a} :: ProcessingJob)

instance Core.FromJSON ProcessingJob where
  parseJSON =
    Core.withObject
      "ProcessingJob"
      ( \x ->
          ProcessingJob'
            Core.<$> (x Core..:? "NetworkConfig")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "AppSpecification")
            Core.<*> (x Core..:? "ProcessingEndTime")
            Core.<*> (x Core..:? "RoleArn")
            Core.<*> (x Core..:? "ProcessingOutputConfig")
            Core.<*> (x Core..:? "ExitMessage")
            Core.<*> (x Core..:? "ExperimentConfig")
            Core.<*> (x Core..:? "ProcessingJobStatus")
            Core.<*> (x Core..:? "Environment" Core..!= Core.mempty)
            Core.<*> (x Core..:? "AutoMLJobArn")
            Core.<*> (x Core..:? "FailureReason")
            Core.<*> (x Core..:? "MonitoringScheduleArn")
            Core.<*> (x Core..:? "ProcessingJobArn")
            Core.<*> (x Core..:? "Tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "ProcessingInputs" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ProcessingStartTime")
            Core.<*> (x Core..:? "StoppingCondition")
            Core.<*> (x Core..:? "ProcessingJobName")
            Core.<*> (x Core..:? "ProcessingResources")
            Core.<*> (x Core..:? "TrainingJobArn")
      )

instance Core.Hashable ProcessingJob

instance Core.NFData ProcessingJob
