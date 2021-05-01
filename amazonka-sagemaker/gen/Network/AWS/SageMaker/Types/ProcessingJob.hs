{-# LANGUAGE DeriveDataTypeable #-}
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
  { networkConfig :: Prelude.Maybe NetworkConfig,
    -- | The time the processing job was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    appSpecification :: Prelude.Maybe AppSpecification,
    -- | The time that the processing job ended.
    processingEndTime :: Prelude.Maybe Prelude.POSIX,
    -- | The ARN of the role used to create the processing job.
    roleArn :: Prelude.Maybe Prelude.Text,
    processingOutputConfig :: Prelude.Maybe ProcessingOutputConfig,
    -- | A string, up to one KB in size, that contains metadata from the
    -- processing container when the processing job exits.
    exitMessage :: Prelude.Maybe Prelude.Text,
    experimentConfig :: Prelude.Maybe ExperimentConfig,
    -- | The status of the processing job.
    processingJobStatus :: Prelude.Maybe ProcessingJobStatus,
    -- | Sets the environment variables in the Docker container.
    environment :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the AutoML job associated with this
    -- processing job.
    autoMLJobArn :: Prelude.Maybe Prelude.Text,
    -- | A string, up to one KB in size, that contains the reason a processing
    -- job failed, if it failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a monitoring schedule for an endpoint associated with this
    -- processing job.
    monitoringScheduleArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the processing job.
    processingJobArn :: Prelude.Maybe Prelude.Text,
    -- | An array of key-value pairs. For more information, see
    -- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
    -- in the /AWS Billing and Cost Management User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The time the processing job was last modified.
    lastModifiedTime :: Prelude.Maybe Prelude.POSIX,
    -- | List of input configurations for the processing job.
    processingInputs :: Prelude.Maybe [ProcessingInput],
    -- | The time that the processing job started.
    processingStartTime :: Prelude.Maybe Prelude.POSIX,
    stoppingCondition :: Prelude.Maybe ProcessingStoppingCondition,
    -- | The name of the processing job.
    processingJobName :: Prelude.Maybe Prelude.Text,
    processingResources :: Prelude.Maybe ProcessingResources,
    -- | The ARN of the training job associated with this processing job.
    trainingJobArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { networkConfig = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      appSpecification = Prelude.Nothing,
      processingEndTime = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      processingOutputConfig = Prelude.Nothing,
      exitMessage = Prelude.Nothing,
      experimentConfig = Prelude.Nothing,
      processingJobStatus = Prelude.Nothing,
      environment = Prelude.Nothing,
      autoMLJobArn = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      monitoringScheduleArn = Prelude.Nothing,
      processingJobArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      processingInputs = Prelude.Nothing,
      processingStartTime = Prelude.Nothing,
      stoppingCondition = Prelude.Nothing,
      processingJobName = Prelude.Nothing,
      processingResources = Prelude.Nothing,
      trainingJobArn = Prelude.Nothing
    }

-- | Undocumented member.
processingJob_networkConfig :: Lens.Lens' ProcessingJob (Prelude.Maybe NetworkConfig)
processingJob_networkConfig = Lens.lens (\ProcessingJob' {networkConfig} -> networkConfig) (\s@ProcessingJob' {} a -> s {networkConfig = a} :: ProcessingJob)

-- | The time the processing job was created.
processingJob_creationTime :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.UTCTime)
processingJob_creationTime = Lens.lens (\ProcessingJob' {creationTime} -> creationTime) (\s@ProcessingJob' {} a -> s {creationTime = a} :: ProcessingJob) Prelude.. Lens.mapping Prelude._Time

-- | Undocumented member.
processingJob_appSpecification :: Lens.Lens' ProcessingJob (Prelude.Maybe AppSpecification)
processingJob_appSpecification = Lens.lens (\ProcessingJob' {appSpecification} -> appSpecification) (\s@ProcessingJob' {} a -> s {appSpecification = a} :: ProcessingJob)

-- | The time that the processing job ended.
processingJob_processingEndTime :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.UTCTime)
processingJob_processingEndTime = Lens.lens (\ProcessingJob' {processingEndTime} -> processingEndTime) (\s@ProcessingJob' {} a -> s {processingEndTime = a} :: ProcessingJob) Prelude.. Lens.mapping Prelude._Time

-- | The ARN of the role used to create the processing job.
processingJob_roleArn :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.Text)
processingJob_roleArn = Lens.lens (\ProcessingJob' {roleArn} -> roleArn) (\s@ProcessingJob' {} a -> s {roleArn = a} :: ProcessingJob)

-- | Undocumented member.
processingJob_processingOutputConfig :: Lens.Lens' ProcessingJob (Prelude.Maybe ProcessingOutputConfig)
processingJob_processingOutputConfig = Lens.lens (\ProcessingJob' {processingOutputConfig} -> processingOutputConfig) (\s@ProcessingJob' {} a -> s {processingOutputConfig = a} :: ProcessingJob)

-- | A string, up to one KB in size, that contains metadata from the
-- processing container when the processing job exits.
processingJob_exitMessage :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.Text)
processingJob_exitMessage = Lens.lens (\ProcessingJob' {exitMessage} -> exitMessage) (\s@ProcessingJob' {} a -> s {exitMessage = a} :: ProcessingJob)

-- | Undocumented member.
processingJob_experimentConfig :: Lens.Lens' ProcessingJob (Prelude.Maybe ExperimentConfig)
processingJob_experimentConfig = Lens.lens (\ProcessingJob' {experimentConfig} -> experimentConfig) (\s@ProcessingJob' {} a -> s {experimentConfig = a} :: ProcessingJob)

-- | The status of the processing job.
processingJob_processingJobStatus :: Lens.Lens' ProcessingJob (Prelude.Maybe ProcessingJobStatus)
processingJob_processingJobStatus = Lens.lens (\ProcessingJob' {processingJobStatus} -> processingJobStatus) (\s@ProcessingJob' {} a -> s {processingJobStatus = a} :: ProcessingJob)

-- | Sets the environment variables in the Docker container.
processingJob_environment :: Lens.Lens' ProcessingJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
processingJob_environment = Lens.lens (\ProcessingJob' {environment} -> environment) (\s@ProcessingJob' {} a -> s {environment = a} :: ProcessingJob) Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon Resource Name (ARN) of the AutoML job associated with this
-- processing job.
processingJob_autoMLJobArn :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.Text)
processingJob_autoMLJobArn = Lens.lens (\ProcessingJob' {autoMLJobArn} -> autoMLJobArn) (\s@ProcessingJob' {} a -> s {autoMLJobArn = a} :: ProcessingJob)

-- | A string, up to one KB in size, that contains the reason a processing
-- job failed, if it failed.
processingJob_failureReason :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.Text)
processingJob_failureReason = Lens.lens (\ProcessingJob' {failureReason} -> failureReason) (\s@ProcessingJob' {} a -> s {failureReason = a} :: ProcessingJob)

-- | The ARN of a monitoring schedule for an endpoint associated with this
-- processing job.
processingJob_monitoringScheduleArn :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.Text)
processingJob_monitoringScheduleArn = Lens.lens (\ProcessingJob' {monitoringScheduleArn} -> monitoringScheduleArn) (\s@ProcessingJob' {} a -> s {monitoringScheduleArn = a} :: ProcessingJob)

-- | The ARN of the processing job.
processingJob_processingJobArn :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.Text)
processingJob_processingJobArn = Lens.lens (\ProcessingJob' {processingJobArn} -> processingJobArn) (\s@ProcessingJob' {} a -> s {processingJobArn = a} :: ProcessingJob)

-- | An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
-- in the /AWS Billing and Cost Management User Guide/.
processingJob_tags :: Lens.Lens' ProcessingJob (Prelude.Maybe [Tag])
processingJob_tags = Lens.lens (\ProcessingJob' {tags} -> tags) (\s@ProcessingJob' {} a -> s {tags = a} :: ProcessingJob) Prelude.. Lens.mapping Prelude._Coerce

-- | The time the processing job was last modified.
processingJob_lastModifiedTime :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.UTCTime)
processingJob_lastModifiedTime = Lens.lens (\ProcessingJob' {lastModifiedTime} -> lastModifiedTime) (\s@ProcessingJob' {} a -> s {lastModifiedTime = a} :: ProcessingJob) Prelude.. Lens.mapping Prelude._Time

-- | List of input configurations for the processing job.
processingJob_processingInputs :: Lens.Lens' ProcessingJob (Prelude.Maybe [ProcessingInput])
processingJob_processingInputs = Lens.lens (\ProcessingJob' {processingInputs} -> processingInputs) (\s@ProcessingJob' {} a -> s {processingInputs = a} :: ProcessingJob) Prelude.. Lens.mapping Prelude._Coerce

-- | The time that the processing job started.
processingJob_processingStartTime :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.UTCTime)
processingJob_processingStartTime = Lens.lens (\ProcessingJob' {processingStartTime} -> processingStartTime) (\s@ProcessingJob' {} a -> s {processingStartTime = a} :: ProcessingJob) Prelude.. Lens.mapping Prelude._Time

-- | Undocumented member.
processingJob_stoppingCondition :: Lens.Lens' ProcessingJob (Prelude.Maybe ProcessingStoppingCondition)
processingJob_stoppingCondition = Lens.lens (\ProcessingJob' {stoppingCondition} -> stoppingCondition) (\s@ProcessingJob' {} a -> s {stoppingCondition = a} :: ProcessingJob)

-- | The name of the processing job.
processingJob_processingJobName :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.Text)
processingJob_processingJobName = Lens.lens (\ProcessingJob' {processingJobName} -> processingJobName) (\s@ProcessingJob' {} a -> s {processingJobName = a} :: ProcessingJob)

-- | Undocumented member.
processingJob_processingResources :: Lens.Lens' ProcessingJob (Prelude.Maybe ProcessingResources)
processingJob_processingResources = Lens.lens (\ProcessingJob' {processingResources} -> processingResources) (\s@ProcessingJob' {} a -> s {processingResources = a} :: ProcessingJob)

-- | The ARN of the training job associated with this processing job.
processingJob_trainingJobArn :: Lens.Lens' ProcessingJob (Prelude.Maybe Prelude.Text)
processingJob_trainingJobArn = Lens.lens (\ProcessingJob' {trainingJobArn} -> trainingJobArn) (\s@ProcessingJob' {} a -> s {trainingJobArn = a} :: ProcessingJob)

instance Prelude.FromJSON ProcessingJob where
  parseJSON =
    Prelude.withObject
      "ProcessingJob"
      ( \x ->
          ProcessingJob'
            Prelude.<$> (x Prelude..:? "NetworkConfig")
            Prelude.<*> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "AppSpecification")
            Prelude.<*> (x Prelude..:? "ProcessingEndTime")
            Prelude.<*> (x Prelude..:? "RoleArn")
            Prelude.<*> (x Prelude..:? "ProcessingOutputConfig")
            Prelude.<*> (x Prelude..:? "ExitMessage")
            Prelude.<*> (x Prelude..:? "ExperimentConfig")
            Prelude.<*> (x Prelude..:? "ProcessingJobStatus")
            Prelude.<*> ( x Prelude..:? "Environment"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "AutoMLJobArn")
            Prelude.<*> (x Prelude..:? "FailureReason")
            Prelude.<*> (x Prelude..:? "MonitoringScheduleArn")
            Prelude.<*> (x Prelude..:? "ProcessingJobArn")
            Prelude.<*> (x Prelude..:? "Tags" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "LastModifiedTime")
            Prelude.<*> ( x Prelude..:? "ProcessingInputs"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "ProcessingStartTime")
            Prelude.<*> (x Prelude..:? "StoppingCondition")
            Prelude.<*> (x Prelude..:? "ProcessingJobName")
            Prelude.<*> (x Prelude..:? "ProcessingResources")
            Prelude.<*> (x Prelude..:? "TrainingJobArn")
      )

instance Prelude.Hashable ProcessingJob

instance Prelude.NFData ProcessingJob
