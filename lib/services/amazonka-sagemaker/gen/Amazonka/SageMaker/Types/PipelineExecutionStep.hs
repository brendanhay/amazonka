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
-- Module      : Amazonka.SageMaker.Types.PipelineExecutionStep
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.PipelineExecutionStep where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.CacheHitResult
import Amazonka.SageMaker.Types.PipelineExecutionStepMetadata
import Amazonka.SageMaker.Types.StepStatus

-- | An execution of a step in a pipeline.
--
-- /See:/ 'newPipelineExecutionStep' smart constructor.
data PipelineExecutionStep = PipelineExecutionStep'
  { -- | The current attempt of the execution step. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/pipelines-retry-policy.html Retry Policy for SageMaker Pipelines steps>.
    attemptCount :: Prelude.Maybe Prelude.Int,
    -- | If this pipeline execution step was cached, details on the cache hit.
    cacheHitResult :: Prelude.Maybe CacheHitResult,
    -- | The time that the step stopped executing.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The reason why the step failed execution. This is only returned if the
    -- step failed its execution.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | Metadata to run the pipeline step.
    metadata :: Prelude.Maybe PipelineExecutionStepMetadata,
    -- | The time that the step started executing.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the step.
    stepDescription :: Prelude.Maybe Prelude.Text,
    -- | The display name of the step.
    stepDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The name of the step that is executed.
    stepName :: Prelude.Maybe Prelude.Text,
    -- | The status of the step execution.
    stepStatus :: Prelude.Maybe StepStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipelineExecutionStep' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attemptCount', 'pipelineExecutionStep_attemptCount' - The current attempt of the execution step. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/pipelines-retry-policy.html Retry Policy for SageMaker Pipelines steps>.
--
-- 'cacheHitResult', 'pipelineExecutionStep_cacheHitResult' - If this pipeline execution step was cached, details on the cache hit.
--
-- 'endTime', 'pipelineExecutionStep_endTime' - The time that the step stopped executing.
--
-- 'failureReason', 'pipelineExecutionStep_failureReason' - The reason why the step failed execution. This is only returned if the
-- step failed its execution.
--
-- 'metadata', 'pipelineExecutionStep_metadata' - Metadata to run the pipeline step.
--
-- 'startTime', 'pipelineExecutionStep_startTime' - The time that the step started executing.
--
-- 'stepDescription', 'pipelineExecutionStep_stepDescription' - The description of the step.
--
-- 'stepDisplayName', 'pipelineExecutionStep_stepDisplayName' - The display name of the step.
--
-- 'stepName', 'pipelineExecutionStep_stepName' - The name of the step that is executed.
--
-- 'stepStatus', 'pipelineExecutionStep_stepStatus' - The status of the step execution.
newPipelineExecutionStep ::
  PipelineExecutionStep
newPipelineExecutionStep =
  PipelineExecutionStep'
    { attemptCount =
        Prelude.Nothing,
      cacheHitResult = Prelude.Nothing,
      endTime = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      metadata = Prelude.Nothing,
      startTime = Prelude.Nothing,
      stepDescription = Prelude.Nothing,
      stepDisplayName = Prelude.Nothing,
      stepName = Prelude.Nothing,
      stepStatus = Prelude.Nothing
    }

-- | The current attempt of the execution step. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/pipelines-retry-policy.html Retry Policy for SageMaker Pipelines steps>.
pipelineExecutionStep_attemptCount :: Lens.Lens' PipelineExecutionStep (Prelude.Maybe Prelude.Int)
pipelineExecutionStep_attemptCount = Lens.lens (\PipelineExecutionStep' {attemptCount} -> attemptCount) (\s@PipelineExecutionStep' {} a -> s {attemptCount = a} :: PipelineExecutionStep)

-- | If this pipeline execution step was cached, details on the cache hit.
pipelineExecutionStep_cacheHitResult :: Lens.Lens' PipelineExecutionStep (Prelude.Maybe CacheHitResult)
pipelineExecutionStep_cacheHitResult = Lens.lens (\PipelineExecutionStep' {cacheHitResult} -> cacheHitResult) (\s@PipelineExecutionStep' {} a -> s {cacheHitResult = a} :: PipelineExecutionStep)

-- | The time that the step stopped executing.
pipelineExecutionStep_endTime :: Lens.Lens' PipelineExecutionStep (Prelude.Maybe Prelude.UTCTime)
pipelineExecutionStep_endTime = Lens.lens (\PipelineExecutionStep' {endTime} -> endTime) (\s@PipelineExecutionStep' {} a -> s {endTime = a} :: PipelineExecutionStep) Prelude.. Lens.mapping Data._Time

-- | The reason why the step failed execution. This is only returned if the
-- step failed its execution.
pipelineExecutionStep_failureReason :: Lens.Lens' PipelineExecutionStep (Prelude.Maybe Prelude.Text)
pipelineExecutionStep_failureReason = Lens.lens (\PipelineExecutionStep' {failureReason} -> failureReason) (\s@PipelineExecutionStep' {} a -> s {failureReason = a} :: PipelineExecutionStep)

-- | Metadata to run the pipeline step.
pipelineExecutionStep_metadata :: Lens.Lens' PipelineExecutionStep (Prelude.Maybe PipelineExecutionStepMetadata)
pipelineExecutionStep_metadata = Lens.lens (\PipelineExecutionStep' {metadata} -> metadata) (\s@PipelineExecutionStep' {} a -> s {metadata = a} :: PipelineExecutionStep)

-- | The time that the step started executing.
pipelineExecutionStep_startTime :: Lens.Lens' PipelineExecutionStep (Prelude.Maybe Prelude.UTCTime)
pipelineExecutionStep_startTime = Lens.lens (\PipelineExecutionStep' {startTime} -> startTime) (\s@PipelineExecutionStep' {} a -> s {startTime = a} :: PipelineExecutionStep) Prelude.. Lens.mapping Data._Time

-- | The description of the step.
pipelineExecutionStep_stepDescription :: Lens.Lens' PipelineExecutionStep (Prelude.Maybe Prelude.Text)
pipelineExecutionStep_stepDescription = Lens.lens (\PipelineExecutionStep' {stepDescription} -> stepDescription) (\s@PipelineExecutionStep' {} a -> s {stepDescription = a} :: PipelineExecutionStep)

-- | The display name of the step.
pipelineExecutionStep_stepDisplayName :: Lens.Lens' PipelineExecutionStep (Prelude.Maybe Prelude.Text)
pipelineExecutionStep_stepDisplayName = Lens.lens (\PipelineExecutionStep' {stepDisplayName} -> stepDisplayName) (\s@PipelineExecutionStep' {} a -> s {stepDisplayName = a} :: PipelineExecutionStep)

-- | The name of the step that is executed.
pipelineExecutionStep_stepName :: Lens.Lens' PipelineExecutionStep (Prelude.Maybe Prelude.Text)
pipelineExecutionStep_stepName = Lens.lens (\PipelineExecutionStep' {stepName} -> stepName) (\s@PipelineExecutionStep' {} a -> s {stepName = a} :: PipelineExecutionStep)

-- | The status of the step execution.
pipelineExecutionStep_stepStatus :: Lens.Lens' PipelineExecutionStep (Prelude.Maybe StepStatus)
pipelineExecutionStep_stepStatus = Lens.lens (\PipelineExecutionStep' {stepStatus} -> stepStatus) (\s@PipelineExecutionStep' {} a -> s {stepStatus = a} :: PipelineExecutionStep)

instance Data.FromJSON PipelineExecutionStep where
  parseJSON =
    Data.withObject
      "PipelineExecutionStep"
      ( \x ->
          PipelineExecutionStep'
            Prelude.<$> (x Data..:? "AttemptCount")
            Prelude.<*> (x Data..:? "CacheHitResult")
            Prelude.<*> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..:? "Metadata")
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "StepDescription")
            Prelude.<*> (x Data..:? "StepDisplayName")
            Prelude.<*> (x Data..:? "StepName")
            Prelude.<*> (x Data..:? "StepStatus")
      )

instance Prelude.Hashable PipelineExecutionStep where
  hashWithSalt _salt PipelineExecutionStep' {..} =
    _salt `Prelude.hashWithSalt` attemptCount
      `Prelude.hashWithSalt` cacheHitResult
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` stepDescription
      `Prelude.hashWithSalt` stepDisplayName
      `Prelude.hashWithSalt` stepName
      `Prelude.hashWithSalt` stepStatus

instance Prelude.NFData PipelineExecutionStep where
  rnf PipelineExecutionStep' {..} =
    Prelude.rnf attemptCount
      `Prelude.seq` Prelude.rnf cacheHitResult
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf stepDescription
      `Prelude.seq` Prelude.rnf stepDisplayName
      `Prelude.seq` Prelude.rnf stepName
      `Prelude.seq` Prelude.rnf stepStatus
