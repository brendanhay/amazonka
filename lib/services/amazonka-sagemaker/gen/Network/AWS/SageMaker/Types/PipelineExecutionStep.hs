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
-- Module      : Network.AWS.SageMaker.Types.PipelineExecutionStep
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.PipelineExecutionStep where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.CacheHitResult
import Network.AWS.SageMaker.Types.PipelineExecutionStepMetadata
import Network.AWS.SageMaker.Types.StepStatus

-- | An execution of a step in a pipeline.
--
-- /See:/ 'newPipelineExecutionStep' smart constructor.
data PipelineExecutionStep = PipelineExecutionStep'
  { -- | The reason why the step failed execution. This is only returned if the
    -- step failed its execution.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The time that the step started executing.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the step that is executed.
    stepName :: Prelude.Maybe Prelude.Text,
    -- | The status of the step execution.
    stepStatus :: Prelude.Maybe StepStatus,
    -- | The time that the step stopped executing.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | Metadata for the step execution.
    metadata :: Prelude.Maybe PipelineExecutionStepMetadata,
    -- | If this pipeline execution step was cached, details on the cache hit.
    cacheHitResult :: Prelude.Maybe CacheHitResult
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
-- 'failureReason', 'pipelineExecutionStep_failureReason' - The reason why the step failed execution. This is only returned if the
-- step failed its execution.
--
-- 'startTime', 'pipelineExecutionStep_startTime' - The time that the step started executing.
--
-- 'stepName', 'pipelineExecutionStep_stepName' - The name of the step that is executed.
--
-- 'stepStatus', 'pipelineExecutionStep_stepStatus' - The status of the step execution.
--
-- 'endTime', 'pipelineExecutionStep_endTime' - The time that the step stopped executing.
--
-- 'metadata', 'pipelineExecutionStep_metadata' - Metadata for the step execution.
--
-- 'cacheHitResult', 'pipelineExecutionStep_cacheHitResult' - If this pipeline execution step was cached, details on the cache hit.
newPipelineExecutionStep ::
  PipelineExecutionStep
newPipelineExecutionStep =
  PipelineExecutionStep'
    { failureReason =
        Prelude.Nothing,
      startTime = Prelude.Nothing,
      stepName = Prelude.Nothing,
      stepStatus = Prelude.Nothing,
      endTime = Prelude.Nothing,
      metadata = Prelude.Nothing,
      cacheHitResult = Prelude.Nothing
    }

-- | The reason why the step failed execution. This is only returned if the
-- step failed its execution.
pipelineExecutionStep_failureReason :: Lens.Lens' PipelineExecutionStep (Prelude.Maybe Prelude.Text)
pipelineExecutionStep_failureReason = Lens.lens (\PipelineExecutionStep' {failureReason} -> failureReason) (\s@PipelineExecutionStep' {} a -> s {failureReason = a} :: PipelineExecutionStep)

-- | The time that the step started executing.
pipelineExecutionStep_startTime :: Lens.Lens' PipelineExecutionStep (Prelude.Maybe Prelude.UTCTime)
pipelineExecutionStep_startTime = Lens.lens (\PipelineExecutionStep' {startTime} -> startTime) (\s@PipelineExecutionStep' {} a -> s {startTime = a} :: PipelineExecutionStep) Prelude.. Lens.mapping Core._Time

-- | The name of the step that is executed.
pipelineExecutionStep_stepName :: Lens.Lens' PipelineExecutionStep (Prelude.Maybe Prelude.Text)
pipelineExecutionStep_stepName = Lens.lens (\PipelineExecutionStep' {stepName} -> stepName) (\s@PipelineExecutionStep' {} a -> s {stepName = a} :: PipelineExecutionStep)

-- | The status of the step execution.
pipelineExecutionStep_stepStatus :: Lens.Lens' PipelineExecutionStep (Prelude.Maybe StepStatus)
pipelineExecutionStep_stepStatus = Lens.lens (\PipelineExecutionStep' {stepStatus} -> stepStatus) (\s@PipelineExecutionStep' {} a -> s {stepStatus = a} :: PipelineExecutionStep)

-- | The time that the step stopped executing.
pipelineExecutionStep_endTime :: Lens.Lens' PipelineExecutionStep (Prelude.Maybe Prelude.UTCTime)
pipelineExecutionStep_endTime = Lens.lens (\PipelineExecutionStep' {endTime} -> endTime) (\s@PipelineExecutionStep' {} a -> s {endTime = a} :: PipelineExecutionStep) Prelude.. Lens.mapping Core._Time

-- | Metadata for the step execution.
pipelineExecutionStep_metadata :: Lens.Lens' PipelineExecutionStep (Prelude.Maybe PipelineExecutionStepMetadata)
pipelineExecutionStep_metadata = Lens.lens (\PipelineExecutionStep' {metadata} -> metadata) (\s@PipelineExecutionStep' {} a -> s {metadata = a} :: PipelineExecutionStep)

-- | If this pipeline execution step was cached, details on the cache hit.
pipelineExecutionStep_cacheHitResult :: Lens.Lens' PipelineExecutionStep (Prelude.Maybe CacheHitResult)
pipelineExecutionStep_cacheHitResult = Lens.lens (\PipelineExecutionStep' {cacheHitResult} -> cacheHitResult) (\s@PipelineExecutionStep' {} a -> s {cacheHitResult = a} :: PipelineExecutionStep)

instance Core.FromJSON PipelineExecutionStep where
  parseJSON =
    Core.withObject
      "PipelineExecutionStep"
      ( \x ->
          PipelineExecutionStep'
            Prelude.<$> (x Core..:? "FailureReason")
            Prelude.<*> (x Core..:? "StartTime")
            Prelude.<*> (x Core..:? "StepName")
            Prelude.<*> (x Core..:? "StepStatus")
            Prelude.<*> (x Core..:? "EndTime")
            Prelude.<*> (x Core..:? "Metadata")
            Prelude.<*> (x Core..:? "CacheHitResult")
      )

instance Prelude.Hashable PipelineExecutionStep

instance Prelude.NFData PipelineExecutionStep
