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
import Network.AWS.SageMaker.Types.CacheHitResult
import Network.AWS.SageMaker.Types.PipelineExecutionStepMetadata
import Network.AWS.SageMaker.Types.StepStatus

-- | An execution of a step in a pipeline.
--
-- /See:/ 'newPipelineExecutionStep' smart constructor.
data PipelineExecutionStep = PipelineExecutionStep'
  { -- | The time that the step started executing.
    startTime :: Core.Maybe Core.POSIX,
    -- | The metadata for the step execution.
    metadata :: Core.Maybe PipelineExecutionStepMetadata,
    -- | The time that the step stopped executing.
    endTime :: Core.Maybe Core.POSIX,
    -- | The reason why the step failed execution. This is only returned if the
    -- step failed its execution.
    failureReason :: Core.Maybe Core.Text,
    -- | The status of the step execution.
    stepStatus :: Core.Maybe StepStatus,
    -- | If this pipeline execution step was cached, details on the cache hit.
    cacheHitResult :: Core.Maybe CacheHitResult,
    -- | The name of the step that is executed.
    stepName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PipelineExecutionStep' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTime', 'pipelineExecutionStep_startTime' - The time that the step started executing.
--
-- 'metadata', 'pipelineExecutionStep_metadata' - The metadata for the step execution.
--
-- 'endTime', 'pipelineExecutionStep_endTime' - The time that the step stopped executing.
--
-- 'failureReason', 'pipelineExecutionStep_failureReason' - The reason why the step failed execution. This is only returned if the
-- step failed its execution.
--
-- 'stepStatus', 'pipelineExecutionStep_stepStatus' - The status of the step execution.
--
-- 'cacheHitResult', 'pipelineExecutionStep_cacheHitResult' - If this pipeline execution step was cached, details on the cache hit.
--
-- 'stepName', 'pipelineExecutionStep_stepName' - The name of the step that is executed.
newPipelineExecutionStep ::
  PipelineExecutionStep
newPipelineExecutionStep =
  PipelineExecutionStep'
    { startTime = Core.Nothing,
      metadata = Core.Nothing,
      endTime = Core.Nothing,
      failureReason = Core.Nothing,
      stepStatus = Core.Nothing,
      cacheHitResult = Core.Nothing,
      stepName = Core.Nothing
    }

-- | The time that the step started executing.
pipelineExecutionStep_startTime :: Lens.Lens' PipelineExecutionStep (Core.Maybe Core.UTCTime)
pipelineExecutionStep_startTime = Lens.lens (\PipelineExecutionStep' {startTime} -> startTime) (\s@PipelineExecutionStep' {} a -> s {startTime = a} :: PipelineExecutionStep) Core.. Lens.mapping Core._Time

-- | The metadata for the step execution.
pipelineExecutionStep_metadata :: Lens.Lens' PipelineExecutionStep (Core.Maybe PipelineExecutionStepMetadata)
pipelineExecutionStep_metadata = Lens.lens (\PipelineExecutionStep' {metadata} -> metadata) (\s@PipelineExecutionStep' {} a -> s {metadata = a} :: PipelineExecutionStep)

-- | The time that the step stopped executing.
pipelineExecutionStep_endTime :: Lens.Lens' PipelineExecutionStep (Core.Maybe Core.UTCTime)
pipelineExecutionStep_endTime = Lens.lens (\PipelineExecutionStep' {endTime} -> endTime) (\s@PipelineExecutionStep' {} a -> s {endTime = a} :: PipelineExecutionStep) Core.. Lens.mapping Core._Time

-- | The reason why the step failed execution. This is only returned if the
-- step failed its execution.
pipelineExecutionStep_failureReason :: Lens.Lens' PipelineExecutionStep (Core.Maybe Core.Text)
pipelineExecutionStep_failureReason = Lens.lens (\PipelineExecutionStep' {failureReason} -> failureReason) (\s@PipelineExecutionStep' {} a -> s {failureReason = a} :: PipelineExecutionStep)

-- | The status of the step execution.
pipelineExecutionStep_stepStatus :: Lens.Lens' PipelineExecutionStep (Core.Maybe StepStatus)
pipelineExecutionStep_stepStatus = Lens.lens (\PipelineExecutionStep' {stepStatus} -> stepStatus) (\s@PipelineExecutionStep' {} a -> s {stepStatus = a} :: PipelineExecutionStep)

-- | If this pipeline execution step was cached, details on the cache hit.
pipelineExecutionStep_cacheHitResult :: Lens.Lens' PipelineExecutionStep (Core.Maybe CacheHitResult)
pipelineExecutionStep_cacheHitResult = Lens.lens (\PipelineExecutionStep' {cacheHitResult} -> cacheHitResult) (\s@PipelineExecutionStep' {} a -> s {cacheHitResult = a} :: PipelineExecutionStep)

-- | The name of the step that is executed.
pipelineExecutionStep_stepName :: Lens.Lens' PipelineExecutionStep (Core.Maybe Core.Text)
pipelineExecutionStep_stepName = Lens.lens (\PipelineExecutionStep' {stepName} -> stepName) (\s@PipelineExecutionStep' {} a -> s {stepName = a} :: PipelineExecutionStep)

instance Core.FromJSON PipelineExecutionStep where
  parseJSON =
    Core.withObject
      "PipelineExecutionStep"
      ( \x ->
          PipelineExecutionStep'
            Core.<$> (x Core..:? "StartTime")
            Core.<*> (x Core..:? "Metadata")
            Core.<*> (x Core..:? "EndTime")
            Core.<*> (x Core..:? "FailureReason")
            Core.<*> (x Core..:? "StepStatus")
            Core.<*> (x Core..:? "CacheHitResult")
            Core.<*> (x Core..:? "StepName")
      )

instance Core.Hashable PipelineExecutionStep

instance Core.NFData PipelineExecutionStep
