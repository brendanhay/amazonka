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
-- Module      : Network.AWS.SageMaker.Types.PipelineExecutionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.PipelineExecutionSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.PipelineExecutionStatus

-- | A pipeline execution summary.
--
-- /See:/ 'newPipelineExecutionSummary' smart constructor.
data PipelineExecutionSummary = PipelineExecutionSummary'
  { -- | The description of the pipeline execution.
    pipelineExecutionDescription :: Core.Maybe Core.Text,
    -- | The start time of the pipeline execution.
    startTime :: Core.Maybe Core.POSIX,
    -- | The display name of the pipeline execution.
    pipelineExecutionDisplayName :: Core.Maybe Core.Text,
    -- | The status of the pipeline execution.
    pipelineExecutionStatus :: Core.Maybe PipelineExecutionStatus,
    -- | The Amazon Resource Name (ARN) of the pipeline execution.
    pipelineExecutionArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PipelineExecutionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineExecutionDescription', 'pipelineExecutionSummary_pipelineExecutionDescription' - The description of the pipeline execution.
--
-- 'startTime', 'pipelineExecutionSummary_startTime' - The start time of the pipeline execution.
--
-- 'pipelineExecutionDisplayName', 'pipelineExecutionSummary_pipelineExecutionDisplayName' - The display name of the pipeline execution.
--
-- 'pipelineExecutionStatus', 'pipelineExecutionSummary_pipelineExecutionStatus' - The status of the pipeline execution.
--
-- 'pipelineExecutionArn', 'pipelineExecutionSummary_pipelineExecutionArn' - The Amazon Resource Name (ARN) of the pipeline execution.
newPipelineExecutionSummary ::
  PipelineExecutionSummary
newPipelineExecutionSummary =
  PipelineExecutionSummary'
    { pipelineExecutionDescription =
        Core.Nothing,
      startTime = Core.Nothing,
      pipelineExecutionDisplayName = Core.Nothing,
      pipelineExecutionStatus = Core.Nothing,
      pipelineExecutionArn = Core.Nothing
    }

-- | The description of the pipeline execution.
pipelineExecutionSummary_pipelineExecutionDescription :: Lens.Lens' PipelineExecutionSummary (Core.Maybe Core.Text)
pipelineExecutionSummary_pipelineExecutionDescription = Lens.lens (\PipelineExecutionSummary' {pipelineExecutionDescription} -> pipelineExecutionDescription) (\s@PipelineExecutionSummary' {} a -> s {pipelineExecutionDescription = a} :: PipelineExecutionSummary)

-- | The start time of the pipeline execution.
pipelineExecutionSummary_startTime :: Lens.Lens' PipelineExecutionSummary (Core.Maybe Core.UTCTime)
pipelineExecutionSummary_startTime = Lens.lens (\PipelineExecutionSummary' {startTime} -> startTime) (\s@PipelineExecutionSummary' {} a -> s {startTime = a} :: PipelineExecutionSummary) Core.. Lens.mapping Core._Time

-- | The display name of the pipeline execution.
pipelineExecutionSummary_pipelineExecutionDisplayName :: Lens.Lens' PipelineExecutionSummary (Core.Maybe Core.Text)
pipelineExecutionSummary_pipelineExecutionDisplayName = Lens.lens (\PipelineExecutionSummary' {pipelineExecutionDisplayName} -> pipelineExecutionDisplayName) (\s@PipelineExecutionSummary' {} a -> s {pipelineExecutionDisplayName = a} :: PipelineExecutionSummary)

-- | The status of the pipeline execution.
pipelineExecutionSummary_pipelineExecutionStatus :: Lens.Lens' PipelineExecutionSummary (Core.Maybe PipelineExecutionStatus)
pipelineExecutionSummary_pipelineExecutionStatus = Lens.lens (\PipelineExecutionSummary' {pipelineExecutionStatus} -> pipelineExecutionStatus) (\s@PipelineExecutionSummary' {} a -> s {pipelineExecutionStatus = a} :: PipelineExecutionSummary)

-- | The Amazon Resource Name (ARN) of the pipeline execution.
pipelineExecutionSummary_pipelineExecutionArn :: Lens.Lens' PipelineExecutionSummary (Core.Maybe Core.Text)
pipelineExecutionSummary_pipelineExecutionArn = Lens.lens (\PipelineExecutionSummary' {pipelineExecutionArn} -> pipelineExecutionArn) (\s@PipelineExecutionSummary' {} a -> s {pipelineExecutionArn = a} :: PipelineExecutionSummary)

instance Core.FromJSON PipelineExecutionSummary where
  parseJSON =
    Core.withObject
      "PipelineExecutionSummary"
      ( \x ->
          PipelineExecutionSummary'
            Core.<$> (x Core..:? "PipelineExecutionDescription")
            Core.<*> (x Core..:? "StartTime")
            Core.<*> (x Core..:? "PipelineExecutionDisplayName")
            Core.<*> (x Core..:? "PipelineExecutionStatus")
            Core.<*> (x Core..:? "PipelineExecutionArn")
      )

instance Core.Hashable PipelineExecutionSummary

instance Core.NFData PipelineExecutionSummary
