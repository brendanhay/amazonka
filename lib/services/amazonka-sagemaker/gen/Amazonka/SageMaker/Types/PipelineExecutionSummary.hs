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
-- Module      : Amazonka.SageMaker.Types.PipelineExecutionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.PipelineExecutionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.PipelineExecutionStatus

-- | A pipeline execution summary.
--
-- /See:/ 'newPipelineExecutionSummary' smart constructor.
data PipelineExecutionSummary = PipelineExecutionSummary'
  { -- | The status of the pipeline execution.
    pipelineExecutionStatus :: Prelude.Maybe PipelineExecutionStatus,
    -- | The start time of the pipeline execution.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the pipeline execution.
    pipelineExecutionArn :: Prelude.Maybe Prelude.Text,
    -- | The display name of the pipeline execution.
    pipelineExecutionDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The description of the pipeline execution.
    pipelineExecutionDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipelineExecutionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineExecutionStatus', 'pipelineExecutionSummary_pipelineExecutionStatus' - The status of the pipeline execution.
--
-- 'startTime', 'pipelineExecutionSummary_startTime' - The start time of the pipeline execution.
--
-- 'pipelineExecutionArn', 'pipelineExecutionSummary_pipelineExecutionArn' - The Amazon Resource Name (ARN) of the pipeline execution.
--
-- 'pipelineExecutionDisplayName', 'pipelineExecutionSummary_pipelineExecutionDisplayName' - The display name of the pipeline execution.
--
-- 'pipelineExecutionDescription', 'pipelineExecutionSummary_pipelineExecutionDescription' - The description of the pipeline execution.
newPipelineExecutionSummary ::
  PipelineExecutionSummary
newPipelineExecutionSummary =
  PipelineExecutionSummary'
    { pipelineExecutionStatus =
        Prelude.Nothing,
      startTime = Prelude.Nothing,
      pipelineExecutionArn = Prelude.Nothing,
      pipelineExecutionDisplayName = Prelude.Nothing,
      pipelineExecutionDescription = Prelude.Nothing
    }

-- | The status of the pipeline execution.
pipelineExecutionSummary_pipelineExecutionStatus :: Lens.Lens' PipelineExecutionSummary (Prelude.Maybe PipelineExecutionStatus)
pipelineExecutionSummary_pipelineExecutionStatus = Lens.lens (\PipelineExecutionSummary' {pipelineExecutionStatus} -> pipelineExecutionStatus) (\s@PipelineExecutionSummary' {} a -> s {pipelineExecutionStatus = a} :: PipelineExecutionSummary)

-- | The start time of the pipeline execution.
pipelineExecutionSummary_startTime :: Lens.Lens' PipelineExecutionSummary (Prelude.Maybe Prelude.UTCTime)
pipelineExecutionSummary_startTime = Lens.lens (\PipelineExecutionSummary' {startTime} -> startTime) (\s@PipelineExecutionSummary' {} a -> s {startTime = a} :: PipelineExecutionSummary) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the pipeline execution.
pipelineExecutionSummary_pipelineExecutionArn :: Lens.Lens' PipelineExecutionSummary (Prelude.Maybe Prelude.Text)
pipelineExecutionSummary_pipelineExecutionArn = Lens.lens (\PipelineExecutionSummary' {pipelineExecutionArn} -> pipelineExecutionArn) (\s@PipelineExecutionSummary' {} a -> s {pipelineExecutionArn = a} :: PipelineExecutionSummary)

-- | The display name of the pipeline execution.
pipelineExecutionSummary_pipelineExecutionDisplayName :: Lens.Lens' PipelineExecutionSummary (Prelude.Maybe Prelude.Text)
pipelineExecutionSummary_pipelineExecutionDisplayName = Lens.lens (\PipelineExecutionSummary' {pipelineExecutionDisplayName} -> pipelineExecutionDisplayName) (\s@PipelineExecutionSummary' {} a -> s {pipelineExecutionDisplayName = a} :: PipelineExecutionSummary)

-- | The description of the pipeline execution.
pipelineExecutionSummary_pipelineExecutionDescription :: Lens.Lens' PipelineExecutionSummary (Prelude.Maybe Prelude.Text)
pipelineExecutionSummary_pipelineExecutionDescription = Lens.lens (\PipelineExecutionSummary' {pipelineExecutionDescription} -> pipelineExecutionDescription) (\s@PipelineExecutionSummary' {} a -> s {pipelineExecutionDescription = a} :: PipelineExecutionSummary)

instance Core.FromJSON PipelineExecutionSummary where
  parseJSON =
    Core.withObject
      "PipelineExecutionSummary"
      ( \x ->
          PipelineExecutionSummary'
            Prelude.<$> (x Core..:? "PipelineExecutionStatus")
            Prelude.<*> (x Core..:? "StartTime")
            Prelude.<*> (x Core..:? "PipelineExecutionArn")
            Prelude.<*> (x Core..:? "PipelineExecutionDisplayName")
            Prelude.<*> (x Core..:? "PipelineExecutionDescription")
      )

instance Prelude.Hashable PipelineExecutionSummary

instance Prelude.NFData PipelineExecutionSummary
