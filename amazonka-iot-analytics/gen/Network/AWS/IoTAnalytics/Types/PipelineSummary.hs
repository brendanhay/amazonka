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
-- Module      : Network.AWS.IoTAnalytics.Types.PipelineSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.PipelineSummary where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types.ReprocessingSummary
import qualified Network.AWS.Lens as Lens

-- | A summary of information about a pipeline.
--
-- /See:/ 'newPipelineSummary' smart constructor.
data PipelineSummary = PipelineSummary'
  { -- | When the pipeline was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | When the pipeline was last updated.
    lastUpdateTime :: Core.Maybe Core.POSIX,
    -- | A summary of information about the pipeline reprocessing.
    reprocessingSummaries :: Core.Maybe [ReprocessingSummary],
    -- | The name of the pipeline.
    pipelineName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PipelineSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'pipelineSummary_creationTime' - When the pipeline was created.
--
-- 'lastUpdateTime', 'pipelineSummary_lastUpdateTime' - When the pipeline was last updated.
--
-- 'reprocessingSummaries', 'pipelineSummary_reprocessingSummaries' - A summary of information about the pipeline reprocessing.
--
-- 'pipelineName', 'pipelineSummary_pipelineName' - The name of the pipeline.
newPipelineSummary ::
  PipelineSummary
newPipelineSummary =
  PipelineSummary'
    { creationTime = Core.Nothing,
      lastUpdateTime = Core.Nothing,
      reprocessingSummaries = Core.Nothing,
      pipelineName = Core.Nothing
    }

-- | When the pipeline was created.
pipelineSummary_creationTime :: Lens.Lens' PipelineSummary (Core.Maybe Core.UTCTime)
pipelineSummary_creationTime = Lens.lens (\PipelineSummary' {creationTime} -> creationTime) (\s@PipelineSummary' {} a -> s {creationTime = a} :: PipelineSummary) Core.. Lens.mapping Core._Time

-- | When the pipeline was last updated.
pipelineSummary_lastUpdateTime :: Lens.Lens' PipelineSummary (Core.Maybe Core.UTCTime)
pipelineSummary_lastUpdateTime = Lens.lens (\PipelineSummary' {lastUpdateTime} -> lastUpdateTime) (\s@PipelineSummary' {} a -> s {lastUpdateTime = a} :: PipelineSummary) Core.. Lens.mapping Core._Time

-- | A summary of information about the pipeline reprocessing.
pipelineSummary_reprocessingSummaries :: Lens.Lens' PipelineSummary (Core.Maybe [ReprocessingSummary])
pipelineSummary_reprocessingSummaries = Lens.lens (\PipelineSummary' {reprocessingSummaries} -> reprocessingSummaries) (\s@PipelineSummary' {} a -> s {reprocessingSummaries = a} :: PipelineSummary) Core.. Lens.mapping Lens._Coerce

-- | The name of the pipeline.
pipelineSummary_pipelineName :: Lens.Lens' PipelineSummary (Core.Maybe Core.Text)
pipelineSummary_pipelineName = Lens.lens (\PipelineSummary' {pipelineName} -> pipelineName) (\s@PipelineSummary' {} a -> s {pipelineName = a} :: PipelineSummary)

instance Core.FromJSON PipelineSummary where
  parseJSON =
    Core.withObject
      "PipelineSummary"
      ( \x ->
          PipelineSummary'
            Core.<$> (x Core..:? "creationTime")
            Core.<*> (x Core..:? "lastUpdateTime")
            Core.<*> ( x Core..:? "reprocessingSummaries"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "pipelineName")
      )

instance Core.Hashable PipelineSummary

instance Core.NFData PipelineSummary
