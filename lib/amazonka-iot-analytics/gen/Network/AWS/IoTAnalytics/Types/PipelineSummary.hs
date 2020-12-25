{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.PipelineSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.PipelineSummary
  ( PipelineSummary (..),

    -- * Smart constructor
    mkPipelineSummary,

    -- * Lenses
    psCreationTime,
    psLastUpdateTime,
    psPipelineName,
    psReprocessingSummaries,
  )
where

import qualified Network.AWS.IoTAnalytics.Types.PipelineName as Types
import qualified Network.AWS.IoTAnalytics.Types.ReprocessingSummary as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A summary of information about a pipeline.
--
-- /See:/ 'mkPipelineSummary' smart constructor.
data PipelineSummary = PipelineSummary'
  { -- | When the pipeline was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | When the pipeline was last updated.
    lastUpdateTime :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the pipeline.
    pipelineName :: Core.Maybe Types.PipelineName,
    -- | A summary of information about the pipeline reprocessing.
    reprocessingSummaries :: Core.Maybe [Types.ReprocessingSummary]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PipelineSummary' value with any optional fields omitted.
mkPipelineSummary ::
  PipelineSummary
mkPipelineSummary =
  PipelineSummary'
    { creationTime = Core.Nothing,
      lastUpdateTime = Core.Nothing,
      pipelineName = Core.Nothing,
      reprocessingSummaries = Core.Nothing
    }

-- | When the pipeline was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psCreationTime :: Lens.Lens' PipelineSummary (Core.Maybe Core.NominalDiffTime)
psCreationTime = Lens.field @"creationTime"
{-# DEPRECATED psCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | When the pipeline was last updated.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psLastUpdateTime :: Lens.Lens' PipelineSummary (Core.Maybe Core.NominalDiffTime)
psLastUpdateTime = Lens.field @"lastUpdateTime"
{-# DEPRECATED psLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

-- | The name of the pipeline.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psPipelineName :: Lens.Lens' PipelineSummary (Core.Maybe Types.PipelineName)
psPipelineName = Lens.field @"pipelineName"
{-# DEPRECATED psPipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | A summary of information about the pipeline reprocessing.
--
-- /Note:/ Consider using 'reprocessingSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psReprocessingSummaries :: Lens.Lens' PipelineSummary (Core.Maybe [Types.ReprocessingSummary])
psReprocessingSummaries = Lens.field @"reprocessingSummaries"
{-# DEPRECATED psReprocessingSummaries "Use generic-lens or generic-optics with 'reprocessingSummaries' instead." #-}

instance Core.FromJSON PipelineSummary where
  parseJSON =
    Core.withObject "PipelineSummary" Core.$
      \x ->
        PipelineSummary'
          Core.<$> (x Core..:? "creationTime")
          Core.<*> (x Core..:? "lastUpdateTime")
          Core.<*> (x Core..:? "pipelineName")
          Core.<*> (x Core..:? "reprocessingSummaries")
