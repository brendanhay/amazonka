{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.PipelineSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.PipelineSummary
  ( PipelineSummary (..),

    -- * Smart constructor
    mkPipelineSummary,

    -- * Lenses
    psCreated,
    psName,
    psUpdated,
    psVersion,
  )
where

import qualified Network.AWS.CodePipeline.Types.PipelineName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns a summary of a pipeline.
--
-- /See:/ 'mkPipelineSummary' smart constructor.
data PipelineSummary = PipelineSummary'
  { -- | The date and time the pipeline was created, in timestamp format.
    created :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the pipeline.
    name :: Core.Maybe Types.PipelineName,
    -- | The date and time of the last update to the pipeline, in timestamp format.
    updated :: Core.Maybe Core.NominalDiffTime,
    -- | The version number of the pipeline.
    version :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PipelineSummary' value with any optional fields omitted.
mkPipelineSummary ::
  PipelineSummary
mkPipelineSummary =
  PipelineSummary'
    { created = Core.Nothing,
      name = Core.Nothing,
      updated = Core.Nothing,
      version = Core.Nothing
    }

-- | The date and time the pipeline was created, in timestamp format.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psCreated :: Lens.Lens' PipelineSummary (Core.Maybe Core.NominalDiffTime)
psCreated = Lens.field @"created"
{-# DEPRECATED psCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | The name of the pipeline.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psName :: Lens.Lens' PipelineSummary (Core.Maybe Types.PipelineName)
psName = Lens.field @"name"
{-# DEPRECATED psName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The date and time of the last update to the pipeline, in timestamp format.
--
-- /Note:/ Consider using 'updated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psUpdated :: Lens.Lens' PipelineSummary (Core.Maybe Core.NominalDiffTime)
psUpdated = Lens.field @"updated"
{-# DEPRECATED psUpdated "Use generic-lens or generic-optics with 'updated' instead." #-}

-- | The version number of the pipeline.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psVersion :: Lens.Lens' PipelineSummary (Core.Maybe Core.Natural)
psVersion = Lens.field @"version"
{-# DEPRECATED psVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON PipelineSummary where
  parseJSON =
    Core.withObject "PipelineSummary" Core.$
      \x ->
        PipelineSummary'
          Core.<$> (x Core..:? "created")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "updated")
          Core.<*> (x Core..:? "version")
