{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.PipelineMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.PipelineMetadata
  ( PipelineMetadata (..),

    -- * Smart constructor
    mkPipelineMetadata,

    -- * Lenses
    pmCreated,
    pmPipelineArn,
    pmUpdated,
  )
where

import qualified Network.AWS.CodePipeline.Types.PipelineArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a pipeline.
--
-- /See:/ 'mkPipelineMetadata' smart constructor.
data PipelineMetadata = PipelineMetadata'
  { -- | The date and time the pipeline was created, in timestamp format.
    created :: Core.Maybe Core.NominalDiffTime,
    -- | The Amazon Resource Name (ARN) of the pipeline.
    pipelineArn :: Core.Maybe Types.PipelineArn,
    -- | The date and time the pipeline was last updated, in timestamp format.
    updated :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PipelineMetadata' value with any optional fields omitted.
mkPipelineMetadata ::
  PipelineMetadata
mkPipelineMetadata =
  PipelineMetadata'
    { created = Core.Nothing,
      pipelineArn = Core.Nothing,
      updated = Core.Nothing
    }

-- | The date and time the pipeline was created, in timestamp format.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmCreated :: Lens.Lens' PipelineMetadata (Core.Maybe Core.NominalDiffTime)
pmCreated = Lens.field @"created"
{-# DEPRECATED pmCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | The Amazon Resource Name (ARN) of the pipeline.
--
-- /Note:/ Consider using 'pipelineArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmPipelineArn :: Lens.Lens' PipelineMetadata (Core.Maybe Types.PipelineArn)
pmPipelineArn = Lens.field @"pipelineArn"
{-# DEPRECATED pmPipelineArn "Use generic-lens or generic-optics with 'pipelineArn' instead." #-}

-- | The date and time the pipeline was last updated, in timestamp format.
--
-- /Note:/ Consider using 'updated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmUpdated :: Lens.Lens' PipelineMetadata (Core.Maybe Core.NominalDiffTime)
pmUpdated = Lens.field @"updated"
{-# DEPRECATED pmUpdated "Use generic-lens or generic-optics with 'updated' instead." #-}

instance Core.FromJSON PipelineMetadata where
  parseJSON =
    Core.withObject "PipelineMetadata" Core.$
      \x ->
        PipelineMetadata'
          Core.<$> (x Core..:? "created")
          Core.<*> (x Core..:? "pipelineArn")
          Core.<*> (x Core..:? "updated")
