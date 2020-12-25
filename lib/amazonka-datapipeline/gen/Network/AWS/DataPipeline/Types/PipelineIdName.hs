{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.PipelineIdName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.PipelineIdName
  ( PipelineIdName (..),

    -- * Smart constructor
    mkPipelineIdName,

    -- * Lenses
    pinId,
    pinName,
  )
where

import qualified Network.AWS.DataPipeline.Types.Id as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the name and identifier of a pipeline.
--
-- /See:/ 'mkPipelineIdName' smart constructor.
data PipelineIdName = PipelineIdName'
  { -- | The ID of the pipeline that was assigned by AWS Data Pipeline. This is a string of the form @df-297EG78HU43EEXAMPLE@ .
    id :: Core.Maybe Types.Id,
    -- | The name of the pipeline.
    name :: Core.Maybe Types.Id
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PipelineIdName' value with any optional fields omitted.
mkPipelineIdName ::
  PipelineIdName
mkPipelineIdName =
  PipelineIdName' {id = Core.Nothing, name = Core.Nothing}

-- | The ID of the pipeline that was assigned by AWS Data Pipeline. This is a string of the form @df-297EG78HU43EEXAMPLE@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pinId :: Lens.Lens' PipelineIdName (Core.Maybe Types.Id)
pinId = Lens.field @"id"
{-# DEPRECATED pinId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The name of the pipeline.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pinName :: Lens.Lens' PipelineIdName (Core.Maybe Types.Id)
pinName = Lens.field @"name"
{-# DEPRECATED pinName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON PipelineIdName where
  parseJSON =
    Core.withObject "PipelineIdName" Core.$
      \x ->
        PipelineIdName'
          Core.<$> (x Core..:? "id") Core.<*> (x Core..:? "name")
