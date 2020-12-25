{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.PipelineObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.PipelineObject
  ( PipelineObject (..),

    -- * Smart constructor
    mkPipelineObject,

    -- * Lenses
    pId,
    pName,
    pFields,
  )
where

import qualified Network.AWS.DataPipeline.Types.Field as Types
import qualified Network.AWS.DataPipeline.Types.Id as Types
import qualified Network.AWS.DataPipeline.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a pipeline object. This can be a logical, physical, or physical attempt pipeline object. The complete set of components of a pipeline defines the pipeline.
--
-- /See:/ 'mkPipelineObject' smart constructor.
data PipelineObject = PipelineObject'
  { -- | The ID of the object.
    id :: Types.Id,
    -- | The name of the object.
    name :: Types.Name,
    -- | Key-value pairs that define the properties of the object.
    fields :: [Types.Field]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PipelineObject' value with any optional fields omitted.
mkPipelineObject ::
  -- | 'id'
  Types.Id ->
  -- | 'name'
  Types.Name ->
  PipelineObject
mkPipelineObject id name =
  PipelineObject' {id, name, fields = Core.mempty}

-- | The ID of the object.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pId :: Lens.Lens' PipelineObject Types.Id
pId = Lens.field @"id"
{-# DEPRECATED pId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The name of the object.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pName :: Lens.Lens' PipelineObject Types.Name
pName = Lens.field @"name"
{-# DEPRECATED pName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Key-value pairs that define the properties of the object.
--
-- /Note:/ Consider using 'fields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pFields :: Lens.Lens' PipelineObject [Types.Field]
pFields = Lens.field @"fields"
{-# DEPRECATED pFields "Use generic-lens or generic-optics with 'fields' instead." #-}

instance Core.FromJSON PipelineObject where
  toJSON PipelineObject {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("id" Core..= id),
            Core.Just ("name" Core..= name),
            Core.Just ("fields" Core..= fields)
          ]
      )

instance Core.FromJSON PipelineObject where
  parseJSON =
    Core.withObject "PipelineObject" Core.$
      \x ->
        PipelineObject'
          Core.<$> (x Core..: "id")
          Core.<*> (x Core..: "name")
          Core.<*> (x Core..:? "fields" Core..!= Core.mempty)
