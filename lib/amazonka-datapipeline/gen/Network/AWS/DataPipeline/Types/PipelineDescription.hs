{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.PipelineDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.PipelineDescription
  ( PipelineDescription (..),

    -- * Smart constructor
    mkPipelineDescription,

    -- * Lenses
    pdPipelineId,
    pdName,
    pdFields,
    pdDescription,
    pdTags,
  )
where

import qualified Network.AWS.DataPipeline.Types.Field as Types
import qualified Network.AWS.DataPipeline.Types.Name as Types
import qualified Network.AWS.DataPipeline.Types.PipelineId as Types
import qualified Network.AWS.DataPipeline.Types.String as Types
import qualified Network.AWS.DataPipeline.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains pipeline metadata.
--
-- /See:/ 'mkPipelineDescription' smart constructor.
data PipelineDescription = PipelineDescription'
  { -- | The pipeline identifier that was assigned by AWS Data Pipeline. This is a string of the form @df-297EG78HU43EEXAMPLE@ .
    pipelineId :: Types.PipelineId,
    -- | The name of the pipeline.
    name :: Types.Name,
    -- | A list of read-only fields that contain metadata about the pipeline: @userId, @accountId, and @pipelineState.
    fields :: [Types.Field],
    -- | Description of the pipeline.
    description :: Core.Maybe Types.String,
    -- | A list of tags to associated with a pipeline. Tags let you control access to pipelines. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-control-access.html Controlling User Access to Pipelines> in the /AWS Data Pipeline Developer Guide/ .
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PipelineDescription' value with any optional fields omitted.
mkPipelineDescription ::
  -- | 'pipelineId'
  Types.PipelineId ->
  -- | 'name'
  Types.Name ->
  PipelineDescription
mkPipelineDescription pipelineId name =
  PipelineDescription'
    { pipelineId,
      name,
      fields = Core.mempty,
      description = Core.Nothing,
      tags = Core.Nothing
    }

-- | The pipeline identifier that was assigned by AWS Data Pipeline. This is a string of the form @df-297EG78HU43EEXAMPLE@ .
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPipelineId :: Lens.Lens' PipelineDescription Types.PipelineId
pdPipelineId = Lens.field @"pipelineId"
{-# DEPRECATED pdPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

-- | The name of the pipeline.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdName :: Lens.Lens' PipelineDescription Types.Name
pdName = Lens.field @"name"
{-# DEPRECATED pdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of read-only fields that contain metadata about the pipeline: @userId, @accountId, and @pipelineState.
--
-- /Note:/ Consider using 'fields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdFields :: Lens.Lens' PipelineDescription [Types.Field]
pdFields = Lens.field @"fields"
{-# DEPRECATED pdFields "Use generic-lens or generic-optics with 'fields' instead." #-}

-- | Description of the pipeline.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdDescription :: Lens.Lens' PipelineDescription (Core.Maybe Types.String)
pdDescription = Lens.field @"description"
{-# DEPRECATED pdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of tags to associated with a pipeline. Tags let you control access to pipelines. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-control-access.html Controlling User Access to Pipelines> in the /AWS Data Pipeline Developer Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdTags :: Lens.Lens' PipelineDescription (Core.Maybe [Types.Tag])
pdTags = Lens.field @"tags"
{-# DEPRECATED pdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON PipelineDescription where
  parseJSON =
    Core.withObject "PipelineDescription" Core.$
      \x ->
        PipelineDescription'
          Core.<$> (x Core..: "pipelineId")
          Core.<*> (x Core..: "name")
          Core.<*> (x Core..:? "fields" Core..!= Core.mempty)
          Core.<*> (x Core..:? "description")
          Core.<*> (x Core..:? "tags")
