{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.PipelineDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DataPipeline.Types.PipelineDescription
  ( PipelineDescription (..)
  -- * Smart constructor
  , mkPipelineDescription
  -- * Lenses
  , pdPipelineId
  , pdName
  , pdFields
  , pdDescription
  , pdTags
  ) where

import qualified Network.AWS.DataPipeline.Types.Field as Types
import qualified Network.AWS.DataPipeline.Types.Name as Types
import qualified Network.AWS.DataPipeline.Types.PipelineId as Types
import qualified Network.AWS.DataPipeline.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains pipeline metadata.
--
-- /See:/ 'mkPipelineDescription' smart constructor.
data PipelineDescription = PipelineDescription'
  { pipelineId :: Types.PipelineId
    -- ^ The pipeline identifier that was assigned by AWS Data Pipeline. This is a string of the form @df-297EG78HU43EEXAMPLE@ .
  , name :: Types.Name
    -- ^ The name of the pipeline.
  , fields :: [Types.Field]
    -- ^ A list of read-only fields that contain metadata about the pipeline: @userId, @accountId, and @pipelineState.
  , description :: Core.Maybe Core.Text
    -- ^ Description of the pipeline.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of tags to associated with a pipeline. Tags let you control access to pipelines. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-control-access.html Controlling User Access to Pipelines> in the /AWS Data Pipeline Developer Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PipelineDescription' value with any optional fields omitted.
mkPipelineDescription
    :: Types.PipelineId -- ^ 'pipelineId'
    -> Types.Name -- ^ 'name'
    -> PipelineDescription
mkPipelineDescription pipelineId name
  = PipelineDescription'{pipelineId, name, fields = Core.mempty,
                         description = Core.Nothing, tags = Core.Nothing}

-- | The pipeline identifier that was assigned by AWS Data Pipeline. This is a string of the form @df-297EG78HU43EEXAMPLE@ .
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPipelineId :: Lens.Lens' PipelineDescription Types.PipelineId
pdPipelineId = Lens.field @"pipelineId"
{-# INLINEABLE pdPipelineId #-}
{-# DEPRECATED pipelineId "Use generic-lens or generic-optics with 'pipelineId' instead"  #-}

-- | The name of the pipeline.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdName :: Lens.Lens' PipelineDescription Types.Name
pdName = Lens.field @"name"
{-# INLINEABLE pdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A list of read-only fields that contain metadata about the pipeline: @userId, @accountId, and @pipelineState.
--
-- /Note:/ Consider using 'fields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdFields :: Lens.Lens' PipelineDescription [Types.Field]
pdFields = Lens.field @"fields"
{-# INLINEABLE pdFields #-}
{-# DEPRECATED fields "Use generic-lens or generic-optics with 'fields' instead"  #-}

-- | Description of the pipeline.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdDescription :: Lens.Lens' PipelineDescription (Core.Maybe Core.Text)
pdDescription = Lens.field @"description"
{-# INLINEABLE pdDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | A list of tags to associated with a pipeline. Tags let you control access to pipelines. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-control-access.html Controlling User Access to Pipelines> in the /AWS Data Pipeline Developer Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdTags :: Lens.Lens' PipelineDescription (Core.Maybe [Types.Tag])
pdTags = Lens.field @"tags"
{-# INLINEABLE pdTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON PipelineDescription where
        parseJSON
          = Core.withObject "PipelineDescription" Core.$
              \ x ->
                PipelineDescription' Core.<$>
                  (x Core..: "pipelineId") Core.<*> x Core..: "name" Core.<*>
                    x Core..:? "fields" Core..!= Core.mempty
                    Core.<*> x Core..:? "description"
                    Core.<*> x Core..:? "tags"
