{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.PutPipelineDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds tasks, schedules, and preconditions to the specified pipeline. You can use @PutPipelineDefinition@ to populate a new pipeline.
--
-- @PutPipelineDefinition@ also validates the configuration as it adds it to the pipeline. Changes to the pipeline are saved unless one of the following three validation errors exists in the pipeline.
--
--     * An object is missing a name or identifier field.
--
--     * A string or reference field is empty.
--
--     * The number of objects in the pipeline exceeds the maximum allowed objects.
--
--     * The pipeline is in a FINISHED state.
--
-- Pipeline object definitions are passed to the @PutPipelineDefinition@ action and returned by the 'GetPipelineDefinition' action.
module Network.AWS.DataPipeline.PutPipelineDefinition
  ( -- * Creating a request
    PutPipelineDefinition (..),
    mkPutPipelineDefinition,

    -- ** Request lenses
    ppdPipelineId,
    ppdPipelineObjects,
    ppdParameterObjects,
    ppdParameterValues,

    -- * Destructuring the response
    PutPipelineDefinitionResponse (..),
    mkPutPipelineDefinitionResponse,

    -- ** Response lenses
    ppdrrsErrored,
    ppdrrsValidationErrors,
    ppdrrsValidationWarnings,
    ppdrrsResponseStatus,
  )
where

import qualified Network.AWS.DataPipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for PutPipelineDefinition.
--
-- /See:/ 'mkPutPipelineDefinition' smart constructor.
data PutPipelineDefinition = PutPipelineDefinition'
  { -- | The ID of the pipeline.
    pipelineId :: Types.Id,
    -- | The objects that define the pipeline. These objects overwrite the existing pipeline definition.
    pipelineObjects :: [Types.PipelineObject],
    -- | The parameter objects used with the pipeline.
    parameterObjects :: Core.Maybe [Types.ParameterObject],
    -- | The parameter values used with the pipeline.
    parameterValues :: Core.Maybe [Types.ParameterValue]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutPipelineDefinition' value with any optional fields omitted.
mkPutPipelineDefinition ::
  -- | 'pipelineId'
  Types.Id ->
  PutPipelineDefinition
mkPutPipelineDefinition pipelineId =
  PutPipelineDefinition'
    { pipelineId,
      pipelineObjects = Core.mempty,
      parameterObjects = Core.Nothing,
      parameterValues = Core.Nothing
    }

-- | The ID of the pipeline.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdPipelineId :: Lens.Lens' PutPipelineDefinition Types.Id
ppdPipelineId = Lens.field @"pipelineId"
{-# DEPRECATED ppdPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

-- | The objects that define the pipeline. These objects overwrite the existing pipeline definition.
--
-- /Note:/ Consider using 'pipelineObjects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdPipelineObjects :: Lens.Lens' PutPipelineDefinition [Types.PipelineObject]
ppdPipelineObjects = Lens.field @"pipelineObjects"
{-# DEPRECATED ppdPipelineObjects "Use generic-lens or generic-optics with 'pipelineObjects' instead." #-}

-- | The parameter objects used with the pipeline.
--
-- /Note:/ Consider using 'parameterObjects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdParameterObjects :: Lens.Lens' PutPipelineDefinition (Core.Maybe [Types.ParameterObject])
ppdParameterObjects = Lens.field @"parameterObjects"
{-# DEPRECATED ppdParameterObjects "Use generic-lens or generic-optics with 'parameterObjects' instead." #-}

-- | The parameter values used with the pipeline.
--
-- /Note:/ Consider using 'parameterValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdParameterValues :: Lens.Lens' PutPipelineDefinition (Core.Maybe [Types.ParameterValue])
ppdParameterValues = Lens.field @"parameterValues"
{-# DEPRECATED ppdParameterValues "Use generic-lens or generic-optics with 'parameterValues' instead." #-}

instance Core.FromJSON PutPipelineDefinition where
  toJSON PutPipelineDefinition {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pipelineId" Core..= pipelineId),
            Core.Just ("pipelineObjects" Core..= pipelineObjects),
            ("parameterObjects" Core..=) Core.<$> parameterObjects,
            ("parameterValues" Core..=) Core.<$> parameterValues
          ]
      )

instance Core.AWSRequest PutPipelineDefinition where
  type Rs PutPipelineDefinition = PutPipelineDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DataPipeline.PutPipelineDefinition")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutPipelineDefinitionResponse'
            Core.<$> (x Core..: "errored")
            Core.<*> (x Core..:? "validationErrors")
            Core.<*> (x Core..:? "validationWarnings")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of PutPipelineDefinition.
--
-- /See:/ 'mkPutPipelineDefinitionResponse' smart constructor.
data PutPipelineDefinitionResponse = PutPipelineDefinitionResponse'
  { -- | Indicates whether there were validation errors, and the pipeline definition is stored but cannot be activated until you correct the pipeline and call @PutPipelineDefinition@ to commit the corrected pipeline.
    errored :: Core.Bool,
    -- | The validation errors that are associated with the objects defined in @pipelineObjects@ .
    validationErrors :: Core.Maybe [Types.ValidationError],
    -- | The validation warnings that are associated with the objects defined in @pipelineObjects@ .
    validationWarnings :: Core.Maybe [Types.ValidationWarning],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutPipelineDefinitionResponse' value with any optional fields omitted.
mkPutPipelineDefinitionResponse ::
  -- | 'errored'
  Core.Bool ->
  -- | 'responseStatus'
  Core.Int ->
  PutPipelineDefinitionResponse
mkPutPipelineDefinitionResponse errored responseStatus =
  PutPipelineDefinitionResponse'
    { errored,
      validationErrors = Core.Nothing,
      validationWarnings = Core.Nothing,
      responseStatus
    }

-- | Indicates whether there were validation errors, and the pipeline definition is stored but cannot be activated until you correct the pipeline and call @PutPipelineDefinition@ to commit the corrected pipeline.
--
-- /Note:/ Consider using 'errored' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdrrsErrored :: Lens.Lens' PutPipelineDefinitionResponse Core.Bool
ppdrrsErrored = Lens.field @"errored"
{-# DEPRECATED ppdrrsErrored "Use generic-lens or generic-optics with 'errored' instead." #-}

-- | The validation errors that are associated with the objects defined in @pipelineObjects@ .
--
-- /Note:/ Consider using 'validationErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdrrsValidationErrors :: Lens.Lens' PutPipelineDefinitionResponse (Core.Maybe [Types.ValidationError])
ppdrrsValidationErrors = Lens.field @"validationErrors"
{-# DEPRECATED ppdrrsValidationErrors "Use generic-lens or generic-optics with 'validationErrors' instead." #-}

-- | The validation warnings that are associated with the objects defined in @pipelineObjects@ .
--
-- /Note:/ Consider using 'validationWarnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdrrsValidationWarnings :: Lens.Lens' PutPipelineDefinitionResponse (Core.Maybe [Types.ValidationWarning])
ppdrrsValidationWarnings = Lens.field @"validationWarnings"
{-# DEPRECATED ppdrrsValidationWarnings "Use generic-lens or generic-optics with 'validationWarnings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdrrsResponseStatus :: Lens.Lens' PutPipelineDefinitionResponse Core.Int
ppdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ppdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
