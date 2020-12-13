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
    ppdPipelineObjects,
    ppdPipelineId,
    ppdParameterObjects,
    ppdParameterValues,

    -- * Destructuring the response
    PutPipelineDefinitionResponse (..),
    mkPutPipelineDefinitionResponse,

    -- ** Response lenses
    ppdrsValidationErrors,
    ppdrsValidationWarnings,
    ppdrsErrored,
    ppdrsResponseStatus,
  )
where

import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for PutPipelineDefinition.
--
-- /See:/ 'mkPutPipelineDefinition' smart constructor.
data PutPipelineDefinition = PutPipelineDefinition'
  { -- | The objects that define the pipeline. These objects overwrite the existing pipeline definition.
    pipelineObjects :: [PipelineObject],
    -- | The ID of the pipeline.
    pipelineId :: Lude.Text,
    -- | The parameter objects used with the pipeline.
    parameterObjects :: Lude.Maybe [ParameterObject],
    -- | The parameter values used with the pipeline.
    parameterValues :: Lude.Maybe [ParameterValue]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutPipelineDefinition' with the minimum fields required to make a request.
--
-- * 'pipelineObjects' - The objects that define the pipeline. These objects overwrite the existing pipeline definition.
-- * 'pipelineId' - The ID of the pipeline.
-- * 'parameterObjects' - The parameter objects used with the pipeline.
-- * 'parameterValues' - The parameter values used with the pipeline.
mkPutPipelineDefinition ::
  -- | 'pipelineId'
  Lude.Text ->
  PutPipelineDefinition
mkPutPipelineDefinition pPipelineId_ =
  PutPipelineDefinition'
    { pipelineObjects = Lude.mempty,
      pipelineId = pPipelineId_,
      parameterObjects = Lude.Nothing,
      parameterValues = Lude.Nothing
    }

-- | The objects that define the pipeline. These objects overwrite the existing pipeline definition.
--
-- /Note:/ Consider using 'pipelineObjects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdPipelineObjects :: Lens.Lens' PutPipelineDefinition [PipelineObject]
ppdPipelineObjects = Lens.lens (pipelineObjects :: PutPipelineDefinition -> [PipelineObject]) (\s a -> s {pipelineObjects = a} :: PutPipelineDefinition)
{-# DEPRECATED ppdPipelineObjects "Use generic-lens or generic-optics with 'pipelineObjects' instead." #-}

-- | The ID of the pipeline.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdPipelineId :: Lens.Lens' PutPipelineDefinition Lude.Text
ppdPipelineId = Lens.lens (pipelineId :: PutPipelineDefinition -> Lude.Text) (\s a -> s {pipelineId = a} :: PutPipelineDefinition)
{-# DEPRECATED ppdPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

-- | The parameter objects used with the pipeline.
--
-- /Note:/ Consider using 'parameterObjects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdParameterObjects :: Lens.Lens' PutPipelineDefinition (Lude.Maybe [ParameterObject])
ppdParameterObjects = Lens.lens (parameterObjects :: PutPipelineDefinition -> Lude.Maybe [ParameterObject]) (\s a -> s {parameterObjects = a} :: PutPipelineDefinition)
{-# DEPRECATED ppdParameterObjects "Use generic-lens or generic-optics with 'parameterObjects' instead." #-}

-- | The parameter values used with the pipeline.
--
-- /Note:/ Consider using 'parameterValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdParameterValues :: Lens.Lens' PutPipelineDefinition (Lude.Maybe [ParameterValue])
ppdParameterValues = Lens.lens (parameterValues :: PutPipelineDefinition -> Lude.Maybe [ParameterValue]) (\s a -> s {parameterValues = a} :: PutPipelineDefinition)
{-# DEPRECATED ppdParameterValues "Use generic-lens or generic-optics with 'parameterValues' instead." #-}

instance Lude.AWSRequest PutPipelineDefinition where
  type Rs PutPipelineDefinition = PutPipelineDefinitionResponse
  request = Req.postJSON dataPipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutPipelineDefinitionResponse'
            Lude.<$> (x Lude..?> "validationErrors" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "validationWarnings" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..:> "errored")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutPipelineDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DataPipeline.PutPipelineDefinition" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutPipelineDefinition where
  toJSON PutPipelineDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("pipelineObjects" Lude..= pipelineObjects),
            Lude.Just ("pipelineId" Lude..= pipelineId),
            ("parameterObjects" Lude..=) Lude.<$> parameterObjects,
            ("parameterValues" Lude..=) Lude.<$> parameterValues
          ]
      )

instance Lude.ToPath PutPipelineDefinition where
  toPath = Lude.const "/"

instance Lude.ToQuery PutPipelineDefinition where
  toQuery = Lude.const Lude.mempty

-- | Contains the output of PutPipelineDefinition.
--
-- /See:/ 'mkPutPipelineDefinitionResponse' smart constructor.
data PutPipelineDefinitionResponse = PutPipelineDefinitionResponse'
  { -- | The validation errors that are associated with the objects defined in @pipelineObjects@ .
    validationErrors :: Lude.Maybe [ValidationError],
    -- | The validation warnings that are associated with the objects defined in @pipelineObjects@ .
    validationWarnings :: Lude.Maybe [ValidationWarning],
    -- | Indicates whether there were validation errors, and the pipeline definition is stored but cannot be activated until you correct the pipeline and call @PutPipelineDefinition@ to commit the corrected pipeline.
    errored :: Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutPipelineDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'validationErrors' - The validation errors that are associated with the objects defined in @pipelineObjects@ .
-- * 'validationWarnings' - The validation warnings that are associated with the objects defined in @pipelineObjects@ .
-- * 'errored' - Indicates whether there were validation errors, and the pipeline definition is stored but cannot be activated until you correct the pipeline and call @PutPipelineDefinition@ to commit the corrected pipeline.
-- * 'responseStatus' - The response status code.
mkPutPipelineDefinitionResponse ::
  -- | 'errored'
  Lude.Bool ->
  -- | 'responseStatus'
  Lude.Int ->
  PutPipelineDefinitionResponse
mkPutPipelineDefinitionResponse pErrored_ pResponseStatus_ =
  PutPipelineDefinitionResponse'
    { validationErrors = Lude.Nothing,
      validationWarnings = Lude.Nothing,
      errored = pErrored_,
      responseStatus = pResponseStatus_
    }

-- | The validation errors that are associated with the objects defined in @pipelineObjects@ .
--
-- /Note:/ Consider using 'validationErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdrsValidationErrors :: Lens.Lens' PutPipelineDefinitionResponse (Lude.Maybe [ValidationError])
ppdrsValidationErrors = Lens.lens (validationErrors :: PutPipelineDefinitionResponse -> Lude.Maybe [ValidationError]) (\s a -> s {validationErrors = a} :: PutPipelineDefinitionResponse)
{-# DEPRECATED ppdrsValidationErrors "Use generic-lens or generic-optics with 'validationErrors' instead." #-}

-- | The validation warnings that are associated with the objects defined in @pipelineObjects@ .
--
-- /Note:/ Consider using 'validationWarnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdrsValidationWarnings :: Lens.Lens' PutPipelineDefinitionResponse (Lude.Maybe [ValidationWarning])
ppdrsValidationWarnings = Lens.lens (validationWarnings :: PutPipelineDefinitionResponse -> Lude.Maybe [ValidationWarning]) (\s a -> s {validationWarnings = a} :: PutPipelineDefinitionResponse)
{-# DEPRECATED ppdrsValidationWarnings "Use generic-lens or generic-optics with 'validationWarnings' instead." #-}

-- | Indicates whether there were validation errors, and the pipeline definition is stored but cannot be activated until you correct the pipeline and call @PutPipelineDefinition@ to commit the corrected pipeline.
--
-- /Note:/ Consider using 'errored' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdrsErrored :: Lens.Lens' PutPipelineDefinitionResponse Lude.Bool
ppdrsErrored = Lens.lens (errored :: PutPipelineDefinitionResponse -> Lude.Bool) (\s a -> s {errored = a} :: PutPipelineDefinitionResponse)
{-# DEPRECATED ppdrsErrored "Use generic-lens or generic-optics with 'errored' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdrsResponseStatus :: Lens.Lens' PutPipelineDefinitionResponse Lude.Int
ppdrsResponseStatus = Lens.lens (responseStatus :: PutPipelineDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutPipelineDefinitionResponse)
{-# DEPRECATED ppdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
