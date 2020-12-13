{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.ValidatePipelineDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates the specified pipeline definition to ensure that it is well formed and can be run without error.
module Network.AWS.DataPipeline.ValidatePipelineDefinition
  ( -- * Creating a request
    ValidatePipelineDefinition (..),
    mkValidatePipelineDefinition,

    -- ** Request lenses
    vpdPipelineObjects,
    vpdPipelineId,
    vpdParameterObjects,
    vpdParameterValues,

    -- * Destructuring the response
    ValidatePipelineDefinitionResponse (..),
    mkValidatePipelineDefinitionResponse,

    -- ** Response lenses
    vpdrsValidationErrors,
    vpdrsValidationWarnings,
    vpdrsErrored,
    vpdrsResponseStatus,
  )
where

import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for ValidatePipelineDefinition.
--
-- /See:/ 'mkValidatePipelineDefinition' smart constructor.
data ValidatePipelineDefinition = ValidatePipelineDefinition'
  { -- | The objects that define the pipeline changes to validate against the pipeline.
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

-- | Creates a value of 'ValidatePipelineDefinition' with the minimum fields required to make a request.
--
-- * 'pipelineObjects' - The objects that define the pipeline changes to validate against the pipeline.
-- * 'pipelineId' - The ID of the pipeline.
-- * 'parameterObjects' - The parameter objects used with the pipeline.
-- * 'parameterValues' - The parameter values used with the pipeline.
mkValidatePipelineDefinition ::
  -- | 'pipelineId'
  Lude.Text ->
  ValidatePipelineDefinition
mkValidatePipelineDefinition pPipelineId_ =
  ValidatePipelineDefinition'
    { pipelineObjects = Lude.mempty,
      pipelineId = pPipelineId_,
      parameterObjects = Lude.Nothing,
      parameterValues = Lude.Nothing
    }

-- | The objects that define the pipeline changes to validate against the pipeline.
--
-- /Note:/ Consider using 'pipelineObjects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpdPipelineObjects :: Lens.Lens' ValidatePipelineDefinition [PipelineObject]
vpdPipelineObjects = Lens.lens (pipelineObjects :: ValidatePipelineDefinition -> [PipelineObject]) (\s a -> s {pipelineObjects = a} :: ValidatePipelineDefinition)
{-# DEPRECATED vpdPipelineObjects "Use generic-lens or generic-optics with 'pipelineObjects' instead." #-}

-- | The ID of the pipeline.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpdPipelineId :: Lens.Lens' ValidatePipelineDefinition Lude.Text
vpdPipelineId = Lens.lens (pipelineId :: ValidatePipelineDefinition -> Lude.Text) (\s a -> s {pipelineId = a} :: ValidatePipelineDefinition)
{-# DEPRECATED vpdPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

-- | The parameter objects used with the pipeline.
--
-- /Note:/ Consider using 'parameterObjects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpdParameterObjects :: Lens.Lens' ValidatePipelineDefinition (Lude.Maybe [ParameterObject])
vpdParameterObjects = Lens.lens (parameterObjects :: ValidatePipelineDefinition -> Lude.Maybe [ParameterObject]) (\s a -> s {parameterObjects = a} :: ValidatePipelineDefinition)
{-# DEPRECATED vpdParameterObjects "Use generic-lens or generic-optics with 'parameterObjects' instead." #-}

-- | The parameter values used with the pipeline.
--
-- /Note:/ Consider using 'parameterValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpdParameterValues :: Lens.Lens' ValidatePipelineDefinition (Lude.Maybe [ParameterValue])
vpdParameterValues = Lens.lens (parameterValues :: ValidatePipelineDefinition -> Lude.Maybe [ParameterValue]) (\s a -> s {parameterValues = a} :: ValidatePipelineDefinition)
{-# DEPRECATED vpdParameterValues "Use generic-lens or generic-optics with 'parameterValues' instead." #-}

instance Lude.AWSRequest ValidatePipelineDefinition where
  type
    Rs ValidatePipelineDefinition =
      ValidatePipelineDefinitionResponse
  request = Req.postJSON dataPipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          ValidatePipelineDefinitionResponse'
            Lude.<$> (x Lude..?> "validationErrors" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "validationWarnings" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..:> "errored")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ValidatePipelineDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DataPipeline.ValidatePipelineDefinition" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ValidatePipelineDefinition where
  toJSON ValidatePipelineDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("pipelineObjects" Lude..= pipelineObjects),
            Lude.Just ("pipelineId" Lude..= pipelineId),
            ("parameterObjects" Lude..=) Lude.<$> parameterObjects,
            ("parameterValues" Lude..=) Lude.<$> parameterValues
          ]
      )

instance Lude.ToPath ValidatePipelineDefinition where
  toPath = Lude.const "/"

instance Lude.ToQuery ValidatePipelineDefinition where
  toQuery = Lude.const Lude.mempty

-- | Contains the output of ValidatePipelineDefinition.
--
-- /See:/ 'mkValidatePipelineDefinitionResponse' smart constructor.
data ValidatePipelineDefinitionResponse = ValidatePipelineDefinitionResponse'
  { -- | Any validation errors that were found.
    validationErrors :: Lude.Maybe [ValidationError],
    -- | Any validation warnings that were found.
    validationWarnings :: Lude.Maybe [ValidationWarning],
    -- | Indicates whether there were validation errors.
    errored :: Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ValidatePipelineDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'validationErrors' - Any validation errors that were found.
-- * 'validationWarnings' - Any validation warnings that were found.
-- * 'errored' - Indicates whether there were validation errors.
-- * 'responseStatus' - The response status code.
mkValidatePipelineDefinitionResponse ::
  -- | 'errored'
  Lude.Bool ->
  -- | 'responseStatus'
  Lude.Int ->
  ValidatePipelineDefinitionResponse
mkValidatePipelineDefinitionResponse pErrored_ pResponseStatus_ =
  ValidatePipelineDefinitionResponse'
    { validationErrors =
        Lude.Nothing,
      validationWarnings = Lude.Nothing,
      errored = pErrored_,
      responseStatus = pResponseStatus_
    }

-- | Any validation errors that were found.
--
-- /Note:/ Consider using 'validationErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpdrsValidationErrors :: Lens.Lens' ValidatePipelineDefinitionResponse (Lude.Maybe [ValidationError])
vpdrsValidationErrors = Lens.lens (validationErrors :: ValidatePipelineDefinitionResponse -> Lude.Maybe [ValidationError]) (\s a -> s {validationErrors = a} :: ValidatePipelineDefinitionResponse)
{-# DEPRECATED vpdrsValidationErrors "Use generic-lens or generic-optics with 'validationErrors' instead." #-}

-- | Any validation warnings that were found.
--
-- /Note:/ Consider using 'validationWarnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpdrsValidationWarnings :: Lens.Lens' ValidatePipelineDefinitionResponse (Lude.Maybe [ValidationWarning])
vpdrsValidationWarnings = Lens.lens (validationWarnings :: ValidatePipelineDefinitionResponse -> Lude.Maybe [ValidationWarning]) (\s a -> s {validationWarnings = a} :: ValidatePipelineDefinitionResponse)
{-# DEPRECATED vpdrsValidationWarnings "Use generic-lens or generic-optics with 'validationWarnings' instead." #-}

-- | Indicates whether there were validation errors.
--
-- /Note:/ Consider using 'errored' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpdrsErrored :: Lens.Lens' ValidatePipelineDefinitionResponse Lude.Bool
vpdrsErrored = Lens.lens (errored :: ValidatePipelineDefinitionResponse -> Lude.Bool) (\s a -> s {errored = a} :: ValidatePipelineDefinitionResponse)
{-# DEPRECATED vpdrsErrored "Use generic-lens or generic-optics with 'errored' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpdrsResponseStatus :: Lens.Lens' ValidatePipelineDefinitionResponse Lude.Int
vpdrsResponseStatus = Lens.lens (responseStatus :: ValidatePipelineDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ValidatePipelineDefinitionResponse)
{-# DEPRECATED vpdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
