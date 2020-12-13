{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.GetPipelineDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the definition of the specified pipeline. You can call @GetPipelineDefinition@ to retrieve the pipeline definition that you provided using 'PutPipelineDefinition' .
module Network.AWS.DataPipeline.GetPipelineDefinition
  ( -- * Creating a request
    GetPipelineDefinition (..),
    mkGetPipelineDefinition,

    -- ** Request lenses
    gpdPipelineId,
    gpdVersion,

    -- * Destructuring the response
    GetPipelineDefinitionResponse (..),
    mkGetPipelineDefinitionResponse,

    -- ** Response lenses
    gpdrsPipelineObjects,
    gpdrsParameterObjects,
    gpdrsParameterValues,
    gpdrsResponseStatus,
  )
where

import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for GetPipelineDefinition.
--
-- /See:/ 'mkGetPipelineDefinition' smart constructor.
data GetPipelineDefinition = GetPipelineDefinition'
  { -- | The ID of the pipeline.
    pipelineId :: Lude.Text,
    -- | The version of the pipeline definition to retrieve. Set this parameter to @latest@ (default) to use the last definition saved to the pipeline or @active@ to use the last definition that was activated.
    version :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPipelineDefinition' with the minimum fields required to make a request.
--
-- * 'pipelineId' - The ID of the pipeline.
-- * 'version' - The version of the pipeline definition to retrieve. Set this parameter to @latest@ (default) to use the last definition saved to the pipeline or @active@ to use the last definition that was activated.
mkGetPipelineDefinition ::
  -- | 'pipelineId'
  Lude.Text ->
  GetPipelineDefinition
mkGetPipelineDefinition pPipelineId_ =
  GetPipelineDefinition'
    { pipelineId = pPipelineId_,
      version = Lude.Nothing
    }

-- | The ID of the pipeline.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdPipelineId :: Lens.Lens' GetPipelineDefinition Lude.Text
gpdPipelineId = Lens.lens (pipelineId :: GetPipelineDefinition -> Lude.Text) (\s a -> s {pipelineId = a} :: GetPipelineDefinition)
{-# DEPRECATED gpdPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

-- | The version of the pipeline definition to retrieve. Set this parameter to @latest@ (default) to use the last definition saved to the pipeline or @active@ to use the last definition that was activated.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdVersion :: Lens.Lens' GetPipelineDefinition (Lude.Maybe Lude.Text)
gpdVersion = Lens.lens (version :: GetPipelineDefinition -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: GetPipelineDefinition)
{-# DEPRECATED gpdVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.AWSRequest GetPipelineDefinition where
  type Rs GetPipelineDefinition = GetPipelineDefinitionResponse
  request = Req.postJSON dataPipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetPipelineDefinitionResponse'
            Lude.<$> (x Lude..?> "pipelineObjects" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "parameterObjects" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "parameterValues" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetPipelineDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DataPipeline.GetPipelineDefinition" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetPipelineDefinition where
  toJSON GetPipelineDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("pipelineId" Lude..= pipelineId),
            ("version" Lude..=) Lude.<$> version
          ]
      )

instance Lude.ToPath GetPipelineDefinition where
  toPath = Lude.const "/"

instance Lude.ToQuery GetPipelineDefinition where
  toQuery = Lude.const Lude.mempty

-- | Contains the output of GetPipelineDefinition.
--
-- /See:/ 'mkGetPipelineDefinitionResponse' smart constructor.
data GetPipelineDefinitionResponse = GetPipelineDefinitionResponse'
  { -- | The objects defined in the pipeline.
    pipelineObjects :: Lude.Maybe [PipelineObject],
    -- | The parameter objects used in the pipeline definition.
    parameterObjects :: Lude.Maybe [ParameterObject],
    -- | The parameter values used in the pipeline definition.
    parameterValues :: Lude.Maybe [ParameterValue],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPipelineDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'pipelineObjects' - The objects defined in the pipeline.
-- * 'parameterObjects' - The parameter objects used in the pipeline definition.
-- * 'parameterValues' - The parameter values used in the pipeline definition.
-- * 'responseStatus' - The response status code.
mkGetPipelineDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetPipelineDefinitionResponse
mkGetPipelineDefinitionResponse pResponseStatus_ =
  GetPipelineDefinitionResponse'
    { pipelineObjects = Lude.Nothing,
      parameterObjects = Lude.Nothing,
      parameterValues = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The objects defined in the pipeline.
--
-- /Note:/ Consider using 'pipelineObjects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdrsPipelineObjects :: Lens.Lens' GetPipelineDefinitionResponse (Lude.Maybe [PipelineObject])
gpdrsPipelineObjects = Lens.lens (pipelineObjects :: GetPipelineDefinitionResponse -> Lude.Maybe [PipelineObject]) (\s a -> s {pipelineObjects = a} :: GetPipelineDefinitionResponse)
{-# DEPRECATED gpdrsPipelineObjects "Use generic-lens or generic-optics with 'pipelineObjects' instead." #-}

-- | The parameter objects used in the pipeline definition.
--
-- /Note:/ Consider using 'parameterObjects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdrsParameterObjects :: Lens.Lens' GetPipelineDefinitionResponse (Lude.Maybe [ParameterObject])
gpdrsParameterObjects = Lens.lens (parameterObjects :: GetPipelineDefinitionResponse -> Lude.Maybe [ParameterObject]) (\s a -> s {parameterObjects = a} :: GetPipelineDefinitionResponse)
{-# DEPRECATED gpdrsParameterObjects "Use generic-lens or generic-optics with 'parameterObjects' instead." #-}

-- | The parameter values used in the pipeline definition.
--
-- /Note:/ Consider using 'parameterValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdrsParameterValues :: Lens.Lens' GetPipelineDefinitionResponse (Lude.Maybe [ParameterValue])
gpdrsParameterValues = Lens.lens (parameterValues :: GetPipelineDefinitionResponse -> Lude.Maybe [ParameterValue]) (\s a -> s {parameterValues = a} :: GetPipelineDefinitionResponse)
{-# DEPRECATED gpdrsParameterValues "Use generic-lens or generic-optics with 'parameterValues' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdrsResponseStatus :: Lens.Lens' GetPipelineDefinitionResponse Lude.Int
gpdrsResponseStatus = Lens.lens (responseStatus :: GetPipelineDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPipelineDefinitionResponse)
{-# DEPRECATED gpdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
