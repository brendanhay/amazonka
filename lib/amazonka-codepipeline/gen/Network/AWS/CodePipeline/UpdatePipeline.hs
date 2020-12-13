{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.UpdatePipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified pipeline with edits or changes to its structure. Use a JSON file with the pipeline structure and @UpdatePipeline@ to provide the full structure of the pipeline. Updating the pipeline increases the version number of the pipeline by 1.
module Network.AWS.CodePipeline.UpdatePipeline
  ( -- * Creating a request
    UpdatePipeline (..),
    mkUpdatePipeline,

    -- ** Request lenses
    upPipeline,

    -- * Destructuring the response
    UpdatePipelineResponse (..),
    mkUpdatePipelineResponse,

    -- ** Response lenses
    uprsPipeline,
    uprsResponseStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of an @UpdatePipeline@ action.
--
-- /See:/ 'mkUpdatePipeline' smart constructor.
newtype UpdatePipeline = UpdatePipeline'
  { -- | The name of the pipeline to be updated.
    pipeline :: PipelineDeclaration
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePipeline' with the minimum fields required to make a request.
--
-- * 'pipeline' - The name of the pipeline to be updated.
mkUpdatePipeline ::
  -- | 'pipeline'
  PipelineDeclaration ->
  UpdatePipeline
mkUpdatePipeline pPipeline_ =
  UpdatePipeline' {pipeline = pPipeline_}

-- | The name of the pipeline to be updated.
--
-- /Note:/ Consider using 'pipeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upPipeline :: Lens.Lens' UpdatePipeline PipelineDeclaration
upPipeline = Lens.lens (pipeline :: UpdatePipeline -> PipelineDeclaration) (\s a -> s {pipeline = a} :: UpdatePipeline)
{-# DEPRECATED upPipeline "Use generic-lens or generic-optics with 'pipeline' instead." #-}

instance Lude.AWSRequest UpdatePipeline where
  type Rs UpdatePipeline = UpdatePipelineResponse
  request = Req.postJSON codePipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdatePipelineResponse'
            Lude.<$> (x Lude..?> "pipeline") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdatePipeline where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodePipeline_20150709.UpdatePipeline" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdatePipeline where
  toJSON UpdatePipeline' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("pipeline" Lude..= pipeline)])

instance Lude.ToPath UpdatePipeline where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdatePipeline where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of an @UpdatePipeline@ action.
--
-- /See:/ 'mkUpdatePipelineResponse' smart constructor.
data UpdatePipelineResponse = UpdatePipelineResponse'
  { -- | The structure of the updated pipeline.
    pipeline :: Lude.Maybe PipelineDeclaration,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePipelineResponse' with the minimum fields required to make a request.
--
-- * 'pipeline' - The structure of the updated pipeline.
-- * 'responseStatus' - The response status code.
mkUpdatePipelineResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdatePipelineResponse
mkUpdatePipelineResponse pResponseStatus_ =
  UpdatePipelineResponse'
    { pipeline = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The structure of the updated pipeline.
--
-- /Note:/ Consider using 'pipeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsPipeline :: Lens.Lens' UpdatePipelineResponse (Lude.Maybe PipelineDeclaration)
uprsPipeline = Lens.lens (pipeline :: UpdatePipelineResponse -> Lude.Maybe PipelineDeclaration) (\s a -> s {pipeline = a} :: UpdatePipelineResponse)
{-# DEPRECATED uprsPipeline "Use generic-lens or generic-optics with 'pipeline' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsResponseStatus :: Lens.Lens' UpdatePipelineResponse Lude.Int
uprsResponseStatus = Lens.lens (responseStatus :: UpdatePipelineResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdatePipelineResponse)
{-# DEPRECATED uprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
