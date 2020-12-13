{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.DeactivatePipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deactivates the specified running pipeline. The pipeline is set to the @DEACTIVATING@ state until the deactivation process completes.
--
-- To resume a deactivated pipeline, use 'ActivatePipeline' . By default, the pipeline resumes from the last completed execution. Optionally, you can specify the date and time to resume the pipeline.
module Network.AWS.DataPipeline.DeactivatePipeline
  ( -- * Creating a request
    DeactivatePipeline (..),
    mkDeactivatePipeline,

    -- ** Request lenses
    dpPipelineId,
    dpCancelActive,

    -- * Destructuring the response
    DeactivatePipelineResponse (..),
    mkDeactivatePipelineResponse,

    -- ** Response lenses
    dprsResponseStatus,
  )
where

import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DeactivatePipeline.
--
-- /See:/ 'mkDeactivatePipeline' smart constructor.
data DeactivatePipeline = DeactivatePipeline'
  { -- | The ID of the pipeline.
    pipelineId :: Lude.Text,
    -- | Indicates whether to cancel any running objects. The default is true, which sets the state of any running objects to @CANCELED@ . If this value is false, the pipeline is deactivated after all running objects finish.
    cancelActive :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeactivatePipeline' with the minimum fields required to make a request.
--
-- * 'pipelineId' - The ID of the pipeline.
-- * 'cancelActive' - Indicates whether to cancel any running objects. The default is true, which sets the state of any running objects to @CANCELED@ . If this value is false, the pipeline is deactivated after all running objects finish.
mkDeactivatePipeline ::
  -- | 'pipelineId'
  Lude.Text ->
  DeactivatePipeline
mkDeactivatePipeline pPipelineId_ =
  DeactivatePipeline'
    { pipelineId = pPipelineId_,
      cancelActive = Lude.Nothing
    }

-- | The ID of the pipeline.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpPipelineId :: Lens.Lens' DeactivatePipeline Lude.Text
dpPipelineId = Lens.lens (pipelineId :: DeactivatePipeline -> Lude.Text) (\s a -> s {pipelineId = a} :: DeactivatePipeline)
{-# DEPRECATED dpPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

-- | Indicates whether to cancel any running objects. The default is true, which sets the state of any running objects to @CANCELED@ . If this value is false, the pipeline is deactivated after all running objects finish.
--
-- /Note:/ Consider using 'cancelActive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpCancelActive :: Lens.Lens' DeactivatePipeline (Lude.Maybe Lude.Bool)
dpCancelActive = Lens.lens (cancelActive :: DeactivatePipeline -> Lude.Maybe Lude.Bool) (\s a -> s {cancelActive = a} :: DeactivatePipeline)
{-# DEPRECATED dpCancelActive "Use generic-lens or generic-optics with 'cancelActive' instead." #-}

instance Lude.AWSRequest DeactivatePipeline where
  type Rs DeactivatePipeline = DeactivatePipelineResponse
  request = Req.postJSON dataPipelineService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeactivatePipelineResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeactivatePipeline where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DataPipeline.DeactivatePipeline" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeactivatePipeline where
  toJSON DeactivatePipeline' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("pipelineId" Lude..= pipelineId),
            ("cancelActive" Lude..=) Lude.<$> cancelActive
          ]
      )

instance Lude.ToPath DeactivatePipeline where
  toPath = Lude.const "/"

instance Lude.ToQuery DeactivatePipeline where
  toQuery = Lude.const Lude.mempty

-- | Contains the output of DeactivatePipeline.
--
-- /See:/ 'mkDeactivatePipelineResponse' smart constructor.
newtype DeactivatePipelineResponse = DeactivatePipelineResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeactivatePipelineResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeactivatePipelineResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeactivatePipelineResponse
mkDeactivatePipelineResponse pResponseStatus_ =
  DeactivatePipelineResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsResponseStatus :: Lens.Lens' DeactivatePipelineResponse Lude.Int
dprsResponseStatus = Lens.lens (responseStatus :: DeactivatePipelineResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeactivatePipelineResponse)
{-# DEPRECATED dprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
