{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.DeletePipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a pipeline, its pipeline definition, and its run history. AWS Data Pipeline attempts to cancel instances associated with the pipeline that are currently being processed by task runners.
--
-- Deleting a pipeline cannot be undone. You cannot query or restore a deleted pipeline. To temporarily pause a pipeline instead of deleting it, call 'SetStatus' with the status set to @PAUSE@ on individual components. Components that are paused by 'SetStatus' can be resumed.
module Network.AWS.DataPipeline.DeletePipeline
  ( -- * Creating a request
    DeletePipeline (..),
    mkDeletePipeline,

    -- ** Request lenses
    dPipelineId,

    -- * Destructuring the response
    DeletePipelineResponse (..),
    mkDeletePipelineResponse,
  )
where

import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DeletePipeline.
--
-- /See:/ 'mkDeletePipeline' smart constructor.
newtype DeletePipeline = DeletePipeline'
  { -- | The ID of the pipeline.
    pipelineId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePipeline' with the minimum fields required to make a request.
--
-- * 'pipelineId' - The ID of the pipeline.
mkDeletePipeline ::
  -- | 'pipelineId'
  Lude.Text ->
  DeletePipeline
mkDeletePipeline pPipelineId_ =
  DeletePipeline' {pipelineId = pPipelineId_}

-- | The ID of the pipeline.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPipelineId :: Lens.Lens' DeletePipeline Lude.Text
dPipelineId = Lens.lens (pipelineId :: DeletePipeline -> Lude.Text) (\s a -> s {pipelineId = a} :: DeletePipeline)
{-# DEPRECATED dPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

instance Lude.AWSRequest DeletePipeline where
  type Rs DeletePipeline = DeletePipelineResponse
  request = Req.postJSON dataPipelineService
  response = Res.receiveNull DeletePipelineResponse'

instance Lude.ToHeaders DeletePipeline where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DataPipeline.DeletePipeline" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeletePipeline where
  toJSON DeletePipeline' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("pipelineId" Lude..= pipelineId)])

instance Lude.ToPath DeletePipeline where
  toPath = Lude.const "/"

instance Lude.ToQuery DeletePipeline where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeletePipelineResponse' smart constructor.
data DeletePipelineResponse = DeletePipelineResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePipelineResponse' with the minimum fields required to make a request.
mkDeletePipelineResponse ::
  DeletePipelineResponse
mkDeletePipelineResponse = DeletePipelineResponse'
