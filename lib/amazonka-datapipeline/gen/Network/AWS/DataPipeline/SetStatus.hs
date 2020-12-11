{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.SetStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests that the status of the specified physical or logical pipeline objects be updated in the specified pipeline. This update might not occur immediately, but is eventually consistent. The status that can be set depends on the type of object (for example, DataNode or Activity). You cannot perform this operation on @FINISHED@ pipelines and attempting to do so returns @InvalidRequestException@ .
module Network.AWS.DataPipeline.SetStatus
  ( -- * Creating a request
    SetStatus (..),
    mkSetStatus,

    -- ** Request lenses
    ssPipelineId,
    ssObjectIds,
    ssStatus,

    -- * Destructuring the response
    SetStatusResponse (..),
    mkSetStatusResponse,
  )
where

import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for SetStatus.
--
-- /See:/ 'mkSetStatus' smart constructor.
data SetStatus = SetStatus'
  { pipelineId :: Lude.Text,
    objectIds :: [Lude.Text],
    status :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetStatus' with the minimum fields required to make a request.
--
-- * 'objectIds' - The IDs of the objects. The corresponding objects can be either physical or components, but not a mix of both types.
-- * 'pipelineId' - The ID of the pipeline that contains the objects.
-- * 'status' - The status to be set on all the objects specified in @objectIds@ . For components, use @PAUSE@ or @RESUME@ . For instances, use @TRY_CANCEL@ , @RERUN@ , or @MARK_FINISHED@ .
mkSetStatus ::
  -- | 'pipelineId'
  Lude.Text ->
  -- | 'status'
  Lude.Text ->
  SetStatus
mkSetStatus pPipelineId_ pStatus_ =
  SetStatus'
    { pipelineId = pPipelineId_,
      objectIds = Lude.mempty,
      status = pStatus_
    }

-- | The ID of the pipeline that contains the objects.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssPipelineId :: Lens.Lens' SetStatus Lude.Text
ssPipelineId = Lens.lens (pipelineId :: SetStatus -> Lude.Text) (\s a -> s {pipelineId = a} :: SetStatus)
{-# DEPRECATED ssPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

-- | The IDs of the objects. The corresponding objects can be either physical or components, but not a mix of both types.
--
-- /Note:/ Consider using 'objectIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssObjectIds :: Lens.Lens' SetStatus [Lude.Text]
ssObjectIds = Lens.lens (objectIds :: SetStatus -> [Lude.Text]) (\s a -> s {objectIds = a} :: SetStatus)
{-# DEPRECATED ssObjectIds "Use generic-lens or generic-optics with 'objectIds' instead." #-}

-- | The status to be set on all the objects specified in @objectIds@ . For components, use @PAUSE@ or @RESUME@ . For instances, use @TRY_CANCEL@ , @RERUN@ , or @MARK_FINISHED@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStatus :: Lens.Lens' SetStatus Lude.Text
ssStatus = Lens.lens (status :: SetStatus -> Lude.Text) (\s a -> s {status = a} :: SetStatus)
{-# DEPRECATED ssStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.AWSRequest SetStatus where
  type Rs SetStatus = SetStatusResponse
  request = Req.postJSON dataPipelineService
  response = Res.receiveNull SetStatusResponse'

instance Lude.ToHeaders SetStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DataPipeline.SetStatus" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SetStatus where
  toJSON SetStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("pipelineId" Lude..= pipelineId),
            Lude.Just ("objectIds" Lude..= objectIds),
            Lude.Just ("status" Lude..= status)
          ]
      )

instance Lude.ToPath SetStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery SetStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSetStatusResponse' smart constructor.
data SetStatusResponse = SetStatusResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetStatusResponse' with the minimum fields required to make a request.
mkSetStatusResponse ::
  SetStatusResponse
mkSetStatusResponse = SetStatusResponse'
