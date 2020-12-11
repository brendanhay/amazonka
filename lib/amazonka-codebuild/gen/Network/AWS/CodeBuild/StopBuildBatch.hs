{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.StopBuildBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running batch build.
module Network.AWS.CodeBuild.StopBuildBatch
  ( -- * Creating a request
    StopBuildBatch (..),
    mkStopBuildBatch,

    -- ** Request lenses
    sbbId,

    -- * Destructuring the response
    StopBuildBatchResponse (..),
    mkStopBuildBatchResponse,

    -- ** Response lenses
    sbbrsBuildBatch,
    sbbrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopBuildBatch' smart constructor.
newtype StopBuildBatch = StopBuildBatch' {id :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopBuildBatch' with the minimum fields required to make a request.
--
-- * 'id' - The identifier of the batch build to stop.
mkStopBuildBatch ::
  -- | 'id'
  Lude.Text ->
  StopBuildBatch
mkStopBuildBatch pId_ = StopBuildBatch' {id = pId_}

-- | The identifier of the batch build to stop.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbId :: Lens.Lens' StopBuildBatch Lude.Text
sbbId = Lens.lens (id :: StopBuildBatch -> Lude.Text) (\s a -> s {id = a} :: StopBuildBatch)
{-# DEPRECATED sbbId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest StopBuildBatch where
  type Rs StopBuildBatch = StopBuildBatchResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopBuildBatchResponse'
            Lude.<$> (x Lude..?> "buildBatch") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopBuildBatch where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.StopBuildBatch" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopBuildBatch where
  toJSON StopBuildBatch' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("id" Lude..= id)])

instance Lude.ToPath StopBuildBatch where
  toPath = Lude.const "/"

instance Lude.ToQuery StopBuildBatch where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopBuildBatchResponse' smart constructor.
data StopBuildBatchResponse = StopBuildBatchResponse'
  { buildBatch ::
      Lude.Maybe BuildBatch,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopBuildBatchResponse' with the minimum fields required to make a request.
--
-- * 'buildBatch' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkStopBuildBatchResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopBuildBatchResponse
mkStopBuildBatchResponse pResponseStatus_ =
  StopBuildBatchResponse'
    { buildBatch = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'buildBatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbrsBuildBatch :: Lens.Lens' StopBuildBatchResponse (Lude.Maybe BuildBatch)
sbbrsBuildBatch = Lens.lens (buildBatch :: StopBuildBatchResponse -> Lude.Maybe BuildBatch) (\s a -> s {buildBatch = a} :: StopBuildBatchResponse)
{-# DEPRECATED sbbrsBuildBatch "Use generic-lens or generic-optics with 'buildBatch' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbrsResponseStatus :: Lens.Lens' StopBuildBatchResponse Lude.Int
sbbrsResponseStatus = Lens.lens (responseStatus :: StopBuildBatchResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopBuildBatchResponse)
{-# DEPRECATED sbbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
