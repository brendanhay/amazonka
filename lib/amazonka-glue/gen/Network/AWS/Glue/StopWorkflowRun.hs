{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.StopWorkflowRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the execution of the specified workflow run.
module Network.AWS.Glue.StopWorkflowRun
  ( -- * Creating a request
    StopWorkflowRun (..),
    mkStopWorkflowRun,

    -- ** Request lenses
    swrwName,
    swrwRunId,

    -- * Destructuring the response
    StopWorkflowRunResponse (..),
    mkStopWorkflowRunResponse,

    -- ** Response lenses
    swrwrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopWorkflowRun' smart constructor.
data StopWorkflowRun = StopWorkflowRun'
  { name :: Lude.Text,
    runId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopWorkflowRun' with the minimum fields required to make a request.
--
-- * 'name' - The name of the workflow to stop.
-- * 'runId' - The ID of the workflow run to stop.
mkStopWorkflowRun ::
  -- | 'name'
  Lude.Text ->
  -- | 'runId'
  Lude.Text ->
  StopWorkflowRun
mkStopWorkflowRun pName_ pRunId_ =
  StopWorkflowRun' {name = pName_, runId = pRunId_}

-- | The name of the workflow to stop.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swrwName :: Lens.Lens' StopWorkflowRun Lude.Text
swrwName = Lens.lens (name :: StopWorkflowRun -> Lude.Text) (\s a -> s {name = a} :: StopWorkflowRun)
{-# DEPRECATED swrwName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the workflow run to stop.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swrwRunId :: Lens.Lens' StopWorkflowRun Lude.Text
swrwRunId = Lens.lens (runId :: StopWorkflowRun -> Lude.Text) (\s a -> s {runId = a} :: StopWorkflowRun)
{-# DEPRECATED swrwRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

instance Lude.AWSRequest StopWorkflowRun where
  type Rs StopWorkflowRun = StopWorkflowRunResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StopWorkflowRunResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopWorkflowRun where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.StopWorkflowRun" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopWorkflowRun where
  toJSON StopWorkflowRun' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("RunId" Lude..= runId)
          ]
      )

instance Lude.ToPath StopWorkflowRun where
  toPath = Lude.const "/"

instance Lude.ToQuery StopWorkflowRun where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopWorkflowRunResponse' smart constructor.
newtype StopWorkflowRunResponse = StopWorkflowRunResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopWorkflowRunResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStopWorkflowRunResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopWorkflowRunResponse
mkStopWorkflowRunResponse pResponseStatus_ =
  StopWorkflowRunResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swrwrsResponseStatus :: Lens.Lens' StopWorkflowRunResponse Lude.Int
swrwrsResponseStatus = Lens.lens (responseStatus :: StopWorkflowRunResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopWorkflowRunResponse)
{-# DEPRECATED swrwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
