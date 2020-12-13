{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    swrRunId,
    swrName,

    -- * Destructuring the response
    StopWorkflowRunResponse (..),
    mkStopWorkflowRunResponse,

    -- ** Response lenses
    swrfrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopWorkflowRun' smart constructor.
data StopWorkflowRun = StopWorkflowRun'
  { -- | The ID of the workflow run to stop.
    runId :: Lude.Text,
    -- | The name of the workflow to stop.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopWorkflowRun' with the minimum fields required to make a request.
--
-- * 'runId' - The ID of the workflow run to stop.
-- * 'name' - The name of the workflow to stop.
mkStopWorkflowRun ::
  -- | 'runId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  StopWorkflowRun
mkStopWorkflowRun pRunId_ pName_ =
  StopWorkflowRun' {runId = pRunId_, name = pName_}

-- | The ID of the workflow run to stop.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swrRunId :: Lens.Lens' StopWorkflowRun Lude.Text
swrRunId = Lens.lens (runId :: StopWorkflowRun -> Lude.Text) (\s a -> s {runId = a} :: StopWorkflowRun)
{-# DEPRECATED swrRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

-- | The name of the workflow to stop.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swrName :: Lens.Lens' StopWorkflowRun Lude.Text
swrName = Lens.lens (name :: StopWorkflowRun -> Lude.Text) (\s a -> s {name = a} :: StopWorkflowRun)
{-# DEPRECATED swrName "Use generic-lens or generic-optics with 'name' instead." #-}

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
          [ Lude.Just ("RunId" Lude..= runId),
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath StopWorkflowRun where
  toPath = Lude.const "/"

instance Lude.ToQuery StopWorkflowRun where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopWorkflowRunResponse' smart constructor.
newtype StopWorkflowRunResponse = StopWorkflowRunResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
swrfrsResponseStatus :: Lens.Lens' StopWorkflowRunResponse Lude.Int
swrfrsResponseStatus = Lens.lens (responseStatus :: StopWorkflowRunResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopWorkflowRunResponse)
{-# DEPRECATED swrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
