{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.StartWorkflowRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a new run of the specified workflow.
module Network.AWS.Glue.StartWorkflowRun
  ( -- * Creating a request
    StartWorkflowRun (..),
    mkStartWorkflowRun,

    -- ** Request lenses
    swrfName,

    -- * Destructuring the response
    StartWorkflowRunResponse (..),
    mkStartWorkflowRunResponse,

    -- ** Response lenses
    swrrsRunId,
    swrrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartWorkflowRun' smart constructor.
newtype StartWorkflowRun = StartWorkflowRun'
  { -- | The name of the workflow to start.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartWorkflowRun' with the minimum fields required to make a request.
--
-- * 'name' - The name of the workflow to start.
mkStartWorkflowRun ::
  -- | 'name'
  Lude.Text ->
  StartWorkflowRun
mkStartWorkflowRun pName_ = StartWorkflowRun' {name = pName_}

-- | The name of the workflow to start.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swrfName :: Lens.Lens' StartWorkflowRun Lude.Text
swrfName = Lens.lens (name :: StartWorkflowRun -> Lude.Text) (\s a -> s {name = a} :: StartWorkflowRun)
{-# DEPRECATED swrfName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest StartWorkflowRun where
  type Rs StartWorkflowRun = StartWorkflowRunResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartWorkflowRunResponse'
            Lude.<$> (x Lude..?> "RunId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartWorkflowRun where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.StartWorkflowRun" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartWorkflowRun where
  toJSON StartWorkflowRun' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath StartWorkflowRun where
  toPath = Lude.const "/"

instance Lude.ToQuery StartWorkflowRun where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartWorkflowRunResponse' smart constructor.
data StartWorkflowRunResponse = StartWorkflowRunResponse'
  { -- | An Id for the new run.
    runId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartWorkflowRunResponse' with the minimum fields required to make a request.
--
-- * 'runId' - An Id for the new run.
-- * 'responseStatus' - The response status code.
mkStartWorkflowRunResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartWorkflowRunResponse
mkStartWorkflowRunResponse pResponseStatus_ =
  StartWorkflowRunResponse'
    { runId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An Id for the new run.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swrrsRunId :: Lens.Lens' StartWorkflowRunResponse (Lude.Maybe Lude.Text)
swrrsRunId = Lens.lens (runId :: StartWorkflowRunResponse -> Lude.Maybe Lude.Text) (\s a -> s {runId = a} :: StartWorkflowRunResponse)
{-# DEPRECATED swrrsRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swrrsResponseStatus :: Lens.Lens' StartWorkflowRunResponse Lude.Int
swrrsResponseStatus = Lens.lens (responseStatus :: StartWorkflowRunResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartWorkflowRunResponse)
{-# DEPRECATED swrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
