{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.ResumeWorkflowRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restarts selected nodes of a previous partially completed workflow run and resumes the workflow run. The selected nodes and all nodes that are downstream from the selected nodes are run.
module Network.AWS.Glue.ResumeWorkflowRun
  ( -- * Creating a request
    ResumeWorkflowRun (..),
    mkResumeWorkflowRun,

    -- ** Request lenses
    rwrName,
    rwrRunId,
    rwrNodeIds,

    -- * Destructuring the response
    ResumeWorkflowRunResponse (..),
    mkResumeWorkflowRunResponse,

    -- ** Response lenses
    rwrrsNodeIds,
    rwrrsRunId,
    rwrrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkResumeWorkflowRun' smart constructor.
data ResumeWorkflowRun = ResumeWorkflowRun'
  { name :: Lude.Text,
    runId :: Lude.Text,
    nodeIds :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResumeWorkflowRun' with the minimum fields required to make a request.
--
-- * 'name' - The name of the workflow to resume.
-- * 'nodeIds' - A list of the node IDs for the nodes you want to restart. The nodes that are to be restarted must have a run attempt in the original run.
-- * 'runId' - The ID of the workflow run to resume.
mkResumeWorkflowRun ::
  -- | 'name'
  Lude.Text ->
  -- | 'runId'
  Lude.Text ->
  ResumeWorkflowRun
mkResumeWorkflowRun pName_ pRunId_ =
  ResumeWorkflowRun'
    { name = pName_,
      runId = pRunId_,
      nodeIds = Lude.mempty
    }

-- | The name of the workflow to resume.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwrName :: Lens.Lens' ResumeWorkflowRun Lude.Text
rwrName = Lens.lens (name :: ResumeWorkflowRun -> Lude.Text) (\s a -> s {name = a} :: ResumeWorkflowRun)
{-# DEPRECATED rwrName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the workflow run to resume.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwrRunId :: Lens.Lens' ResumeWorkflowRun Lude.Text
rwrRunId = Lens.lens (runId :: ResumeWorkflowRun -> Lude.Text) (\s a -> s {runId = a} :: ResumeWorkflowRun)
{-# DEPRECATED rwrRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

-- | A list of the node IDs for the nodes you want to restart. The nodes that are to be restarted must have a run attempt in the original run.
--
-- /Note:/ Consider using 'nodeIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwrNodeIds :: Lens.Lens' ResumeWorkflowRun [Lude.Text]
rwrNodeIds = Lens.lens (nodeIds :: ResumeWorkflowRun -> [Lude.Text]) (\s a -> s {nodeIds = a} :: ResumeWorkflowRun)
{-# DEPRECATED rwrNodeIds "Use generic-lens or generic-optics with 'nodeIds' instead." #-}

instance Lude.AWSRequest ResumeWorkflowRun where
  type Rs ResumeWorkflowRun = ResumeWorkflowRunResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          ResumeWorkflowRunResponse'
            Lude.<$> (x Lude..?> "NodeIds" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "RunId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ResumeWorkflowRun where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.ResumeWorkflowRun" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ResumeWorkflowRun where
  toJSON ResumeWorkflowRun' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("RunId" Lude..= runId),
            Lude.Just ("NodeIds" Lude..= nodeIds)
          ]
      )

instance Lude.ToPath ResumeWorkflowRun where
  toPath = Lude.const "/"

instance Lude.ToQuery ResumeWorkflowRun where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkResumeWorkflowRunResponse' smart constructor.
data ResumeWorkflowRunResponse = ResumeWorkflowRunResponse'
  { nodeIds ::
      Lude.Maybe [Lude.Text],
    runId :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ResumeWorkflowRunResponse' with the minimum fields required to make a request.
--
-- * 'nodeIds' - A list of the node IDs for the nodes that were actually restarted.
-- * 'responseStatus' - The response status code.
-- * 'runId' - The new ID assigned to the resumed workflow run. Each resume of a workflow run will have a new run ID.
mkResumeWorkflowRunResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ResumeWorkflowRunResponse
mkResumeWorkflowRunResponse pResponseStatus_ =
  ResumeWorkflowRunResponse'
    { nodeIds = Lude.Nothing,
      runId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of the node IDs for the nodes that were actually restarted.
--
-- /Note:/ Consider using 'nodeIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwrrsNodeIds :: Lens.Lens' ResumeWorkflowRunResponse (Lude.Maybe [Lude.Text])
rwrrsNodeIds = Lens.lens (nodeIds :: ResumeWorkflowRunResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {nodeIds = a} :: ResumeWorkflowRunResponse)
{-# DEPRECATED rwrrsNodeIds "Use generic-lens or generic-optics with 'nodeIds' instead." #-}

-- | The new ID assigned to the resumed workflow run. Each resume of a workflow run will have a new run ID.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwrrsRunId :: Lens.Lens' ResumeWorkflowRunResponse (Lude.Maybe Lude.Text)
rwrrsRunId = Lens.lens (runId :: ResumeWorkflowRunResponse -> Lude.Maybe Lude.Text) (\s a -> s {runId = a} :: ResumeWorkflowRunResponse)
{-# DEPRECATED rwrrsRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwrrsResponseStatus :: Lens.Lens' ResumeWorkflowRunResponse Lude.Int
rwrrsResponseStatus = Lens.lens (responseStatus :: ResumeWorkflowRunResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ResumeWorkflowRunResponse)
{-# DEPRECATED rwrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
