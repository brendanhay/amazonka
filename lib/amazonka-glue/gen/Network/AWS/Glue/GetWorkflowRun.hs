{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetWorkflowRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the metadata for a given workflow run.
module Network.AWS.Glue.GetWorkflowRun
  ( -- * Creating a request
    GetWorkflowRun (..),
    mkGetWorkflowRun,

    -- ** Request lenses
    gwrwIncludeGraph,
    gwrwName,
    gwrwRunId,

    -- * Destructuring the response
    GetWorkflowRunResponse (..),
    mkGetWorkflowRunResponse,

    -- ** Response lenses
    gwrwrsRun,
    gwrwrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetWorkflowRun' smart constructor.
data GetWorkflowRun = GetWorkflowRun'
  { includeGraph ::
      Lude.Maybe Lude.Bool,
    name :: Lude.Text,
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

-- | Creates a value of 'GetWorkflowRun' with the minimum fields required to make a request.
--
-- * 'includeGraph' - Specifies whether to include the workflow graph in response or not.
-- * 'name' - Name of the workflow being run.
-- * 'runId' - The ID of the workflow run.
mkGetWorkflowRun ::
  -- | 'name'
  Lude.Text ->
  -- | 'runId'
  Lude.Text ->
  GetWorkflowRun
mkGetWorkflowRun pName_ pRunId_ =
  GetWorkflowRun'
    { includeGraph = Lude.Nothing,
      name = pName_,
      runId = pRunId_
    }

-- | Specifies whether to include the workflow graph in response or not.
--
-- /Note:/ Consider using 'includeGraph' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrwIncludeGraph :: Lens.Lens' GetWorkflowRun (Lude.Maybe Lude.Bool)
gwrwIncludeGraph = Lens.lens (includeGraph :: GetWorkflowRun -> Lude.Maybe Lude.Bool) (\s a -> s {includeGraph = a} :: GetWorkflowRun)
{-# DEPRECATED gwrwIncludeGraph "Use generic-lens or generic-optics with 'includeGraph' instead." #-}

-- | Name of the workflow being run.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrwName :: Lens.Lens' GetWorkflowRun Lude.Text
gwrwName = Lens.lens (name :: GetWorkflowRun -> Lude.Text) (\s a -> s {name = a} :: GetWorkflowRun)
{-# DEPRECATED gwrwName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the workflow run.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrwRunId :: Lens.Lens' GetWorkflowRun Lude.Text
gwrwRunId = Lens.lens (runId :: GetWorkflowRun -> Lude.Text) (\s a -> s {runId = a} :: GetWorkflowRun)
{-# DEPRECATED gwrwRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

instance Lude.AWSRequest GetWorkflowRun where
  type Rs GetWorkflowRun = GetWorkflowRunResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetWorkflowRunResponse'
            Lude.<$> (x Lude..?> "Run") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetWorkflowRun where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetWorkflowRun" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetWorkflowRun where
  toJSON GetWorkflowRun' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IncludeGraph" Lude..=) Lude.<$> includeGraph,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("RunId" Lude..= runId)
          ]
      )

instance Lude.ToPath GetWorkflowRun where
  toPath = Lude.const "/"

instance Lude.ToQuery GetWorkflowRun where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetWorkflowRunResponse' smart constructor.
data GetWorkflowRunResponse = GetWorkflowRunResponse'
  { run ::
      Lude.Maybe WorkflowRun,
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

-- | Creates a value of 'GetWorkflowRunResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'run' - The requested workflow run metadata.
mkGetWorkflowRunResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetWorkflowRunResponse
mkGetWorkflowRunResponse pResponseStatus_ =
  GetWorkflowRunResponse'
    { run = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The requested workflow run metadata.
--
-- /Note:/ Consider using 'run' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrwrsRun :: Lens.Lens' GetWorkflowRunResponse (Lude.Maybe WorkflowRun)
gwrwrsRun = Lens.lens (run :: GetWorkflowRunResponse -> Lude.Maybe WorkflowRun) (\s a -> s {run = a} :: GetWorkflowRunResponse)
{-# DEPRECATED gwrwrsRun "Use generic-lens or generic-optics with 'run' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrwrsResponseStatus :: Lens.Lens' GetWorkflowRunResponse Lude.Int
gwrwrsResponseStatus = Lens.lens (responseStatus :: GetWorkflowRunResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetWorkflowRunResponse)
{-# DEPRECATED gwrwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
