{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetWorkflowRuns
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata for all runs of a given workflow.
module Network.AWS.Glue.GetWorkflowRuns
  ( -- * Creating a request
    GetWorkflowRuns (..),
    mkGetWorkflowRuns,

    -- ** Request lenses
    gwrIncludeGraph,
    gwrNextToken,
    gwrMaxResults,
    gwrName,

    -- * Destructuring the response
    GetWorkflowRunsResponse (..),
    mkGetWorkflowRunsResponse,

    -- ** Response lenses
    gwrrsRuns,
    gwrrsNextToken,
    gwrrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetWorkflowRuns' smart constructor.
data GetWorkflowRuns = GetWorkflowRuns'
  { includeGraph ::
      Lude.Maybe Lude.Bool,
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetWorkflowRuns' with the minimum fields required to make a request.
--
-- * 'includeGraph' - Specifies whether to include the workflow graph in response or not.
-- * 'maxResults' - The maximum number of workflow runs to be included in the response.
-- * 'name' - Name of the workflow whose metadata of runs should be returned.
-- * 'nextToken' - The maximum size of the response.
mkGetWorkflowRuns ::
  -- | 'name'
  Lude.Text ->
  GetWorkflowRuns
mkGetWorkflowRuns pName_ =
  GetWorkflowRuns'
    { includeGraph = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      name = pName_
    }

-- | Specifies whether to include the workflow graph in response or not.
--
-- /Note:/ Consider using 'includeGraph' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrIncludeGraph :: Lens.Lens' GetWorkflowRuns (Lude.Maybe Lude.Bool)
gwrIncludeGraph = Lens.lens (includeGraph :: GetWorkflowRuns -> Lude.Maybe Lude.Bool) (\s a -> s {includeGraph = a} :: GetWorkflowRuns)
{-# DEPRECATED gwrIncludeGraph "Use generic-lens or generic-optics with 'includeGraph' instead." #-}

-- | The maximum size of the response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrNextToken :: Lens.Lens' GetWorkflowRuns (Lude.Maybe Lude.Text)
gwrNextToken = Lens.lens (nextToken :: GetWorkflowRuns -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetWorkflowRuns)
{-# DEPRECATED gwrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of workflow runs to be included in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrMaxResults :: Lens.Lens' GetWorkflowRuns (Lude.Maybe Lude.Natural)
gwrMaxResults = Lens.lens (maxResults :: GetWorkflowRuns -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetWorkflowRuns)
{-# DEPRECATED gwrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Name of the workflow whose metadata of runs should be returned.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrName :: Lens.Lens' GetWorkflowRuns Lude.Text
gwrName = Lens.lens (name :: GetWorkflowRuns -> Lude.Text) (\s a -> s {name = a} :: GetWorkflowRuns)
{-# DEPRECATED gwrName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest GetWorkflowRuns where
  type Rs GetWorkflowRuns = GetWorkflowRunsResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetWorkflowRunsResponse'
            Lude.<$> (x Lude..?> "Runs")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetWorkflowRuns where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetWorkflowRuns" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetWorkflowRuns where
  toJSON GetWorkflowRuns' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IncludeGraph" Lude..=) Lude.<$> includeGraph,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath GetWorkflowRuns where
  toPath = Lude.const "/"

instance Lude.ToQuery GetWorkflowRuns where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetWorkflowRunsResponse' smart constructor.
data GetWorkflowRunsResponse = GetWorkflowRunsResponse'
  { runs ::
      Lude.Maybe (Lude.NonEmpty WorkflowRun),
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetWorkflowRunsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A continuation token, if not all requested workflow runs have been returned.
-- * 'responseStatus' - The response status code.
-- * 'runs' - A list of workflow run metadata objects.
mkGetWorkflowRunsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetWorkflowRunsResponse
mkGetWorkflowRunsResponse pResponseStatus_ =
  GetWorkflowRunsResponse'
    { runs = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of workflow run metadata objects.
--
-- /Note:/ Consider using 'runs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrrsRuns :: Lens.Lens' GetWorkflowRunsResponse (Lude.Maybe (Lude.NonEmpty WorkflowRun))
gwrrsRuns = Lens.lens (runs :: GetWorkflowRunsResponse -> Lude.Maybe (Lude.NonEmpty WorkflowRun)) (\s a -> s {runs = a} :: GetWorkflowRunsResponse)
{-# DEPRECATED gwrrsRuns "Use generic-lens or generic-optics with 'runs' instead." #-}

-- | A continuation token, if not all requested workflow runs have been returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrrsNextToken :: Lens.Lens' GetWorkflowRunsResponse (Lude.Maybe Lude.Text)
gwrrsNextToken = Lens.lens (nextToken :: GetWorkflowRunsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetWorkflowRunsResponse)
{-# DEPRECATED gwrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrrsResponseStatus :: Lens.Lens' GetWorkflowRunsResponse Lude.Int
gwrrsResponseStatus = Lens.lens (responseStatus :: GetWorkflowRunsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetWorkflowRunsResponse)
{-# DEPRECATED gwrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
