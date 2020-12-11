{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetDataflowGraph
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Transforms a Python script into a directed acyclic graph (DAG).
module Network.AWS.Glue.GetDataflowGraph
  ( -- * Creating a request
    GetDataflowGraph (..),
    mkGetDataflowGraph,

    -- ** Request lenses
    gdgPythonScript,

    -- * Destructuring the response
    GetDataflowGraphResponse (..),
    mkGetDataflowGraphResponse,

    -- ** Response lenses
    gdgrsDagEdges,
    gdgrsDagNodes,
    gdgrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDataflowGraph' smart constructor.
newtype GetDataflowGraph = GetDataflowGraph'
  { pythonScript ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDataflowGraph' with the minimum fields required to make a request.
--
-- * 'pythonScript' - The Python script to transform.
mkGetDataflowGraph ::
  GetDataflowGraph
mkGetDataflowGraph = GetDataflowGraph' {pythonScript = Lude.Nothing}

-- | The Python script to transform.
--
-- /Note:/ Consider using 'pythonScript' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdgPythonScript :: Lens.Lens' GetDataflowGraph (Lude.Maybe Lude.Text)
gdgPythonScript = Lens.lens (pythonScript :: GetDataflowGraph -> Lude.Maybe Lude.Text) (\s a -> s {pythonScript = a} :: GetDataflowGraph)
{-# DEPRECATED gdgPythonScript "Use generic-lens or generic-optics with 'pythonScript' instead." #-}

instance Lude.AWSRequest GetDataflowGraph where
  type Rs GetDataflowGraph = GetDataflowGraphResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDataflowGraphResponse'
            Lude.<$> (x Lude..?> "DagEdges" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "DagNodes" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDataflowGraph where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetDataflowGraph" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDataflowGraph where
  toJSON GetDataflowGraph' {..} =
    Lude.object
      (Lude.catMaybes [("PythonScript" Lude..=) Lude.<$> pythonScript])

instance Lude.ToPath GetDataflowGraph where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDataflowGraph where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDataflowGraphResponse' smart constructor.
data GetDataflowGraphResponse = GetDataflowGraphResponse'
  { dagEdges ::
      Lude.Maybe [CodeGenEdge],
    dagNodes :: Lude.Maybe [CodeGenNode],
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

-- | Creates a value of 'GetDataflowGraphResponse' with the minimum fields required to make a request.
--
-- * 'dagEdges' - A list of the edges in the resulting DAG.
-- * 'dagNodes' - A list of the nodes in the resulting DAG.
-- * 'responseStatus' - The response status code.
mkGetDataflowGraphResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDataflowGraphResponse
mkGetDataflowGraphResponse pResponseStatus_ =
  GetDataflowGraphResponse'
    { dagEdges = Lude.Nothing,
      dagNodes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of the edges in the resulting DAG.
--
-- /Note:/ Consider using 'dagEdges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdgrsDagEdges :: Lens.Lens' GetDataflowGraphResponse (Lude.Maybe [CodeGenEdge])
gdgrsDagEdges = Lens.lens (dagEdges :: GetDataflowGraphResponse -> Lude.Maybe [CodeGenEdge]) (\s a -> s {dagEdges = a} :: GetDataflowGraphResponse)
{-# DEPRECATED gdgrsDagEdges "Use generic-lens or generic-optics with 'dagEdges' instead." #-}

-- | A list of the nodes in the resulting DAG.
--
-- /Note:/ Consider using 'dagNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdgrsDagNodes :: Lens.Lens' GetDataflowGraphResponse (Lude.Maybe [CodeGenNode])
gdgrsDagNodes = Lens.lens (dagNodes :: GetDataflowGraphResponse -> Lude.Maybe [CodeGenNode]) (\s a -> s {dagNodes = a} :: GetDataflowGraphResponse)
{-# DEPRECATED gdgrsDagNodes "Use generic-lens or generic-optics with 'dagNodes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdgrsResponseStatus :: Lens.Lens' GetDataflowGraphResponse Lude.Int
gdgrsResponseStatus = Lens.lens (responseStatus :: GetDataflowGraphResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDataflowGraphResponse)
{-# DEPRECATED gdgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
