{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    gdgrrsDagEdges,
    gdgrrsDagNodes,
    gdgrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDataflowGraph' smart constructor.
newtype GetDataflowGraph = GetDataflowGraph'
  { -- | The Python script to transform.
    pythonScript :: Core.Maybe Types.PythonScript
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDataflowGraph' value with any optional fields omitted.
mkGetDataflowGraph ::
  GetDataflowGraph
mkGetDataflowGraph = GetDataflowGraph' {pythonScript = Core.Nothing}

-- | The Python script to transform.
--
-- /Note:/ Consider using 'pythonScript' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdgPythonScript :: Lens.Lens' GetDataflowGraph (Core.Maybe Types.PythonScript)
gdgPythonScript = Lens.field @"pythonScript"
{-# DEPRECATED gdgPythonScript "Use generic-lens or generic-optics with 'pythonScript' instead." #-}

instance Core.FromJSON GetDataflowGraph where
  toJSON GetDataflowGraph {..} =
    Core.object
      (Core.catMaybes [("PythonScript" Core..=) Core.<$> pythonScript])

instance Core.AWSRequest GetDataflowGraph where
  type Rs GetDataflowGraph = GetDataflowGraphResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.GetDataflowGraph")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataflowGraphResponse'
            Core.<$> (x Core..:? "DagEdges")
            Core.<*> (x Core..:? "DagNodes")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetDataflowGraphResponse' smart constructor.
data GetDataflowGraphResponse = GetDataflowGraphResponse'
  { -- | A list of the edges in the resulting DAG.
    dagEdges :: Core.Maybe [Types.CodeGenEdge],
    -- | A list of the nodes in the resulting DAG.
    dagNodes :: Core.Maybe [Types.CodeGenNode],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDataflowGraphResponse' value with any optional fields omitted.
mkGetDataflowGraphResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDataflowGraphResponse
mkGetDataflowGraphResponse responseStatus =
  GetDataflowGraphResponse'
    { dagEdges = Core.Nothing,
      dagNodes = Core.Nothing,
      responseStatus
    }

-- | A list of the edges in the resulting DAG.
--
-- /Note:/ Consider using 'dagEdges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdgrrsDagEdges :: Lens.Lens' GetDataflowGraphResponse (Core.Maybe [Types.CodeGenEdge])
gdgrrsDagEdges = Lens.field @"dagEdges"
{-# DEPRECATED gdgrrsDagEdges "Use generic-lens or generic-optics with 'dagEdges' instead." #-}

-- | A list of the nodes in the resulting DAG.
--
-- /Note:/ Consider using 'dagNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdgrrsDagNodes :: Lens.Lens' GetDataflowGraphResponse (Core.Maybe [Types.CodeGenNode])
gdgrrsDagNodes = Lens.field @"dagNodes"
{-# DEPRECATED gdgrrsDagNodes "Use generic-lens or generic-optics with 'dagNodes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdgrrsResponseStatus :: Lens.Lens' GetDataflowGraphResponse Core.Int
gdgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
