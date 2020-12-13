{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.WorkflowGraph
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.WorkflowGraph
  ( WorkflowGraph (..),

    -- * Smart constructor
    mkWorkflowGraph,

    -- * Lenses
    wgEdges,
    wgNodes,
  )
where

import Network.AWS.Glue.Types.Edge
import Network.AWS.Glue.Types.Node
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A workflow graph represents the complete workflow containing all the AWS Glue components present in the workflow and all the directed connections between them.
--
-- /See:/ 'mkWorkflowGraph' smart constructor.
data WorkflowGraph = WorkflowGraph'
  { -- | A list of all the directed connections between the nodes belonging to the workflow.
    edges :: Lude.Maybe [Edge],
    -- | A list of the the AWS Glue components belong to the workflow represented as nodes.
    nodes :: Lude.Maybe [Node]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkflowGraph' with the minimum fields required to make a request.
--
-- * 'edges' - A list of all the directed connections between the nodes belonging to the workflow.
-- * 'nodes' - A list of the the AWS Glue components belong to the workflow represented as nodes.
mkWorkflowGraph ::
  WorkflowGraph
mkWorkflowGraph =
  WorkflowGraph' {edges = Lude.Nothing, nodes = Lude.Nothing}

-- | A list of all the directed connections between the nodes belonging to the workflow.
--
-- /Note:/ Consider using 'edges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgEdges :: Lens.Lens' WorkflowGraph (Lude.Maybe [Edge])
wgEdges = Lens.lens (edges :: WorkflowGraph -> Lude.Maybe [Edge]) (\s a -> s {edges = a} :: WorkflowGraph)
{-# DEPRECATED wgEdges "Use generic-lens or generic-optics with 'edges' instead." #-}

-- | A list of the the AWS Glue components belong to the workflow represented as nodes.
--
-- /Note:/ Consider using 'nodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgNodes :: Lens.Lens' WorkflowGraph (Lude.Maybe [Node])
wgNodes = Lens.lens (nodes :: WorkflowGraph -> Lude.Maybe [Node]) (\s a -> s {nodes = a} :: WorkflowGraph)
{-# DEPRECATED wgNodes "Use generic-lens or generic-optics with 'nodes' instead." #-}

instance Lude.FromJSON WorkflowGraph where
  parseJSON =
    Lude.withObject
      "WorkflowGraph"
      ( \x ->
          WorkflowGraph'
            Lude.<$> (x Lude..:? "Edges" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Nodes" Lude..!= Lude.mempty)
      )
