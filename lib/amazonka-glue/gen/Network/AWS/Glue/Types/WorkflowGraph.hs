{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.WorkflowGraph
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.WorkflowGraph
  ( WorkflowGraph (..)
  -- * Smart constructor
  , mkWorkflowGraph
  -- * Lenses
  , wgEdges
  , wgNodes
  ) where

import qualified Network.AWS.Glue.Types.Edge as Types
import qualified Network.AWS.Glue.Types.Node as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A workflow graph represents the complete workflow containing all the AWS Glue components present in the workflow and all the directed connections between them.
--
-- /See:/ 'mkWorkflowGraph' smart constructor.
data WorkflowGraph = WorkflowGraph'
  { edges :: Core.Maybe [Types.Edge]
    -- ^ A list of all the directed connections between the nodes belonging to the workflow.
  , nodes :: Core.Maybe [Types.Node]
    -- ^ A list of the the AWS Glue components belong to the workflow represented as nodes.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'WorkflowGraph' value with any optional fields omitted.
mkWorkflowGraph
    :: WorkflowGraph
mkWorkflowGraph
  = WorkflowGraph'{edges = Core.Nothing, nodes = Core.Nothing}

-- | A list of all the directed connections between the nodes belonging to the workflow.
--
-- /Note:/ Consider using 'edges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgEdges :: Lens.Lens' WorkflowGraph (Core.Maybe [Types.Edge])
wgEdges = Lens.field @"edges"
{-# INLINEABLE wgEdges #-}
{-# DEPRECATED edges "Use generic-lens or generic-optics with 'edges' instead"  #-}

-- | A list of the the AWS Glue components belong to the workflow represented as nodes.
--
-- /Note:/ Consider using 'nodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgNodes :: Lens.Lens' WorkflowGraph (Core.Maybe [Types.Node])
wgNodes = Lens.field @"nodes"
{-# INLINEABLE wgNodes #-}
{-# DEPRECATED nodes "Use generic-lens or generic-optics with 'nodes' instead"  #-}

instance Core.FromJSON WorkflowGraph where
        parseJSON
          = Core.withObject "WorkflowGraph" Core.$
              \ x ->
                WorkflowGraph' Core.<$>
                  (x Core..:? "Edges") Core.<*> x Core..:? "Nodes"
