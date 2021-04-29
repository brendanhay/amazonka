{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.WorkflowGraph
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.WorkflowGraph where

import Network.AWS.Glue.Types.Edge
import Network.AWS.Glue.Types.Node
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A workflow graph represents the complete workflow containing all the AWS
-- Glue components present in the workflow and all the directed connections
-- between them.
--
-- /See:/ 'newWorkflowGraph' smart constructor.
data WorkflowGraph = WorkflowGraph'
  { -- | A list of the the AWS Glue components belong to the workflow represented
    -- as nodes.
    nodes :: Prelude.Maybe [Node],
    -- | A list of all the directed connections between the nodes belonging to
    -- the workflow.
    edges :: Prelude.Maybe [Edge]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'WorkflowGraph' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodes', 'workflowGraph_nodes' - A list of the the AWS Glue components belong to the workflow represented
-- as nodes.
--
-- 'edges', 'workflowGraph_edges' - A list of all the directed connections between the nodes belonging to
-- the workflow.
newWorkflowGraph ::
  WorkflowGraph
newWorkflowGraph =
  WorkflowGraph'
    { nodes = Prelude.Nothing,
      edges = Prelude.Nothing
    }

-- | A list of the the AWS Glue components belong to the workflow represented
-- as nodes.
workflowGraph_nodes :: Lens.Lens' WorkflowGraph (Prelude.Maybe [Node])
workflowGraph_nodes = Lens.lens (\WorkflowGraph' {nodes} -> nodes) (\s@WorkflowGraph' {} a -> s {nodes = a} :: WorkflowGraph) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of all the directed connections between the nodes belonging to
-- the workflow.
workflowGraph_edges :: Lens.Lens' WorkflowGraph (Prelude.Maybe [Edge])
workflowGraph_edges = Lens.lens (\WorkflowGraph' {edges} -> edges) (\s@WorkflowGraph' {} a -> s {edges = a} :: WorkflowGraph) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON WorkflowGraph where
  parseJSON =
    Prelude.withObject
      "WorkflowGraph"
      ( \x ->
          WorkflowGraph'
            Prelude.<$> (x Prelude..:? "Nodes" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "Edges" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable WorkflowGraph

instance Prelude.NFData WorkflowGraph
