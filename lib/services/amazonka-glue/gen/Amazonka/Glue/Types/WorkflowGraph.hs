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
-- Module      : Amazonka.Glue.Types.WorkflowGraph
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.WorkflowGraph where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types.Edge
import Amazonka.Glue.Types.Node
import qualified Amazonka.Prelude as Prelude

-- | A workflow graph represents the complete workflow containing all the
-- Glue components present in the workflow and all the directed connections
-- between them.
--
-- /See:/ 'newWorkflowGraph' smart constructor.
data WorkflowGraph = WorkflowGraph'
  { -- | A list of all the directed connections between the nodes belonging to
    -- the workflow.
    edges :: Prelude.Maybe [Edge],
    -- | A list of the the Glue components belong to the workflow represented as
    -- nodes.
    nodes :: Prelude.Maybe [Node]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowGraph' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'edges', 'workflowGraph_edges' - A list of all the directed connections between the nodes belonging to
-- the workflow.
--
-- 'nodes', 'workflowGraph_nodes' - A list of the the Glue components belong to the workflow represented as
-- nodes.
newWorkflowGraph ::
  WorkflowGraph
newWorkflowGraph =
  WorkflowGraph'
    { edges = Prelude.Nothing,
      nodes = Prelude.Nothing
    }

-- | A list of all the directed connections between the nodes belonging to
-- the workflow.
workflowGraph_edges :: Lens.Lens' WorkflowGraph (Prelude.Maybe [Edge])
workflowGraph_edges = Lens.lens (\WorkflowGraph' {edges} -> edges) (\s@WorkflowGraph' {} a -> s {edges = a} :: WorkflowGraph) Prelude.. Lens.mapping Lens.coerced

-- | A list of the the Glue components belong to the workflow represented as
-- nodes.
workflowGraph_nodes :: Lens.Lens' WorkflowGraph (Prelude.Maybe [Node])
workflowGraph_nodes = Lens.lens (\WorkflowGraph' {nodes} -> nodes) (\s@WorkflowGraph' {} a -> s {nodes = a} :: WorkflowGraph) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON WorkflowGraph where
  parseJSON =
    Core.withObject
      "WorkflowGraph"
      ( \x ->
          WorkflowGraph'
            Prelude.<$> (x Core..:? "Edges" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Nodes" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable WorkflowGraph where
  hashWithSalt _salt WorkflowGraph' {..} =
    _salt `Prelude.hashWithSalt` edges
      `Prelude.hashWithSalt` nodes

instance Prelude.NFData WorkflowGraph where
  rnf WorkflowGraph' {..} =
    Prelude.rnf edges `Prelude.seq` Prelude.rnf nodes
