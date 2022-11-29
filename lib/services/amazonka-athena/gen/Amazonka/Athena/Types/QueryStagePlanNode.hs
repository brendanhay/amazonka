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
-- Module      : Amazonka.Athena.Types.QueryStagePlanNode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.QueryStagePlanNode where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Stage plan information such as name, identifier, sub plans, and remote
-- sources.
--
-- /See:/ 'newQueryStagePlanNode' smart constructor.
data QueryStagePlanNode = QueryStagePlanNode'
  { -- | Name of the query stage plan that describes the operation this stage is
    -- performing as part of query execution.
    name :: Prelude.Maybe Prelude.Text,
    -- | Source plan node IDs.
    remoteSources :: Prelude.Maybe [Prelude.Text],
    -- | Stage plan information such as name, identifier, sub plans, and remote
    -- sources of child plan nodes\/
    children :: Prelude.Maybe [QueryStagePlanNode],
    -- | Information about the operation this query stage plan node is
    -- performing.
    identifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryStagePlanNode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'queryStagePlanNode_name' - Name of the query stage plan that describes the operation this stage is
-- performing as part of query execution.
--
-- 'remoteSources', 'queryStagePlanNode_remoteSources' - Source plan node IDs.
--
-- 'children', 'queryStagePlanNode_children' - Stage plan information such as name, identifier, sub plans, and remote
-- sources of child plan nodes\/
--
-- 'identifier', 'queryStagePlanNode_identifier' - Information about the operation this query stage plan node is
-- performing.
newQueryStagePlanNode ::
  QueryStagePlanNode
newQueryStagePlanNode =
  QueryStagePlanNode'
    { name = Prelude.Nothing,
      remoteSources = Prelude.Nothing,
      children = Prelude.Nothing,
      identifier = Prelude.Nothing
    }

-- | Name of the query stage plan that describes the operation this stage is
-- performing as part of query execution.
queryStagePlanNode_name :: Lens.Lens' QueryStagePlanNode (Prelude.Maybe Prelude.Text)
queryStagePlanNode_name = Lens.lens (\QueryStagePlanNode' {name} -> name) (\s@QueryStagePlanNode' {} a -> s {name = a} :: QueryStagePlanNode)

-- | Source plan node IDs.
queryStagePlanNode_remoteSources :: Lens.Lens' QueryStagePlanNode (Prelude.Maybe [Prelude.Text])
queryStagePlanNode_remoteSources = Lens.lens (\QueryStagePlanNode' {remoteSources} -> remoteSources) (\s@QueryStagePlanNode' {} a -> s {remoteSources = a} :: QueryStagePlanNode) Prelude.. Lens.mapping Lens.coerced

-- | Stage plan information such as name, identifier, sub plans, and remote
-- sources of child plan nodes\/
queryStagePlanNode_children :: Lens.Lens' QueryStagePlanNode (Prelude.Maybe [QueryStagePlanNode])
queryStagePlanNode_children = Lens.lens (\QueryStagePlanNode' {children} -> children) (\s@QueryStagePlanNode' {} a -> s {children = a} :: QueryStagePlanNode) Prelude.. Lens.mapping Lens.coerced

-- | Information about the operation this query stage plan node is
-- performing.
queryStagePlanNode_identifier :: Lens.Lens' QueryStagePlanNode (Prelude.Maybe Prelude.Text)
queryStagePlanNode_identifier = Lens.lens (\QueryStagePlanNode' {identifier} -> identifier) (\s@QueryStagePlanNode' {} a -> s {identifier = a} :: QueryStagePlanNode)

instance Core.FromJSON QueryStagePlanNode where
  parseJSON =
    Core.withObject
      "QueryStagePlanNode"
      ( \x ->
          QueryStagePlanNode'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "RemoteSources" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Children" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Identifier")
      )

instance Prelude.Hashable QueryStagePlanNode where
  hashWithSalt _salt QueryStagePlanNode' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` remoteSources
      `Prelude.hashWithSalt` children
      `Prelude.hashWithSalt` identifier

instance Prelude.NFData QueryStagePlanNode where
  rnf QueryStagePlanNode' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf remoteSources
      `Prelude.seq` Prelude.rnf children
      `Prelude.seq` Prelude.rnf identifier
