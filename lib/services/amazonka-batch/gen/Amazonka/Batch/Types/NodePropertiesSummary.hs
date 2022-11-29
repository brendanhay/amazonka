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
-- Module      : Amazonka.Batch.Types.NodePropertiesSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.NodePropertiesSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the properties of a node that\'s associated
-- with a multi-node parallel job.
--
-- /See:/ 'newNodePropertiesSummary' smart constructor.
data NodePropertiesSummary = NodePropertiesSummary'
  { -- | Specifies whether the current node is the main node for a multi-node
    -- parallel job.
    isMainNode :: Prelude.Maybe Prelude.Bool,
    -- | The node index for the node. Node index numbering begins at zero. This
    -- index is also available on the node with the @AWS_BATCH_JOB_NODE_INDEX@
    -- environment variable.
    nodeIndex :: Prelude.Maybe Prelude.Int,
    -- | The number of nodes that are associated with a multi-node parallel job.
    numNodes :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodePropertiesSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isMainNode', 'nodePropertiesSummary_isMainNode' - Specifies whether the current node is the main node for a multi-node
-- parallel job.
--
-- 'nodeIndex', 'nodePropertiesSummary_nodeIndex' - The node index for the node. Node index numbering begins at zero. This
-- index is also available on the node with the @AWS_BATCH_JOB_NODE_INDEX@
-- environment variable.
--
-- 'numNodes', 'nodePropertiesSummary_numNodes' - The number of nodes that are associated with a multi-node parallel job.
newNodePropertiesSummary ::
  NodePropertiesSummary
newNodePropertiesSummary =
  NodePropertiesSummary'
    { isMainNode =
        Prelude.Nothing,
      nodeIndex = Prelude.Nothing,
      numNodes = Prelude.Nothing
    }

-- | Specifies whether the current node is the main node for a multi-node
-- parallel job.
nodePropertiesSummary_isMainNode :: Lens.Lens' NodePropertiesSummary (Prelude.Maybe Prelude.Bool)
nodePropertiesSummary_isMainNode = Lens.lens (\NodePropertiesSummary' {isMainNode} -> isMainNode) (\s@NodePropertiesSummary' {} a -> s {isMainNode = a} :: NodePropertiesSummary)

-- | The node index for the node. Node index numbering begins at zero. This
-- index is also available on the node with the @AWS_BATCH_JOB_NODE_INDEX@
-- environment variable.
nodePropertiesSummary_nodeIndex :: Lens.Lens' NodePropertiesSummary (Prelude.Maybe Prelude.Int)
nodePropertiesSummary_nodeIndex = Lens.lens (\NodePropertiesSummary' {nodeIndex} -> nodeIndex) (\s@NodePropertiesSummary' {} a -> s {nodeIndex = a} :: NodePropertiesSummary)

-- | The number of nodes that are associated with a multi-node parallel job.
nodePropertiesSummary_numNodes :: Lens.Lens' NodePropertiesSummary (Prelude.Maybe Prelude.Int)
nodePropertiesSummary_numNodes = Lens.lens (\NodePropertiesSummary' {numNodes} -> numNodes) (\s@NodePropertiesSummary' {} a -> s {numNodes = a} :: NodePropertiesSummary)

instance Core.FromJSON NodePropertiesSummary where
  parseJSON =
    Core.withObject
      "NodePropertiesSummary"
      ( \x ->
          NodePropertiesSummary'
            Prelude.<$> (x Core..:? "isMainNode")
            Prelude.<*> (x Core..:? "nodeIndex")
            Prelude.<*> (x Core..:? "numNodes")
      )

instance Prelude.Hashable NodePropertiesSummary where
  hashWithSalt _salt NodePropertiesSummary' {..} =
    _salt `Prelude.hashWithSalt` isMainNode
      `Prelude.hashWithSalt` nodeIndex
      `Prelude.hashWithSalt` numNodes

instance Prelude.NFData NodePropertiesSummary where
  rnf NodePropertiesSummary' {..} =
    Prelude.rnf isMainNode
      `Prelude.seq` Prelude.rnf nodeIndex
      `Prelude.seq` Prelude.rnf numNodes
