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
-- Module      : Amazonka.Batch.Types.NodeDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.NodeDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the details of a multi-node parallel job node.
--
-- /See:/ 'newNodeDetails' smart constructor.
data NodeDetails = NodeDetails'
  { -- | Specifies whether the current node is the main node for a multi-node
    -- parallel job.
    isMainNode :: Prelude.Maybe Prelude.Bool,
    -- | The node index for the node. Node index numbering starts at zero. This
    -- index is also available on the node with the @AWS_BATCH_JOB_NODE_INDEX@
    -- environment variable.
    nodeIndex :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isMainNode', 'nodeDetails_isMainNode' - Specifies whether the current node is the main node for a multi-node
-- parallel job.
--
-- 'nodeIndex', 'nodeDetails_nodeIndex' - The node index for the node. Node index numbering starts at zero. This
-- index is also available on the node with the @AWS_BATCH_JOB_NODE_INDEX@
-- environment variable.
newNodeDetails ::
  NodeDetails
newNodeDetails =
  NodeDetails'
    { isMainNode = Prelude.Nothing,
      nodeIndex = Prelude.Nothing
    }

-- | Specifies whether the current node is the main node for a multi-node
-- parallel job.
nodeDetails_isMainNode :: Lens.Lens' NodeDetails (Prelude.Maybe Prelude.Bool)
nodeDetails_isMainNode = Lens.lens (\NodeDetails' {isMainNode} -> isMainNode) (\s@NodeDetails' {} a -> s {isMainNode = a} :: NodeDetails)

-- | The node index for the node. Node index numbering starts at zero. This
-- index is also available on the node with the @AWS_BATCH_JOB_NODE_INDEX@
-- environment variable.
nodeDetails_nodeIndex :: Lens.Lens' NodeDetails (Prelude.Maybe Prelude.Int)
nodeDetails_nodeIndex = Lens.lens (\NodeDetails' {nodeIndex} -> nodeIndex) (\s@NodeDetails' {} a -> s {nodeIndex = a} :: NodeDetails)

instance Data.FromJSON NodeDetails where
  parseJSON =
    Data.withObject
      "NodeDetails"
      ( \x ->
          NodeDetails'
            Prelude.<$> (x Data..:? "isMainNode")
            Prelude.<*> (x Data..:? "nodeIndex")
      )

instance Prelude.Hashable NodeDetails where
  hashWithSalt _salt NodeDetails' {..} =
    _salt
      `Prelude.hashWithSalt` isMainNode
      `Prelude.hashWithSalt` nodeIndex

instance Prelude.NFData NodeDetails where
  rnf NodeDetails' {..} =
    Prelude.rnf isMainNode
      `Prelude.seq` Prelude.rnf nodeIndex
