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
-- Module      : Network.AWS.Batch.Types.NodeDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.NodeDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing the details of a multi-node parallel job node.
--
-- /See:/ 'newNodeDetails' smart constructor.
data NodeDetails = NodeDetails'
  { -- | Specifies whether the current node is the main node for a multi-node
    -- parallel job.
    isMainNode :: Prelude.Maybe Prelude.Bool,
    -- | The node index for the node. Node index numbering begins at zero. This
    -- index is also available on the node with the @AWS_BATCH_JOB_NODE_INDEX@
    -- environment variable.
    nodeIndex :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'nodeIndex', 'nodeDetails_nodeIndex' - The node index for the node. Node index numbering begins at zero. This
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

-- | The node index for the node. Node index numbering begins at zero. This
-- index is also available on the node with the @AWS_BATCH_JOB_NODE_INDEX@
-- environment variable.
nodeDetails_nodeIndex :: Lens.Lens' NodeDetails (Prelude.Maybe Prelude.Int)
nodeDetails_nodeIndex = Lens.lens (\NodeDetails' {nodeIndex} -> nodeIndex) (\s@NodeDetails' {} a -> s {nodeIndex = a} :: NodeDetails)

instance Prelude.FromJSON NodeDetails where
  parseJSON =
    Prelude.withObject
      "NodeDetails"
      ( \x ->
          NodeDetails'
            Prelude.<$> (x Prelude..:? "isMainNode")
            Prelude.<*> (x Prelude..:? "nodeIndex")
      )

instance Prelude.Hashable NodeDetails

instance Prelude.NFData NodeDetails
