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
-- Module      : Network.AWS.Batch.Types.NodeOverrides
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.NodeOverrides where

import Network.AWS.Batch.Types.NodePropertyOverride
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Object representing any node overrides to a job definition that\'s used
-- in a SubmitJob API operation.
--
-- This isn\'t applicable to jobs running on Fargate resources and
-- shouldn\'t be provided; use @containerOverrides@ instead.
--
-- /See:/ 'newNodeOverrides' smart constructor.
data NodeOverrides = NodeOverrides'
  { -- | The node property overrides for the job.
    nodePropertyOverrides :: Core.Maybe [NodePropertyOverride],
    -- | The number of nodes to use with a multi-node parallel job. This value
    -- overrides the number of nodes that are specified in the job definition.
    -- To use this override:
    --
    -- -   There must be at least one node range in your job definition that
    --     has an open upper boundary (such as @:@ or @n:@).
    --
    -- -   The lower boundary of the node range specified in the job definition
    --     must be fewer than the number of nodes specified in the override.
    --
    -- -   The main node index specified in the job definition must be fewer
    --     than the number of nodes specified in the override.
    numNodes :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NodeOverrides' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodePropertyOverrides', 'nodeOverrides_nodePropertyOverrides' - The node property overrides for the job.
--
-- 'numNodes', 'nodeOverrides_numNodes' - The number of nodes to use with a multi-node parallel job. This value
-- overrides the number of nodes that are specified in the job definition.
-- To use this override:
--
-- -   There must be at least one node range in your job definition that
--     has an open upper boundary (such as @:@ or @n:@).
--
-- -   The lower boundary of the node range specified in the job definition
--     must be fewer than the number of nodes specified in the override.
--
-- -   The main node index specified in the job definition must be fewer
--     than the number of nodes specified in the override.
newNodeOverrides ::
  NodeOverrides
newNodeOverrides =
  NodeOverrides'
    { nodePropertyOverrides =
        Core.Nothing,
      numNodes = Core.Nothing
    }

-- | The node property overrides for the job.
nodeOverrides_nodePropertyOverrides :: Lens.Lens' NodeOverrides (Core.Maybe [NodePropertyOverride])
nodeOverrides_nodePropertyOverrides = Lens.lens (\NodeOverrides' {nodePropertyOverrides} -> nodePropertyOverrides) (\s@NodeOverrides' {} a -> s {nodePropertyOverrides = a} :: NodeOverrides) Core.. Lens.mapping Lens._Coerce

-- | The number of nodes to use with a multi-node parallel job. This value
-- overrides the number of nodes that are specified in the job definition.
-- To use this override:
--
-- -   There must be at least one node range in your job definition that
--     has an open upper boundary (such as @:@ or @n:@).
--
-- -   The lower boundary of the node range specified in the job definition
--     must be fewer than the number of nodes specified in the override.
--
-- -   The main node index specified in the job definition must be fewer
--     than the number of nodes specified in the override.
nodeOverrides_numNodes :: Lens.Lens' NodeOverrides (Core.Maybe Core.Int)
nodeOverrides_numNodes = Lens.lens (\NodeOverrides' {numNodes} -> numNodes) (\s@NodeOverrides' {} a -> s {numNodes = a} :: NodeOverrides)

instance Core.Hashable NodeOverrides

instance Core.NFData NodeOverrides

instance Core.ToJSON NodeOverrides where
  toJSON NodeOverrides' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nodePropertyOverrides" Core..=)
              Core.<$> nodePropertyOverrides,
            ("numNodes" Core..=) Core.<$> numNodes
          ]
      )
