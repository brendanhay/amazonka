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
-- Module      : Amazonka.Batch.Types.NodeOverrides
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.NodeOverrides where

import Amazonka.Batch.Types.NodePropertyOverride
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents any node overrides to a job definition that\'s
-- used in a SubmitJob API operation.
--
-- This parameter isn\'t applicable to jobs that are running on Fargate
-- resources. Don\'t provide it for these jobs. Rather, use
-- @containerOverrides@ instead.
--
-- /See:/ 'newNodeOverrides' smart constructor.
data NodeOverrides = NodeOverrides'
  { -- | The number of nodes to use with a multi-node parallel job. This value
    -- overrides the number of nodes that are specified in the job definition.
    -- To use this override, you must meet the following conditions:
    --
    -- -   There must be at least one node range in your job definition that
    --     has an open upper boundary, such as @:@ or @n:@.
    --
    -- -   The lower boundary of the node range that\'s specified in the job
    --     definition must be fewer than the number of nodes specified in the
    --     override.
    --
    -- -   The main node index that\'s specified in the job definition must be
    --     fewer than the number of nodes specified in the override.
    numNodes :: Prelude.Maybe Prelude.Int,
    -- | The node property overrides for the job.
    nodePropertyOverrides :: Prelude.Maybe [NodePropertyOverride]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeOverrides' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numNodes', 'nodeOverrides_numNodes' - The number of nodes to use with a multi-node parallel job. This value
-- overrides the number of nodes that are specified in the job definition.
-- To use this override, you must meet the following conditions:
--
-- -   There must be at least one node range in your job definition that
--     has an open upper boundary, such as @:@ or @n:@.
--
-- -   The lower boundary of the node range that\'s specified in the job
--     definition must be fewer than the number of nodes specified in the
--     override.
--
-- -   The main node index that\'s specified in the job definition must be
--     fewer than the number of nodes specified in the override.
--
-- 'nodePropertyOverrides', 'nodeOverrides_nodePropertyOverrides' - The node property overrides for the job.
newNodeOverrides ::
  NodeOverrides
newNodeOverrides =
  NodeOverrides'
    { numNodes = Prelude.Nothing,
      nodePropertyOverrides = Prelude.Nothing
    }

-- | The number of nodes to use with a multi-node parallel job. This value
-- overrides the number of nodes that are specified in the job definition.
-- To use this override, you must meet the following conditions:
--
-- -   There must be at least one node range in your job definition that
--     has an open upper boundary, such as @:@ or @n:@.
--
-- -   The lower boundary of the node range that\'s specified in the job
--     definition must be fewer than the number of nodes specified in the
--     override.
--
-- -   The main node index that\'s specified in the job definition must be
--     fewer than the number of nodes specified in the override.
nodeOverrides_numNodes :: Lens.Lens' NodeOverrides (Prelude.Maybe Prelude.Int)
nodeOverrides_numNodes = Lens.lens (\NodeOverrides' {numNodes} -> numNodes) (\s@NodeOverrides' {} a -> s {numNodes = a} :: NodeOverrides)

-- | The node property overrides for the job.
nodeOverrides_nodePropertyOverrides :: Lens.Lens' NodeOverrides (Prelude.Maybe [NodePropertyOverride])
nodeOverrides_nodePropertyOverrides = Lens.lens (\NodeOverrides' {nodePropertyOverrides} -> nodePropertyOverrides) (\s@NodeOverrides' {} a -> s {nodePropertyOverrides = a} :: NodeOverrides) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable NodeOverrides where
  hashWithSalt _salt NodeOverrides' {..} =
    _salt `Prelude.hashWithSalt` numNodes
      `Prelude.hashWithSalt` nodePropertyOverrides

instance Prelude.NFData NodeOverrides where
  rnf NodeOverrides' {..} =
    Prelude.rnf numNodes
      `Prelude.seq` Prelude.rnf nodePropertyOverrides

instance Core.ToJSON NodeOverrides where
  toJSON NodeOverrides' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("numNodes" Core..=) Prelude.<$> numNodes,
            ("nodePropertyOverrides" Core..=)
              Prelude.<$> nodePropertyOverrides
          ]
      )
