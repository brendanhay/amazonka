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
-- Module      : Amazonka.Batch.Types.NodeRangeProperty
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.NodeRangeProperty where

import Amazonka.Batch.Types.ContainerProperties
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the properties of the node range for a
-- multi-node parallel job.
--
-- /See:/ 'newNodeRangeProperty' smart constructor.
data NodeRangeProperty = NodeRangeProperty'
  { -- | The container details for the node range.
    container :: Prelude.Maybe ContainerProperties,
    -- | The range of nodes, using node index values. A range of @0:3@ indicates
    -- nodes with index values of @0@ through @3@. If the starting range value
    -- is omitted (@:n@), then @0@ is used to start the range. If the ending
    -- range value is omitted (@n:@), then the highest possible node index is
    -- used to end the range. Your accumulative node ranges must account for
    -- all nodes (@0:n@). You can nest node ranges (for example, @0:10@ and
    -- @4:5@). In this case, the @4:5@ range properties override the @0:10@
    -- properties.
    targetNodes :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeRangeProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'container', 'nodeRangeProperty_container' - The container details for the node range.
--
-- 'targetNodes', 'nodeRangeProperty_targetNodes' - The range of nodes, using node index values. A range of @0:3@ indicates
-- nodes with index values of @0@ through @3@. If the starting range value
-- is omitted (@:n@), then @0@ is used to start the range. If the ending
-- range value is omitted (@n:@), then the highest possible node index is
-- used to end the range. Your accumulative node ranges must account for
-- all nodes (@0:n@). You can nest node ranges (for example, @0:10@ and
-- @4:5@). In this case, the @4:5@ range properties override the @0:10@
-- properties.
newNodeRangeProperty ::
  -- | 'targetNodes'
  Prelude.Text ->
  NodeRangeProperty
newNodeRangeProperty pTargetNodes_ =
  NodeRangeProperty'
    { container = Prelude.Nothing,
      targetNodes = pTargetNodes_
    }

-- | The container details for the node range.
nodeRangeProperty_container :: Lens.Lens' NodeRangeProperty (Prelude.Maybe ContainerProperties)
nodeRangeProperty_container = Lens.lens (\NodeRangeProperty' {container} -> container) (\s@NodeRangeProperty' {} a -> s {container = a} :: NodeRangeProperty)

-- | The range of nodes, using node index values. A range of @0:3@ indicates
-- nodes with index values of @0@ through @3@. If the starting range value
-- is omitted (@:n@), then @0@ is used to start the range. If the ending
-- range value is omitted (@n:@), then the highest possible node index is
-- used to end the range. Your accumulative node ranges must account for
-- all nodes (@0:n@). You can nest node ranges (for example, @0:10@ and
-- @4:5@). In this case, the @4:5@ range properties override the @0:10@
-- properties.
nodeRangeProperty_targetNodes :: Lens.Lens' NodeRangeProperty Prelude.Text
nodeRangeProperty_targetNodes = Lens.lens (\NodeRangeProperty' {targetNodes} -> targetNodes) (\s@NodeRangeProperty' {} a -> s {targetNodes = a} :: NodeRangeProperty)

instance Data.FromJSON NodeRangeProperty where
  parseJSON =
    Data.withObject
      "NodeRangeProperty"
      ( \x ->
          NodeRangeProperty'
            Prelude.<$> (x Data..:? "container")
            Prelude.<*> (x Data..: "targetNodes")
      )

instance Prelude.Hashable NodeRangeProperty where
  hashWithSalt _salt NodeRangeProperty' {..} =
    _salt `Prelude.hashWithSalt` container
      `Prelude.hashWithSalt` targetNodes

instance Prelude.NFData NodeRangeProperty where
  rnf NodeRangeProperty' {..} =
    Prelude.rnf container
      `Prelude.seq` Prelude.rnf targetNodes

instance Data.ToJSON NodeRangeProperty where
  toJSON NodeRangeProperty' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("container" Data..=) Prelude.<$> container,
            Prelude.Just ("targetNodes" Data..= targetNodes)
          ]
      )
