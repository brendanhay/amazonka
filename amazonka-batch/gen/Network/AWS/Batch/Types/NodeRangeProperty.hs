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
-- Module      : Network.AWS.Batch.Types.NodeRangeProperty
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.NodeRangeProperty where

import Network.AWS.Batch.Types.ContainerProperties
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing the properties of the node range for a multi-node
-- parallel job.
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
    -- all nodes (@0:n@). You can nest node ranges, for example @0:10@ and
    -- @4:5@, in which case the @4:5@ range properties override the @0:10@
    -- properties.
    targetNodes :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- all nodes (@0:n@). You can nest node ranges, for example @0:10@ and
-- @4:5@, in which case the @4:5@ range properties override the @0:10@
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
-- all nodes (@0:n@). You can nest node ranges, for example @0:10@ and
-- @4:5@, in which case the @4:5@ range properties override the @0:10@
-- properties.
nodeRangeProperty_targetNodes :: Lens.Lens' NodeRangeProperty Prelude.Text
nodeRangeProperty_targetNodes = Lens.lens (\NodeRangeProperty' {targetNodes} -> targetNodes) (\s@NodeRangeProperty' {} a -> s {targetNodes = a} :: NodeRangeProperty)

instance Prelude.FromJSON NodeRangeProperty where
  parseJSON =
    Prelude.withObject
      "NodeRangeProperty"
      ( \x ->
          NodeRangeProperty'
            Prelude.<$> (x Prelude..:? "container")
            Prelude.<*> (x Prelude..: "targetNodes")
      )

instance Prelude.Hashable NodeRangeProperty

instance Prelude.NFData NodeRangeProperty

instance Prelude.ToJSON NodeRangeProperty where
  toJSON NodeRangeProperty' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("container" Prelude..=) Prelude.<$> container,
            Prelude.Just ("targetNodes" Prelude..= targetNodes)
          ]
      )
