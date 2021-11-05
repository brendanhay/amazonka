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
-- Module      : Amazonka.AppMesh.Types.WeightedTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.WeightedTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a target and its relative weight. Traffic is
-- distributed across targets according to their relative weight. For
-- example, a weighted target with a relative weight of 50 receives five
-- times as much traffic as one with a relative weight of 10. The total
-- weight for all targets combined must be less than or equal to 100.
--
-- /See:/ 'newWeightedTarget' smart constructor.
data WeightedTarget = WeightedTarget'
  { -- | The virtual node to associate with the weighted target.
    virtualNode :: Prelude.Text,
    -- | The relative weight of the weighted target.
    weight :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WeightedTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualNode', 'weightedTarget_virtualNode' - The virtual node to associate with the weighted target.
--
-- 'weight', 'weightedTarget_weight' - The relative weight of the weighted target.
newWeightedTarget ::
  -- | 'virtualNode'
  Prelude.Text ->
  -- | 'weight'
  Prelude.Natural ->
  WeightedTarget
newWeightedTarget pVirtualNode_ pWeight_ =
  WeightedTarget'
    { virtualNode = pVirtualNode_,
      weight = pWeight_
    }

-- | The virtual node to associate with the weighted target.
weightedTarget_virtualNode :: Lens.Lens' WeightedTarget Prelude.Text
weightedTarget_virtualNode = Lens.lens (\WeightedTarget' {virtualNode} -> virtualNode) (\s@WeightedTarget' {} a -> s {virtualNode = a} :: WeightedTarget)

-- | The relative weight of the weighted target.
weightedTarget_weight :: Lens.Lens' WeightedTarget Prelude.Natural
weightedTarget_weight = Lens.lens (\WeightedTarget' {weight} -> weight) (\s@WeightedTarget' {} a -> s {weight = a} :: WeightedTarget)

instance Core.FromJSON WeightedTarget where
  parseJSON =
    Core.withObject
      "WeightedTarget"
      ( \x ->
          WeightedTarget'
            Prelude.<$> (x Core..: "virtualNode")
            Prelude.<*> (x Core..: "weight")
      )

instance Prelude.Hashable WeightedTarget

instance Prelude.NFData WeightedTarget

instance Core.ToJSON WeightedTarget where
  toJSON WeightedTarget' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("virtualNode" Core..= virtualNode),
            Prelude.Just ("weight" Core..= weight)
          ]
      )
