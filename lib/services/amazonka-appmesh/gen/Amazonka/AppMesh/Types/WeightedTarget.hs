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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.WeightedTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a target and its relative weight. Traffic is
-- distributed across targets according to their relative weight. For
-- example, a weighted target with a relative weight of 50 receives five
-- times as much traffic as one with a relative weight of 10. The total
-- weight for all targets combined must be less than or equal to 100.
--
-- /See:/ 'newWeightedTarget' smart constructor.
data WeightedTarget = WeightedTarget'
  { -- | The targeted port of the weighted object.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The virtual node to associate with the weighted target.
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
-- 'port', 'weightedTarget_port' - The targeted port of the weighted object.
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
    { port = Prelude.Nothing,
      virtualNode = pVirtualNode_,
      weight = pWeight_
    }

-- | The targeted port of the weighted object.
weightedTarget_port :: Lens.Lens' WeightedTarget (Prelude.Maybe Prelude.Natural)
weightedTarget_port = Lens.lens (\WeightedTarget' {port} -> port) (\s@WeightedTarget' {} a -> s {port = a} :: WeightedTarget)

-- | The virtual node to associate with the weighted target.
weightedTarget_virtualNode :: Lens.Lens' WeightedTarget Prelude.Text
weightedTarget_virtualNode = Lens.lens (\WeightedTarget' {virtualNode} -> virtualNode) (\s@WeightedTarget' {} a -> s {virtualNode = a} :: WeightedTarget)

-- | The relative weight of the weighted target.
weightedTarget_weight :: Lens.Lens' WeightedTarget Prelude.Natural
weightedTarget_weight = Lens.lens (\WeightedTarget' {weight} -> weight) (\s@WeightedTarget' {} a -> s {weight = a} :: WeightedTarget)

instance Data.FromJSON WeightedTarget where
  parseJSON =
    Data.withObject
      "WeightedTarget"
      ( \x ->
          WeightedTarget'
            Prelude.<$> (x Data..:? "port")
            Prelude.<*> (x Data..: "virtualNode")
            Prelude.<*> (x Data..: "weight")
      )

instance Prelude.Hashable WeightedTarget where
  hashWithSalt _salt WeightedTarget' {..} =
    _salt
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` virtualNode
      `Prelude.hashWithSalt` weight

instance Prelude.NFData WeightedTarget where
  rnf WeightedTarget' {..} =
    Prelude.rnf port
      `Prelude.seq` Prelude.rnf virtualNode
      `Prelude.seq` Prelude.rnf weight

instance Data.ToJSON WeightedTarget where
  toJSON WeightedTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("port" Data..=) Prelude.<$> port,
            Prelude.Just ("virtualNode" Data..= virtualNode),
            Prelude.Just ("weight" Data..= weight)
          ]
      )
