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
-- Module      : Amazonka.IoTFleetWise.Types.NodeCounts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.NodeCounts where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the number of nodes and node types in a vehicle
-- network.
--
-- /See:/ 'newNodeCounts' smart constructor.
data NodeCounts = NodeCounts'
  { -- | The total number of nodes in a vehicle network that represent actuators.
    totalActuators :: Prelude.Maybe Prelude.Int,
    -- | The total number of nodes in a vehicle network that represent
    -- attributes.
    totalAttributes :: Prelude.Maybe Prelude.Int,
    -- | The total number of nodes in a vehicle network that represent branches.
    totalBranches :: Prelude.Maybe Prelude.Int,
    -- | The total number of nodes in a vehicle network.
    totalNodes :: Prelude.Maybe Prelude.Int,
    -- | The total number of nodes in a vehicle network that represent sensors.
    totalSensors :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeCounts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalActuators', 'nodeCounts_totalActuators' - The total number of nodes in a vehicle network that represent actuators.
--
-- 'totalAttributes', 'nodeCounts_totalAttributes' - The total number of nodes in a vehicle network that represent
-- attributes.
--
-- 'totalBranches', 'nodeCounts_totalBranches' - The total number of nodes in a vehicle network that represent branches.
--
-- 'totalNodes', 'nodeCounts_totalNodes' - The total number of nodes in a vehicle network.
--
-- 'totalSensors', 'nodeCounts_totalSensors' - The total number of nodes in a vehicle network that represent sensors.
newNodeCounts ::
  NodeCounts
newNodeCounts =
  NodeCounts'
    { totalActuators = Prelude.Nothing,
      totalAttributes = Prelude.Nothing,
      totalBranches = Prelude.Nothing,
      totalNodes = Prelude.Nothing,
      totalSensors = Prelude.Nothing
    }

-- | The total number of nodes in a vehicle network that represent actuators.
nodeCounts_totalActuators :: Lens.Lens' NodeCounts (Prelude.Maybe Prelude.Int)
nodeCounts_totalActuators = Lens.lens (\NodeCounts' {totalActuators} -> totalActuators) (\s@NodeCounts' {} a -> s {totalActuators = a} :: NodeCounts)

-- | The total number of nodes in a vehicle network that represent
-- attributes.
nodeCounts_totalAttributes :: Lens.Lens' NodeCounts (Prelude.Maybe Prelude.Int)
nodeCounts_totalAttributes = Lens.lens (\NodeCounts' {totalAttributes} -> totalAttributes) (\s@NodeCounts' {} a -> s {totalAttributes = a} :: NodeCounts)

-- | The total number of nodes in a vehicle network that represent branches.
nodeCounts_totalBranches :: Lens.Lens' NodeCounts (Prelude.Maybe Prelude.Int)
nodeCounts_totalBranches = Lens.lens (\NodeCounts' {totalBranches} -> totalBranches) (\s@NodeCounts' {} a -> s {totalBranches = a} :: NodeCounts)

-- | The total number of nodes in a vehicle network.
nodeCounts_totalNodes :: Lens.Lens' NodeCounts (Prelude.Maybe Prelude.Int)
nodeCounts_totalNodes = Lens.lens (\NodeCounts' {totalNodes} -> totalNodes) (\s@NodeCounts' {} a -> s {totalNodes = a} :: NodeCounts)

-- | The total number of nodes in a vehicle network that represent sensors.
nodeCounts_totalSensors :: Lens.Lens' NodeCounts (Prelude.Maybe Prelude.Int)
nodeCounts_totalSensors = Lens.lens (\NodeCounts' {totalSensors} -> totalSensors) (\s@NodeCounts' {} a -> s {totalSensors = a} :: NodeCounts)

instance Data.FromJSON NodeCounts where
  parseJSON =
    Data.withObject
      "NodeCounts"
      ( \x ->
          NodeCounts'
            Prelude.<$> (x Data..:? "totalActuators")
            Prelude.<*> (x Data..:? "totalAttributes")
            Prelude.<*> (x Data..:? "totalBranches")
            Prelude.<*> (x Data..:? "totalNodes")
            Prelude.<*> (x Data..:? "totalSensors")
      )

instance Prelude.Hashable NodeCounts where
  hashWithSalt _salt NodeCounts' {..} =
    _salt `Prelude.hashWithSalt` totalActuators
      `Prelude.hashWithSalt` totalAttributes
      `Prelude.hashWithSalt` totalBranches
      `Prelude.hashWithSalt` totalNodes
      `Prelude.hashWithSalt` totalSensors

instance Prelude.NFData NodeCounts where
  rnf NodeCounts' {..} =
    Prelude.rnf totalActuators
      `Prelude.seq` Prelude.rnf totalAttributes
      `Prelude.seq` Prelude.rnf totalBranches
      `Prelude.seq` Prelude.rnf totalNodes
      `Prelude.seq` Prelude.rnf totalSensors
