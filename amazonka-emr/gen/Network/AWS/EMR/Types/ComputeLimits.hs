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
-- Module      : Network.AWS.EMR.Types.ComputeLimits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ComputeLimits where

import Network.AWS.EMR.Types.ComputeLimitsUnitType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The EC2 unit limits for a managed scaling policy. The managed scaling
-- activity of a cluster can not be above or below these limits. The limit
-- only applies to the core and task nodes. The master node cannot be
-- scaled after initial configuration.
--
-- /See:/ 'newComputeLimits' smart constructor.
data ComputeLimits = ComputeLimits'
  { -- | The upper boundary of On-Demand EC2 units. It is measured through vCPU
    -- cores or instances for instance groups and measured through units for
    -- instance fleets. The On-Demand units are not allowed to scale beyond
    -- this boundary. The parameter is used to split capacity allocation
    -- between On-Demand and Spot Instances.
    maximumOnDemandCapacityUnits :: Prelude.Maybe Prelude.Int,
    -- | The upper boundary of EC2 units for core node type in a cluster. It is
    -- measured through vCPU cores or instances for instance groups and
    -- measured through units for instance fleets. The core units are not
    -- allowed to scale beyond this boundary. The parameter is used to split
    -- capacity allocation between core and task nodes.
    maximumCoreCapacityUnits :: Prelude.Maybe Prelude.Int,
    -- | The unit type used for specifying a managed scaling policy.
    unitType :: ComputeLimitsUnitType,
    -- | The lower boundary of EC2 units. It is measured through vCPU cores or
    -- instances for instance groups and measured through units for instance
    -- fleets. Managed scaling activities are not allowed beyond this boundary.
    -- The limit only applies to the core and task nodes. The master node
    -- cannot be scaled after initial configuration.
    minimumCapacityUnits :: Prelude.Int,
    -- | The upper boundary of EC2 units. It is measured through vCPU cores or
    -- instances for instance groups and measured through units for instance
    -- fleets. Managed scaling activities are not allowed beyond this boundary.
    -- The limit only applies to the core and task nodes. The master node
    -- cannot be scaled after initial configuration.
    maximumCapacityUnits :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ComputeLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumOnDemandCapacityUnits', 'computeLimits_maximumOnDemandCapacityUnits' - The upper boundary of On-Demand EC2 units. It is measured through vCPU
-- cores or instances for instance groups and measured through units for
-- instance fleets. The On-Demand units are not allowed to scale beyond
-- this boundary. The parameter is used to split capacity allocation
-- between On-Demand and Spot Instances.
--
-- 'maximumCoreCapacityUnits', 'computeLimits_maximumCoreCapacityUnits' - The upper boundary of EC2 units for core node type in a cluster. It is
-- measured through vCPU cores or instances for instance groups and
-- measured through units for instance fleets. The core units are not
-- allowed to scale beyond this boundary. The parameter is used to split
-- capacity allocation between core and task nodes.
--
-- 'unitType', 'computeLimits_unitType' - The unit type used for specifying a managed scaling policy.
--
-- 'minimumCapacityUnits', 'computeLimits_minimumCapacityUnits' - The lower boundary of EC2 units. It is measured through vCPU cores or
-- instances for instance groups and measured through units for instance
-- fleets. Managed scaling activities are not allowed beyond this boundary.
-- The limit only applies to the core and task nodes. The master node
-- cannot be scaled after initial configuration.
--
-- 'maximumCapacityUnits', 'computeLimits_maximumCapacityUnits' - The upper boundary of EC2 units. It is measured through vCPU cores or
-- instances for instance groups and measured through units for instance
-- fleets. Managed scaling activities are not allowed beyond this boundary.
-- The limit only applies to the core and task nodes. The master node
-- cannot be scaled after initial configuration.
newComputeLimits ::
  -- | 'unitType'
  ComputeLimitsUnitType ->
  -- | 'minimumCapacityUnits'
  Prelude.Int ->
  -- | 'maximumCapacityUnits'
  Prelude.Int ->
  ComputeLimits
newComputeLimits
  pUnitType_
  pMinimumCapacityUnits_
  pMaximumCapacityUnits_ =
    ComputeLimits'
      { maximumOnDemandCapacityUnits =
          Prelude.Nothing,
        maximumCoreCapacityUnits = Prelude.Nothing,
        unitType = pUnitType_,
        minimumCapacityUnits = pMinimumCapacityUnits_,
        maximumCapacityUnits = pMaximumCapacityUnits_
      }

-- | The upper boundary of On-Demand EC2 units. It is measured through vCPU
-- cores or instances for instance groups and measured through units for
-- instance fleets. The On-Demand units are not allowed to scale beyond
-- this boundary. The parameter is used to split capacity allocation
-- between On-Demand and Spot Instances.
computeLimits_maximumOnDemandCapacityUnits :: Lens.Lens' ComputeLimits (Prelude.Maybe Prelude.Int)
computeLimits_maximumOnDemandCapacityUnits = Lens.lens (\ComputeLimits' {maximumOnDemandCapacityUnits} -> maximumOnDemandCapacityUnits) (\s@ComputeLimits' {} a -> s {maximumOnDemandCapacityUnits = a} :: ComputeLimits)

-- | The upper boundary of EC2 units for core node type in a cluster. It is
-- measured through vCPU cores or instances for instance groups and
-- measured through units for instance fleets. The core units are not
-- allowed to scale beyond this boundary. The parameter is used to split
-- capacity allocation between core and task nodes.
computeLimits_maximumCoreCapacityUnits :: Lens.Lens' ComputeLimits (Prelude.Maybe Prelude.Int)
computeLimits_maximumCoreCapacityUnits = Lens.lens (\ComputeLimits' {maximumCoreCapacityUnits} -> maximumCoreCapacityUnits) (\s@ComputeLimits' {} a -> s {maximumCoreCapacityUnits = a} :: ComputeLimits)

-- | The unit type used for specifying a managed scaling policy.
computeLimits_unitType :: Lens.Lens' ComputeLimits ComputeLimitsUnitType
computeLimits_unitType = Lens.lens (\ComputeLimits' {unitType} -> unitType) (\s@ComputeLimits' {} a -> s {unitType = a} :: ComputeLimits)

-- | The lower boundary of EC2 units. It is measured through vCPU cores or
-- instances for instance groups and measured through units for instance
-- fleets. Managed scaling activities are not allowed beyond this boundary.
-- The limit only applies to the core and task nodes. The master node
-- cannot be scaled after initial configuration.
computeLimits_minimumCapacityUnits :: Lens.Lens' ComputeLimits Prelude.Int
computeLimits_minimumCapacityUnits = Lens.lens (\ComputeLimits' {minimumCapacityUnits} -> minimumCapacityUnits) (\s@ComputeLimits' {} a -> s {minimumCapacityUnits = a} :: ComputeLimits)

-- | The upper boundary of EC2 units. It is measured through vCPU cores or
-- instances for instance groups and measured through units for instance
-- fleets. Managed scaling activities are not allowed beyond this boundary.
-- The limit only applies to the core and task nodes. The master node
-- cannot be scaled after initial configuration.
computeLimits_maximumCapacityUnits :: Lens.Lens' ComputeLimits Prelude.Int
computeLimits_maximumCapacityUnits = Lens.lens (\ComputeLimits' {maximumCapacityUnits} -> maximumCapacityUnits) (\s@ComputeLimits' {} a -> s {maximumCapacityUnits = a} :: ComputeLimits)

instance Prelude.FromJSON ComputeLimits where
  parseJSON =
    Prelude.withObject
      "ComputeLimits"
      ( \x ->
          ComputeLimits'
            Prelude.<$> (x Prelude..:? "MaximumOnDemandCapacityUnits")
            Prelude.<*> (x Prelude..:? "MaximumCoreCapacityUnits")
            Prelude.<*> (x Prelude..: "UnitType")
            Prelude.<*> (x Prelude..: "MinimumCapacityUnits")
            Prelude.<*> (x Prelude..: "MaximumCapacityUnits")
      )

instance Prelude.Hashable ComputeLimits

instance Prelude.NFData ComputeLimits

instance Prelude.ToJSON ComputeLimits where
  toJSON ComputeLimits' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("MaximumOnDemandCapacityUnits" Prelude..=)
              Prelude.<$> maximumOnDemandCapacityUnits,
            ("MaximumCoreCapacityUnits" Prelude..=)
              Prelude.<$> maximumCoreCapacityUnits,
            Prelude.Just ("UnitType" Prelude..= unitType),
            Prelude.Just
              ( "MinimumCapacityUnits"
                  Prelude..= minimumCapacityUnits
              ),
            Prelude.Just
              ( "MaximumCapacityUnits"
                  Prelude..= maximumCapacityUnits
              )
          ]
      )
