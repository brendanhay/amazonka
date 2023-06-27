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
-- Module      : Amazonka.EC2.Types.TargetCapacitySpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TargetCapacitySpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.DefaultTargetCapacityType
import Amazonka.EC2.Types.TargetCapacityUnitType
import qualified Amazonka.Prelude as Prelude

-- | The number of units to request. You can choose to set the target
-- capacity in terms of instances or a performance characteristic that is
-- important to your application workload, such as vCPUs, memory, or I\/O.
-- If the request type is @maintain@, you can specify a target capacity of
-- 0 and add capacity later.
--
-- You can use the On-Demand Instance @MaxTotalPrice@ parameter, the Spot
-- Instance @MaxTotalPrice@, or both to ensure that your fleet cost does
-- not exceed your budget. If you set a maximum price per hour for the
-- On-Demand Instances and Spot Instances in your request, EC2 Fleet will
-- launch instances until it reaches the maximum amount that you\'re
-- willing to pay. When the maximum amount you\'re willing to pay is
-- reached, the fleet stops launching instances even if it hasn’t met the
-- target capacity. The @MaxTotalPrice@ parameters are located in
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_OnDemandOptions.html OnDemandOptions>
-- and
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotOptions SpotOptions>.
--
-- /See:/ 'newTargetCapacitySpecification' smart constructor.
data TargetCapacitySpecification = TargetCapacitySpecification'
  { -- | The default @TotalTargetCapacity@, which is either @Spot@ or
    -- @On-Demand@.
    defaultTargetCapacityType :: Prelude.Maybe DefaultTargetCapacityType,
    -- | The number of On-Demand units to request. If you specify a target
    -- capacity for Spot units, you cannot specify a target capacity for
    -- On-Demand units.
    onDemandTargetCapacity :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of Spot units to launch. If you specify a target
    -- capacity for On-Demand units, you cannot specify a target capacity for
    -- Spot units.
    spotTargetCapacity :: Prelude.Maybe Prelude.Int,
    -- | The unit for the target capacity. @TargetCapacityUnitType@ can only be
    -- specified when @InstanceRequirements@ is specified.
    --
    -- Default: @units@ (translates to number of instances)
    targetCapacityUnitType :: Prelude.Maybe TargetCapacityUnitType,
    -- | The number of units to request, filled using
    -- @DefaultTargetCapacityType@.
    totalTargetCapacity :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetCapacitySpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultTargetCapacityType', 'targetCapacitySpecification_defaultTargetCapacityType' - The default @TotalTargetCapacity@, which is either @Spot@ or
-- @On-Demand@.
--
-- 'onDemandTargetCapacity', 'targetCapacitySpecification_onDemandTargetCapacity' - The number of On-Demand units to request. If you specify a target
-- capacity for Spot units, you cannot specify a target capacity for
-- On-Demand units.
--
-- 'spotTargetCapacity', 'targetCapacitySpecification_spotTargetCapacity' - The maximum number of Spot units to launch. If you specify a target
-- capacity for On-Demand units, you cannot specify a target capacity for
-- Spot units.
--
-- 'targetCapacityUnitType', 'targetCapacitySpecification_targetCapacityUnitType' - The unit for the target capacity. @TargetCapacityUnitType@ can only be
-- specified when @InstanceRequirements@ is specified.
--
-- Default: @units@ (translates to number of instances)
--
-- 'totalTargetCapacity', 'targetCapacitySpecification_totalTargetCapacity' - The number of units to request, filled using
-- @DefaultTargetCapacityType@.
newTargetCapacitySpecification ::
  TargetCapacitySpecification
newTargetCapacitySpecification =
  TargetCapacitySpecification'
    { defaultTargetCapacityType =
        Prelude.Nothing,
      onDemandTargetCapacity = Prelude.Nothing,
      spotTargetCapacity = Prelude.Nothing,
      targetCapacityUnitType = Prelude.Nothing,
      totalTargetCapacity = Prelude.Nothing
    }

-- | The default @TotalTargetCapacity@, which is either @Spot@ or
-- @On-Demand@.
targetCapacitySpecification_defaultTargetCapacityType :: Lens.Lens' TargetCapacitySpecification (Prelude.Maybe DefaultTargetCapacityType)
targetCapacitySpecification_defaultTargetCapacityType = Lens.lens (\TargetCapacitySpecification' {defaultTargetCapacityType} -> defaultTargetCapacityType) (\s@TargetCapacitySpecification' {} a -> s {defaultTargetCapacityType = a} :: TargetCapacitySpecification)

-- | The number of On-Demand units to request. If you specify a target
-- capacity for Spot units, you cannot specify a target capacity for
-- On-Demand units.
targetCapacitySpecification_onDemandTargetCapacity :: Lens.Lens' TargetCapacitySpecification (Prelude.Maybe Prelude.Int)
targetCapacitySpecification_onDemandTargetCapacity = Lens.lens (\TargetCapacitySpecification' {onDemandTargetCapacity} -> onDemandTargetCapacity) (\s@TargetCapacitySpecification' {} a -> s {onDemandTargetCapacity = a} :: TargetCapacitySpecification)

-- | The maximum number of Spot units to launch. If you specify a target
-- capacity for On-Demand units, you cannot specify a target capacity for
-- Spot units.
targetCapacitySpecification_spotTargetCapacity :: Lens.Lens' TargetCapacitySpecification (Prelude.Maybe Prelude.Int)
targetCapacitySpecification_spotTargetCapacity = Lens.lens (\TargetCapacitySpecification' {spotTargetCapacity} -> spotTargetCapacity) (\s@TargetCapacitySpecification' {} a -> s {spotTargetCapacity = a} :: TargetCapacitySpecification)

-- | The unit for the target capacity. @TargetCapacityUnitType@ can only be
-- specified when @InstanceRequirements@ is specified.
--
-- Default: @units@ (translates to number of instances)
targetCapacitySpecification_targetCapacityUnitType :: Lens.Lens' TargetCapacitySpecification (Prelude.Maybe TargetCapacityUnitType)
targetCapacitySpecification_targetCapacityUnitType = Lens.lens (\TargetCapacitySpecification' {targetCapacityUnitType} -> targetCapacityUnitType) (\s@TargetCapacitySpecification' {} a -> s {targetCapacityUnitType = a} :: TargetCapacitySpecification)

-- | The number of units to request, filled using
-- @DefaultTargetCapacityType@.
targetCapacitySpecification_totalTargetCapacity :: Lens.Lens' TargetCapacitySpecification (Prelude.Maybe Prelude.Int)
targetCapacitySpecification_totalTargetCapacity = Lens.lens (\TargetCapacitySpecification' {totalTargetCapacity} -> totalTargetCapacity) (\s@TargetCapacitySpecification' {} a -> s {totalTargetCapacity = a} :: TargetCapacitySpecification)

instance Data.FromXML TargetCapacitySpecification where
  parseXML x =
    TargetCapacitySpecification'
      Prelude.<$> (x Data..@? "defaultTargetCapacityType")
      Prelude.<*> (x Data..@? "onDemandTargetCapacity")
      Prelude.<*> (x Data..@? "spotTargetCapacity")
      Prelude.<*> (x Data..@? "targetCapacityUnitType")
      Prelude.<*> (x Data..@? "totalTargetCapacity")

instance Prelude.Hashable TargetCapacitySpecification where
  hashWithSalt _salt TargetCapacitySpecification' {..} =
    _salt
      `Prelude.hashWithSalt` defaultTargetCapacityType
      `Prelude.hashWithSalt` onDemandTargetCapacity
      `Prelude.hashWithSalt` spotTargetCapacity
      `Prelude.hashWithSalt` targetCapacityUnitType
      `Prelude.hashWithSalt` totalTargetCapacity

instance Prelude.NFData TargetCapacitySpecification where
  rnf TargetCapacitySpecification' {..} =
    Prelude.rnf defaultTargetCapacityType
      `Prelude.seq` Prelude.rnf onDemandTargetCapacity
      `Prelude.seq` Prelude.rnf spotTargetCapacity
      `Prelude.seq` Prelude.rnf targetCapacityUnitType
      `Prelude.seq` Prelude.rnf totalTargetCapacity
