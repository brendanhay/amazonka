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
-- Module      : Amazonka.EMR.Types.OnDemandProvisioningSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.OnDemandProvisioningSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.OnDemandCapacityReservationOptions
import Amazonka.EMR.Types.OnDemandProvisioningAllocationStrategy
import qualified Amazonka.Prelude as Prelude

-- | The launch specification for On-Demand Instances in the instance fleet,
-- which determines the allocation strategy.
--
-- The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions. On-Demand Instances
-- allocation strategy is available in Amazon EMR version 5.12.1 and later.
--
-- /See:/ 'newOnDemandProvisioningSpecification' smart constructor.
data OnDemandProvisioningSpecification = OnDemandProvisioningSpecification'
  { -- | The launch specification for On-Demand instances in the instance fleet,
    -- which determines the allocation strategy.
    capacityReservationOptions :: Prelude.Maybe OnDemandCapacityReservationOptions,
    -- | Specifies the strategy to use in launching On-Demand instance fleets.
    -- Currently, the only option is @lowest-price@ (the default), which
    -- launches the lowest price first.
    allocationStrategy :: OnDemandProvisioningAllocationStrategy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OnDemandProvisioningSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityReservationOptions', 'onDemandProvisioningSpecification_capacityReservationOptions' - The launch specification for On-Demand instances in the instance fleet,
-- which determines the allocation strategy.
--
-- 'allocationStrategy', 'onDemandProvisioningSpecification_allocationStrategy' - Specifies the strategy to use in launching On-Demand instance fleets.
-- Currently, the only option is @lowest-price@ (the default), which
-- launches the lowest price first.
newOnDemandProvisioningSpecification ::
  -- | 'allocationStrategy'
  OnDemandProvisioningAllocationStrategy ->
  OnDemandProvisioningSpecification
newOnDemandProvisioningSpecification
  pAllocationStrategy_ =
    OnDemandProvisioningSpecification'
      { capacityReservationOptions =
          Prelude.Nothing,
        allocationStrategy =
          pAllocationStrategy_
      }

-- | The launch specification for On-Demand instances in the instance fleet,
-- which determines the allocation strategy.
onDemandProvisioningSpecification_capacityReservationOptions :: Lens.Lens' OnDemandProvisioningSpecification (Prelude.Maybe OnDemandCapacityReservationOptions)
onDemandProvisioningSpecification_capacityReservationOptions = Lens.lens (\OnDemandProvisioningSpecification' {capacityReservationOptions} -> capacityReservationOptions) (\s@OnDemandProvisioningSpecification' {} a -> s {capacityReservationOptions = a} :: OnDemandProvisioningSpecification)

-- | Specifies the strategy to use in launching On-Demand instance fleets.
-- Currently, the only option is @lowest-price@ (the default), which
-- launches the lowest price first.
onDemandProvisioningSpecification_allocationStrategy :: Lens.Lens' OnDemandProvisioningSpecification OnDemandProvisioningAllocationStrategy
onDemandProvisioningSpecification_allocationStrategy = Lens.lens (\OnDemandProvisioningSpecification' {allocationStrategy} -> allocationStrategy) (\s@OnDemandProvisioningSpecification' {} a -> s {allocationStrategy = a} :: OnDemandProvisioningSpecification)

instance
  Data.FromJSON
    OnDemandProvisioningSpecification
  where
  parseJSON =
    Data.withObject
      "OnDemandProvisioningSpecification"
      ( \x ->
          OnDemandProvisioningSpecification'
            Prelude.<$> (x Data..:? "CapacityReservationOptions")
            Prelude.<*> (x Data..: "AllocationStrategy")
      )

instance
  Prelude.Hashable
    OnDemandProvisioningSpecification
  where
  hashWithSalt
    _salt
    OnDemandProvisioningSpecification' {..} =
      _salt
        `Prelude.hashWithSalt` capacityReservationOptions
        `Prelude.hashWithSalt` allocationStrategy

instance
  Prelude.NFData
    OnDemandProvisioningSpecification
  where
  rnf OnDemandProvisioningSpecification' {..} =
    Prelude.rnf capacityReservationOptions `Prelude.seq`
      Prelude.rnf allocationStrategy

instance
  Data.ToJSON
    OnDemandProvisioningSpecification
  where
  toJSON OnDemandProvisioningSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CapacityReservationOptions" Data..=)
              Prelude.<$> capacityReservationOptions,
            Prelude.Just
              ("AllocationStrategy" Data..= allocationStrategy)
          ]
      )
