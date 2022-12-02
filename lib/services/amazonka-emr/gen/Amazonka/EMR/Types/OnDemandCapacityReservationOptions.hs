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
-- Module      : Amazonka.EMR.Types.OnDemandCapacityReservationOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.OnDemandCapacityReservationOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.OnDemandCapacityReservationPreference
import Amazonka.EMR.Types.OnDemandCapacityReservationUsageStrategy
import qualified Amazonka.Prelude as Prelude

-- | Describes the strategy for using unused Capacity Reservations for
-- fulfilling On-Demand capacity.
--
-- /See:/ 'newOnDemandCapacityReservationOptions' smart constructor.
data OnDemandCapacityReservationOptions = OnDemandCapacityReservationOptions'
  { -- | Indicates the instance\'s Capacity Reservation preferences. Possible
    -- preferences include:
    --
    -- -   @open@ - The instance can run in any open Capacity Reservation that
    --     has matching attributes (instance type, platform, Availability
    --     Zone).
    --
    -- -   @none@ - The instance avoids running in a Capacity Reservation even
    --     if one is available. The instance runs as an On-Demand Instance.
    capacityReservationPreference :: Prelude.Maybe OnDemandCapacityReservationPreference,
    -- | The ARN of the Capacity Reservation resource group in which to run the
    -- instance.
    capacityReservationResourceGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to use unused Capacity Reservations for fulfilling
    -- On-Demand capacity.
    --
    -- If you specify @use-capacity-reservations-first@, the fleet uses unused
    -- Capacity Reservations to fulfill On-Demand capacity up to the target
    -- On-Demand capacity. If multiple instance pools have unused Capacity
    -- Reservations, the On-Demand allocation strategy (@lowest-price@) is
    -- applied. If the number of unused Capacity Reservations is less than the
    -- On-Demand target capacity, the remaining On-Demand target capacity is
    -- launched according to the On-Demand allocation strategy
    -- (@lowest-price@).
    --
    -- If you do not specify a value, the fleet fulfills the On-Demand capacity
    -- according to the chosen On-Demand allocation strategy.
    usageStrategy :: Prelude.Maybe OnDemandCapacityReservationUsageStrategy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OnDemandCapacityReservationOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityReservationPreference', 'onDemandCapacityReservationOptions_capacityReservationPreference' - Indicates the instance\'s Capacity Reservation preferences. Possible
-- preferences include:
--
-- -   @open@ - The instance can run in any open Capacity Reservation that
--     has matching attributes (instance type, platform, Availability
--     Zone).
--
-- -   @none@ - The instance avoids running in a Capacity Reservation even
--     if one is available. The instance runs as an On-Demand Instance.
--
-- 'capacityReservationResourceGroupArn', 'onDemandCapacityReservationOptions_capacityReservationResourceGroupArn' - The ARN of the Capacity Reservation resource group in which to run the
-- instance.
--
-- 'usageStrategy', 'onDemandCapacityReservationOptions_usageStrategy' - Indicates whether to use unused Capacity Reservations for fulfilling
-- On-Demand capacity.
--
-- If you specify @use-capacity-reservations-first@, the fleet uses unused
-- Capacity Reservations to fulfill On-Demand capacity up to the target
-- On-Demand capacity. If multiple instance pools have unused Capacity
-- Reservations, the On-Demand allocation strategy (@lowest-price@) is
-- applied. If the number of unused Capacity Reservations is less than the
-- On-Demand target capacity, the remaining On-Demand target capacity is
-- launched according to the On-Demand allocation strategy
-- (@lowest-price@).
--
-- If you do not specify a value, the fleet fulfills the On-Demand capacity
-- according to the chosen On-Demand allocation strategy.
newOnDemandCapacityReservationOptions ::
  OnDemandCapacityReservationOptions
newOnDemandCapacityReservationOptions =
  OnDemandCapacityReservationOptions'
    { capacityReservationPreference =
        Prelude.Nothing,
      capacityReservationResourceGroupArn =
        Prelude.Nothing,
      usageStrategy = Prelude.Nothing
    }

-- | Indicates the instance\'s Capacity Reservation preferences. Possible
-- preferences include:
--
-- -   @open@ - The instance can run in any open Capacity Reservation that
--     has matching attributes (instance type, platform, Availability
--     Zone).
--
-- -   @none@ - The instance avoids running in a Capacity Reservation even
--     if one is available. The instance runs as an On-Demand Instance.
onDemandCapacityReservationOptions_capacityReservationPreference :: Lens.Lens' OnDemandCapacityReservationOptions (Prelude.Maybe OnDemandCapacityReservationPreference)
onDemandCapacityReservationOptions_capacityReservationPreference = Lens.lens (\OnDemandCapacityReservationOptions' {capacityReservationPreference} -> capacityReservationPreference) (\s@OnDemandCapacityReservationOptions' {} a -> s {capacityReservationPreference = a} :: OnDemandCapacityReservationOptions)

-- | The ARN of the Capacity Reservation resource group in which to run the
-- instance.
onDemandCapacityReservationOptions_capacityReservationResourceGroupArn :: Lens.Lens' OnDemandCapacityReservationOptions (Prelude.Maybe Prelude.Text)
onDemandCapacityReservationOptions_capacityReservationResourceGroupArn = Lens.lens (\OnDemandCapacityReservationOptions' {capacityReservationResourceGroupArn} -> capacityReservationResourceGroupArn) (\s@OnDemandCapacityReservationOptions' {} a -> s {capacityReservationResourceGroupArn = a} :: OnDemandCapacityReservationOptions)

-- | Indicates whether to use unused Capacity Reservations for fulfilling
-- On-Demand capacity.
--
-- If you specify @use-capacity-reservations-first@, the fleet uses unused
-- Capacity Reservations to fulfill On-Demand capacity up to the target
-- On-Demand capacity. If multiple instance pools have unused Capacity
-- Reservations, the On-Demand allocation strategy (@lowest-price@) is
-- applied. If the number of unused Capacity Reservations is less than the
-- On-Demand target capacity, the remaining On-Demand target capacity is
-- launched according to the On-Demand allocation strategy
-- (@lowest-price@).
--
-- If you do not specify a value, the fleet fulfills the On-Demand capacity
-- according to the chosen On-Demand allocation strategy.
onDemandCapacityReservationOptions_usageStrategy :: Lens.Lens' OnDemandCapacityReservationOptions (Prelude.Maybe OnDemandCapacityReservationUsageStrategy)
onDemandCapacityReservationOptions_usageStrategy = Lens.lens (\OnDemandCapacityReservationOptions' {usageStrategy} -> usageStrategy) (\s@OnDemandCapacityReservationOptions' {} a -> s {usageStrategy = a} :: OnDemandCapacityReservationOptions)

instance
  Data.FromJSON
    OnDemandCapacityReservationOptions
  where
  parseJSON =
    Data.withObject
      "OnDemandCapacityReservationOptions"
      ( \x ->
          OnDemandCapacityReservationOptions'
            Prelude.<$> (x Data..:? "CapacityReservationPreference")
            Prelude.<*> (x Data..:? "CapacityReservationResourceGroupArn")
            Prelude.<*> (x Data..:? "UsageStrategy")
      )

instance
  Prelude.Hashable
    OnDemandCapacityReservationOptions
  where
  hashWithSalt
    _salt
    OnDemandCapacityReservationOptions' {..} =
      _salt
        `Prelude.hashWithSalt` capacityReservationPreference
        `Prelude.hashWithSalt` capacityReservationResourceGroupArn
        `Prelude.hashWithSalt` usageStrategy

instance
  Prelude.NFData
    OnDemandCapacityReservationOptions
  where
  rnf OnDemandCapacityReservationOptions' {..} =
    Prelude.rnf capacityReservationPreference
      `Prelude.seq` Prelude.rnf capacityReservationResourceGroupArn
      `Prelude.seq` Prelude.rnf usageStrategy

instance
  Data.ToJSON
    OnDemandCapacityReservationOptions
  where
  toJSON OnDemandCapacityReservationOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CapacityReservationPreference" Data..=)
              Prelude.<$> capacityReservationPreference,
            ("CapacityReservationResourceGroupArn" Data..=)
              Prelude.<$> capacityReservationResourceGroupArn,
            ("UsageStrategy" Data..=) Prelude.<$> usageStrategy
          ]
      )
