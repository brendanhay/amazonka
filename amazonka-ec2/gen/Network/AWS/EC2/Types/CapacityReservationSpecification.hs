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
-- Module      : Network.AWS.EC2.Types.CapacityReservationSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CapacityReservationSpecification where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CapacityReservationPreference
import Network.AWS.EC2.Types.CapacityReservationTarget
import qualified Network.AWS.Lens as Lens

-- | Describes an instance\'s Capacity Reservation targeting option. You can
-- specify only one parameter at a time. If you specify
-- @CapacityReservationPreference@ and @CapacityReservationTarget@, the
-- request fails.
--
-- Use the @CapacityReservationPreference@ parameter to configure the
-- instance to run as an On-Demand Instance or to run in any @open@
-- Capacity Reservation that has matching attributes (instance type,
-- platform, Availability Zone). Use the @CapacityReservationTarget@
-- parameter to explicitly target a specific Capacity Reservation or a
-- Capacity Reservation group.
--
-- /See:/ 'newCapacityReservationSpecification' smart constructor.
data CapacityReservationSpecification = CapacityReservationSpecification'
  { -- | Indicates the instance\'s Capacity Reservation preferences. Possible
    -- preferences include:
    --
    -- -   @open@ - The instance can run in any @open@ Capacity Reservation
    --     that has matching attributes (instance type, platform, Availability
    --     Zone).
    --
    -- -   @none@ - The instance avoids running in a Capacity Reservation even
    --     if one is available. The instance runs as an On-Demand Instance.
    capacityReservationPreference :: Core.Maybe CapacityReservationPreference,
    -- | Information about the target Capacity Reservation or Capacity
    -- Reservation group.
    capacityReservationTarget :: Core.Maybe CapacityReservationTarget
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CapacityReservationSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityReservationPreference', 'capacityReservationSpecification_capacityReservationPreference' - Indicates the instance\'s Capacity Reservation preferences. Possible
-- preferences include:
--
-- -   @open@ - The instance can run in any @open@ Capacity Reservation
--     that has matching attributes (instance type, platform, Availability
--     Zone).
--
-- -   @none@ - The instance avoids running in a Capacity Reservation even
--     if one is available. The instance runs as an On-Demand Instance.
--
-- 'capacityReservationTarget', 'capacityReservationSpecification_capacityReservationTarget' - Information about the target Capacity Reservation or Capacity
-- Reservation group.
newCapacityReservationSpecification ::
  CapacityReservationSpecification
newCapacityReservationSpecification =
  CapacityReservationSpecification'
    { capacityReservationPreference =
        Core.Nothing,
      capacityReservationTarget = Core.Nothing
    }

-- | Indicates the instance\'s Capacity Reservation preferences. Possible
-- preferences include:
--
-- -   @open@ - The instance can run in any @open@ Capacity Reservation
--     that has matching attributes (instance type, platform, Availability
--     Zone).
--
-- -   @none@ - The instance avoids running in a Capacity Reservation even
--     if one is available. The instance runs as an On-Demand Instance.
capacityReservationSpecification_capacityReservationPreference :: Lens.Lens' CapacityReservationSpecification (Core.Maybe CapacityReservationPreference)
capacityReservationSpecification_capacityReservationPreference = Lens.lens (\CapacityReservationSpecification' {capacityReservationPreference} -> capacityReservationPreference) (\s@CapacityReservationSpecification' {} a -> s {capacityReservationPreference = a} :: CapacityReservationSpecification)

-- | Information about the target Capacity Reservation or Capacity
-- Reservation group.
capacityReservationSpecification_capacityReservationTarget :: Lens.Lens' CapacityReservationSpecification (Core.Maybe CapacityReservationTarget)
capacityReservationSpecification_capacityReservationTarget = Lens.lens (\CapacityReservationSpecification' {capacityReservationTarget} -> capacityReservationTarget) (\s@CapacityReservationSpecification' {} a -> s {capacityReservationTarget = a} :: CapacityReservationSpecification)

instance
  Core.Hashable
    CapacityReservationSpecification

instance Core.NFData CapacityReservationSpecification

instance
  Core.ToQuery
    CapacityReservationSpecification
  where
  toQuery CapacityReservationSpecification' {..} =
    Core.mconcat
      [ "CapacityReservationPreference"
          Core.=: capacityReservationPreference,
        "CapacityReservationTarget"
          Core.=: capacityReservationTarget
      ]
