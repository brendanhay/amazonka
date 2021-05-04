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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateCapacityReservationSpecificationRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateCapacityReservationSpecificationRequest where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CapacityReservationPreference
import Network.AWS.EC2.Types.CapacityReservationTarget
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an instance\'s Capacity Reservation targeting option. You can
-- specify only one option at a time. Use the
-- @CapacityReservationPreference@ parameter to configure the instance to
-- run in On-Demand capacity or to run in any @open@ Capacity Reservation
-- that has matching attributes (instance type, platform, Availability
-- Zone). Use the @CapacityReservationTarget@ parameter to explicitly
-- target a specific Capacity Reservation or a Capacity Reservation group.
--
-- /See:/ 'newLaunchTemplateCapacityReservationSpecificationRequest' smart constructor.
data LaunchTemplateCapacityReservationSpecificationRequest = LaunchTemplateCapacityReservationSpecificationRequest'
  { -- | Indicates the instance\'s Capacity Reservation preferences. Possible
    -- preferences include:
    --
    -- -   @open@ - The instance can run in any @open@ Capacity Reservation
    --     that has matching attributes (instance type, platform, Availability
    --     Zone).
    --
    -- -   @none@ - The instance avoids running in a Capacity Reservation even
    --     if one is available. The instance runs in On-Demand capacity.
    capacityReservationPreference :: Prelude.Maybe CapacityReservationPreference,
    -- | Information about the target Capacity Reservation or Capacity
    -- Reservation group.
    capacityReservationTarget :: Prelude.Maybe CapacityReservationTarget
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateCapacityReservationSpecificationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityReservationPreference', 'launchTemplateCapacityReservationSpecificationRequest_capacityReservationPreference' - Indicates the instance\'s Capacity Reservation preferences. Possible
-- preferences include:
--
-- -   @open@ - The instance can run in any @open@ Capacity Reservation
--     that has matching attributes (instance type, platform, Availability
--     Zone).
--
-- -   @none@ - The instance avoids running in a Capacity Reservation even
--     if one is available. The instance runs in On-Demand capacity.
--
-- 'capacityReservationTarget', 'launchTemplateCapacityReservationSpecificationRequest_capacityReservationTarget' - Information about the target Capacity Reservation or Capacity
-- Reservation group.
newLaunchTemplateCapacityReservationSpecificationRequest ::
  LaunchTemplateCapacityReservationSpecificationRequest
newLaunchTemplateCapacityReservationSpecificationRequest =
  LaunchTemplateCapacityReservationSpecificationRequest'
    { capacityReservationPreference =
        Prelude.Nothing,
      capacityReservationTarget =
        Prelude.Nothing
    }

-- | Indicates the instance\'s Capacity Reservation preferences. Possible
-- preferences include:
--
-- -   @open@ - The instance can run in any @open@ Capacity Reservation
--     that has matching attributes (instance type, platform, Availability
--     Zone).
--
-- -   @none@ - The instance avoids running in a Capacity Reservation even
--     if one is available. The instance runs in On-Demand capacity.
launchTemplateCapacityReservationSpecificationRequest_capacityReservationPreference :: Lens.Lens' LaunchTemplateCapacityReservationSpecificationRequest (Prelude.Maybe CapacityReservationPreference)
launchTemplateCapacityReservationSpecificationRequest_capacityReservationPreference = Lens.lens (\LaunchTemplateCapacityReservationSpecificationRequest' {capacityReservationPreference} -> capacityReservationPreference) (\s@LaunchTemplateCapacityReservationSpecificationRequest' {} a -> s {capacityReservationPreference = a} :: LaunchTemplateCapacityReservationSpecificationRequest)

-- | Information about the target Capacity Reservation or Capacity
-- Reservation group.
launchTemplateCapacityReservationSpecificationRequest_capacityReservationTarget :: Lens.Lens' LaunchTemplateCapacityReservationSpecificationRequest (Prelude.Maybe CapacityReservationTarget)
launchTemplateCapacityReservationSpecificationRequest_capacityReservationTarget = Lens.lens (\LaunchTemplateCapacityReservationSpecificationRequest' {capacityReservationTarget} -> capacityReservationTarget) (\s@LaunchTemplateCapacityReservationSpecificationRequest' {} a -> s {capacityReservationTarget = a} :: LaunchTemplateCapacityReservationSpecificationRequest)

instance
  Prelude.Hashable
    LaunchTemplateCapacityReservationSpecificationRequest

instance
  Prelude.NFData
    LaunchTemplateCapacityReservationSpecificationRequest

instance
  Prelude.ToQuery
    LaunchTemplateCapacityReservationSpecificationRequest
  where
  toQuery
    LaunchTemplateCapacityReservationSpecificationRequest' {..} =
      Prelude.mconcat
        [ "CapacityReservationPreference"
            Prelude.=: capacityReservationPreference,
          "CapacityReservationTarget"
            Prelude.=: capacityReservationTarget
        ]
