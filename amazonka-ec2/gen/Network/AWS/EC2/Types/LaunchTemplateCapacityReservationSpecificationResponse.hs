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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateCapacityReservationSpecificationResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateCapacityReservationSpecificationResponse where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CapacityReservationPreference
import Network.AWS.EC2.Types.CapacityReservationTargetResponse
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the Capacity Reservation targeting option.
--
-- /See:/ 'newLaunchTemplateCapacityReservationSpecificationResponse' smart constructor.
data LaunchTemplateCapacityReservationSpecificationResponse = LaunchTemplateCapacityReservationSpecificationResponse'
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
    capacityReservationTarget :: Prelude.Maybe CapacityReservationTargetResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateCapacityReservationSpecificationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityReservationPreference', 'launchTemplateCapacityReservationSpecificationResponse_capacityReservationPreference' - Indicates the instance\'s Capacity Reservation preferences. Possible
-- preferences include:
--
-- -   @open@ - The instance can run in any @open@ Capacity Reservation
--     that has matching attributes (instance type, platform, Availability
--     Zone).
--
-- -   @none@ - The instance avoids running in a Capacity Reservation even
--     if one is available. The instance runs in On-Demand capacity.
--
-- 'capacityReservationTarget', 'launchTemplateCapacityReservationSpecificationResponse_capacityReservationTarget' - Information about the target Capacity Reservation or Capacity
-- Reservation group.
newLaunchTemplateCapacityReservationSpecificationResponse ::
  LaunchTemplateCapacityReservationSpecificationResponse
newLaunchTemplateCapacityReservationSpecificationResponse =
  LaunchTemplateCapacityReservationSpecificationResponse'
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
launchTemplateCapacityReservationSpecificationResponse_capacityReservationPreference :: Lens.Lens' LaunchTemplateCapacityReservationSpecificationResponse (Prelude.Maybe CapacityReservationPreference)
launchTemplateCapacityReservationSpecificationResponse_capacityReservationPreference = Lens.lens (\LaunchTemplateCapacityReservationSpecificationResponse' {capacityReservationPreference} -> capacityReservationPreference) (\s@LaunchTemplateCapacityReservationSpecificationResponse' {} a -> s {capacityReservationPreference = a} :: LaunchTemplateCapacityReservationSpecificationResponse)

-- | Information about the target Capacity Reservation or Capacity
-- Reservation group.
launchTemplateCapacityReservationSpecificationResponse_capacityReservationTarget :: Lens.Lens' LaunchTemplateCapacityReservationSpecificationResponse (Prelude.Maybe CapacityReservationTargetResponse)
launchTemplateCapacityReservationSpecificationResponse_capacityReservationTarget = Lens.lens (\LaunchTemplateCapacityReservationSpecificationResponse' {capacityReservationTarget} -> capacityReservationTarget) (\s@LaunchTemplateCapacityReservationSpecificationResponse' {} a -> s {capacityReservationTarget = a} :: LaunchTemplateCapacityReservationSpecificationResponse)

instance
  Prelude.FromXML
    LaunchTemplateCapacityReservationSpecificationResponse
  where
  parseXML x =
    LaunchTemplateCapacityReservationSpecificationResponse'
      Prelude.<$> (x Prelude..@? "capacityReservationPreference")
        Prelude.<*> (x Prelude..@? "capacityReservationTarget")

instance
  Prelude.Hashable
    LaunchTemplateCapacityReservationSpecificationResponse

instance
  Prelude.NFData
    LaunchTemplateCapacityReservationSpecificationResponse
