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
-- Module      : Network.AWS.EC2.Types.CapacityReservationSpecificationResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CapacityReservationSpecificationResponse where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CapacityReservationPreference
import Network.AWS.EC2.Types.CapacityReservationTargetResponse
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the instance\'s Capacity Reservation targeting preferences.
-- The action returns the @capacityReservationPreference@ response element
-- if the instance is configured to run in On-Demand capacity, or if it is
-- configured in run in any @open@ Capacity Reservation that has matching
-- attributes (instance type, platform, Availability Zone). The action
-- returns the @capacityReservationTarget@ response element if the instance
-- explicily targets a specific Capacity Reservation or Capacity
-- Reservation group.
--
-- /See:/ 'newCapacityReservationSpecificationResponse' smart constructor.
data CapacityReservationSpecificationResponse = CapacityReservationSpecificationResponse'
  { -- | Describes the instance\'s Capacity Reservation preferences. Possible
    -- preferences include:
    --
    -- -   @open@ - The instance can run in any @open@ Capacity Reservation
    --     that has matching attributes (instance type, platform, Availability
    --     Zone).
    --
    -- -   @none@ - The instance avoids running in a Capacity Reservation even
    --     if one is available. The instance runs in On-Demand capacity.
    capacityReservationPreference :: Prelude.Maybe CapacityReservationPreference,
    -- | Information about the targeted Capacity Reservation or Capacity
    -- Reservation group.
    capacityReservationTarget :: Prelude.Maybe CapacityReservationTargetResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CapacityReservationSpecificationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityReservationPreference', 'capacityReservationSpecificationResponse_capacityReservationPreference' - Describes the instance\'s Capacity Reservation preferences. Possible
-- preferences include:
--
-- -   @open@ - The instance can run in any @open@ Capacity Reservation
--     that has matching attributes (instance type, platform, Availability
--     Zone).
--
-- -   @none@ - The instance avoids running in a Capacity Reservation even
--     if one is available. The instance runs in On-Demand capacity.
--
-- 'capacityReservationTarget', 'capacityReservationSpecificationResponse_capacityReservationTarget' - Information about the targeted Capacity Reservation or Capacity
-- Reservation group.
newCapacityReservationSpecificationResponse ::
  CapacityReservationSpecificationResponse
newCapacityReservationSpecificationResponse =
  CapacityReservationSpecificationResponse'
    { capacityReservationPreference =
        Prelude.Nothing,
      capacityReservationTarget =
        Prelude.Nothing
    }

-- | Describes the instance\'s Capacity Reservation preferences. Possible
-- preferences include:
--
-- -   @open@ - The instance can run in any @open@ Capacity Reservation
--     that has matching attributes (instance type, platform, Availability
--     Zone).
--
-- -   @none@ - The instance avoids running in a Capacity Reservation even
--     if one is available. The instance runs in On-Demand capacity.
capacityReservationSpecificationResponse_capacityReservationPreference :: Lens.Lens' CapacityReservationSpecificationResponse (Prelude.Maybe CapacityReservationPreference)
capacityReservationSpecificationResponse_capacityReservationPreference = Lens.lens (\CapacityReservationSpecificationResponse' {capacityReservationPreference} -> capacityReservationPreference) (\s@CapacityReservationSpecificationResponse' {} a -> s {capacityReservationPreference = a} :: CapacityReservationSpecificationResponse)

-- | Information about the targeted Capacity Reservation or Capacity
-- Reservation group.
capacityReservationSpecificationResponse_capacityReservationTarget :: Lens.Lens' CapacityReservationSpecificationResponse (Prelude.Maybe CapacityReservationTargetResponse)
capacityReservationSpecificationResponse_capacityReservationTarget = Lens.lens (\CapacityReservationSpecificationResponse' {capacityReservationTarget} -> capacityReservationTarget) (\s@CapacityReservationSpecificationResponse' {} a -> s {capacityReservationTarget = a} :: CapacityReservationSpecificationResponse)

instance
  Prelude.FromXML
    CapacityReservationSpecificationResponse
  where
  parseXML x =
    CapacityReservationSpecificationResponse'
      Prelude.<$> (x Prelude..@? "capacityReservationPreference")
        Prelude.<*> (x Prelude..@? "capacityReservationTarget")

instance
  Prelude.Hashable
    CapacityReservationSpecificationResponse

instance
  Prelude.NFData
    CapacityReservationSpecificationResponse
