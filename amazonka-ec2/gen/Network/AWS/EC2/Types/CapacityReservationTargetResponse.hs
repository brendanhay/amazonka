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
-- Module      : Network.AWS.EC2.Types.CapacityReservationTargetResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CapacityReservationTargetResponse where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes a target Capacity Reservation or Capacity Reservation group.
--
-- /See:/ 'newCapacityReservationTargetResponse' smart constructor.
data CapacityReservationTargetResponse = CapacityReservationTargetResponse'
  { -- | The ARN of the targeted Capacity Reservation group.
    capacityReservationResourceGroupArn :: Core.Maybe Core.Text,
    -- | The ID of the targeted Capacity Reservation.
    capacityReservationId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CapacityReservationTargetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityReservationResourceGroupArn', 'capacityReservationTargetResponse_capacityReservationResourceGroupArn' - The ARN of the targeted Capacity Reservation group.
--
-- 'capacityReservationId', 'capacityReservationTargetResponse_capacityReservationId' - The ID of the targeted Capacity Reservation.
newCapacityReservationTargetResponse ::
  CapacityReservationTargetResponse
newCapacityReservationTargetResponse =
  CapacityReservationTargetResponse'
    { capacityReservationResourceGroupArn =
        Core.Nothing,
      capacityReservationId = Core.Nothing
    }

-- | The ARN of the targeted Capacity Reservation group.
capacityReservationTargetResponse_capacityReservationResourceGroupArn :: Lens.Lens' CapacityReservationTargetResponse (Core.Maybe Core.Text)
capacityReservationTargetResponse_capacityReservationResourceGroupArn = Lens.lens (\CapacityReservationTargetResponse' {capacityReservationResourceGroupArn} -> capacityReservationResourceGroupArn) (\s@CapacityReservationTargetResponse' {} a -> s {capacityReservationResourceGroupArn = a} :: CapacityReservationTargetResponse)

-- | The ID of the targeted Capacity Reservation.
capacityReservationTargetResponse_capacityReservationId :: Lens.Lens' CapacityReservationTargetResponse (Core.Maybe Core.Text)
capacityReservationTargetResponse_capacityReservationId = Lens.lens (\CapacityReservationTargetResponse' {capacityReservationId} -> capacityReservationId) (\s@CapacityReservationTargetResponse' {} a -> s {capacityReservationId = a} :: CapacityReservationTargetResponse)

instance
  Core.FromXML
    CapacityReservationTargetResponse
  where
  parseXML x =
    CapacityReservationTargetResponse'
      Core.<$> (x Core..@? "capacityReservationResourceGroupArn")
      Core.<*> (x Core..@? "capacityReservationId")

instance
  Core.Hashable
    CapacityReservationTargetResponse

instance
  Core.NFData
    CapacityReservationTargetResponse
