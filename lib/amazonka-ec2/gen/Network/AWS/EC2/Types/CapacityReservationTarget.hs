{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservationTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CapacityReservationTarget
  ( CapacityReservationTarget (..),

    -- * Smart constructor
    mkCapacityReservationTarget,

    -- * Lenses
    crtCapacityReservationId,
    crtCapacityReservationResourceGroupArn,
  )
where

import qualified Network.AWS.EC2.Types.CapacityReservationId as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a target Capacity Reservation or Capacity Reservation group.
--
-- /See:/ 'mkCapacityReservationTarget' smart constructor.
data CapacityReservationTarget = CapacityReservationTarget'
  { -- | The ID of the Capacity Reservation in which to run the instance.
    capacityReservationId :: Core.Maybe Types.CapacityReservationId,
    -- | The ARN of the Capacity Reservation resource group in which to run the instance.
    capacityReservationResourceGroupArn :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CapacityReservationTarget' value with any optional fields omitted.
mkCapacityReservationTarget ::
  CapacityReservationTarget
mkCapacityReservationTarget =
  CapacityReservationTarget'
    { capacityReservationId = Core.Nothing,
      capacityReservationResourceGroupArn = Core.Nothing
    }

-- | The ID of the Capacity Reservation in which to run the instance.
--
-- /Note:/ Consider using 'capacityReservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtCapacityReservationId :: Lens.Lens' CapacityReservationTarget (Core.Maybe Types.CapacityReservationId)
crtCapacityReservationId = Lens.field @"capacityReservationId"
{-# DEPRECATED crtCapacityReservationId "Use generic-lens or generic-optics with 'capacityReservationId' instead." #-}

-- | The ARN of the Capacity Reservation resource group in which to run the instance.
--
-- /Note:/ Consider using 'capacityReservationResourceGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtCapacityReservationResourceGroupArn :: Lens.Lens' CapacityReservationTarget (Core.Maybe Types.String)
crtCapacityReservationResourceGroupArn = Lens.field @"capacityReservationResourceGroupArn"
{-# DEPRECATED crtCapacityReservationResourceGroupArn "Use generic-lens or generic-optics with 'capacityReservationResourceGroupArn' instead." #-}
