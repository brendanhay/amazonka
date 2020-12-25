{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CapacityReservationGroup
  ( CapacityReservationGroup (..),

    -- * Smart constructor
    mkCapacityReservationGroup,

    -- * Lenses
    crgGroupArn,
    crgOwnerId,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a resource group to which a Capacity Reservation has been added.
--
-- /See:/ 'mkCapacityReservationGroup' smart constructor.
data CapacityReservationGroup = CapacityReservationGroup'
  { -- | The ARN of the resource group.
    groupArn :: Core.Maybe Types.String,
    -- | The ID of the AWS account that owns the resource group.
    ownerId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CapacityReservationGroup' value with any optional fields omitted.
mkCapacityReservationGroup ::
  CapacityReservationGroup
mkCapacityReservationGroup =
  CapacityReservationGroup'
    { groupArn = Core.Nothing,
      ownerId = Core.Nothing
    }

-- | The ARN of the resource group.
--
-- /Note:/ Consider using 'groupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgGroupArn :: Lens.Lens' CapacityReservationGroup (Core.Maybe Types.String)
crgGroupArn = Lens.field @"groupArn"
{-# DEPRECATED crgGroupArn "Use generic-lens or generic-optics with 'groupArn' instead." #-}

-- | The ID of the AWS account that owns the resource group.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgOwnerId :: Lens.Lens' CapacityReservationGroup (Core.Maybe Types.String)
crgOwnerId = Lens.field @"ownerId"
{-# DEPRECATED crgOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

instance Core.FromXML CapacityReservationGroup where
  parseXML x =
    CapacityReservationGroup'
      Core.<$> (x Core..@? "groupArn") Core.<*> (x Core..@? "ownerId")
