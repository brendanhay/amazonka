{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.CapacityReservationGroup
  ( CapacityReservationGroup (..)
  -- * Smart constructor
  , mkCapacityReservationGroup
  -- * Lenses
  , crgGroupArn
  , crgOwnerId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a resource group to which a Capacity Reservation has been added.
--
-- /See:/ 'mkCapacityReservationGroup' smart constructor.
data CapacityReservationGroup = CapacityReservationGroup'
  { groupArn :: Core.Maybe Core.Text
    -- ^ The ARN of the resource group.
  , ownerId :: Core.Maybe Core.Text
    -- ^ The ID of the AWS account that owns the resource group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CapacityReservationGroup' value with any optional fields omitted.
mkCapacityReservationGroup
    :: CapacityReservationGroup
mkCapacityReservationGroup
  = CapacityReservationGroup'{groupArn = Core.Nothing,
                              ownerId = Core.Nothing}

-- | The ARN of the resource group.
--
-- /Note:/ Consider using 'groupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgGroupArn :: Lens.Lens' CapacityReservationGroup (Core.Maybe Core.Text)
crgGroupArn = Lens.field @"groupArn"
{-# INLINEABLE crgGroupArn #-}
{-# DEPRECATED groupArn "Use generic-lens or generic-optics with 'groupArn' instead"  #-}

-- | The ID of the AWS account that owns the resource group.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgOwnerId :: Lens.Lens' CapacityReservationGroup (Core.Maybe Core.Text)
crgOwnerId = Lens.field @"ownerId"
{-# INLINEABLE crgOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

instance Core.FromXML CapacityReservationGroup where
        parseXML x
          = CapacityReservationGroup' Core.<$>
              (x Core..@? "groupArn") Core.<*> x Core..@? "ownerId"
