{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesPlacement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstancesPlacement
  ( ScheduledInstancesPlacement (..),

    -- * Smart constructor
    mkScheduledInstancesPlacement,

    -- * Lenses
    sipAvailabilityZone,
    sipGroupName,
  )
where

import qualified Network.AWS.EC2.Types.GroupName as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the placement for a Scheduled Instance.
--
-- /See:/ 'mkScheduledInstancesPlacement' smart constructor.
data ScheduledInstancesPlacement = ScheduledInstancesPlacement'
  { -- | The Availability Zone.
    availabilityZone :: Core.Maybe Types.String,
    -- | The name of the placement group.
    groupName :: Core.Maybe Types.GroupName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScheduledInstancesPlacement' value with any optional fields omitted.
mkScheduledInstancesPlacement ::
  ScheduledInstancesPlacement
mkScheduledInstancesPlacement =
  ScheduledInstancesPlacement'
    { availabilityZone = Core.Nothing,
      groupName = Core.Nothing
    }

-- | The Availability Zone.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipAvailabilityZone :: Lens.Lens' ScheduledInstancesPlacement (Core.Maybe Types.String)
sipAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED sipAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The name of the placement group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipGroupName :: Lens.Lens' ScheduledInstancesPlacement (Core.Maybe Types.GroupName)
sipGroupName = Lens.field @"groupName"
{-# DEPRECATED sipGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}
