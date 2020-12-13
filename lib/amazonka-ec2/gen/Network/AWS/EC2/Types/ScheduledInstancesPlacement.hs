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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the placement for a Scheduled Instance.
--
-- /See:/ 'mkScheduledInstancesPlacement' smart constructor.
data ScheduledInstancesPlacement = ScheduledInstancesPlacement'
  { -- | The Availability Zone.
    availabilityZone :: Lude.Maybe Lude.Text,
    -- | The name of the placement group.
    groupName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScheduledInstancesPlacement' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The Availability Zone.
-- * 'groupName' - The name of the placement group.
mkScheduledInstancesPlacement ::
  ScheduledInstancesPlacement
mkScheduledInstancesPlacement =
  ScheduledInstancesPlacement'
    { availabilityZone = Lude.Nothing,
      groupName = Lude.Nothing
    }

-- | The Availability Zone.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipAvailabilityZone :: Lens.Lens' ScheduledInstancesPlacement (Lude.Maybe Lude.Text)
sipAvailabilityZone = Lens.lens (availabilityZone :: ScheduledInstancesPlacement -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: ScheduledInstancesPlacement)
{-# DEPRECATED sipAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The name of the placement group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipGroupName :: Lens.Lens' ScheduledInstancesPlacement (Lude.Maybe Lude.Text)
sipGroupName = Lens.lens (groupName :: ScheduledInstancesPlacement -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: ScheduledInstancesPlacement)
{-# DEPRECATED sipGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.ToQuery ScheduledInstancesPlacement where
  toQuery ScheduledInstancesPlacement' {..} =
    Lude.mconcat
      [ "AvailabilityZone" Lude.=: availabilityZone,
        "GroupName" Lude.=: groupName
      ]
