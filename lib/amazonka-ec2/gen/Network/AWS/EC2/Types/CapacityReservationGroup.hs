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
    crgOwnerId,
    crgGroupARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a resource group to which a Capacity Reservation has been added.
--
-- /See:/ 'mkCapacityReservationGroup' smart constructor.
data CapacityReservationGroup = CapacityReservationGroup'
  { -- | The ID of the AWS account that owns the resource group.
    ownerId :: Lude.Maybe Lude.Text,
    -- | The ARN of the resource group.
    groupARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CapacityReservationGroup' with the minimum fields required to make a request.
--
-- * 'ownerId' - The ID of the AWS account that owns the resource group.
-- * 'groupARN' - The ARN of the resource group.
mkCapacityReservationGroup ::
  CapacityReservationGroup
mkCapacityReservationGroup =
  CapacityReservationGroup'
    { ownerId = Lude.Nothing,
      groupARN = Lude.Nothing
    }

-- | The ID of the AWS account that owns the resource group.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgOwnerId :: Lens.Lens' CapacityReservationGroup (Lude.Maybe Lude.Text)
crgOwnerId = Lens.lens (ownerId :: CapacityReservationGroup -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: CapacityReservationGroup)
{-# DEPRECATED crgOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The ARN of the resource group.
--
-- /Note:/ Consider using 'groupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgGroupARN :: Lens.Lens' CapacityReservationGroup (Lude.Maybe Lude.Text)
crgGroupARN = Lens.lens (groupARN :: CapacityReservationGroup -> Lude.Maybe Lude.Text) (\s a -> s {groupARN = a} :: CapacityReservationGroup)
{-# DEPRECATED crgGroupARN "Use generic-lens or generic-optics with 'groupARN' instead." #-}

instance Lude.FromXML CapacityReservationGroup where
  parseXML x =
    CapacityReservationGroup'
      Lude.<$> (x Lude..@? "ownerId") Lude.<*> (x Lude..@? "groupArn")
