{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ReservationUtilizationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ReservationUtilizationGroup
  ( ReservationUtilizationGroup (..),

    -- * Smart constructor
    mkReservationUtilizationGroup,

    -- * Lenses
    rugValue,
    rugKey,
    rugAttributes,
    rugUtilization,
  )
where

import Network.AWS.CostExplorer.Types.ReservationAggregates
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A group of reservations that share a set of attributes.
--
-- /See:/ 'mkReservationUtilizationGroup' smart constructor.
data ReservationUtilizationGroup = ReservationUtilizationGroup'
  { -- | The value of a specific reservation attribute.
    value :: Lude.Maybe Lude.Text,
    -- | The key for a specific reservation attribute.
    key :: Lude.Maybe Lude.Text,
    -- | The attributes for this group of reservations.
    attributes :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | How much you used this group of reservations.
    utilization :: Lude.Maybe ReservationAggregates
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReservationUtilizationGroup' with the minimum fields required to make a request.
--
-- * 'value' - The value of a specific reservation attribute.
-- * 'key' - The key for a specific reservation attribute.
-- * 'attributes' - The attributes for this group of reservations.
-- * 'utilization' - How much you used this group of reservations.
mkReservationUtilizationGroup ::
  ReservationUtilizationGroup
mkReservationUtilizationGroup =
  ReservationUtilizationGroup'
    { value = Lude.Nothing,
      key = Lude.Nothing,
      attributes = Lude.Nothing,
      utilization = Lude.Nothing
    }

-- | The value of a specific reservation attribute.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rugValue :: Lens.Lens' ReservationUtilizationGroup (Lude.Maybe Lude.Text)
rugValue = Lens.lens (value :: ReservationUtilizationGroup -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: ReservationUtilizationGroup)
{-# DEPRECATED rugValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The key for a specific reservation attribute.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rugKey :: Lens.Lens' ReservationUtilizationGroup (Lude.Maybe Lude.Text)
rugKey = Lens.lens (key :: ReservationUtilizationGroup -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: ReservationUtilizationGroup)
{-# DEPRECATED rugKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The attributes for this group of reservations.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rugAttributes :: Lens.Lens' ReservationUtilizationGroup (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
rugAttributes = Lens.lens (attributes :: ReservationUtilizationGroup -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: ReservationUtilizationGroup)
{-# DEPRECATED rugAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | How much you used this group of reservations.
--
-- /Note:/ Consider using 'utilization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rugUtilization :: Lens.Lens' ReservationUtilizationGroup (Lude.Maybe ReservationAggregates)
rugUtilization = Lens.lens (utilization :: ReservationUtilizationGroup -> Lude.Maybe ReservationAggregates) (\s a -> s {utilization = a} :: ReservationUtilizationGroup)
{-# DEPRECATED rugUtilization "Use generic-lens or generic-optics with 'utilization' instead." #-}

instance Lude.FromJSON ReservationUtilizationGroup where
  parseJSON =
    Lude.withObject
      "ReservationUtilizationGroup"
      ( \x ->
          ReservationUtilizationGroup'
            Lude.<$> (x Lude..:? "Value")
            Lude.<*> (x Lude..:? "Key")
            Lude.<*> (x Lude..:? "Attributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Utilization")
      )
