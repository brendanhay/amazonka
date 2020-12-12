{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ReservationCoverageGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ReservationCoverageGroup
  ( ReservationCoverageGroup (..),

    -- * Smart constructor
    mkReservationCoverageGroup,

    -- * Lenses
    rcgCoverage,
    rcgAttributes,
  )
where

import Network.AWS.CostExplorer.Types.Coverage
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A group of reservations that share a set of attributes.
--
-- /See:/ 'mkReservationCoverageGroup' smart constructor.
data ReservationCoverageGroup = ReservationCoverageGroup'
  { coverage ::
      Lude.Maybe Coverage,
    attributes ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReservationCoverageGroup' with the minimum fields required to make a request.
--
-- * 'attributes' - The attributes for this group of reservations.
-- * 'coverage' - How much instance usage this group of reservations covered.
mkReservationCoverageGroup ::
  ReservationCoverageGroup
mkReservationCoverageGroup =
  ReservationCoverageGroup'
    { coverage = Lude.Nothing,
      attributes = Lude.Nothing
    }

-- | How much instance usage this group of reservations covered.
--
-- /Note:/ Consider using 'coverage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcgCoverage :: Lens.Lens' ReservationCoverageGroup (Lude.Maybe Coverage)
rcgCoverage = Lens.lens (coverage :: ReservationCoverageGroup -> Lude.Maybe Coverage) (\s a -> s {coverage = a} :: ReservationCoverageGroup)
{-# DEPRECATED rcgCoverage "Use generic-lens or generic-optics with 'coverage' instead." #-}

-- | The attributes for this group of reservations.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcgAttributes :: Lens.Lens' ReservationCoverageGroup (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
rcgAttributes = Lens.lens (attributes :: ReservationCoverageGroup -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: ReservationCoverageGroup)
{-# DEPRECATED rcgAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Lude.FromJSON ReservationCoverageGroup where
  parseJSON =
    Lude.withObject
      "ReservationCoverageGroup"
      ( \x ->
          ReservationCoverageGroup'
            Lude.<$> (x Lude..:? "Coverage")
            Lude.<*> (x Lude..:? "Attributes" Lude..!= Lude.mempty)
      )
