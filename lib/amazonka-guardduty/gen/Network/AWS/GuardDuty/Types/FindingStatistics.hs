{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.FindingStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.FindingStatistics
  ( FindingStatistics (..),

    -- * Smart constructor
    mkFindingStatistics,

    -- * Lenses
    fsCountBySeverity,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about finding statistics.
--
-- /See:/ 'mkFindingStatistics' smart constructor.
newtype FindingStatistics = FindingStatistics'
  { countBySeverity ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Int))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FindingStatistics' with the minimum fields required to make a request.
--
-- * 'countBySeverity' - Represents a map of severity to count statistics for a set of findings.
mkFindingStatistics ::
  FindingStatistics
mkFindingStatistics =
  FindingStatistics' {countBySeverity = Lude.Nothing}

-- | Represents a map of severity to count statistics for a set of findings.
--
-- /Note:/ Consider using 'countBySeverity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsCountBySeverity :: Lens.Lens' FindingStatistics (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Int)))
fsCountBySeverity = Lens.lens (countBySeverity :: FindingStatistics -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Int))) (\s a -> s {countBySeverity = a} :: FindingStatistics)
{-# DEPRECATED fsCountBySeverity "Use generic-lens or generic-optics with 'countBySeverity' instead." #-}

instance Lude.FromJSON FindingStatistics where
  parseJSON =
    Lude.withObject
      "FindingStatistics"
      ( \x ->
          FindingStatistics'
            Lude.<$> (x Lude..:? "countBySeverity" Lude..!= Lude.mempty)
      )
