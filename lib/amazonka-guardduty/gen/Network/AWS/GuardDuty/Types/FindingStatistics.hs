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

import qualified Network.AWS.GuardDuty.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about finding statistics.
--
-- /See:/ 'mkFindingStatistics' smart constructor.
newtype FindingStatistics = FindingStatistics'
  { -- | Represents a map of severity to count statistics for a set of findings.
    countBySeverity :: Core.Maybe (Core.HashMap Types.String Core.Int)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'FindingStatistics' value with any optional fields omitted.
mkFindingStatistics ::
  FindingStatistics
mkFindingStatistics =
  FindingStatistics' {countBySeverity = Core.Nothing}

-- | Represents a map of severity to count statistics for a set of findings.
--
-- /Note:/ Consider using 'countBySeverity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsCountBySeverity :: Lens.Lens' FindingStatistics (Core.Maybe (Core.HashMap Types.String Core.Int))
fsCountBySeverity = Lens.field @"countBySeverity"
{-# DEPRECATED fsCountBySeverity "Use generic-lens or generic-optics with 'countBySeverity' instead." #-}

instance Core.FromJSON FindingStatistics where
  parseJSON =
    Core.withObject "FindingStatistics" Core.$
      \x -> FindingStatistics' Core.<$> (x Core..:? "countBySeverity")
