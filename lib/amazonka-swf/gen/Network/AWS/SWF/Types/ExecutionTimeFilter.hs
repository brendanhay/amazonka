{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ExecutionTimeFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ExecutionTimeFilter
  ( ExecutionTimeFilter (..),

    -- * Smart constructor
    mkExecutionTimeFilter,

    -- * Lenses
    etfOldestDate,
    etfLatestDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Used to filter the workflow executions in visibility APIs by various time-based rules. Each parameter, if specified, defines a rule that must be satisfied by each returned query result. The parameter values are in the <https://en.wikipedia.org/wiki/Unix_time Unix Time format> . For example: @"oldestDate": 1325376070.@
--
-- /See:/ 'mkExecutionTimeFilter' smart constructor.
data ExecutionTimeFilter = ExecutionTimeFilter'
  { -- | Specifies the oldest start or close date and time to return.
    oldestDate :: Core.NominalDiffTime,
    -- | Specifies the latest start or close date and time to return.
    latestDate :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ExecutionTimeFilter' value with any optional fields omitted.
mkExecutionTimeFilter ::
  -- | 'oldestDate'
  Core.NominalDiffTime ->
  ExecutionTimeFilter
mkExecutionTimeFilter oldestDate =
  ExecutionTimeFilter' {oldestDate, latestDate = Core.Nothing}

-- | Specifies the oldest start or close date and time to return.
--
-- /Note:/ Consider using 'oldestDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etfOldestDate :: Lens.Lens' ExecutionTimeFilter Core.NominalDiffTime
etfOldestDate = Lens.field @"oldestDate"
{-# DEPRECATED etfOldestDate "Use generic-lens or generic-optics with 'oldestDate' instead." #-}

-- | Specifies the latest start or close date and time to return.
--
-- /Note:/ Consider using 'latestDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etfLatestDate :: Lens.Lens' ExecutionTimeFilter (Core.Maybe Core.NominalDiffTime)
etfLatestDate = Lens.field @"latestDate"
{-# DEPRECATED etfLatestDate "Use generic-lens or generic-optics with 'latestDate' instead." #-}

instance Core.FromJSON ExecutionTimeFilter where
  toJSON ExecutionTimeFilter {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("oldestDate" Core..= oldestDate),
            ("latestDate" Core..=) Core.<$> latestDate
          ]
      )
