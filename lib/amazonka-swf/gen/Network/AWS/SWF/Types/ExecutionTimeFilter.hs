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
    etfLatestDate,
    etfOldestDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Used to filter the workflow executions in visibility APIs by various time-based rules. Each parameter, if specified, defines a rule that must be satisfied by each returned query result. The parameter values are in the <https://en.wikipedia.org/wiki/Unix_time Unix Time format> . For example: @"oldestDate": 1325376070.@
--
-- /See:/ 'mkExecutionTimeFilter' smart constructor.
data ExecutionTimeFilter = ExecutionTimeFilter'
  { -- | Specifies the latest start or close date and time to return.
    latestDate :: Lude.Maybe Lude.Timestamp,
    -- | Specifies the oldest start or close date and time to return.
    oldestDate :: Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExecutionTimeFilter' with the minimum fields required to make a request.
--
-- * 'latestDate' - Specifies the latest start or close date and time to return.
-- * 'oldestDate' - Specifies the oldest start or close date and time to return.
mkExecutionTimeFilter ::
  -- | 'oldestDate'
  Lude.Timestamp ->
  ExecutionTimeFilter
mkExecutionTimeFilter pOldestDate_ =
  ExecutionTimeFilter'
    { latestDate = Lude.Nothing,
      oldestDate = pOldestDate_
    }

-- | Specifies the latest start or close date and time to return.
--
-- /Note:/ Consider using 'latestDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etfLatestDate :: Lens.Lens' ExecutionTimeFilter (Lude.Maybe Lude.Timestamp)
etfLatestDate = Lens.lens (latestDate :: ExecutionTimeFilter -> Lude.Maybe Lude.Timestamp) (\s a -> s {latestDate = a} :: ExecutionTimeFilter)
{-# DEPRECATED etfLatestDate "Use generic-lens or generic-optics with 'latestDate' instead." #-}

-- | Specifies the oldest start or close date and time to return.
--
-- /Note:/ Consider using 'oldestDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etfOldestDate :: Lens.Lens' ExecutionTimeFilter Lude.Timestamp
etfOldestDate = Lens.lens (oldestDate :: ExecutionTimeFilter -> Lude.Timestamp) (\s a -> s {oldestDate = a} :: ExecutionTimeFilter)
{-# DEPRECATED etfOldestDate "Use generic-lens or generic-optics with 'oldestDate' instead." #-}

instance Lude.ToJSON ExecutionTimeFilter where
  toJSON ExecutionTimeFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("latestDate" Lude..=) Lude.<$> latestDate,
            Lude.Just ("oldestDate" Lude..= oldestDate)
          ]
      )
