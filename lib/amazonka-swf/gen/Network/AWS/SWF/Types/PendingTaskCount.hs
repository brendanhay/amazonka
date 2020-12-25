{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.PendingTaskCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.PendingTaskCount
  ( PendingTaskCount (..),

    -- * Smart constructor
    mkPendingTaskCount,

    -- * Lenses
    ptcCount,
    ptcTruncated,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the count of tasks in a task list.
--
-- /See:/ 'mkPendingTaskCount' smart constructor.
data PendingTaskCount = PendingTaskCount'
  { -- | The number of tasks in the task list.
    count :: Core.Natural,
    -- | If set to true, indicates that the actual count was more than the maximum supported by this API and the count returned is the truncated value.
    truncated :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PendingTaskCount' value with any optional fields omitted.
mkPendingTaskCount ::
  -- | 'count'
  Core.Natural ->
  PendingTaskCount
mkPendingTaskCount count =
  PendingTaskCount' {count, truncated = Core.Nothing}

-- | The number of tasks in the task list.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptcCount :: Lens.Lens' PendingTaskCount Core.Natural
ptcCount = Lens.field @"count"
{-# DEPRECATED ptcCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | If set to true, indicates that the actual count was more than the maximum supported by this API and the count returned is the truncated value.
--
-- /Note:/ Consider using 'truncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptcTruncated :: Lens.Lens' PendingTaskCount (Core.Maybe Core.Bool)
ptcTruncated = Lens.field @"truncated"
{-# DEPRECATED ptcTruncated "Use generic-lens or generic-optics with 'truncated' instead." #-}

instance Core.FromJSON PendingTaskCount where
  parseJSON =
    Core.withObject "PendingTaskCount" Core.$
      \x ->
        PendingTaskCount'
          Core.<$> (x Core..: "count") Core.<*> (x Core..:? "truncated")
