{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.RestoreWindow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.RestoreWindow
  ( RestoreWindow (..)
  -- * Smart constructor
  , mkRestoreWindow
  -- * Lenses
  , rwEarliestTime
  , rwLatestTime
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Earliest and latest time an instance can be restored to:
--
-- /See:/ 'mkRestoreWindow' smart constructor.
data RestoreWindow = RestoreWindow'
  { earliestTime :: Core.Maybe Core.UTCTime
    -- ^ The earliest time you can restore an instance to.
  , latestTime :: Core.Maybe Core.UTCTime
    -- ^ The latest time you can restore an instance to.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RestoreWindow' value with any optional fields omitted.
mkRestoreWindow
    :: RestoreWindow
mkRestoreWindow
  = RestoreWindow'{earliestTime = Core.Nothing,
                   latestTime = Core.Nothing}

-- | The earliest time you can restore an instance to.
--
-- /Note:/ Consider using 'earliestTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwEarliestTime :: Lens.Lens' RestoreWindow (Core.Maybe Core.UTCTime)
rwEarliestTime = Lens.field @"earliestTime"
{-# INLINEABLE rwEarliestTime #-}
{-# DEPRECATED earliestTime "Use generic-lens or generic-optics with 'earliestTime' instead"  #-}

-- | The latest time you can restore an instance to.
--
-- /Note:/ Consider using 'latestTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwLatestTime :: Lens.Lens' RestoreWindow (Core.Maybe Core.UTCTime)
rwLatestTime = Lens.field @"latestTime"
{-# INLINEABLE rwLatestTime #-}
{-# DEPRECATED latestTime "Use generic-lens or generic-optics with 'latestTime' instead"  #-}

instance Core.FromXML RestoreWindow where
        parseXML x
          = RestoreWindow' Core.<$>
              (x Core..@? "EarliestTime") Core.<*> x Core..@? "LatestTime"
