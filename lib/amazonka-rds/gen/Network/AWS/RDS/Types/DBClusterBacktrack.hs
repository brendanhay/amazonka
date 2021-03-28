{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBClusterBacktrack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.DBClusterBacktrack
  ( DBClusterBacktrack (..)
  -- * Smart constructor
  , mkDBClusterBacktrack
  -- * Lenses
  , dbcbBacktrackIdentifier
  , dbcbBacktrackRequestCreationTime
  , dbcbBacktrackTo
  , dbcbBacktrackedFrom
  , dbcbDBClusterIdentifier
  , dbcbStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This data type is used as a response element in the @DescribeDBClusterBacktracks@ action.
--
-- /See:/ 'mkDBClusterBacktrack' smart constructor.
data DBClusterBacktrack = DBClusterBacktrack'
  { backtrackIdentifier :: Core.Maybe Core.Text
    -- ^ Contains the backtrack identifier.
  , backtrackRequestCreationTime :: Core.Maybe Core.UTCTime
    -- ^ The timestamp of the time at which the backtrack was requested.
  , backtrackTo :: Core.Maybe Core.UTCTime
    -- ^ The timestamp of the time to which the DB cluster was backtracked.
  , backtrackedFrom :: Core.Maybe Core.UTCTime
    -- ^ The timestamp of the time from which the DB cluster was backtracked.
  , dBClusterIdentifier :: Core.Maybe Core.Text
    -- ^ Contains a user-supplied DB cluster identifier. This identifier is the unique key that identifies a DB cluster.
  , status :: Core.Maybe Core.Text
    -- ^ The status of the backtrack. This property returns one of the following values:
--
--
--     * @applying@ - The backtrack is currently being applied to or rolled back from the DB cluster.
--
--
--     * @completed@ - The backtrack has successfully been applied to or rolled back from the DB cluster.
--
--
--     * @failed@ - An error occurred while the backtrack was applied to or rolled back from the DB cluster.
--
--
--     * @pending@ - The backtrack is currently pending application to or rollback from the DB cluster.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DBClusterBacktrack' value with any optional fields omitted.
mkDBClusterBacktrack
    :: DBClusterBacktrack
mkDBClusterBacktrack
  = DBClusterBacktrack'{backtrackIdentifier = Core.Nothing,
                        backtrackRequestCreationTime = Core.Nothing,
                        backtrackTo = Core.Nothing, backtrackedFrom = Core.Nothing,
                        dBClusterIdentifier = Core.Nothing, status = Core.Nothing}

-- | Contains the backtrack identifier.
--
-- /Note:/ Consider using 'backtrackIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcbBacktrackIdentifier :: Lens.Lens' DBClusterBacktrack (Core.Maybe Core.Text)
dbcbBacktrackIdentifier = Lens.field @"backtrackIdentifier"
{-# INLINEABLE dbcbBacktrackIdentifier #-}
{-# DEPRECATED backtrackIdentifier "Use generic-lens or generic-optics with 'backtrackIdentifier' instead"  #-}

-- | The timestamp of the time at which the backtrack was requested.
--
-- /Note:/ Consider using 'backtrackRequestCreationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcbBacktrackRequestCreationTime :: Lens.Lens' DBClusterBacktrack (Core.Maybe Core.UTCTime)
dbcbBacktrackRequestCreationTime = Lens.field @"backtrackRequestCreationTime"
{-# INLINEABLE dbcbBacktrackRequestCreationTime #-}
{-# DEPRECATED backtrackRequestCreationTime "Use generic-lens or generic-optics with 'backtrackRequestCreationTime' instead"  #-}

-- | The timestamp of the time to which the DB cluster was backtracked.
--
-- /Note:/ Consider using 'backtrackTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcbBacktrackTo :: Lens.Lens' DBClusterBacktrack (Core.Maybe Core.UTCTime)
dbcbBacktrackTo = Lens.field @"backtrackTo"
{-# INLINEABLE dbcbBacktrackTo #-}
{-# DEPRECATED backtrackTo "Use generic-lens or generic-optics with 'backtrackTo' instead"  #-}

-- | The timestamp of the time from which the DB cluster was backtracked.
--
-- /Note:/ Consider using 'backtrackedFrom' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcbBacktrackedFrom :: Lens.Lens' DBClusterBacktrack (Core.Maybe Core.UTCTime)
dbcbBacktrackedFrom = Lens.field @"backtrackedFrom"
{-# INLINEABLE dbcbBacktrackedFrom #-}
{-# DEPRECATED backtrackedFrom "Use generic-lens or generic-optics with 'backtrackedFrom' instead"  #-}

-- | Contains a user-supplied DB cluster identifier. This identifier is the unique key that identifies a DB cluster.
--
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcbDBClusterIdentifier :: Lens.Lens' DBClusterBacktrack (Core.Maybe Core.Text)
dbcbDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# INLINEABLE dbcbDBClusterIdentifier #-}
{-# DEPRECATED dBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead"  #-}

-- | The status of the backtrack. This property returns one of the following values:
--
--
--     * @applying@ - The backtrack is currently being applied to or rolled back from the DB cluster.
--
--
--     * @completed@ - The backtrack has successfully been applied to or rolled back from the DB cluster.
--
--
--     * @failed@ - An error occurred while the backtrack was applied to or rolled back from the DB cluster.
--
--
--     * @pending@ - The backtrack is currently pending application to or rollback from the DB cluster.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcbStatus :: Lens.Lens' DBClusterBacktrack (Core.Maybe Core.Text)
dbcbStatus = Lens.field @"status"
{-# INLINEABLE dbcbStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromXML DBClusterBacktrack where
        parseXML x
          = DBClusterBacktrack' Core.<$>
              (x Core..@? "BacktrackIdentifier") Core.<*>
                x Core..@? "BacktrackRequestCreationTime"
                Core.<*> x Core..@? "BacktrackTo"
                Core.<*> x Core..@? "BacktrackedFrom"
                Core.<*> x Core..@? "DBClusterIdentifier"
                Core.<*> x Core..@? "Status"
