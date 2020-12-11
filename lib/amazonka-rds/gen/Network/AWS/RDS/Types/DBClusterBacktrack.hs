-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBClusterBacktrack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBClusterBacktrack
  ( DBClusterBacktrack (..),

    -- * Smart constructor
    mkDBClusterBacktrack,

    -- * Lenses
    dcbStatus,
    dcbBacktrackIdentifier,
    dcbBacktrackTo,
    dcbDBClusterIdentifier,
    dcbBacktrackedFrom,
    dcbBacktrackRequestCreationTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This data type is used as a response element in the @DescribeDBClusterBacktracks@ action.
--
-- /See:/ 'mkDBClusterBacktrack' smart constructor.
data DBClusterBacktrack = DBClusterBacktrack'
  { status ::
      Lude.Maybe Lude.Text,
    backtrackIdentifier :: Lude.Maybe Lude.Text,
    backtrackTo :: Lude.Maybe Lude.ISO8601,
    dbClusterIdentifier :: Lude.Maybe Lude.Text,
    backtrackedFrom :: Lude.Maybe Lude.ISO8601,
    backtrackRequestCreationTime ::
      Lude.Maybe Lude.ISO8601
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DBClusterBacktrack' with the minimum fields required to make a request.
--
-- * 'backtrackIdentifier' - Contains the backtrack identifier.
-- * 'backtrackRequestCreationTime' - The timestamp of the time at which the backtrack was requested.
-- * 'backtrackTo' - The timestamp of the time to which the DB cluster was backtracked.
-- * 'backtrackedFrom' - The timestamp of the time from which the DB cluster was backtracked.
-- * 'dbClusterIdentifier' - Contains a user-supplied DB cluster identifier. This identifier is the unique key that identifies a DB cluster.
-- * 'status' - The status of the backtrack. This property returns one of the following values:
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
mkDBClusterBacktrack ::
  DBClusterBacktrack
mkDBClusterBacktrack =
  DBClusterBacktrack'
    { status = Lude.Nothing,
      backtrackIdentifier = Lude.Nothing,
      backtrackTo = Lude.Nothing,
      dbClusterIdentifier = Lude.Nothing,
      backtrackedFrom = Lude.Nothing,
      backtrackRequestCreationTime = Lude.Nothing
    }

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
dcbStatus :: Lens.Lens' DBClusterBacktrack (Lude.Maybe Lude.Text)
dcbStatus = Lens.lens (status :: DBClusterBacktrack -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: DBClusterBacktrack)
{-# DEPRECATED dcbStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Contains the backtrack identifier.
--
-- /Note:/ Consider using 'backtrackIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbBacktrackIdentifier :: Lens.Lens' DBClusterBacktrack (Lude.Maybe Lude.Text)
dcbBacktrackIdentifier = Lens.lens (backtrackIdentifier :: DBClusterBacktrack -> Lude.Maybe Lude.Text) (\s a -> s {backtrackIdentifier = a} :: DBClusterBacktrack)
{-# DEPRECATED dcbBacktrackIdentifier "Use generic-lens or generic-optics with 'backtrackIdentifier' instead." #-}

-- | The timestamp of the time to which the DB cluster was backtracked.
--
-- /Note:/ Consider using 'backtrackTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbBacktrackTo :: Lens.Lens' DBClusterBacktrack (Lude.Maybe Lude.ISO8601)
dcbBacktrackTo = Lens.lens (backtrackTo :: DBClusterBacktrack -> Lude.Maybe Lude.ISO8601) (\s a -> s {backtrackTo = a} :: DBClusterBacktrack)
{-# DEPRECATED dcbBacktrackTo "Use generic-lens or generic-optics with 'backtrackTo' instead." #-}

-- | Contains a user-supplied DB cluster identifier. This identifier is the unique key that identifies a DB cluster.
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbDBClusterIdentifier :: Lens.Lens' DBClusterBacktrack (Lude.Maybe Lude.Text)
dcbDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: DBClusterBacktrack -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: DBClusterBacktrack)
{-# DEPRECATED dcbDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

-- | The timestamp of the time from which the DB cluster was backtracked.
--
-- /Note:/ Consider using 'backtrackedFrom' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbBacktrackedFrom :: Lens.Lens' DBClusterBacktrack (Lude.Maybe Lude.ISO8601)
dcbBacktrackedFrom = Lens.lens (backtrackedFrom :: DBClusterBacktrack -> Lude.Maybe Lude.ISO8601) (\s a -> s {backtrackedFrom = a} :: DBClusterBacktrack)
{-# DEPRECATED dcbBacktrackedFrom "Use generic-lens or generic-optics with 'backtrackedFrom' instead." #-}

-- | The timestamp of the time at which the backtrack was requested.
--
-- /Note:/ Consider using 'backtrackRequestCreationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbBacktrackRequestCreationTime :: Lens.Lens' DBClusterBacktrack (Lude.Maybe Lude.ISO8601)
dcbBacktrackRequestCreationTime = Lens.lens (backtrackRequestCreationTime :: DBClusterBacktrack -> Lude.Maybe Lude.ISO8601) (\s a -> s {backtrackRequestCreationTime = a} :: DBClusterBacktrack)
{-# DEPRECATED dcbBacktrackRequestCreationTime "Use generic-lens or generic-optics with 'backtrackRequestCreationTime' instead." #-}

instance Lude.FromXML DBClusterBacktrack where
  parseXML x =
    DBClusterBacktrack'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "BacktrackIdentifier")
      Lude.<*> (x Lude..@? "BacktrackTo")
      Lude.<*> (x Lude..@? "DBClusterIdentifier")
      Lude.<*> (x Lude..@? "BacktrackedFrom")
      Lude.<*> (x Lude..@? "BacktrackRequestCreationTime")
