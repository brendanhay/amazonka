{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBClusterBacktrack
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBClusterBacktrack where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | This data type is used as a response element in the
-- @DescribeDBClusterBacktracks@ action.
--
-- /See:/ 'newDBClusterBacktrack' smart constructor.
data DBClusterBacktrack = DBClusterBacktrack'
  { -- | The status of the backtrack. This property returns one of the following
    -- values:
    --
    -- -   @applying@ - The backtrack is currently being applied to or rolled
    --     back from the DB cluster.
    --
    -- -   @completed@ - The backtrack has successfully been applied to or
    --     rolled back from the DB cluster.
    --
    -- -   @failed@ - An error occurred while the backtrack was applied to or
    --     rolled back from the DB cluster.
    --
    -- -   @pending@ - The backtrack is currently pending application to or
    --     rollback from the DB cluster.
    status :: Prelude.Maybe Prelude.Text,
    -- | Contains the backtrack identifier.
    backtrackIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of the time to which the DB cluster was backtracked.
    backtrackTo :: Prelude.Maybe Core.ISO8601,
    -- | Contains a user-supplied DB cluster identifier. This identifier is the
    -- unique key that identifies a DB cluster.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of the time at which the backtrack was requested.
    backtrackRequestCreationTime :: Prelude.Maybe Core.ISO8601,
    -- | The timestamp of the time from which the DB cluster was backtracked.
    backtrackedFrom :: Prelude.Maybe Core.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBClusterBacktrack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'dbClusterBacktrack_status' - The status of the backtrack. This property returns one of the following
-- values:
--
-- -   @applying@ - The backtrack is currently being applied to or rolled
--     back from the DB cluster.
--
-- -   @completed@ - The backtrack has successfully been applied to or
--     rolled back from the DB cluster.
--
-- -   @failed@ - An error occurred while the backtrack was applied to or
--     rolled back from the DB cluster.
--
-- -   @pending@ - The backtrack is currently pending application to or
--     rollback from the DB cluster.
--
-- 'backtrackIdentifier', 'dbClusterBacktrack_backtrackIdentifier' - Contains the backtrack identifier.
--
-- 'backtrackTo', 'dbClusterBacktrack_backtrackTo' - The timestamp of the time to which the DB cluster was backtracked.
--
-- 'dbClusterIdentifier', 'dbClusterBacktrack_dbClusterIdentifier' - Contains a user-supplied DB cluster identifier. This identifier is the
-- unique key that identifies a DB cluster.
--
-- 'backtrackRequestCreationTime', 'dbClusterBacktrack_backtrackRequestCreationTime' - The timestamp of the time at which the backtrack was requested.
--
-- 'backtrackedFrom', 'dbClusterBacktrack_backtrackedFrom' - The timestamp of the time from which the DB cluster was backtracked.
newDBClusterBacktrack ::
  DBClusterBacktrack
newDBClusterBacktrack =
  DBClusterBacktrack'
    { status = Prelude.Nothing,
      backtrackIdentifier = Prelude.Nothing,
      backtrackTo = Prelude.Nothing,
      dbClusterIdentifier = Prelude.Nothing,
      backtrackRequestCreationTime = Prelude.Nothing,
      backtrackedFrom = Prelude.Nothing
    }

-- | The status of the backtrack. This property returns one of the following
-- values:
--
-- -   @applying@ - The backtrack is currently being applied to or rolled
--     back from the DB cluster.
--
-- -   @completed@ - The backtrack has successfully been applied to or
--     rolled back from the DB cluster.
--
-- -   @failed@ - An error occurred while the backtrack was applied to or
--     rolled back from the DB cluster.
--
-- -   @pending@ - The backtrack is currently pending application to or
--     rollback from the DB cluster.
dbClusterBacktrack_status :: Lens.Lens' DBClusterBacktrack (Prelude.Maybe Prelude.Text)
dbClusterBacktrack_status = Lens.lens (\DBClusterBacktrack' {status} -> status) (\s@DBClusterBacktrack' {} a -> s {status = a} :: DBClusterBacktrack)

-- | Contains the backtrack identifier.
dbClusterBacktrack_backtrackIdentifier :: Lens.Lens' DBClusterBacktrack (Prelude.Maybe Prelude.Text)
dbClusterBacktrack_backtrackIdentifier = Lens.lens (\DBClusterBacktrack' {backtrackIdentifier} -> backtrackIdentifier) (\s@DBClusterBacktrack' {} a -> s {backtrackIdentifier = a} :: DBClusterBacktrack)

-- | The timestamp of the time to which the DB cluster was backtracked.
dbClusterBacktrack_backtrackTo :: Lens.Lens' DBClusterBacktrack (Prelude.Maybe Prelude.UTCTime)
dbClusterBacktrack_backtrackTo = Lens.lens (\DBClusterBacktrack' {backtrackTo} -> backtrackTo) (\s@DBClusterBacktrack' {} a -> s {backtrackTo = a} :: DBClusterBacktrack) Prelude.. Lens.mapping Core._Time

-- | Contains a user-supplied DB cluster identifier. This identifier is the
-- unique key that identifies a DB cluster.
dbClusterBacktrack_dbClusterIdentifier :: Lens.Lens' DBClusterBacktrack (Prelude.Maybe Prelude.Text)
dbClusterBacktrack_dbClusterIdentifier = Lens.lens (\DBClusterBacktrack' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@DBClusterBacktrack' {} a -> s {dbClusterIdentifier = a} :: DBClusterBacktrack)

-- | The timestamp of the time at which the backtrack was requested.
dbClusterBacktrack_backtrackRequestCreationTime :: Lens.Lens' DBClusterBacktrack (Prelude.Maybe Prelude.UTCTime)
dbClusterBacktrack_backtrackRequestCreationTime = Lens.lens (\DBClusterBacktrack' {backtrackRequestCreationTime} -> backtrackRequestCreationTime) (\s@DBClusterBacktrack' {} a -> s {backtrackRequestCreationTime = a} :: DBClusterBacktrack) Prelude.. Lens.mapping Core._Time

-- | The timestamp of the time from which the DB cluster was backtracked.
dbClusterBacktrack_backtrackedFrom :: Lens.Lens' DBClusterBacktrack (Prelude.Maybe Prelude.UTCTime)
dbClusterBacktrack_backtrackedFrom = Lens.lens (\DBClusterBacktrack' {backtrackedFrom} -> backtrackedFrom) (\s@DBClusterBacktrack' {} a -> s {backtrackedFrom = a} :: DBClusterBacktrack) Prelude.. Lens.mapping Core._Time

instance Core.FromXML DBClusterBacktrack where
  parseXML x =
    DBClusterBacktrack'
      Prelude.<$> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "BacktrackIdentifier")
      Prelude.<*> (x Core..@? "BacktrackTo")
      Prelude.<*> (x Core..@? "DBClusterIdentifier")
      Prelude.<*> (x Core..@? "BacktrackRequestCreationTime")
      Prelude.<*> (x Core..@? "BacktrackedFrom")

instance Prelude.Hashable DBClusterBacktrack

instance Prelude.NFData DBClusterBacktrack
