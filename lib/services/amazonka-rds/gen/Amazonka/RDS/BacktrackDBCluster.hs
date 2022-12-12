{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RDS.BacktrackDBCluster
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Backtracks a DB cluster to a specific time, without creating a new DB
-- cluster.
--
-- For more information on backtracking, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/AuroraMySQL.Managing.Backtrack.html Backtracking an Aurora DB Cluster>
-- in the /Amazon Aurora User Guide/.
--
-- This action applies only to Aurora MySQL DB clusters.
module Amazonka.RDS.BacktrackDBCluster
  ( -- * Creating a Request
    BacktrackDBCluster (..),
    newBacktrackDBCluster,

    -- * Request Lenses
    backtrackDBCluster_force,
    backtrackDBCluster_useEarliestTimeOnPointInTimeUnavailable,
    backtrackDBCluster_dbClusterIdentifier,
    backtrackDBCluster_backtrackTo,

    -- * Destructuring the Response
    DBClusterBacktrack (..),
    newDBClusterBacktrack,

    -- * Response Lenses
    dbClusterBacktrack_backtrackIdentifier,
    dbClusterBacktrack_backtrackRequestCreationTime,
    dbClusterBacktrack_backtrackTo,
    dbClusterBacktrack_backtrackedFrom,
    dbClusterBacktrack_dbClusterIdentifier,
    dbClusterBacktrack_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newBacktrackDBCluster' smart constructor.
data BacktrackDBCluster = BacktrackDBCluster'
  { -- | A value that indicates whether to force the DB cluster to backtrack when
    -- binary logging is enabled. Otherwise, an error occurs when binary
    -- logging is enabled.
    force :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether to backtrack the DB cluster to the
    -- earliest possible backtrack time when /BacktrackTo/ is set to a
    -- timestamp earlier than the earliest backtrack time. When this parameter
    -- is disabled and /BacktrackTo/ is set to a timestamp earlier than the
    -- earliest backtrack time, an error occurs.
    useEarliestTimeOnPointInTimeUnavailable :: Prelude.Maybe Prelude.Bool,
    -- | The DB cluster identifier of the DB cluster to be backtracked. This
    -- parameter is stored as a lowercase string.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 alphanumeric characters or hyphens.
    --
    -- -   First character must be a letter.
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens.
    --
    -- Example: @my-cluster1@
    dbClusterIdentifier :: Prelude.Text,
    -- | The timestamp of the time to backtrack the DB cluster to, specified in
    -- ISO 8601 format. For more information about ISO 8601, see the
    -- <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
    --
    -- If the specified time isn\'t a consistent time for the DB cluster,
    -- Aurora automatically chooses the nearest possible consistent time for
    -- the DB cluster.
    --
    -- Constraints:
    --
    -- -   Must contain a valid ISO 8601 timestamp.
    --
    -- -   Can\'t contain a timestamp set in the future.
    --
    -- Example: @2017-07-08T18:00Z@
    backtrackTo :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BacktrackDBCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'force', 'backtrackDBCluster_force' - A value that indicates whether to force the DB cluster to backtrack when
-- binary logging is enabled. Otherwise, an error occurs when binary
-- logging is enabled.
--
-- 'useEarliestTimeOnPointInTimeUnavailable', 'backtrackDBCluster_useEarliestTimeOnPointInTimeUnavailable' - A value that indicates whether to backtrack the DB cluster to the
-- earliest possible backtrack time when /BacktrackTo/ is set to a
-- timestamp earlier than the earliest backtrack time. When this parameter
-- is disabled and /BacktrackTo/ is set to a timestamp earlier than the
-- earliest backtrack time, an error occurs.
--
-- 'dbClusterIdentifier', 'backtrackDBCluster_dbClusterIdentifier' - The DB cluster identifier of the DB cluster to be backtracked. This
-- parameter is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens.
--
-- -   First character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
--
-- Example: @my-cluster1@
--
-- 'backtrackTo', 'backtrackDBCluster_backtrackTo' - The timestamp of the time to backtrack the DB cluster to, specified in
-- ISO 8601 format. For more information about ISO 8601, see the
-- <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- If the specified time isn\'t a consistent time for the DB cluster,
-- Aurora automatically chooses the nearest possible consistent time for
-- the DB cluster.
--
-- Constraints:
--
-- -   Must contain a valid ISO 8601 timestamp.
--
-- -   Can\'t contain a timestamp set in the future.
--
-- Example: @2017-07-08T18:00Z@
newBacktrackDBCluster ::
  -- | 'dbClusterIdentifier'
  Prelude.Text ->
  -- | 'backtrackTo'
  Prelude.UTCTime ->
  BacktrackDBCluster
newBacktrackDBCluster
  pDBClusterIdentifier_
  pBacktrackTo_ =
    BacktrackDBCluster'
      { force = Prelude.Nothing,
        useEarliestTimeOnPointInTimeUnavailable =
          Prelude.Nothing,
        dbClusterIdentifier = pDBClusterIdentifier_,
        backtrackTo = Data._Time Lens.# pBacktrackTo_
      }

-- | A value that indicates whether to force the DB cluster to backtrack when
-- binary logging is enabled. Otherwise, an error occurs when binary
-- logging is enabled.
backtrackDBCluster_force :: Lens.Lens' BacktrackDBCluster (Prelude.Maybe Prelude.Bool)
backtrackDBCluster_force = Lens.lens (\BacktrackDBCluster' {force} -> force) (\s@BacktrackDBCluster' {} a -> s {force = a} :: BacktrackDBCluster)

-- | A value that indicates whether to backtrack the DB cluster to the
-- earliest possible backtrack time when /BacktrackTo/ is set to a
-- timestamp earlier than the earliest backtrack time. When this parameter
-- is disabled and /BacktrackTo/ is set to a timestamp earlier than the
-- earliest backtrack time, an error occurs.
backtrackDBCluster_useEarliestTimeOnPointInTimeUnavailable :: Lens.Lens' BacktrackDBCluster (Prelude.Maybe Prelude.Bool)
backtrackDBCluster_useEarliestTimeOnPointInTimeUnavailable = Lens.lens (\BacktrackDBCluster' {useEarliestTimeOnPointInTimeUnavailable} -> useEarliestTimeOnPointInTimeUnavailable) (\s@BacktrackDBCluster' {} a -> s {useEarliestTimeOnPointInTimeUnavailable = a} :: BacktrackDBCluster)

-- | The DB cluster identifier of the DB cluster to be backtracked. This
-- parameter is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens.
--
-- -   First character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
--
-- Example: @my-cluster1@
backtrackDBCluster_dbClusterIdentifier :: Lens.Lens' BacktrackDBCluster Prelude.Text
backtrackDBCluster_dbClusterIdentifier = Lens.lens (\BacktrackDBCluster' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@BacktrackDBCluster' {} a -> s {dbClusterIdentifier = a} :: BacktrackDBCluster)

-- | The timestamp of the time to backtrack the DB cluster to, specified in
-- ISO 8601 format. For more information about ISO 8601, see the
-- <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- If the specified time isn\'t a consistent time for the DB cluster,
-- Aurora automatically chooses the nearest possible consistent time for
-- the DB cluster.
--
-- Constraints:
--
-- -   Must contain a valid ISO 8601 timestamp.
--
-- -   Can\'t contain a timestamp set in the future.
--
-- Example: @2017-07-08T18:00Z@
backtrackDBCluster_backtrackTo :: Lens.Lens' BacktrackDBCluster Prelude.UTCTime
backtrackDBCluster_backtrackTo = Lens.lens (\BacktrackDBCluster' {backtrackTo} -> backtrackTo) (\s@BacktrackDBCluster' {} a -> s {backtrackTo = a} :: BacktrackDBCluster) Prelude.. Data._Time

instance Core.AWSRequest BacktrackDBCluster where
  type
    AWSResponse BacktrackDBCluster =
      DBClusterBacktrack
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "BacktrackDBClusterResult"
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable BacktrackDBCluster where
  hashWithSalt _salt BacktrackDBCluster' {..} =
    _salt `Prelude.hashWithSalt` force
      `Prelude.hashWithSalt` useEarliestTimeOnPointInTimeUnavailable
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` backtrackTo

instance Prelude.NFData BacktrackDBCluster where
  rnf BacktrackDBCluster' {..} =
    Prelude.rnf force
      `Prelude.seq` Prelude.rnf useEarliestTimeOnPointInTimeUnavailable
      `Prelude.seq` Prelude.rnf dbClusterIdentifier
      `Prelude.seq` Prelude.rnf backtrackTo

instance Data.ToHeaders BacktrackDBCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath BacktrackDBCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery BacktrackDBCluster where
  toQuery BacktrackDBCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("BacktrackDBCluster" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "Force" Data.=: force,
        "UseEarliestTimeOnPointInTimeUnavailable"
          Data.=: useEarliestTimeOnPointInTimeUnavailable,
        "DBClusterIdentifier" Data.=: dbClusterIdentifier,
        "BacktrackTo" Data.=: backtrackTo
      ]
