{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.BacktrackDBCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Backtracks a DB cluster to a specific time, without creating a new DB cluster.
--
-- For more information on backtracking, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/AuroraMySQL.Managing.Backtrack.html Backtracking an Aurora DB Cluster> in the /Amazon Aurora User Guide./
module Network.AWS.RDS.BacktrackDBCluster
  ( -- * Creating a request
    BacktrackDBCluster (..),
    mkBacktrackDBCluster,

    -- ** Request lenses
    bdcForce,
    bdcUseEarliestTimeOnPointInTimeUnavailable,
    bdcDBClusterIdentifier,
    bdcBacktrackTo,

    -- * Destructuring the response
    DBClusterBacktrack (..),
    mkDBClusterBacktrack,

    -- ** Response lenses
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
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkBacktrackDBCluster' smart constructor.
data BacktrackDBCluster = BacktrackDBCluster'
  { force ::
      Lude.Maybe Lude.Bool,
    useEarliestTimeOnPointInTimeUnavailable ::
      Lude.Maybe Lude.Bool,
    dbClusterIdentifier :: Lude.Text,
    backtrackTo :: Lude.DateTime
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BacktrackDBCluster' with the minimum fields required to make a request.
--
-- * 'backtrackTo' - The timestamp of the time to backtrack the DB cluster to, specified in ISO 8601 format. For more information about ISO 8601, see the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Constraints:
--
--     * Must contain a valid ISO 8601 timestamp.
--
--
--     * Can't contain a timestamp set in the future.
--
--
-- Example: @2017-07-08T18:00Z@
-- * 'dbClusterIdentifier' - The DB cluster identifier of the DB cluster to be backtracked. This parameter is stored as a lowercase string.
--
-- Constraints:
--
--     * Must contain from 1 to 63 alphanumeric characters or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens.
--
--
-- Example: @my-cluster1@
-- * 'force' - A value that indicates whether to force the DB cluster to backtrack when binary logging is enabled. Otherwise, an error occurs when binary logging is enabled.
-- * 'useEarliestTimeOnPointInTimeUnavailable' - A value that indicates whether to backtrack the DB cluster to the earliest possible backtrack time when /BacktrackTo/ is set to a timestamp earlier than the earliest backtrack time. When this parameter is disabled and /BacktrackTo/ is set to a timestamp earlier than the earliest backtrack time, an error occurs.
mkBacktrackDBCluster ::
  -- | 'dbClusterIdentifier'
  Lude.Text ->
  -- | 'backtrackTo'
  Lude.DateTime ->
  BacktrackDBCluster
mkBacktrackDBCluster pDBClusterIdentifier_ pBacktrackTo_ =
  BacktrackDBCluster'
    { force = Lude.Nothing,
      useEarliestTimeOnPointInTimeUnavailable = Lude.Nothing,
      dbClusterIdentifier = pDBClusterIdentifier_,
      backtrackTo = pBacktrackTo_
    }

-- | A value that indicates whether to force the DB cluster to backtrack when binary logging is enabled. Otherwise, an error occurs when binary logging is enabled.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdcForce :: Lens.Lens' BacktrackDBCluster (Lude.Maybe Lude.Bool)
bdcForce = Lens.lens (force :: BacktrackDBCluster -> Lude.Maybe Lude.Bool) (\s a -> s {force = a} :: BacktrackDBCluster)
{-# DEPRECATED bdcForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | A value that indicates whether to backtrack the DB cluster to the earliest possible backtrack time when /BacktrackTo/ is set to a timestamp earlier than the earliest backtrack time. When this parameter is disabled and /BacktrackTo/ is set to a timestamp earlier than the earliest backtrack time, an error occurs.
--
-- /Note:/ Consider using 'useEarliestTimeOnPointInTimeUnavailable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdcUseEarliestTimeOnPointInTimeUnavailable :: Lens.Lens' BacktrackDBCluster (Lude.Maybe Lude.Bool)
bdcUseEarliestTimeOnPointInTimeUnavailable = Lens.lens (useEarliestTimeOnPointInTimeUnavailable :: BacktrackDBCluster -> Lude.Maybe Lude.Bool) (\s a -> s {useEarliestTimeOnPointInTimeUnavailable = a} :: BacktrackDBCluster)
{-# DEPRECATED bdcUseEarliestTimeOnPointInTimeUnavailable "Use generic-lens or generic-optics with 'useEarliestTimeOnPointInTimeUnavailable' instead." #-}

-- | The DB cluster identifier of the DB cluster to be backtracked. This parameter is stored as a lowercase string.
--
-- Constraints:
--
--     * Must contain from 1 to 63 alphanumeric characters or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens.
--
--
-- Example: @my-cluster1@
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdcDBClusterIdentifier :: Lens.Lens' BacktrackDBCluster Lude.Text
bdcDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: BacktrackDBCluster -> Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: BacktrackDBCluster)
{-# DEPRECATED bdcDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

-- | The timestamp of the time to backtrack the DB cluster to, specified in ISO 8601 format. For more information about ISO 8601, see the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Constraints:
--
--     * Must contain a valid ISO 8601 timestamp.
--
--
--     * Can't contain a timestamp set in the future.
--
--
-- Example: @2017-07-08T18:00Z@
--
-- /Note:/ Consider using 'backtrackTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdcBacktrackTo :: Lens.Lens' BacktrackDBCluster Lude.DateTime
bdcBacktrackTo = Lens.lens (backtrackTo :: BacktrackDBCluster -> Lude.DateTime) (\s a -> s {backtrackTo = a} :: BacktrackDBCluster)
{-# DEPRECATED bdcBacktrackTo "Use generic-lens or generic-optics with 'backtrackTo' instead." #-}

instance Lude.AWSRequest BacktrackDBCluster where
  type Rs BacktrackDBCluster = DBClusterBacktrack
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "BacktrackDBClusterResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders BacktrackDBCluster where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath BacktrackDBCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery BacktrackDBCluster where
  toQuery BacktrackDBCluster' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("BacktrackDBCluster" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "Force" Lude.=: force,
        "UseEarliestTimeOnPointInTimeUnavailable"
          Lude.=: useEarliestTimeOnPointInTimeUnavailable,
        "DBClusterIdentifier" Lude.=: dbClusterIdentifier,
        "BacktrackTo" Lude.=: backtrackTo
      ]
