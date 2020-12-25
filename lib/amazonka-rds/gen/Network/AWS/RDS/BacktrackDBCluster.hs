{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    bdbcDBClusterIdentifier,
    bdbcBacktrackTo,
    bdbcForce,
    bdbcUseEarliestTimeOnPointInTimeUnavailable,

    -- * Destructuring the response
    Types.DBClusterBacktrack (..),
    Types.mkDBClusterBacktrack,

    -- ** Response lenses
    Types.dbcbBacktrackIdentifier,
    Types.dbcbBacktrackRequestCreationTime,
    Types.dbcbBacktrackTo,
    Types.dbcbBacktrackedFrom,
    Types.dbcbDBClusterIdentifier,
    Types.dbcbStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkBacktrackDBCluster' smart constructor.
data BacktrackDBCluster = BacktrackDBCluster'
  { -- | The DB cluster identifier of the DB cluster to be backtracked. This parameter is stored as a lowercase string.
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
    dBClusterIdentifier :: Types.String,
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
    backtrackTo :: Core.UTCTime,
    -- | A value that indicates whether to force the DB cluster to backtrack when binary logging is enabled. Otherwise, an error occurs when binary logging is enabled.
    force :: Core.Maybe Core.Bool,
    -- | A value that indicates whether to backtrack the DB cluster to the earliest possible backtrack time when /BacktrackTo/ is set to a timestamp earlier than the earliest backtrack time. When this parameter is disabled and /BacktrackTo/ is set to a timestamp earlier than the earliest backtrack time, an error occurs.
    useEarliestTimeOnPointInTimeUnavailable :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BacktrackDBCluster' value with any optional fields omitted.
mkBacktrackDBCluster ::
  -- | 'dBClusterIdentifier'
  Types.String ->
  -- | 'backtrackTo'
  Core.UTCTime ->
  BacktrackDBCluster
mkBacktrackDBCluster dBClusterIdentifier backtrackTo =
  BacktrackDBCluster'
    { dBClusterIdentifier,
      backtrackTo,
      force = Core.Nothing,
      useEarliestTimeOnPointInTimeUnavailable = Core.Nothing
    }

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
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdbcDBClusterIdentifier :: Lens.Lens' BacktrackDBCluster Types.String
bdbcDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# DEPRECATED bdbcDBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead." #-}

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
bdbcBacktrackTo :: Lens.Lens' BacktrackDBCluster Core.UTCTime
bdbcBacktrackTo = Lens.field @"backtrackTo"
{-# DEPRECATED bdbcBacktrackTo "Use generic-lens or generic-optics with 'backtrackTo' instead." #-}

-- | A value that indicates whether to force the DB cluster to backtrack when binary logging is enabled. Otherwise, an error occurs when binary logging is enabled.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdbcForce :: Lens.Lens' BacktrackDBCluster (Core.Maybe Core.Bool)
bdbcForce = Lens.field @"force"
{-# DEPRECATED bdbcForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | A value that indicates whether to backtrack the DB cluster to the earliest possible backtrack time when /BacktrackTo/ is set to a timestamp earlier than the earliest backtrack time. When this parameter is disabled and /BacktrackTo/ is set to a timestamp earlier than the earliest backtrack time, an error occurs.
--
-- /Note:/ Consider using 'useEarliestTimeOnPointInTimeUnavailable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdbcUseEarliestTimeOnPointInTimeUnavailable :: Lens.Lens' BacktrackDBCluster (Core.Maybe Core.Bool)
bdbcUseEarliestTimeOnPointInTimeUnavailable = Lens.field @"useEarliestTimeOnPointInTimeUnavailable"
{-# DEPRECATED bdbcUseEarliestTimeOnPointInTimeUnavailable "Use generic-lens or generic-optics with 'useEarliestTimeOnPointInTimeUnavailable' instead." #-}

instance Core.AWSRequest BacktrackDBCluster where
  type Rs BacktrackDBCluster = Types.DBClusterBacktrack
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "BacktrackDBCluster")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBClusterIdentifier" dBClusterIdentifier)
                Core.<> (Core.toQueryValue "BacktrackTo" backtrackTo)
                Core.<> (Core.toQueryValue "Force" Core.<$> force)
                Core.<> ( Core.toQueryValue "UseEarliestTimeOnPointInTimeUnavailable"
                            Core.<$> useEarliestTimeOnPointInTimeUnavailable
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "BacktrackDBClusterResult"
      (\s h x -> Core.parseXML x)
