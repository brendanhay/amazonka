{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.StopDBCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an Amazon Aurora DB cluster. When you stop a DB cluster, Aurora retains the DB cluster's metadata, including its endpoints and DB parameter groups. Aurora also retains the transaction logs so you can do a point-in-time restore if necessary.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-cluster-stop-start.html Stopping and Starting an Aurora Cluster> in the /Amazon Aurora User Guide./
module Network.AWS.RDS.StopDBCluster
  ( -- * Creating a request
    StopDBCluster (..),
    mkStopDBCluster,

    -- ** Request lenses
    sDBClusterIdentifier,

    -- * Destructuring the response
    StopDBClusterResponse (..),
    mkStopDBClusterResponse,

    -- ** Response lenses
    sdbcrfrsDBCluster,
    sdbcrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopDBCluster' smart constructor.
newtype StopDBCluster = StopDBCluster'
  { -- | The DB cluster identifier of the Amazon Aurora DB cluster to be stopped. This parameter is stored as a lowercase string.
    dBClusterIdentifier :: Types.DBClusterIdentifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopDBCluster' value with any optional fields omitted.
mkStopDBCluster ::
  -- | 'dBClusterIdentifier'
  Types.DBClusterIdentifier ->
  StopDBCluster
mkStopDBCluster dBClusterIdentifier =
  StopDBCluster' {dBClusterIdentifier}

-- | The DB cluster identifier of the Amazon Aurora DB cluster to be stopped. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDBClusterIdentifier :: Lens.Lens' StopDBCluster Types.DBClusterIdentifier
sDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# DEPRECATED sDBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead." #-}

instance Core.AWSRequest StopDBCluster where
  type Rs StopDBCluster = StopDBClusterResponse
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
            ( Core.pure ("Action", "StopDBCluster")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBClusterIdentifier" dBClusterIdentifier)
            )
      }
  response =
    Response.receiveXMLWrapper
      "StopDBClusterResult"
      ( \s h x ->
          StopDBClusterResponse'
            Core.<$> (x Core..@? "DBCluster") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStopDBClusterResponse' smart constructor.
data StopDBClusterResponse = StopDBClusterResponse'
  { dBCluster :: Core.Maybe Types.DBCluster,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StopDBClusterResponse' value with any optional fields omitted.
mkStopDBClusterResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StopDBClusterResponse
mkStopDBClusterResponse responseStatus =
  StopDBClusterResponse' {dBCluster = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdbcrfrsDBCluster :: Lens.Lens' StopDBClusterResponse (Core.Maybe Types.DBCluster)
sdbcrfrsDBCluster = Lens.field @"dBCluster"
{-# DEPRECATED sdbcrfrsDBCluster "Use generic-lens or generic-optics with 'dBCluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdbcrfrsResponseStatus :: Lens.Lens' StopDBClusterResponse Core.Int
sdbcrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sdbcrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
