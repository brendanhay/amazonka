{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.StartOnDemandReplicationRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an on-demand replication run for the specified replication job. This replication run starts immediately. This replication run is in addition to the ones already scheduled.
--
-- There is a limit on the number of on-demand replications runs that you can request in a 24-hour period.
module Network.AWS.SMS.StartOnDemandReplicationRun
  ( -- * Creating a request
    StartOnDemandReplicationRun (..),
    mkStartOnDemandReplicationRun,

    -- ** Request lenses
    sodrrReplicationJobId,
    sodrrDescription,

    -- * Destructuring the response
    StartOnDemandReplicationRunResponse (..),
    mkStartOnDemandReplicationRunResponse,

    -- ** Response lenses
    sodrrrrsReplicationRunId,
    sodrrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkStartOnDemandReplicationRun' smart constructor.
data StartOnDemandReplicationRun = StartOnDemandReplicationRun'
  { -- | The ID of the replication job.
    replicationJobId :: Types.ReplicationJobId,
    -- | The description of the replication run.
    description :: Core.Maybe Types.Description
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartOnDemandReplicationRun' value with any optional fields omitted.
mkStartOnDemandReplicationRun ::
  -- | 'replicationJobId'
  Types.ReplicationJobId ->
  StartOnDemandReplicationRun
mkStartOnDemandReplicationRun replicationJobId =
  StartOnDemandReplicationRun'
    { replicationJobId,
      description = Core.Nothing
    }

-- | The ID of the replication job.
--
-- /Note:/ Consider using 'replicationJobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sodrrReplicationJobId :: Lens.Lens' StartOnDemandReplicationRun Types.ReplicationJobId
sodrrReplicationJobId = Lens.field @"replicationJobId"
{-# DEPRECATED sodrrReplicationJobId "Use generic-lens or generic-optics with 'replicationJobId' instead." #-}

-- | The description of the replication run.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sodrrDescription :: Lens.Lens' StartOnDemandReplicationRun (Core.Maybe Types.Description)
sodrrDescription = Lens.field @"description"
{-# DEPRECATED sodrrDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.FromJSON StartOnDemandReplicationRun where
  toJSON StartOnDemandReplicationRun {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("replicationJobId" Core..= replicationJobId),
            ("description" Core..=) Core.<$> description
          ]
      )

instance Core.AWSRequest StartOnDemandReplicationRun where
  type
    Rs StartOnDemandReplicationRun =
      StartOnDemandReplicationRunResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSServerMigrationService_V2016_10_24.StartOnDemandReplicationRun"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartOnDemandReplicationRunResponse'
            Core.<$> (x Core..:? "replicationRunId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartOnDemandReplicationRunResponse' smart constructor.
data StartOnDemandReplicationRunResponse = StartOnDemandReplicationRunResponse'
  { -- | The ID of the replication run.
    replicationRunId :: Core.Maybe Types.ReplicationRunId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartOnDemandReplicationRunResponse' value with any optional fields omitted.
mkStartOnDemandReplicationRunResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartOnDemandReplicationRunResponse
mkStartOnDemandReplicationRunResponse responseStatus =
  StartOnDemandReplicationRunResponse'
    { replicationRunId =
        Core.Nothing,
      responseStatus
    }

-- | The ID of the replication run.
--
-- /Note:/ Consider using 'replicationRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sodrrrrsReplicationRunId :: Lens.Lens' StartOnDemandReplicationRunResponse (Core.Maybe Types.ReplicationRunId)
sodrrrrsReplicationRunId = Lens.field @"replicationRunId"
{-# DEPRECATED sodrrrrsReplicationRunId "Use generic-lens or generic-optics with 'replicationRunId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sodrrrrsResponseStatus :: Lens.Lens' StartOnDemandReplicationRunResponse Core.Int
sodrrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sodrrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
