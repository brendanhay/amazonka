{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.StartOnDemandAppReplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an on-demand replication run for the specified application.
module Network.AWS.SMS.StartOnDemandAppReplication
  ( -- * Creating a request
    StartOnDemandAppReplication (..),
    mkStartOnDemandAppReplication,

    -- ** Request lenses
    sodarAppId,
    sodarDescription,

    -- * Destructuring the response
    StartOnDemandAppReplicationResponse (..),
    mkStartOnDemandAppReplicationResponse,

    -- ** Response lenses
    sodarrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkStartOnDemandAppReplication' smart constructor.
data StartOnDemandAppReplication = StartOnDemandAppReplication'
  { -- | The ID of the application.
    appId :: Types.AppId,
    -- | The description of the replication run.
    description :: Core.Maybe Types.Description
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartOnDemandAppReplication' value with any optional fields omitted.
mkStartOnDemandAppReplication ::
  -- | 'appId'
  Types.AppId ->
  StartOnDemandAppReplication
mkStartOnDemandAppReplication appId =
  StartOnDemandAppReplication' {appId, description = Core.Nothing}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sodarAppId :: Lens.Lens' StartOnDemandAppReplication Types.AppId
sodarAppId = Lens.field @"appId"
{-# DEPRECATED sodarAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

-- | The description of the replication run.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sodarDescription :: Lens.Lens' StartOnDemandAppReplication (Core.Maybe Types.Description)
sodarDescription = Lens.field @"description"
{-# DEPRECATED sodarDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.FromJSON StartOnDemandAppReplication where
  toJSON StartOnDemandAppReplication {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("appId" Core..= appId),
            ("description" Core..=) Core.<$> description
          ]
      )

instance Core.AWSRequest StartOnDemandAppReplication where
  type
    Rs StartOnDemandAppReplication =
      StartOnDemandAppReplicationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSServerMigrationService_V2016_10_24.StartOnDemandAppReplication"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartOnDemandAppReplicationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartOnDemandAppReplicationResponse' smart constructor.
newtype StartOnDemandAppReplicationResponse = StartOnDemandAppReplicationResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartOnDemandAppReplicationResponse' value with any optional fields omitted.
mkStartOnDemandAppReplicationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartOnDemandAppReplicationResponse
mkStartOnDemandAppReplicationResponse responseStatus =
  StartOnDemandAppReplicationResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sodarrrsResponseStatus :: Lens.Lens' StartOnDemandAppReplicationResponse Core.Int
sodarrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sodarrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
