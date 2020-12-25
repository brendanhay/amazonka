{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.StopAppReplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops replicating the specified application by deleting the replication job for each server in the application.
module Network.AWS.SMS.StopAppReplication
  ( -- * Creating a request
    StopAppReplication (..),
    mkStopAppReplication,

    -- ** Request lenses
    sAppId,

    -- * Destructuring the response
    StopAppReplicationResponse (..),
    mkStopAppReplicationResponse,

    -- ** Response lenses
    sarrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkStopAppReplication' smart constructor.
newtype StopAppReplication = StopAppReplication'
  { -- | The ID of the application.
    appId :: Core.Maybe Types.AppId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopAppReplication' value with any optional fields omitted.
mkStopAppReplication ::
  StopAppReplication
mkStopAppReplication = StopAppReplication' {appId = Core.Nothing}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAppId :: Lens.Lens' StopAppReplication (Core.Maybe Types.AppId)
sAppId = Lens.field @"appId"
{-# DEPRECATED sAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

instance Core.FromJSON StopAppReplication where
  toJSON StopAppReplication {..} =
    Core.object (Core.catMaybes [("appId" Core..=) Core.<$> appId])

instance Core.AWSRequest StopAppReplication where
  type Rs StopAppReplication = StopAppReplicationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSServerMigrationService_V2016_10_24.StopAppReplication"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopAppReplicationResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStopAppReplicationResponse' smart constructor.
newtype StopAppReplicationResponse = StopAppReplicationResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopAppReplicationResponse' value with any optional fields omitted.
mkStopAppReplicationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StopAppReplicationResponse
mkStopAppReplicationResponse responseStatus =
  StopAppReplicationResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarrrsResponseStatus :: Lens.Lens' StopAppReplicationResponse Core.Int
sarrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sarrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
