{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.StartAppReplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts replicating the specified application by creating replication jobs for each server in the application.
module Network.AWS.SMS.StartAppReplication
  ( -- * Creating a request
    StartAppReplication (..),
    mkStartAppReplication,

    -- ** Request lenses
    sarAppId,

    -- * Destructuring the response
    StartAppReplicationResponse (..),
    mkStartAppReplicationResponse,

    -- ** Response lenses
    srsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkStartAppReplication' smart constructor.
newtype StartAppReplication = StartAppReplication'
  { -- | The ID of the application.
    appId :: Core.Maybe Types.AppId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartAppReplication' value with any optional fields omitted.
mkStartAppReplication ::
  StartAppReplication
mkStartAppReplication = StartAppReplication' {appId = Core.Nothing}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarAppId :: Lens.Lens' StartAppReplication (Core.Maybe Types.AppId)
sarAppId = Lens.field @"appId"
{-# DEPRECATED sarAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

instance Core.FromJSON StartAppReplication where
  toJSON StartAppReplication {..} =
    Core.object (Core.catMaybes [("appId" Core..=) Core.<$> appId])

instance Core.AWSRequest StartAppReplication where
  type Rs StartAppReplication = StartAppReplicationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSServerMigrationService_V2016_10_24.StartAppReplication"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartAppReplicationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartAppReplicationResponse' smart constructor.
newtype StartAppReplicationResponse = StartAppReplicationResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartAppReplicationResponse' value with any optional fields omitted.
mkStartAppReplicationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartAppReplicationResponse
mkStartAppReplicationResponse responseStatus =
  StartAppReplicationResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartAppReplicationResponse Core.Int
srsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
