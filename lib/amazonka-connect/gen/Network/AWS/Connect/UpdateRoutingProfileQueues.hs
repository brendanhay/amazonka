{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateRoutingProfileQueues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the properties associated with a set of queues for a routing profile.
module Network.AWS.Connect.UpdateRoutingProfileQueues
  ( -- * Creating a request
    UpdateRoutingProfileQueues (..),
    mkUpdateRoutingProfileQueues,

    -- ** Request lenses
    urpqInstanceId,
    urpqRoutingProfileId,
    urpqQueueConfigs,

    -- * Destructuring the response
    UpdateRoutingProfileQueuesResponse (..),
    mkUpdateRoutingProfileQueuesResponse,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateRoutingProfileQueues' smart constructor.
data UpdateRoutingProfileQueues = UpdateRoutingProfileQueues'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The identifier of the routing profile.
    routingProfileId :: Types.RoutingProfileId,
    -- | The queues to be updated for this routing profile.
    queueConfigs :: Core.NonEmpty Types.RoutingProfileQueueConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRoutingProfileQueues' value with any optional fields omitted.
mkUpdateRoutingProfileQueues ::
  -- | 'instanceId'
  Types.InstanceId ->
  -- | 'routingProfileId'
  Types.RoutingProfileId ->
  -- | 'queueConfigs'
  Core.NonEmpty Types.RoutingProfileQueueConfig ->
  UpdateRoutingProfileQueues
mkUpdateRoutingProfileQueues
  instanceId
  routingProfileId
  queueConfigs =
    UpdateRoutingProfileQueues'
      { instanceId,
        routingProfileId,
        queueConfigs
      }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpqInstanceId :: Lens.Lens' UpdateRoutingProfileQueues Types.InstanceId
urpqInstanceId = Lens.field @"instanceId"
{-# DEPRECATED urpqInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The identifier of the routing profile.
--
-- /Note:/ Consider using 'routingProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpqRoutingProfileId :: Lens.Lens' UpdateRoutingProfileQueues Types.RoutingProfileId
urpqRoutingProfileId = Lens.field @"routingProfileId"
{-# DEPRECATED urpqRoutingProfileId "Use generic-lens or generic-optics with 'routingProfileId' instead." #-}

-- | The queues to be updated for this routing profile.
--
-- /Note:/ Consider using 'queueConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpqQueueConfigs :: Lens.Lens' UpdateRoutingProfileQueues (Core.NonEmpty Types.RoutingProfileQueueConfig)
urpqQueueConfigs = Lens.field @"queueConfigs"
{-# DEPRECATED urpqQueueConfigs "Use generic-lens or generic-optics with 'queueConfigs' instead." #-}

instance Core.FromJSON UpdateRoutingProfileQueues where
  toJSON UpdateRoutingProfileQueues {..} =
    Core.object
      (Core.catMaybes [Core.Just ("QueueConfigs" Core..= queueConfigs)])

instance Core.AWSRequest UpdateRoutingProfileQueues where
  type
    Rs UpdateRoutingProfileQueues =
      UpdateRoutingProfileQueuesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/routing-profiles/" Core.<> (Core.toText instanceId)
                Core.<> ("/")
                Core.<> (Core.toText routingProfileId)
                Core.<> ("/queues")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UpdateRoutingProfileQueuesResponse'

-- | /See:/ 'mkUpdateRoutingProfileQueuesResponse' smart constructor.
data UpdateRoutingProfileQueuesResponse = UpdateRoutingProfileQueuesResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRoutingProfileQueuesResponse' value with any optional fields omitted.
mkUpdateRoutingProfileQueuesResponse ::
  UpdateRoutingProfileQueuesResponse
mkUpdateRoutingProfileQueuesResponse =
  UpdateRoutingProfileQueuesResponse'
