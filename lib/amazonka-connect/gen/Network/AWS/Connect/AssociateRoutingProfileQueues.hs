{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.AssociateRoutingProfileQueues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a set of queues with a routing profile.
module Network.AWS.Connect.AssociateRoutingProfileQueues
    (
    -- * Creating a request
      AssociateRoutingProfileQueues (..)
    , mkAssociateRoutingProfileQueues
    -- ** Request lenses
    , arpqInstanceId
    , arpqRoutingProfileId
    , arpqQueueConfigs

    -- * Destructuring the response
    , AssociateRoutingProfileQueuesResponse (..)
    , mkAssociateRoutingProfileQueuesResponse
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateRoutingProfileQueues' smart constructor.
data AssociateRoutingProfileQueues = AssociateRoutingProfileQueues'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , routingProfileId :: Types.RoutingProfileId
    -- ^ The identifier of the routing profile.
  , queueConfigs :: Core.NonEmpty Types.RoutingProfileQueueConfig
    -- ^ The queues to associate with this routing profile.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateRoutingProfileQueues' value with any optional fields omitted.
mkAssociateRoutingProfileQueues
    :: Types.InstanceId -- ^ 'instanceId'
    -> Types.RoutingProfileId -- ^ 'routingProfileId'
    -> Core.NonEmpty Types.RoutingProfileQueueConfig -- ^ 'queueConfigs'
    -> AssociateRoutingProfileQueues
mkAssociateRoutingProfileQueues instanceId routingProfileId
  queueConfigs
  = AssociateRoutingProfileQueues'{instanceId, routingProfileId,
                                   queueConfigs}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arpqInstanceId :: Lens.Lens' AssociateRoutingProfileQueues Types.InstanceId
arpqInstanceId = Lens.field @"instanceId"
{-# INLINEABLE arpqInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The identifier of the routing profile.
--
-- /Note:/ Consider using 'routingProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arpqRoutingProfileId :: Lens.Lens' AssociateRoutingProfileQueues Types.RoutingProfileId
arpqRoutingProfileId = Lens.field @"routingProfileId"
{-# INLINEABLE arpqRoutingProfileId #-}
{-# DEPRECATED routingProfileId "Use generic-lens or generic-optics with 'routingProfileId' instead"  #-}

-- | The queues to associate with this routing profile.
--
-- /Note:/ Consider using 'queueConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arpqQueueConfigs :: Lens.Lens' AssociateRoutingProfileQueues (Core.NonEmpty Types.RoutingProfileQueueConfig)
arpqQueueConfigs = Lens.field @"queueConfigs"
{-# INLINEABLE arpqQueueConfigs #-}
{-# DEPRECATED queueConfigs "Use generic-lens or generic-optics with 'queueConfigs' instead"  #-}

instance Core.ToQuery AssociateRoutingProfileQueues where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssociateRoutingProfileQueues where
        toHeaders AssociateRoutingProfileQueues{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssociateRoutingProfileQueues where
        toJSON AssociateRoutingProfileQueues{..}
          = Core.object
              (Core.catMaybes [Core.Just ("QueueConfigs" Core..= queueConfigs)])

instance Core.AWSRequest AssociateRoutingProfileQueues where
        type Rs AssociateRoutingProfileQueues =
             AssociateRoutingProfileQueuesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/routing-profiles/" Core.<> Core.toText instanceId Core.<> "/"
                             Core.<> Core.toText routingProfileId
                             Core.<> "/associate-queues",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull AssociateRoutingProfileQueuesResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateRoutingProfileQueuesResponse' smart constructor.
data AssociateRoutingProfileQueuesResponse = AssociateRoutingProfileQueuesResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateRoutingProfileQueuesResponse' value with any optional fields omitted.
mkAssociateRoutingProfileQueuesResponse
    :: AssociateRoutingProfileQueuesResponse
mkAssociateRoutingProfileQueuesResponse
  = AssociateRoutingProfileQueuesResponse'
