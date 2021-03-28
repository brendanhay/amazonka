{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DisassociateRoutingProfileQueues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a set of queues from a routing profile.
module Network.AWS.Connect.DisassociateRoutingProfileQueues
    (
    -- * Creating a request
      DisassociateRoutingProfileQueues (..)
    , mkDisassociateRoutingProfileQueues
    -- ** Request lenses
    , drpqInstanceId
    , drpqRoutingProfileId
    , drpqQueueReferences

    -- * Destructuring the response
    , DisassociateRoutingProfileQueuesResponse (..)
    , mkDisassociateRoutingProfileQueuesResponse
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateRoutingProfileQueues' smart constructor.
data DisassociateRoutingProfileQueues = DisassociateRoutingProfileQueues'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , routingProfileId :: Types.RoutingProfileId
    -- ^ The identifier of the routing profile.
  , queueReferences :: [Types.RoutingProfileQueueReference]
    -- ^ The queues to disassociate from this routing profile.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateRoutingProfileQueues' value with any optional fields omitted.
mkDisassociateRoutingProfileQueues
    :: Types.InstanceId -- ^ 'instanceId'
    -> Types.RoutingProfileId -- ^ 'routingProfileId'
    -> DisassociateRoutingProfileQueues
mkDisassociateRoutingProfileQueues instanceId routingProfileId
  = DisassociateRoutingProfileQueues'{instanceId, routingProfileId,
                                      queueReferences = Core.mempty}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpqInstanceId :: Lens.Lens' DisassociateRoutingProfileQueues Types.InstanceId
drpqInstanceId = Lens.field @"instanceId"
{-# INLINEABLE drpqInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The identifier of the routing profile.
--
-- /Note:/ Consider using 'routingProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpqRoutingProfileId :: Lens.Lens' DisassociateRoutingProfileQueues Types.RoutingProfileId
drpqRoutingProfileId = Lens.field @"routingProfileId"
{-# INLINEABLE drpqRoutingProfileId #-}
{-# DEPRECATED routingProfileId "Use generic-lens or generic-optics with 'routingProfileId' instead"  #-}

-- | The queues to disassociate from this routing profile.
--
-- /Note:/ Consider using 'queueReferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpqQueueReferences :: Lens.Lens' DisassociateRoutingProfileQueues [Types.RoutingProfileQueueReference]
drpqQueueReferences = Lens.field @"queueReferences"
{-# INLINEABLE drpqQueueReferences #-}
{-# DEPRECATED queueReferences "Use generic-lens or generic-optics with 'queueReferences' instead"  #-}

instance Core.ToQuery DisassociateRoutingProfileQueues where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisassociateRoutingProfileQueues where
        toHeaders DisassociateRoutingProfileQueues{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DisassociateRoutingProfileQueues where
        toJSON DisassociateRoutingProfileQueues{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("QueueReferences" Core..= queueReferences)])

instance Core.AWSRequest DisassociateRoutingProfileQueues where
        type Rs DisassociateRoutingProfileQueues =
             DisassociateRoutingProfileQueuesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/routing-profiles/" Core.<> Core.toText instanceId Core.<> "/"
                             Core.<> Core.toText routingProfileId
                             Core.<> "/disassociate-queues",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DisassociateRoutingProfileQueuesResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateRoutingProfileQueuesResponse' smart constructor.
data DisassociateRoutingProfileQueuesResponse = DisassociateRoutingProfileQueuesResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateRoutingProfileQueuesResponse' value with any optional fields omitted.
mkDisassociateRoutingProfileQueuesResponse
    :: DisassociateRoutingProfileQueuesResponse
mkDisassociateRoutingProfileQueuesResponse
  = DisassociateRoutingProfileQueuesResponse'
