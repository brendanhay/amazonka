{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateRoutingProfileDefaultOutboundQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the default outbound queue of a routing profile.
module Network.AWS.Connect.UpdateRoutingProfileDefaultOutboundQueue
    (
    -- * Creating a request
      UpdateRoutingProfileDefaultOutboundQueue (..)
    , mkUpdateRoutingProfileDefaultOutboundQueue
    -- ** Request lenses
    , urpdoqInstanceId
    , urpdoqRoutingProfileId
    , urpdoqDefaultOutboundQueueId

    -- * Destructuring the response
    , UpdateRoutingProfileDefaultOutboundQueueResponse (..)
    , mkUpdateRoutingProfileDefaultOutboundQueueResponse
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateRoutingProfileDefaultOutboundQueue' smart constructor.
data UpdateRoutingProfileDefaultOutboundQueue = UpdateRoutingProfileDefaultOutboundQueue'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , routingProfileId :: Types.RoutingProfileId
    -- ^ The identifier of the routing profile.
  , defaultOutboundQueueId :: Types.DefaultOutboundQueueId
    -- ^ The identifier for the default outbound queue.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRoutingProfileDefaultOutboundQueue' value with any optional fields omitted.
mkUpdateRoutingProfileDefaultOutboundQueue
    :: Types.InstanceId -- ^ 'instanceId'
    -> Types.RoutingProfileId -- ^ 'routingProfileId'
    -> Types.DefaultOutboundQueueId -- ^ 'defaultOutboundQueueId'
    -> UpdateRoutingProfileDefaultOutboundQueue
mkUpdateRoutingProfileDefaultOutboundQueue instanceId
  routingProfileId defaultOutboundQueueId
  = UpdateRoutingProfileDefaultOutboundQueue'{instanceId,
                                              routingProfileId, defaultOutboundQueueId}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpdoqInstanceId :: Lens.Lens' UpdateRoutingProfileDefaultOutboundQueue Types.InstanceId
urpdoqInstanceId = Lens.field @"instanceId"
{-# INLINEABLE urpdoqInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The identifier of the routing profile.
--
-- /Note:/ Consider using 'routingProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpdoqRoutingProfileId :: Lens.Lens' UpdateRoutingProfileDefaultOutboundQueue Types.RoutingProfileId
urpdoqRoutingProfileId = Lens.field @"routingProfileId"
{-# INLINEABLE urpdoqRoutingProfileId #-}
{-# DEPRECATED routingProfileId "Use generic-lens or generic-optics with 'routingProfileId' instead"  #-}

-- | The identifier for the default outbound queue.
--
-- /Note:/ Consider using 'defaultOutboundQueueId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpdoqDefaultOutboundQueueId :: Lens.Lens' UpdateRoutingProfileDefaultOutboundQueue Types.DefaultOutboundQueueId
urpdoqDefaultOutboundQueueId = Lens.field @"defaultOutboundQueueId"
{-# INLINEABLE urpdoqDefaultOutboundQueueId #-}
{-# DEPRECATED defaultOutboundQueueId "Use generic-lens or generic-optics with 'defaultOutboundQueueId' instead"  #-}

instance Core.ToQuery UpdateRoutingProfileDefaultOutboundQueue
         where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateRoutingProfileDefaultOutboundQueue
         where
        toHeaders UpdateRoutingProfileDefaultOutboundQueue{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateRoutingProfileDefaultOutboundQueue
         where
        toJSON UpdateRoutingProfileDefaultOutboundQueue{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("DefaultOutboundQueueId" Core..= defaultOutboundQueueId)])

instance Core.AWSRequest UpdateRoutingProfileDefaultOutboundQueue
         where
        type Rs UpdateRoutingProfileDefaultOutboundQueue =
             UpdateRoutingProfileDefaultOutboundQueueResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/routing-profiles/" Core.<> Core.toText instanceId Core.<> "/"
                             Core.<> Core.toText routingProfileId
                             Core.<> "/default-outbound-queue",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull
              UpdateRoutingProfileDefaultOutboundQueueResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateRoutingProfileDefaultOutboundQueueResponse' smart constructor.
data UpdateRoutingProfileDefaultOutboundQueueResponse = UpdateRoutingProfileDefaultOutboundQueueResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRoutingProfileDefaultOutboundQueueResponse' value with any optional fields omitted.
mkUpdateRoutingProfileDefaultOutboundQueueResponse
    :: UpdateRoutingProfileDefaultOutboundQueueResponse
mkUpdateRoutingProfileDefaultOutboundQueueResponse
  = UpdateRoutingProfileDefaultOutboundQueueResponse'
