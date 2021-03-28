{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateRoutingProfileConcurrency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the channels that agents can handle in the Contact Control Panel (CCP) for a routing profile.
module Network.AWS.Connect.UpdateRoutingProfileConcurrency
    (
    -- * Creating a request
      UpdateRoutingProfileConcurrency (..)
    , mkUpdateRoutingProfileConcurrency
    -- ** Request lenses
    , urpcInstanceId
    , urpcRoutingProfileId
    , urpcMediaConcurrencies

    -- * Destructuring the response
    , UpdateRoutingProfileConcurrencyResponse (..)
    , mkUpdateRoutingProfileConcurrencyResponse
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateRoutingProfileConcurrency' smart constructor.
data UpdateRoutingProfileConcurrency = UpdateRoutingProfileConcurrency'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , routingProfileId :: Types.RoutingProfileId
    -- ^ The identifier of the routing profile.
  , mediaConcurrencies :: [Types.MediaConcurrency]
    -- ^ The channels agents can handle in the Contact Control Panel (CCP).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRoutingProfileConcurrency' value with any optional fields omitted.
mkUpdateRoutingProfileConcurrency
    :: Types.InstanceId -- ^ 'instanceId'
    -> Types.RoutingProfileId -- ^ 'routingProfileId'
    -> UpdateRoutingProfileConcurrency
mkUpdateRoutingProfileConcurrency instanceId routingProfileId
  = UpdateRoutingProfileConcurrency'{instanceId, routingProfileId,
                                     mediaConcurrencies = Core.mempty}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpcInstanceId :: Lens.Lens' UpdateRoutingProfileConcurrency Types.InstanceId
urpcInstanceId = Lens.field @"instanceId"
{-# INLINEABLE urpcInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The identifier of the routing profile.
--
-- /Note:/ Consider using 'routingProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpcRoutingProfileId :: Lens.Lens' UpdateRoutingProfileConcurrency Types.RoutingProfileId
urpcRoutingProfileId = Lens.field @"routingProfileId"
{-# INLINEABLE urpcRoutingProfileId #-}
{-# DEPRECATED routingProfileId "Use generic-lens or generic-optics with 'routingProfileId' instead"  #-}

-- | The channels agents can handle in the Contact Control Panel (CCP).
--
-- /Note:/ Consider using 'mediaConcurrencies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpcMediaConcurrencies :: Lens.Lens' UpdateRoutingProfileConcurrency [Types.MediaConcurrency]
urpcMediaConcurrencies = Lens.field @"mediaConcurrencies"
{-# INLINEABLE urpcMediaConcurrencies #-}
{-# DEPRECATED mediaConcurrencies "Use generic-lens or generic-optics with 'mediaConcurrencies' instead"  #-}

instance Core.ToQuery UpdateRoutingProfileConcurrency where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateRoutingProfileConcurrency where
        toHeaders UpdateRoutingProfileConcurrency{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateRoutingProfileConcurrency where
        toJSON UpdateRoutingProfileConcurrency{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("MediaConcurrencies" Core..= mediaConcurrencies)])

instance Core.AWSRequest UpdateRoutingProfileConcurrency where
        type Rs UpdateRoutingProfileConcurrency =
             UpdateRoutingProfileConcurrencyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/routing-profiles/" Core.<> Core.toText instanceId Core.<> "/"
                             Core.<> Core.toText routingProfileId
                             Core.<> "/concurrency",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull UpdateRoutingProfileConcurrencyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateRoutingProfileConcurrencyResponse' smart constructor.
data UpdateRoutingProfileConcurrencyResponse = UpdateRoutingProfileConcurrencyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRoutingProfileConcurrencyResponse' value with any optional fields omitted.
mkUpdateRoutingProfileConcurrencyResponse
    :: UpdateRoutingProfileConcurrencyResponse
mkUpdateRoutingProfileConcurrencyResponse
  = UpdateRoutingProfileConcurrencyResponse'
