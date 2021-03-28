{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.GetSubscriptionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the @SubscriptionState@ , either @Active@ or @Inactive@ .
module Network.AWS.Shield.GetSubscriptionState
    (
    -- * Creating a request
      GetSubscriptionState (..)
    , mkGetSubscriptionState

    -- * Destructuring the response
    , GetSubscriptionStateResponse (..)
    , mkGetSubscriptionStateResponse
    -- ** Response lenses
    , gssrrsSubscriptionState
    , gssrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkGetSubscriptionState' smart constructor.
data GetSubscriptionState = GetSubscriptionState'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSubscriptionState' value with any optional fields omitted.
mkGetSubscriptionState
    :: GetSubscriptionState
mkGetSubscriptionState = GetSubscriptionState'

instance Core.ToQuery GetSubscriptionState where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetSubscriptionState where
        toHeaders GetSubscriptionState{..}
          = Core.pure
              ("X-Amz-Target", "AWSShield_20160616.GetSubscriptionState")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetSubscriptionState where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest GetSubscriptionState where
        type Rs GetSubscriptionState = GetSubscriptionStateResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetSubscriptionStateResponse' Core.<$>
                   (x Core..: "SubscriptionState") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetSubscriptionStateResponse' smart constructor.
data GetSubscriptionStateResponse = GetSubscriptionStateResponse'
  { subscriptionState :: Types.SubscriptionState
    -- ^ The status of the subscription.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSubscriptionStateResponse' value with any optional fields omitted.
mkGetSubscriptionStateResponse
    :: Types.SubscriptionState -- ^ 'subscriptionState'
    -> Core.Int -- ^ 'responseStatus'
    -> GetSubscriptionStateResponse
mkGetSubscriptionStateResponse subscriptionState responseStatus
  = GetSubscriptionStateResponse'{subscriptionState, responseStatus}

-- | The status of the subscription.
--
-- /Note:/ Consider using 'subscriptionState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssrrsSubscriptionState :: Lens.Lens' GetSubscriptionStateResponse Types.SubscriptionState
gssrrsSubscriptionState = Lens.field @"subscriptionState"
{-# INLINEABLE gssrrsSubscriptionState #-}
{-# DEPRECATED subscriptionState "Use generic-lens or generic-optics with 'subscriptionState' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssrrsResponseStatus :: Lens.Lens' GetSubscriptionStateResponse Core.Int
gssrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gssrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
