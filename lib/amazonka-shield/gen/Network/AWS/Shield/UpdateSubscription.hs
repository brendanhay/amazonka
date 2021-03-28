{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.UpdateSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the details of an existing subscription. Only enter values for parameters you want to change. Empty parameters are not updated.
module Network.AWS.Shield.UpdateSubscription
    (
    -- * Creating a request
      UpdateSubscription (..)
    , mkUpdateSubscription
    -- ** Request lenses
    , usAutoRenew

    -- * Destructuring the response
    , UpdateSubscriptionResponse (..)
    , mkUpdateSubscriptionResponse
    -- ** Response lenses
    , usrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkUpdateSubscription' smart constructor.
newtype UpdateSubscription = UpdateSubscription'
  { autoRenew :: Core.Maybe Types.AutoRenew
    -- ^ When you initally create a subscription, @AutoRenew@ is set to @ENABLED@ . If @ENABLED@ , the subscription will be automatically renewed at the end of the existing subscription period. You can change this by submitting an @UpdateSubscription@ request. If the @UpdateSubscription@ request does not included a value for @AutoRenew@ , the existing value for @AutoRenew@ remains unchanged.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSubscription' value with any optional fields omitted.
mkUpdateSubscription
    :: UpdateSubscription
mkUpdateSubscription
  = UpdateSubscription'{autoRenew = Core.Nothing}

-- | When you initally create a subscription, @AutoRenew@ is set to @ENABLED@ . If @ENABLED@ , the subscription will be automatically renewed at the end of the existing subscription period. You can change this by submitting an @UpdateSubscription@ request. If the @UpdateSubscription@ request does not included a value for @AutoRenew@ , the existing value for @AutoRenew@ remains unchanged.
--
-- /Note:/ Consider using 'autoRenew' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usAutoRenew :: Lens.Lens' UpdateSubscription (Core.Maybe Types.AutoRenew)
usAutoRenew = Lens.field @"autoRenew"
{-# INLINEABLE usAutoRenew #-}
{-# DEPRECATED autoRenew "Use generic-lens or generic-optics with 'autoRenew' instead"  #-}

instance Core.ToQuery UpdateSubscription where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateSubscription where
        toHeaders UpdateSubscription{..}
          = Core.pure
              ("X-Amz-Target", "AWSShield_20160616.UpdateSubscription")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateSubscription where
        toJSON UpdateSubscription{..}
          = Core.object
              (Core.catMaybes [("AutoRenew" Core..=) Core.<$> autoRenew])

instance Core.AWSRequest UpdateSubscription where
        type Rs UpdateSubscription = UpdateSubscriptionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateSubscriptionResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateSubscriptionResponse' smart constructor.
newtype UpdateSubscriptionResponse = UpdateSubscriptionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSubscriptionResponse' value with any optional fields omitted.
mkUpdateSubscriptionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateSubscriptionResponse
mkUpdateSubscriptionResponse responseStatus
  = UpdateSubscriptionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsResponseStatus :: Lens.Lens' UpdateSubscriptionResponse Core.Int
usrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE usrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
