{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.CreateSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates AWS Shield Advanced for an account.
--
-- When you initally create a subscription, your subscription is set to be automatically renewed at the end of the existing subscription period. You can change this by submitting an @UpdateSubscription@ request. 
module Network.AWS.Shield.CreateSubscription
    (
    -- * Creating a request
      CreateSubscription (..)
    , mkCreateSubscription

    -- * Destructuring the response
    , CreateSubscriptionResponse (..)
    , mkCreateSubscriptionResponse
    -- ** Response lenses
    , csrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkCreateSubscription' smart constructor.
data CreateSubscription = CreateSubscription'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSubscription' value with any optional fields omitted.
mkCreateSubscription
    :: CreateSubscription
mkCreateSubscription = CreateSubscription'

instance Core.ToQuery CreateSubscription where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateSubscription where
        toHeaders CreateSubscription{..}
          = Core.pure
              ("X-Amz-Target", "AWSShield_20160616.CreateSubscription")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateSubscription where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest CreateSubscription where
        type Rs CreateSubscription = CreateSubscriptionResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 CreateSubscriptionResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateSubscriptionResponse' smart constructor.
newtype CreateSubscriptionResponse = CreateSubscriptionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSubscriptionResponse' value with any optional fields omitted.
mkCreateSubscriptionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateSubscriptionResponse
mkCreateSubscriptionResponse responseStatus
  = CreateSubscriptionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsResponseStatus :: Lens.Lens' CreateSubscriptionResponse Core.Int
csrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE csrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
