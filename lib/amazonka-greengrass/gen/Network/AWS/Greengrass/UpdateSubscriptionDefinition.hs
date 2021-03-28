{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.UpdateSubscriptionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a subscription definition.
module Network.AWS.Greengrass.UpdateSubscriptionDefinition
    (
    -- * Creating a request
      UpdateSubscriptionDefinition (..)
    , mkUpdateSubscriptionDefinition
    -- ** Request lenses
    , usdSubscriptionDefinitionId
    , usdName

    -- * Destructuring the response
    , UpdateSubscriptionDefinitionResponse (..)
    , mkUpdateSubscriptionDefinitionResponse
    -- ** Response lenses
    , usdrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateSubscriptionDefinition' smart constructor.
data UpdateSubscriptionDefinition = UpdateSubscriptionDefinition'
  { subscriptionDefinitionId :: Core.Text
    -- ^ The ID of the subscription definition.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the definition.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSubscriptionDefinition' value with any optional fields omitted.
mkUpdateSubscriptionDefinition
    :: Core.Text -- ^ 'subscriptionDefinitionId'
    -> UpdateSubscriptionDefinition
mkUpdateSubscriptionDefinition subscriptionDefinitionId
  = UpdateSubscriptionDefinition'{subscriptionDefinitionId,
                                  name = Core.Nothing}

-- | The ID of the subscription definition.
--
-- /Note:/ Consider using 'subscriptionDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdSubscriptionDefinitionId :: Lens.Lens' UpdateSubscriptionDefinition Core.Text
usdSubscriptionDefinitionId = Lens.field @"subscriptionDefinitionId"
{-# INLINEABLE usdSubscriptionDefinitionId #-}
{-# DEPRECATED subscriptionDefinitionId "Use generic-lens or generic-optics with 'subscriptionDefinitionId' instead"  #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdName :: Lens.Lens' UpdateSubscriptionDefinition (Core.Maybe Core.Text)
usdName = Lens.field @"name"
{-# INLINEABLE usdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery UpdateSubscriptionDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateSubscriptionDefinition where
        toHeaders UpdateSubscriptionDefinition{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateSubscriptionDefinition where
        toJSON UpdateSubscriptionDefinition{..}
          = Core.object (Core.catMaybes [("Name" Core..=) Core.<$> name])

instance Core.AWSRequest UpdateSubscriptionDefinition where
        type Rs UpdateSubscriptionDefinition =
             UpdateSubscriptionDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/greengrass/definition/subscriptions/" Core.<>
                             Core.toText subscriptionDefinitionId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateSubscriptionDefinitionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateSubscriptionDefinitionResponse' smart constructor.
newtype UpdateSubscriptionDefinitionResponse = UpdateSubscriptionDefinitionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSubscriptionDefinitionResponse' value with any optional fields omitted.
mkUpdateSubscriptionDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateSubscriptionDefinitionResponse
mkUpdateSubscriptionDefinitionResponse responseStatus
  = UpdateSubscriptionDefinitionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdrrsResponseStatus :: Lens.Lens' UpdateSubscriptionDefinitionResponse Core.Int
usdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE usdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
