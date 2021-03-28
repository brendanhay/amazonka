{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.DeleteSubscriptionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a subscription definition.
module Network.AWS.Greengrass.DeleteSubscriptionDefinition
    (
    -- * Creating a request
      DeleteSubscriptionDefinition (..)
    , mkDeleteSubscriptionDefinition
    -- ** Request lenses
    , dsdSubscriptionDefinitionId

    -- * Destructuring the response
    , DeleteSubscriptionDefinitionResponse (..)
    , mkDeleteSubscriptionDefinitionResponse
    -- ** Response lenses
    , dsdrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteSubscriptionDefinition' smart constructor.
newtype DeleteSubscriptionDefinition = DeleteSubscriptionDefinition'
  { subscriptionDefinitionId :: Core.Text
    -- ^ The ID of the subscription definition.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSubscriptionDefinition' value with any optional fields omitted.
mkDeleteSubscriptionDefinition
    :: Core.Text -- ^ 'subscriptionDefinitionId'
    -> DeleteSubscriptionDefinition
mkDeleteSubscriptionDefinition subscriptionDefinitionId
  = DeleteSubscriptionDefinition'{subscriptionDefinitionId}

-- | The ID of the subscription definition.
--
-- /Note:/ Consider using 'subscriptionDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdSubscriptionDefinitionId :: Lens.Lens' DeleteSubscriptionDefinition Core.Text
dsdSubscriptionDefinitionId = Lens.field @"subscriptionDefinitionId"
{-# INLINEABLE dsdSubscriptionDefinitionId #-}
{-# DEPRECATED subscriptionDefinitionId "Use generic-lens or generic-optics with 'subscriptionDefinitionId' instead"  #-}

instance Core.ToQuery DeleteSubscriptionDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteSubscriptionDefinition where
        toHeaders DeleteSubscriptionDefinition{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteSubscriptionDefinition where
        type Rs DeleteSubscriptionDefinition =
             DeleteSubscriptionDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/greengrass/definition/subscriptions/" Core.<>
                             Core.toText subscriptionDefinitionId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteSubscriptionDefinitionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteSubscriptionDefinitionResponse' smart constructor.
newtype DeleteSubscriptionDefinitionResponse = DeleteSubscriptionDefinitionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSubscriptionDefinitionResponse' value with any optional fields omitted.
mkDeleteSubscriptionDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteSubscriptionDefinitionResponse
mkDeleteSubscriptionDefinitionResponse responseStatus
  = DeleteSubscriptionDefinitionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdrrsResponseStatus :: Lens.Lens' DeleteSubscriptionDefinitionResponse Core.Int
dsdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
