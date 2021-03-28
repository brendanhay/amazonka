{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.UpdateSubscriber
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a subscriber.
module Network.AWS.Budgets.UpdateSubscriber
    (
    -- * Creating a request
      UpdateSubscriber (..)
    , mkUpdateSubscriber
    -- ** Request lenses
    , usAccountId
    , usBudgetName
    , usNotification
    , usOldSubscriber
    , usNewSubscriber

    -- * Destructuring the response
    , UpdateSubscriberResponse (..)
    , mkUpdateSubscriberResponse
    -- ** Response lenses
    , usrrsResponseStatus
    ) where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request of UpdateSubscriber 
--
-- /See:/ 'mkUpdateSubscriber' smart constructor.
data UpdateSubscriber = UpdateSubscriber'
  { accountId :: Types.AccountId
    -- ^ The @accountId@ that is associated with the budget whose subscriber you want to update.
  , budgetName :: Types.BudgetName
    -- ^ The name of the budget whose subscriber you want to update.
  , notification :: Types.Notification
    -- ^ The notification whose subscriber you want to update.
  , oldSubscriber :: Types.Subscriber
    -- ^ The previous subscriber that is associated with a budget notification.
  , newSubscriber :: Types.Subscriber
    -- ^ The updated subscriber that is associated with a budget notification.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSubscriber' value with any optional fields omitted.
mkUpdateSubscriber
    :: Types.AccountId -- ^ 'accountId'
    -> Types.BudgetName -- ^ 'budgetName'
    -> Types.Notification -- ^ 'notification'
    -> Types.Subscriber -- ^ 'oldSubscriber'
    -> Types.Subscriber -- ^ 'newSubscriber'
    -> UpdateSubscriber
mkUpdateSubscriber accountId budgetName notification oldSubscriber
  newSubscriber
  = UpdateSubscriber'{accountId, budgetName, notification,
                      oldSubscriber, newSubscriber}

-- | The @accountId@ that is associated with the budget whose subscriber you want to update.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usAccountId :: Lens.Lens' UpdateSubscriber Types.AccountId
usAccountId = Lens.field @"accountId"
{-# INLINEABLE usAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The name of the budget whose subscriber you want to update.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usBudgetName :: Lens.Lens' UpdateSubscriber Types.BudgetName
usBudgetName = Lens.field @"budgetName"
{-# INLINEABLE usBudgetName #-}
{-# DEPRECATED budgetName "Use generic-lens or generic-optics with 'budgetName' instead"  #-}

-- | The notification whose subscriber you want to update.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usNotification :: Lens.Lens' UpdateSubscriber Types.Notification
usNotification = Lens.field @"notification"
{-# INLINEABLE usNotification #-}
{-# DEPRECATED notification "Use generic-lens or generic-optics with 'notification' instead"  #-}

-- | The previous subscriber that is associated with a budget notification.
--
-- /Note:/ Consider using 'oldSubscriber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usOldSubscriber :: Lens.Lens' UpdateSubscriber Types.Subscriber
usOldSubscriber = Lens.field @"oldSubscriber"
{-# INLINEABLE usOldSubscriber #-}
{-# DEPRECATED oldSubscriber "Use generic-lens or generic-optics with 'oldSubscriber' instead"  #-}

-- | The updated subscriber that is associated with a budget notification.
--
-- /Note:/ Consider using 'newSubscriber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usNewSubscriber :: Lens.Lens' UpdateSubscriber Types.Subscriber
usNewSubscriber = Lens.field @"newSubscriber"
{-# INLINEABLE usNewSubscriber #-}
{-# DEPRECATED newSubscriber "Use generic-lens or generic-optics with 'newSubscriber' instead"  #-}

instance Core.ToQuery UpdateSubscriber where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateSubscriber where
        toHeaders UpdateSubscriber{..}
          = Core.pure
              ("X-Amz-Target", "AWSBudgetServiceGateway.UpdateSubscriber")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateSubscriber where
        toJSON UpdateSubscriber{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AccountId" Core..= accountId),
                  Core.Just ("BudgetName" Core..= budgetName),
                  Core.Just ("Notification" Core..= notification),
                  Core.Just ("OldSubscriber" Core..= oldSubscriber),
                  Core.Just ("NewSubscriber" Core..= newSubscriber)])

instance Core.AWSRequest UpdateSubscriber where
        type Rs UpdateSubscriber = UpdateSubscriberResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateSubscriberResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Response of UpdateSubscriber 
--
-- /See:/ 'mkUpdateSubscriberResponse' smart constructor.
newtype UpdateSubscriberResponse = UpdateSubscriberResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSubscriberResponse' value with any optional fields omitted.
mkUpdateSubscriberResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateSubscriberResponse
mkUpdateSubscriberResponse responseStatus
  = UpdateSubscriberResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsResponseStatus :: Lens.Lens' UpdateSubscriberResponse Core.Int
usrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE usrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
