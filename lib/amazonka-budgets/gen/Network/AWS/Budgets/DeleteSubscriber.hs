{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.DeleteSubscriber
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a subscriber.
--
-- /Important:/ Deleting the last subscriber to a notification also deletes the notification.
module Network.AWS.Budgets.DeleteSubscriber
    (
    -- * Creating a request
      DeleteSubscriber (..)
    , mkDeleteSubscriber
    -- ** Request lenses
    , dsAccountId
    , dsBudgetName
    , dsNotification
    , dsSubscriber

    -- * Destructuring the response
    , DeleteSubscriberResponse (..)
    , mkDeleteSubscriberResponse
    -- ** Response lenses
    , dsrrsResponseStatus
    ) where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request of DeleteSubscriber 
--
-- /See:/ 'mkDeleteSubscriber' smart constructor.
data DeleteSubscriber = DeleteSubscriber'
  { accountId :: Types.AccountId
    -- ^ The @accountId@ that is associated with the budget whose subscriber you want to delete.
  , budgetName :: Types.BudgetName
    -- ^ The name of the budget whose subscriber you want to delete.
  , notification :: Types.Notification
    -- ^ The notification whose subscriber you want to delete.
  , subscriber :: Types.Subscriber
    -- ^ The subscriber that you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSubscriber' value with any optional fields omitted.
mkDeleteSubscriber
    :: Types.AccountId -- ^ 'accountId'
    -> Types.BudgetName -- ^ 'budgetName'
    -> Types.Notification -- ^ 'notification'
    -> Types.Subscriber -- ^ 'subscriber'
    -> DeleteSubscriber
mkDeleteSubscriber accountId budgetName notification subscriber
  = DeleteSubscriber'{accountId, budgetName, notification,
                      subscriber}

-- | The @accountId@ that is associated with the budget whose subscriber you want to delete.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsAccountId :: Lens.Lens' DeleteSubscriber Types.AccountId
dsAccountId = Lens.field @"accountId"
{-# INLINEABLE dsAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The name of the budget whose subscriber you want to delete.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsBudgetName :: Lens.Lens' DeleteSubscriber Types.BudgetName
dsBudgetName = Lens.field @"budgetName"
{-# INLINEABLE dsBudgetName #-}
{-# DEPRECATED budgetName "Use generic-lens or generic-optics with 'budgetName' instead"  #-}

-- | The notification whose subscriber you want to delete.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsNotification :: Lens.Lens' DeleteSubscriber Types.Notification
dsNotification = Lens.field @"notification"
{-# INLINEABLE dsNotification #-}
{-# DEPRECATED notification "Use generic-lens or generic-optics with 'notification' instead"  #-}

-- | The subscriber that you want to delete.
--
-- /Note:/ Consider using 'subscriber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSubscriber :: Lens.Lens' DeleteSubscriber Types.Subscriber
dsSubscriber = Lens.field @"subscriber"
{-# INLINEABLE dsSubscriber #-}
{-# DEPRECATED subscriber "Use generic-lens or generic-optics with 'subscriber' instead"  #-}

instance Core.ToQuery DeleteSubscriber where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteSubscriber where
        toHeaders DeleteSubscriber{..}
          = Core.pure
              ("X-Amz-Target", "AWSBudgetServiceGateway.DeleteSubscriber")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteSubscriber where
        toJSON DeleteSubscriber{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AccountId" Core..= accountId),
                  Core.Just ("BudgetName" Core..= budgetName),
                  Core.Just ("Notification" Core..= notification),
                  Core.Just ("Subscriber" Core..= subscriber)])

instance Core.AWSRequest DeleteSubscriber where
        type Rs DeleteSubscriber = DeleteSubscriberResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteSubscriberResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Response of DeleteSubscriber 
--
-- /See:/ 'mkDeleteSubscriberResponse' smart constructor.
newtype DeleteSubscriberResponse = DeleteSubscriberResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSubscriberResponse' value with any optional fields omitted.
mkDeleteSubscriberResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteSubscriberResponse
mkDeleteSubscriberResponse responseStatus
  = DeleteSubscriberResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DeleteSubscriberResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
