{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.CreateSubscriber
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a subscriber. You must create the associated budget and notification before you create the subscriber.
module Network.AWS.Budgets.CreateSubscriber
    (
    -- * Creating a request
      CreateSubscriber (..)
    , mkCreateSubscriber
    -- ** Request lenses
    , csAccountId
    , csBudgetName
    , csNotification
    , csSubscriber

    -- * Destructuring the response
    , CreateSubscriberResponse (..)
    , mkCreateSubscriberResponse
    -- ** Response lenses
    , csrrsResponseStatus
    ) where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request of CreateSubscriber 
--
-- /See:/ 'mkCreateSubscriber' smart constructor.
data CreateSubscriber = CreateSubscriber'
  { accountId :: Types.AccountId
    -- ^ The @accountId@ that is associated with the budget that you want to create a subscriber for.
  , budgetName :: Types.BudgetName
    -- ^ The name of the budget that you want to subscribe to. Budget names must be unique within an account.
  , notification :: Types.Notification
    -- ^ The notification that you want to create a subscriber for.
  , subscriber :: Types.Subscriber
    -- ^ The subscriber that you want to associate with a budget notification.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSubscriber' value with any optional fields omitted.
mkCreateSubscriber
    :: Types.AccountId -- ^ 'accountId'
    -> Types.BudgetName -- ^ 'budgetName'
    -> Types.Notification -- ^ 'notification'
    -> Types.Subscriber -- ^ 'subscriber'
    -> CreateSubscriber
mkCreateSubscriber accountId budgetName notification subscriber
  = CreateSubscriber'{accountId, budgetName, notification,
                      subscriber}

-- | The @accountId@ that is associated with the budget that you want to create a subscriber for.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csAccountId :: Lens.Lens' CreateSubscriber Types.AccountId
csAccountId = Lens.field @"accountId"
{-# INLINEABLE csAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The name of the budget that you want to subscribe to. Budget names must be unique within an account.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csBudgetName :: Lens.Lens' CreateSubscriber Types.BudgetName
csBudgetName = Lens.field @"budgetName"
{-# INLINEABLE csBudgetName #-}
{-# DEPRECATED budgetName "Use generic-lens or generic-optics with 'budgetName' instead"  #-}

-- | The notification that you want to create a subscriber for.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csNotification :: Lens.Lens' CreateSubscriber Types.Notification
csNotification = Lens.field @"notification"
{-# INLINEABLE csNotification #-}
{-# DEPRECATED notification "Use generic-lens or generic-optics with 'notification' instead"  #-}

-- | The subscriber that you want to associate with a budget notification.
--
-- /Note:/ Consider using 'subscriber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSubscriber :: Lens.Lens' CreateSubscriber Types.Subscriber
csSubscriber = Lens.field @"subscriber"
{-# INLINEABLE csSubscriber #-}
{-# DEPRECATED subscriber "Use generic-lens or generic-optics with 'subscriber' instead"  #-}

instance Core.ToQuery CreateSubscriber where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateSubscriber where
        toHeaders CreateSubscriber{..}
          = Core.pure
              ("X-Amz-Target", "AWSBudgetServiceGateway.CreateSubscriber")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateSubscriber where
        toJSON CreateSubscriber{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AccountId" Core..= accountId),
                  Core.Just ("BudgetName" Core..= budgetName),
                  Core.Just ("Notification" Core..= notification),
                  Core.Just ("Subscriber" Core..= subscriber)])

instance Core.AWSRequest CreateSubscriber where
        type Rs CreateSubscriber = CreateSubscriberResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 CreateSubscriberResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Response of CreateSubscriber 
--
-- /See:/ 'mkCreateSubscriberResponse' smart constructor.
newtype CreateSubscriberResponse = CreateSubscriberResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSubscriberResponse' value with any optional fields omitted.
mkCreateSubscriberResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateSubscriberResponse
mkCreateSubscriberResponse responseStatus
  = CreateSubscriberResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsResponseStatus :: Lens.Lens' CreateSubscriberResponse Core.Int
csrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE csrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
