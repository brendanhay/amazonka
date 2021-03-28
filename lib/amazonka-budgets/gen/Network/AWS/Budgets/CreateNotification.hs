{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.CreateNotification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a notification. You must create the budget before you create the associated notification.
module Network.AWS.Budgets.CreateNotification
    (
    -- * Creating a request
      CreateNotification (..)
    , mkCreateNotification
    -- ** Request lenses
    , cnAccountId
    , cnBudgetName
    , cnNotification
    , cnSubscribers

    -- * Destructuring the response
    , CreateNotificationResponse (..)
    , mkCreateNotificationResponse
    -- ** Response lenses
    , cnrrsResponseStatus
    ) where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request of CreateNotification 
--
-- /See:/ 'mkCreateNotification' smart constructor.
data CreateNotification = CreateNotification'
  { accountId :: Types.AccountId
    -- ^ The @accountId@ that is associated with the budget that you want to create a notification for.
  , budgetName :: Types.BudgetName
    -- ^ The name of the budget that you want AWS to notify you about. Budget names must be unique within an account.
  , notification :: Types.Notification
    -- ^ The notification that you want to create.
  , subscribers :: Core.NonEmpty Types.Subscriber
    -- ^ A list of subscribers that you want to associate with the notification. Each notification can have one SNS subscriber and up to 10 email subscribers.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNotification' value with any optional fields omitted.
mkCreateNotification
    :: Types.AccountId -- ^ 'accountId'
    -> Types.BudgetName -- ^ 'budgetName'
    -> Types.Notification -- ^ 'notification'
    -> Core.NonEmpty Types.Subscriber -- ^ 'subscribers'
    -> CreateNotification
mkCreateNotification accountId budgetName notification subscribers
  = CreateNotification'{accountId, budgetName, notification,
                        subscribers}

-- | The @accountId@ that is associated with the budget that you want to create a notification for.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnAccountId :: Lens.Lens' CreateNotification Types.AccountId
cnAccountId = Lens.field @"accountId"
{-# INLINEABLE cnAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The name of the budget that you want AWS to notify you about. Budget names must be unique within an account.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnBudgetName :: Lens.Lens' CreateNotification Types.BudgetName
cnBudgetName = Lens.field @"budgetName"
{-# INLINEABLE cnBudgetName #-}
{-# DEPRECATED budgetName "Use generic-lens or generic-optics with 'budgetName' instead"  #-}

-- | The notification that you want to create.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnNotification :: Lens.Lens' CreateNotification Types.Notification
cnNotification = Lens.field @"notification"
{-# INLINEABLE cnNotification #-}
{-# DEPRECATED notification "Use generic-lens or generic-optics with 'notification' instead"  #-}

-- | A list of subscribers that you want to associate with the notification. Each notification can have one SNS subscriber and up to 10 email subscribers.
--
-- /Note:/ Consider using 'subscribers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnSubscribers :: Lens.Lens' CreateNotification (Core.NonEmpty Types.Subscriber)
cnSubscribers = Lens.field @"subscribers"
{-# INLINEABLE cnSubscribers #-}
{-# DEPRECATED subscribers "Use generic-lens or generic-optics with 'subscribers' instead"  #-}

instance Core.ToQuery CreateNotification where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateNotification where
        toHeaders CreateNotification{..}
          = Core.pure
              ("X-Amz-Target", "AWSBudgetServiceGateway.CreateNotification")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateNotification where
        toJSON CreateNotification{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AccountId" Core..= accountId),
                  Core.Just ("BudgetName" Core..= budgetName),
                  Core.Just ("Notification" Core..= notification),
                  Core.Just ("Subscribers" Core..= subscribers)])

instance Core.AWSRequest CreateNotification where
        type Rs CreateNotification = CreateNotificationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 CreateNotificationResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Response of CreateNotification 
--
-- /See:/ 'mkCreateNotificationResponse' smart constructor.
newtype CreateNotificationResponse = CreateNotificationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNotificationResponse' value with any optional fields omitted.
mkCreateNotificationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateNotificationResponse
mkCreateNotificationResponse responseStatus
  = CreateNotificationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnrrsResponseStatus :: Lens.Lens' CreateNotificationResponse Core.Int
cnrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cnrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
