{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.DeleteNotification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a notification.
--
-- /Important:/ Deleting a notification also deletes the subscribers that are associated with the notification.
module Network.AWS.Budgets.DeleteNotification
    (
    -- * Creating a request
      DeleteNotification (..)
    , mkDeleteNotification
    -- ** Request lenses
    , dnAccountId
    , dnBudgetName
    , dnNotification

    -- * Destructuring the response
    , DeleteNotificationResponse (..)
    , mkDeleteNotificationResponse
    -- ** Response lenses
    , dnrrsResponseStatus
    ) where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request of DeleteNotification 
--
-- /See:/ 'mkDeleteNotification' smart constructor.
data DeleteNotification = DeleteNotification'
  { accountId :: Types.AccountId
    -- ^ The @accountId@ that is associated with the budget whose notification you want to delete.
  , budgetName :: Types.BudgetName
    -- ^ The name of the budget whose notification you want to delete.
  , notification :: Types.Notification
    -- ^ The notification that you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNotification' value with any optional fields omitted.
mkDeleteNotification
    :: Types.AccountId -- ^ 'accountId'
    -> Types.BudgetName -- ^ 'budgetName'
    -> Types.Notification -- ^ 'notification'
    -> DeleteNotification
mkDeleteNotification accountId budgetName notification
  = DeleteNotification'{accountId, budgetName, notification}

-- | The @accountId@ that is associated with the budget whose notification you want to delete.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnAccountId :: Lens.Lens' DeleteNotification Types.AccountId
dnAccountId = Lens.field @"accountId"
{-# INLINEABLE dnAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The name of the budget whose notification you want to delete.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnBudgetName :: Lens.Lens' DeleteNotification Types.BudgetName
dnBudgetName = Lens.field @"budgetName"
{-# INLINEABLE dnBudgetName #-}
{-# DEPRECATED budgetName "Use generic-lens or generic-optics with 'budgetName' instead"  #-}

-- | The notification that you want to delete.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnNotification :: Lens.Lens' DeleteNotification Types.Notification
dnNotification = Lens.field @"notification"
{-# INLINEABLE dnNotification #-}
{-# DEPRECATED notification "Use generic-lens or generic-optics with 'notification' instead"  #-}

instance Core.ToQuery DeleteNotification where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteNotification where
        toHeaders DeleteNotification{..}
          = Core.pure
              ("X-Amz-Target", "AWSBudgetServiceGateway.DeleteNotification")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteNotification where
        toJSON DeleteNotification{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AccountId" Core..= accountId),
                  Core.Just ("BudgetName" Core..= budgetName),
                  Core.Just ("Notification" Core..= notification)])

instance Core.AWSRequest DeleteNotification where
        type Rs DeleteNotification = DeleteNotificationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteNotificationResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Response of DeleteNotification 
--
-- /See:/ 'mkDeleteNotificationResponse' smart constructor.
newtype DeleteNotificationResponse = DeleteNotificationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNotificationResponse' value with any optional fields omitted.
mkDeleteNotificationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteNotificationResponse
mkDeleteNotificationResponse responseStatus
  = DeleteNotificationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnrrsResponseStatus :: Lens.Lens' DeleteNotificationResponse Core.Int
dnrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dnrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
