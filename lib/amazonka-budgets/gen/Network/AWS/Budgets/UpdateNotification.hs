{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.UpdateNotification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a notification.
module Network.AWS.Budgets.UpdateNotification
    (
    -- * Creating a request
      UpdateNotification (..)
    , mkUpdateNotification
    -- ** Request lenses
    , unAccountId
    , unBudgetName
    , unOldNotification
    , unNewNotification

    -- * Destructuring the response
    , UpdateNotificationResponse (..)
    , mkUpdateNotificationResponse
    -- ** Response lenses
    , unrrsResponseStatus
    ) where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request of UpdateNotification 
--
-- /See:/ 'mkUpdateNotification' smart constructor.
data UpdateNotification = UpdateNotification'
  { accountId :: Types.AccountId
    -- ^ The @accountId@ that is associated with the budget whose notification you want to update.
  , budgetName :: Types.BudgetName
    -- ^ The name of the budget whose notification you want to update.
  , oldNotification :: Types.Notification
    -- ^ The previous notification that is associated with a budget.
  , newNotification :: Types.Notification
    -- ^ The updated notification to be associated with a budget.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateNotification' value with any optional fields omitted.
mkUpdateNotification
    :: Types.AccountId -- ^ 'accountId'
    -> Types.BudgetName -- ^ 'budgetName'
    -> Types.Notification -- ^ 'oldNotification'
    -> Types.Notification -- ^ 'newNotification'
    -> UpdateNotification
mkUpdateNotification accountId budgetName oldNotification
  newNotification
  = UpdateNotification'{accountId, budgetName, oldNotification,
                        newNotification}

-- | The @accountId@ that is associated with the budget whose notification you want to update.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unAccountId :: Lens.Lens' UpdateNotification Types.AccountId
unAccountId = Lens.field @"accountId"
{-# INLINEABLE unAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The name of the budget whose notification you want to update.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unBudgetName :: Lens.Lens' UpdateNotification Types.BudgetName
unBudgetName = Lens.field @"budgetName"
{-# INLINEABLE unBudgetName #-}
{-# DEPRECATED budgetName "Use generic-lens or generic-optics with 'budgetName' instead"  #-}

-- | The previous notification that is associated with a budget.
--
-- /Note:/ Consider using 'oldNotification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unOldNotification :: Lens.Lens' UpdateNotification Types.Notification
unOldNotification = Lens.field @"oldNotification"
{-# INLINEABLE unOldNotification #-}
{-# DEPRECATED oldNotification "Use generic-lens or generic-optics with 'oldNotification' instead"  #-}

-- | The updated notification to be associated with a budget.
--
-- /Note:/ Consider using 'newNotification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unNewNotification :: Lens.Lens' UpdateNotification Types.Notification
unNewNotification = Lens.field @"newNotification"
{-# INLINEABLE unNewNotification #-}
{-# DEPRECATED newNotification "Use generic-lens or generic-optics with 'newNotification' instead"  #-}

instance Core.ToQuery UpdateNotification where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateNotification where
        toHeaders UpdateNotification{..}
          = Core.pure
              ("X-Amz-Target", "AWSBudgetServiceGateway.UpdateNotification")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateNotification where
        toJSON UpdateNotification{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AccountId" Core..= accountId),
                  Core.Just ("BudgetName" Core..= budgetName),
                  Core.Just ("OldNotification" Core..= oldNotification),
                  Core.Just ("NewNotification" Core..= newNotification)])

instance Core.AWSRequest UpdateNotification where
        type Rs UpdateNotification = UpdateNotificationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateNotificationResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Response of UpdateNotification 
--
-- /See:/ 'mkUpdateNotificationResponse' smart constructor.
newtype UpdateNotificationResponse = UpdateNotificationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateNotificationResponse' value with any optional fields omitted.
mkUpdateNotificationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateNotificationResponse
mkUpdateNotificationResponse responseStatus
  = UpdateNotificationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unrrsResponseStatus :: Lens.Lens' UpdateNotificationResponse Core.Int
unrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE unrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
