{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.BatchDeleteScheduledAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more scheduled actions for the specified Auto Scaling group.
module Network.AWS.AutoScaling.BatchDeleteScheduledAction
    (
    -- * Creating a request
      BatchDeleteScheduledAction (..)
    , mkBatchDeleteScheduledAction
    -- ** Request lenses
    , bdsaAutoScalingGroupName
    , bdsaScheduledActionNames

    -- * Destructuring the response
    , BatchDeleteScheduledActionResponse (..)
    , mkBatchDeleteScheduledActionResponse
    -- ** Response lenses
    , bdsarrsFailedScheduledActions
    , bdsarrsResponseStatus
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchDeleteScheduledAction' smart constructor.
data BatchDeleteScheduledAction = BatchDeleteScheduledAction'
  { autoScalingGroupName :: Types.AutoScalingGroupName
    -- ^ The name of the Auto Scaling group.
  , scheduledActionNames :: [Types.ResourceName]
    -- ^ The names of the scheduled actions to delete. The maximum number allowed is 50. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDeleteScheduledAction' value with any optional fields omitted.
mkBatchDeleteScheduledAction
    :: Types.AutoScalingGroupName -- ^ 'autoScalingGroupName'
    -> BatchDeleteScheduledAction
mkBatchDeleteScheduledAction autoScalingGroupName
  = BatchDeleteScheduledAction'{autoScalingGroupName,
                                scheduledActionNames = Core.mempty}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsaAutoScalingGroupName :: Lens.Lens' BatchDeleteScheduledAction Types.AutoScalingGroupName
bdsaAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# INLINEABLE bdsaAutoScalingGroupName #-}
{-# DEPRECATED autoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead"  #-}

-- | The names of the scheduled actions to delete. The maximum number allowed is 50. 
--
-- /Note:/ Consider using 'scheduledActionNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsaScheduledActionNames :: Lens.Lens' BatchDeleteScheduledAction [Types.ResourceName]
bdsaScheduledActionNames = Lens.field @"scheduledActionNames"
{-# INLINEABLE bdsaScheduledActionNames #-}
{-# DEPRECATED scheduledActionNames "Use generic-lens or generic-optics with 'scheduledActionNames' instead"  #-}

instance Core.ToQuery BatchDeleteScheduledAction where
        toQuery BatchDeleteScheduledAction{..}
          = Core.toQueryPair "Action"
              ("BatchDeleteScheduledAction" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "AutoScalingGroupName" autoScalingGroupName
              Core.<>
              Core.toQueryPair "ScheduledActionNames"
                (Core.toQueryList "member" scheduledActionNames)

instance Core.ToHeaders BatchDeleteScheduledAction where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest BatchDeleteScheduledAction where
        type Rs BatchDeleteScheduledAction =
             BatchDeleteScheduledActionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "BatchDeleteScheduledActionResult"
              (\ s h x ->
                 BatchDeleteScheduledActionResponse' Core.<$>
                   (x Core..@? "FailedScheduledActions" Core..<@>
                      Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchDeleteScheduledActionResponse' smart constructor.
data BatchDeleteScheduledActionResponse = BatchDeleteScheduledActionResponse'
  { failedScheduledActions :: Core.Maybe [Types.FailedScheduledUpdateGroupActionRequest]
    -- ^ The names of the scheduled actions that could not be deleted, including an error message.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDeleteScheduledActionResponse' value with any optional fields omitted.
mkBatchDeleteScheduledActionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchDeleteScheduledActionResponse
mkBatchDeleteScheduledActionResponse responseStatus
  = BatchDeleteScheduledActionResponse'{failedScheduledActions =
                                          Core.Nothing,
                                        responseStatus}

-- | The names of the scheduled actions that could not be deleted, including an error message.
--
-- /Note:/ Consider using 'failedScheduledActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsarrsFailedScheduledActions :: Lens.Lens' BatchDeleteScheduledActionResponse (Core.Maybe [Types.FailedScheduledUpdateGroupActionRequest])
bdsarrsFailedScheduledActions = Lens.field @"failedScheduledActions"
{-# INLINEABLE bdsarrsFailedScheduledActions #-}
{-# DEPRECATED failedScheduledActions "Use generic-lens or generic-optics with 'failedScheduledActions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsarrsResponseStatus :: Lens.Lens' BatchDeleteScheduledActionResponse Core.Int
bdsarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bdsarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
