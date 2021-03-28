{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.BatchPutScheduledUpdateGroupAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates one or more scheduled scaling actions for an Auto Scaling group. If you leave a parameter unspecified when updating a scheduled scaling action, the corresponding value remains unchanged.
module Network.AWS.AutoScaling.BatchPutScheduledUpdateGroupAction
    (
    -- * Creating a request
      BatchPutScheduledUpdateGroupAction (..)
    , mkBatchPutScheduledUpdateGroupAction
    -- ** Request lenses
    , bpsugaAutoScalingGroupName
    , bpsugaScheduledUpdateGroupActions

    -- * Destructuring the response
    , BatchPutScheduledUpdateGroupActionResponse (..)
    , mkBatchPutScheduledUpdateGroupActionResponse
    -- ** Response lenses
    , bpsugarrsFailedScheduledUpdateGroupActions
    , bpsugarrsResponseStatus
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchPutScheduledUpdateGroupAction' smart constructor.
data BatchPutScheduledUpdateGroupAction = BatchPutScheduledUpdateGroupAction'
  { autoScalingGroupName :: Types.AutoScalingGroupName
    -- ^ The name of the Auto Scaling group.
  , scheduledUpdateGroupActions :: [Types.ScheduledUpdateGroupActionRequest]
    -- ^ One or more scheduled actions. The maximum number allowed is 50.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BatchPutScheduledUpdateGroupAction' value with any optional fields omitted.
mkBatchPutScheduledUpdateGroupAction
    :: Types.AutoScalingGroupName -- ^ 'autoScalingGroupName'
    -> BatchPutScheduledUpdateGroupAction
mkBatchPutScheduledUpdateGroupAction autoScalingGroupName
  = BatchPutScheduledUpdateGroupAction'{autoScalingGroupName,
                                        scheduledUpdateGroupActions = Core.mempty}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpsugaAutoScalingGroupName :: Lens.Lens' BatchPutScheduledUpdateGroupAction Types.AutoScalingGroupName
bpsugaAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# INLINEABLE bpsugaAutoScalingGroupName #-}
{-# DEPRECATED autoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead"  #-}

-- | One or more scheduled actions. The maximum number allowed is 50.
--
-- /Note:/ Consider using 'scheduledUpdateGroupActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpsugaScheduledUpdateGroupActions :: Lens.Lens' BatchPutScheduledUpdateGroupAction [Types.ScheduledUpdateGroupActionRequest]
bpsugaScheduledUpdateGroupActions = Lens.field @"scheduledUpdateGroupActions"
{-# INLINEABLE bpsugaScheduledUpdateGroupActions #-}
{-# DEPRECATED scheduledUpdateGroupActions "Use generic-lens or generic-optics with 'scheduledUpdateGroupActions' instead"  #-}

instance Core.ToQuery BatchPutScheduledUpdateGroupAction where
        toQuery BatchPutScheduledUpdateGroupAction{..}
          = Core.toQueryPair "Action"
              ("BatchPutScheduledUpdateGroupAction" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "AutoScalingGroupName" autoScalingGroupName
              Core.<>
              Core.toQueryPair "ScheduledUpdateGroupActions"
                (Core.toQueryList "member" scheduledUpdateGroupActions)

instance Core.ToHeaders BatchPutScheduledUpdateGroupAction where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest BatchPutScheduledUpdateGroupAction where
        type Rs BatchPutScheduledUpdateGroupAction =
             BatchPutScheduledUpdateGroupActionResponse
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
          = Response.receiveXMLWrapper
              "BatchPutScheduledUpdateGroupActionResult"
              (\ s h x ->
                 BatchPutScheduledUpdateGroupActionResponse' Core.<$>
                   (x Core..@? "FailedScheduledUpdateGroupActions" Core..<@>
                      Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchPutScheduledUpdateGroupActionResponse' smart constructor.
data BatchPutScheduledUpdateGroupActionResponse = BatchPutScheduledUpdateGroupActionResponse'
  { failedScheduledUpdateGroupActions :: Core.Maybe [Types.FailedScheduledUpdateGroupActionRequest]
    -- ^ The names of the scheduled actions that could not be created or updated, including an error message.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchPutScheduledUpdateGroupActionResponse' value with any optional fields omitted.
mkBatchPutScheduledUpdateGroupActionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchPutScheduledUpdateGroupActionResponse
mkBatchPutScheduledUpdateGroupActionResponse responseStatus
  = BatchPutScheduledUpdateGroupActionResponse'{failedScheduledUpdateGroupActions
                                                  = Core.Nothing,
                                                responseStatus}

-- | The names of the scheduled actions that could not be created or updated, including an error message.
--
-- /Note:/ Consider using 'failedScheduledUpdateGroupActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpsugarrsFailedScheduledUpdateGroupActions :: Lens.Lens' BatchPutScheduledUpdateGroupActionResponse (Core.Maybe [Types.FailedScheduledUpdateGroupActionRequest])
bpsugarrsFailedScheduledUpdateGroupActions = Lens.field @"failedScheduledUpdateGroupActions"
{-# INLINEABLE bpsugarrsFailedScheduledUpdateGroupActions #-}
{-# DEPRECATED failedScheduledUpdateGroupActions "Use generic-lens or generic-optics with 'failedScheduledUpdateGroupActions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpsugarrsResponseStatus :: Lens.Lens' BatchPutScheduledUpdateGroupActionResponse Core.Int
bpsugarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bpsugarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
