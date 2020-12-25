{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    BatchDeleteScheduledAction (..),
    mkBatchDeleteScheduledAction,

    -- ** Request lenses
    bdsaAutoScalingGroupName,
    bdsaScheduledActionNames,

    -- * Destructuring the response
    BatchDeleteScheduledActionResponse (..),
    mkBatchDeleteScheduledActionResponse,

    -- ** Response lenses
    bdsarrsFailedScheduledActions,
    bdsarrsResponseStatus,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchDeleteScheduledAction' smart constructor.
data BatchDeleteScheduledAction = BatchDeleteScheduledAction'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Types.AutoScalingGroupName,
    -- | The names of the scheduled actions to delete. The maximum number allowed is 50.
    scheduledActionNames :: [Types.ResourceName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDeleteScheduledAction' value with any optional fields omitted.
mkBatchDeleteScheduledAction ::
  -- | 'autoScalingGroupName'
  Types.AutoScalingGroupName ->
  BatchDeleteScheduledAction
mkBatchDeleteScheduledAction autoScalingGroupName =
  BatchDeleteScheduledAction'
    { autoScalingGroupName,
      scheduledActionNames = Core.mempty
    }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsaAutoScalingGroupName :: Lens.Lens' BatchDeleteScheduledAction Types.AutoScalingGroupName
bdsaAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# DEPRECATED bdsaAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The names of the scheduled actions to delete. The maximum number allowed is 50.
--
-- /Note:/ Consider using 'scheduledActionNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsaScheduledActionNames :: Lens.Lens' BatchDeleteScheduledAction [Types.ResourceName]
bdsaScheduledActionNames = Lens.field @"scheduledActionNames"
{-# DEPRECATED bdsaScheduledActionNames "Use generic-lens or generic-optics with 'scheduledActionNames' instead." #-}

instance Core.AWSRequest BatchDeleteScheduledAction where
  type
    Rs BatchDeleteScheduledAction =
      BatchDeleteScheduledActionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "BatchDeleteScheduledAction")
                Core.<> (Core.pure ("Version", "2011-01-01"))
                Core.<> (Core.toQueryValue "AutoScalingGroupName" autoScalingGroupName)
                Core.<> ( Core.toQueryValue
                            "ScheduledActionNames"
                            (Core.toQueryList "member" scheduledActionNames)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "BatchDeleteScheduledActionResult"
      ( \s h x ->
          BatchDeleteScheduledActionResponse'
            Core.<$> ( x Core..@? "FailedScheduledActions"
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkBatchDeleteScheduledActionResponse' smart constructor.
data BatchDeleteScheduledActionResponse = BatchDeleteScheduledActionResponse'
  { -- | The names of the scheduled actions that could not be deleted, including an error message.
    failedScheduledActions :: Core.Maybe [Types.FailedScheduledUpdateGroupActionRequest],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDeleteScheduledActionResponse' value with any optional fields omitted.
mkBatchDeleteScheduledActionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchDeleteScheduledActionResponse
mkBatchDeleteScheduledActionResponse responseStatus =
  BatchDeleteScheduledActionResponse'
    { failedScheduledActions =
        Core.Nothing,
      responseStatus
    }

-- | The names of the scheduled actions that could not be deleted, including an error message.
--
-- /Note:/ Consider using 'failedScheduledActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsarrsFailedScheduledActions :: Lens.Lens' BatchDeleteScheduledActionResponse (Core.Maybe [Types.FailedScheduledUpdateGroupActionRequest])
bdsarrsFailedScheduledActions = Lens.field @"failedScheduledActions"
{-# DEPRECATED bdsarrsFailedScheduledActions "Use generic-lens or generic-optics with 'failedScheduledActions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsarrsResponseStatus :: Lens.Lens' BatchDeleteScheduledActionResponse Core.Int
bdsarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bdsarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
