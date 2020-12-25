{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    BatchPutScheduledUpdateGroupAction (..),
    mkBatchPutScheduledUpdateGroupAction,

    -- ** Request lenses
    bpsugaAutoScalingGroupName,
    bpsugaScheduledUpdateGroupActions,

    -- * Destructuring the response
    BatchPutScheduledUpdateGroupActionResponse (..),
    mkBatchPutScheduledUpdateGroupActionResponse,

    -- ** Response lenses
    bpsugarrsFailedScheduledUpdateGroupActions,
    bpsugarrsResponseStatus,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchPutScheduledUpdateGroupAction' smart constructor.
data BatchPutScheduledUpdateGroupAction = BatchPutScheduledUpdateGroupAction'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Types.AutoScalingGroupName,
    -- | One or more scheduled actions. The maximum number allowed is 50.
    scheduledUpdateGroupActions :: [Types.ScheduledUpdateGroupActionRequest]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchPutScheduledUpdateGroupAction' value with any optional fields omitted.
mkBatchPutScheduledUpdateGroupAction ::
  -- | 'autoScalingGroupName'
  Types.AutoScalingGroupName ->
  BatchPutScheduledUpdateGroupAction
mkBatchPutScheduledUpdateGroupAction autoScalingGroupName =
  BatchPutScheduledUpdateGroupAction'
    { autoScalingGroupName,
      scheduledUpdateGroupActions = Core.mempty
    }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpsugaAutoScalingGroupName :: Lens.Lens' BatchPutScheduledUpdateGroupAction Types.AutoScalingGroupName
bpsugaAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# DEPRECATED bpsugaAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | One or more scheduled actions. The maximum number allowed is 50.
--
-- /Note:/ Consider using 'scheduledUpdateGroupActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpsugaScheduledUpdateGroupActions :: Lens.Lens' BatchPutScheduledUpdateGroupAction [Types.ScheduledUpdateGroupActionRequest]
bpsugaScheduledUpdateGroupActions = Lens.field @"scheduledUpdateGroupActions"
{-# DEPRECATED bpsugaScheduledUpdateGroupActions "Use generic-lens or generic-optics with 'scheduledUpdateGroupActions' instead." #-}

instance Core.AWSRequest BatchPutScheduledUpdateGroupAction where
  type
    Rs BatchPutScheduledUpdateGroupAction =
      BatchPutScheduledUpdateGroupActionResponse
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
            ( Core.pure ("Action", "BatchPutScheduledUpdateGroupAction")
                Core.<> (Core.pure ("Version", "2011-01-01"))
                Core.<> (Core.toQueryValue "AutoScalingGroupName" autoScalingGroupName)
                Core.<> ( Core.toQueryValue
                            "ScheduledUpdateGroupActions"
                            (Core.toQueryList "member" scheduledUpdateGroupActions)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "BatchPutScheduledUpdateGroupActionResult"
      ( \s h x ->
          BatchPutScheduledUpdateGroupActionResponse'
            Core.<$> ( x Core..@? "FailedScheduledUpdateGroupActions"
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkBatchPutScheduledUpdateGroupActionResponse' smart constructor.
data BatchPutScheduledUpdateGroupActionResponse = BatchPutScheduledUpdateGroupActionResponse'
  { -- | The names of the scheduled actions that could not be created or updated, including an error message.
    failedScheduledUpdateGroupActions :: Core.Maybe [Types.FailedScheduledUpdateGroupActionRequest],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchPutScheduledUpdateGroupActionResponse' value with any optional fields omitted.
mkBatchPutScheduledUpdateGroupActionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchPutScheduledUpdateGroupActionResponse
mkBatchPutScheduledUpdateGroupActionResponse responseStatus =
  BatchPutScheduledUpdateGroupActionResponse'
    { failedScheduledUpdateGroupActions =
        Core.Nothing,
      responseStatus
    }

-- | The names of the scheduled actions that could not be created or updated, including an error message.
--
-- /Note:/ Consider using 'failedScheduledUpdateGroupActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpsugarrsFailedScheduledUpdateGroupActions :: Lens.Lens' BatchPutScheduledUpdateGroupActionResponse (Core.Maybe [Types.FailedScheduledUpdateGroupActionRequest])
bpsugarrsFailedScheduledUpdateGroupActions = Lens.field @"failedScheduledUpdateGroupActions"
{-# DEPRECATED bpsugarrsFailedScheduledUpdateGroupActions "Use generic-lens or generic-optics with 'failedScheduledUpdateGroupActions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpsugarrsResponseStatus :: Lens.Lens' BatchPutScheduledUpdateGroupActionResponse Core.Int
bpsugarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bpsugarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
