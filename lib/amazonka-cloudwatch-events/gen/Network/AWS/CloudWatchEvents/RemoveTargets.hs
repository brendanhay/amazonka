{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.RemoveTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified targets from the specified rule. When the rule is triggered, those targets are no longer be invoked.
--
-- When you remove a target, when the associated rule triggers, removed targets might continue to be invoked. Allow a short period of time for changes to take effect.
-- This action can partially fail if too many requests are made at the same time. If that happens, @FailedEntryCount@ is non-zero in the response and each entry in @FailedEntries@ provides the ID of the failed target and the error code.
module Network.AWS.CloudWatchEvents.RemoveTargets
  ( -- * Creating a request
    RemoveTargets (..),
    mkRemoveTargets,

    -- ** Request lenses
    rtRule,
    rtIds,
    rtEventBusName,
    rtForce,

    -- * Destructuring the response
    RemoveTargetsResponse (..),
    mkRemoveTargetsResponse,

    -- ** Response lenses
    rtrrsFailedEntries,
    rtrrsFailedEntryCount,
    rtrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemoveTargets' smart constructor.
data RemoveTargets = RemoveTargets'
  { -- | The name of the rule.
    rule :: Types.RuleName,
    -- | The IDs of the targets to remove from the rule.
    ids :: Core.NonEmpty Types.TargetId,
    -- | The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
    eventBusName :: Core.Maybe Types.EventBusNameOrArn,
    -- | If this is a managed rule, created by an AWS service on your behalf, you must specify @Force@ as @True@ to remove targets. This parameter is ignored for rules that are not managed rules. You can check whether a rule is a managed rule by using @DescribeRule@ or @ListRules@ and checking the @ManagedBy@ field of the response.
    force :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTargets' value with any optional fields omitted.
mkRemoveTargets ::
  -- | 'rule'
  Types.RuleName ->
  -- | 'ids'
  Core.NonEmpty Types.TargetId ->
  RemoveTargets
mkRemoveTargets rule ids =
  RemoveTargets'
    { rule,
      ids,
      eventBusName = Core.Nothing,
      force = Core.Nothing
    }

-- | The name of the rule.
--
-- /Note:/ Consider using 'rule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtRule :: Lens.Lens' RemoveTargets Types.RuleName
rtRule = Lens.field @"rule"
{-# DEPRECATED rtRule "Use generic-lens or generic-optics with 'rule' instead." #-}

-- | The IDs of the targets to remove from the rule.
--
-- /Note:/ Consider using 'ids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtIds :: Lens.Lens' RemoveTargets (Core.NonEmpty Types.TargetId)
rtIds = Lens.field @"ids"
{-# DEPRECATED rtIds "Use generic-lens or generic-optics with 'ids' instead." #-}

-- | The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
--
-- /Note:/ Consider using 'eventBusName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtEventBusName :: Lens.Lens' RemoveTargets (Core.Maybe Types.EventBusNameOrArn)
rtEventBusName = Lens.field @"eventBusName"
{-# DEPRECATED rtEventBusName "Use generic-lens or generic-optics with 'eventBusName' instead." #-}

-- | If this is a managed rule, created by an AWS service on your behalf, you must specify @Force@ as @True@ to remove targets. This parameter is ignored for rules that are not managed rules. You can check whether a rule is a managed rule by using @DescribeRule@ or @ListRules@ and checking the @ManagedBy@ field of the response.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtForce :: Lens.Lens' RemoveTargets (Core.Maybe Core.Bool)
rtForce = Lens.field @"force"
{-# DEPRECATED rtForce "Use generic-lens or generic-optics with 'force' instead." #-}

instance Core.FromJSON RemoveTargets where
  toJSON RemoveTargets {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Rule" Core..= rule),
            Core.Just ("Ids" Core..= ids),
            ("EventBusName" Core..=) Core.<$> eventBusName,
            ("Force" Core..=) Core.<$> force
          ]
      )

instance Core.AWSRequest RemoveTargets where
  type Rs RemoveTargets = RemoveTargetsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSEvents.RemoveTargets")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RemoveTargetsResponse'
            Core.<$> (x Core..:? "FailedEntries")
            Core.<*> (x Core..:? "FailedEntryCount")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRemoveTargetsResponse' smart constructor.
data RemoveTargetsResponse = RemoveTargetsResponse'
  { -- | The failed target entries.
    failedEntries :: Core.Maybe [Types.RemoveTargetsResultEntry],
    -- | The number of failed entries.
    failedEntryCount :: Core.Maybe Core.Int,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTargetsResponse' value with any optional fields omitted.
mkRemoveTargetsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RemoveTargetsResponse
mkRemoveTargetsResponse responseStatus =
  RemoveTargetsResponse'
    { failedEntries = Core.Nothing,
      failedEntryCount = Core.Nothing,
      responseStatus
    }

-- | The failed target entries.
--
-- /Note:/ Consider using 'failedEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrrsFailedEntries :: Lens.Lens' RemoveTargetsResponse (Core.Maybe [Types.RemoveTargetsResultEntry])
rtrrsFailedEntries = Lens.field @"failedEntries"
{-# DEPRECATED rtrrsFailedEntries "Use generic-lens or generic-optics with 'failedEntries' instead." #-}

-- | The number of failed entries.
--
-- /Note:/ Consider using 'failedEntryCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrrsFailedEntryCount :: Lens.Lens' RemoveTargetsResponse (Core.Maybe Core.Int)
rtrrsFailedEntryCount = Lens.field @"failedEntryCount"
{-# DEPRECATED rtrrsFailedEntryCount "Use generic-lens or generic-optics with 'failedEntryCount' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrrsResponseStatus :: Lens.Lens' RemoveTargetsResponse Core.Int
rtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
