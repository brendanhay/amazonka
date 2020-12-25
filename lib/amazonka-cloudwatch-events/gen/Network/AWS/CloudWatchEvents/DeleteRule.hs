{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.DeleteRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified rule.
--
-- Before you can delete the rule, you must remove all targets, using 'RemoveTargets' .
-- When you delete a rule, incoming events might continue to match to the deleted rule. Allow a short period of time for changes to take effect.
-- Managed rules are rules created and managed by another AWS service on your behalf. These rules are created by those other AWS services to support functionality in those services. You can delete these rules using the @Force@ option, but you should do so only if you are sure the other service is not still using that rule.
module Network.AWS.CloudWatchEvents.DeleteRule
  ( -- * Creating a request
    DeleteRule (..),
    mkDeleteRule,

    -- ** Request lenses
    drName,
    drEventBusName,
    drForce,

    -- * Destructuring the response
    DeleteRuleResponse (..),
    mkDeleteRuleResponse,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteRule' smart constructor.
data DeleteRule = DeleteRule'
  { -- | The name of the rule.
    name :: Types.RuleName,
    -- | The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
    eventBusName :: Core.Maybe Types.EventBusNameOrArn,
    -- | If this is a managed rule, created by an AWS service on your behalf, you must specify @Force@ as @True@ to delete the rule. This parameter is ignored for rules that are not managed rules. You can check whether a rule is a managed rule by using @DescribeRule@ or @ListRules@ and checking the @ManagedBy@ field of the response.
    force :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRule' value with any optional fields omitted.
mkDeleteRule ::
  -- | 'name'
  Types.RuleName ->
  DeleteRule
mkDeleteRule name =
  DeleteRule'
    { name,
      eventBusName = Core.Nothing,
      force = Core.Nothing
    }

-- | The name of the rule.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drName :: Lens.Lens' DeleteRule Types.RuleName
drName = Lens.field @"name"
{-# DEPRECATED drName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
--
-- /Note:/ Consider using 'eventBusName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drEventBusName :: Lens.Lens' DeleteRule (Core.Maybe Types.EventBusNameOrArn)
drEventBusName = Lens.field @"eventBusName"
{-# DEPRECATED drEventBusName "Use generic-lens or generic-optics with 'eventBusName' instead." #-}

-- | If this is a managed rule, created by an AWS service on your behalf, you must specify @Force@ as @True@ to delete the rule. This parameter is ignored for rules that are not managed rules. You can check whether a rule is a managed rule by using @DescribeRule@ or @ListRules@ and checking the @ManagedBy@ field of the response.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drForce :: Lens.Lens' DeleteRule (Core.Maybe Core.Bool)
drForce = Lens.field @"force"
{-# DEPRECATED drForce "Use generic-lens or generic-optics with 'force' instead." #-}

instance Core.FromJSON DeleteRule where
  toJSON DeleteRule {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("EventBusName" Core..=) Core.<$> eventBusName,
            ("Force" Core..=) Core.<$> force
          ]
      )

instance Core.AWSRequest DeleteRule where
  type Rs DeleteRule = DeleteRuleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSEvents.DeleteRule")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteRuleResponse'

-- | /See:/ 'mkDeleteRuleResponse' smart constructor.
data DeleteRuleResponse = DeleteRuleResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRuleResponse' value with any optional fields omitted.
mkDeleteRuleResponse ::
  DeleteRuleResponse
mkDeleteRuleResponse = DeleteRuleResponse'
