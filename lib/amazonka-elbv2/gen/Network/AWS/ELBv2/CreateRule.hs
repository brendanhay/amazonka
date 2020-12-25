{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.CreateRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a rule for the specified listener. The listener must be associated with an Application Load Balancer.
--
-- Each rule consists of a priority, one or more actions, and one or more conditions. Rules are evaluated in priority order, from the lowest value to the highest value. When the conditions for a rule are met, its actions are performed. If the conditions for no rules are met, the actions for the default rule are performed. For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-listeners.html#listener-rules Listener rules> in the /Application Load Balancers Guide/ .
module Network.AWS.ELBv2.CreateRule
  ( -- * Creating a request
    CreateRule (..),
    mkCreateRule,

    -- ** Request lenses
    crListenerArn,
    crConditions,
    crPriority,
    crActions,
    crTags,

    -- * Destructuring the response
    CreateRuleResponse (..),
    mkCreateRuleResponse,

    -- ** Response lenses
    crrrsRules,
    crrrsResponseStatus,
  )
where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateRule' smart constructor.
data CreateRule = CreateRule'
  { -- | The Amazon Resource Name (ARN) of the listener.
    listenerArn :: Types.ListenerArn,
    -- | The conditions.
    conditions :: [Types.RuleCondition],
    -- | The rule priority. A listener can't have multiple rules with the same priority.
    priority :: Core.Natural,
    -- | The actions.
    actions :: [Types.Action],
    -- | The tags to assign to the rule.
    tags :: Core.Maybe (Core.NonEmpty Types.Tag)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRule' value with any optional fields omitted.
mkCreateRule ::
  -- | 'listenerArn'
  Types.ListenerArn ->
  -- | 'priority'
  Core.Natural ->
  CreateRule
mkCreateRule listenerArn priority =
  CreateRule'
    { listenerArn,
      conditions = Core.mempty,
      priority,
      actions = Core.mempty,
      tags = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the listener.
--
-- /Note:/ Consider using 'listenerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crListenerArn :: Lens.Lens' CreateRule Types.ListenerArn
crListenerArn = Lens.field @"listenerArn"
{-# DEPRECATED crListenerArn "Use generic-lens or generic-optics with 'listenerArn' instead." #-}

-- | The conditions.
--
-- /Note:/ Consider using 'conditions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crConditions :: Lens.Lens' CreateRule [Types.RuleCondition]
crConditions = Lens.field @"conditions"
{-# DEPRECATED crConditions "Use generic-lens or generic-optics with 'conditions' instead." #-}

-- | The rule priority. A listener can't have multiple rules with the same priority.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crPriority :: Lens.Lens' CreateRule Core.Natural
crPriority = Lens.field @"priority"
{-# DEPRECATED crPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The actions.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crActions :: Lens.Lens' CreateRule [Types.Action]
crActions = Lens.field @"actions"
{-# DEPRECATED crActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | The tags to assign to the rule.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTags :: Lens.Lens' CreateRule (Core.Maybe (Core.NonEmpty Types.Tag))
crTags = Lens.field @"tags"
{-# DEPRECATED crTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.AWSRequest CreateRule where
  type Rs CreateRule = CreateRuleResponse
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
            ( Core.pure ("Action", "CreateRule")
                Core.<> (Core.pure ("Version", "2015-12-01"))
                Core.<> (Core.toQueryValue "ListenerArn" listenerArn)
                Core.<> ( Core.toQueryValue
                            "Conditions"
                            (Core.toQueryList "member" conditions)
                        )
                Core.<> (Core.toQueryValue "Priority" priority)
                Core.<> (Core.toQueryValue "Actions" (Core.toQueryList "member" actions))
                Core.<> ( Core.toQueryValue
                            "Tags"
                            (Core.toQueryList "member" Core.<$> tags)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateRuleResult"
      ( \s h x ->
          CreateRuleResponse'
            Core.<$> (x Core..@? "Rules" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateRuleResponse' smart constructor.
data CreateRuleResponse = CreateRuleResponse'
  { -- | Information about the rule.
    rules :: Core.Maybe [Types.Rule],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRuleResponse' value with any optional fields omitted.
mkCreateRuleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateRuleResponse
mkCreateRuleResponse responseStatus =
  CreateRuleResponse' {rules = Core.Nothing, responseStatus}

-- | Information about the rule.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsRules :: Lens.Lens' CreateRuleResponse (Core.Maybe [Types.Rule])
crrrsRules = Lens.field @"rules"
{-# DEPRECATED crrrsRules "Use generic-lens or generic-optics with 'rules' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsResponseStatus :: Lens.Lens' CreateRuleResponse Core.Int
crrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
