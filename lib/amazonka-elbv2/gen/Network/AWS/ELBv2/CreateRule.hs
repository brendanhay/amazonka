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
    crPriority,
    crActions,
    crListenerARN,
    crConditions,
    crTags,

    -- * Destructuring the response
    CreateRuleResponse (..),
    mkCreateRuleResponse,

    -- ** Response lenses
    crrsRules,
    crrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateRule' smart constructor.
data CreateRule = CreateRule'
  { -- | The rule priority. A listener can't have multiple rules with the same priority.
    priority :: Lude.Natural,
    -- | The actions.
    actions :: [Action],
    -- | The Amazon Resource Name (ARN) of the listener.
    listenerARN :: Lude.Text,
    -- | The conditions.
    conditions :: [RuleCondition],
    -- | The tags to assign to the rule.
    tags :: Lude.Maybe (Lude.NonEmpty Tag)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRule' with the minimum fields required to make a request.
--
-- * 'priority' - The rule priority. A listener can't have multiple rules with the same priority.
-- * 'actions' - The actions.
-- * 'listenerARN' - The Amazon Resource Name (ARN) of the listener.
-- * 'conditions' - The conditions.
-- * 'tags' - The tags to assign to the rule.
mkCreateRule ::
  -- | 'priority'
  Lude.Natural ->
  -- | 'listenerARN'
  Lude.Text ->
  CreateRule
mkCreateRule pPriority_ pListenerARN_ =
  CreateRule'
    { priority = pPriority_,
      actions = Lude.mempty,
      listenerARN = pListenerARN_,
      conditions = Lude.mempty,
      tags = Lude.Nothing
    }

-- | The rule priority. A listener can't have multiple rules with the same priority.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crPriority :: Lens.Lens' CreateRule Lude.Natural
crPriority = Lens.lens (priority :: CreateRule -> Lude.Natural) (\s a -> s {priority = a} :: CreateRule)
{-# DEPRECATED crPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The actions.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crActions :: Lens.Lens' CreateRule [Action]
crActions = Lens.lens (actions :: CreateRule -> [Action]) (\s a -> s {actions = a} :: CreateRule)
{-# DEPRECATED crActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | The Amazon Resource Name (ARN) of the listener.
--
-- /Note:/ Consider using 'listenerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crListenerARN :: Lens.Lens' CreateRule Lude.Text
crListenerARN = Lens.lens (listenerARN :: CreateRule -> Lude.Text) (\s a -> s {listenerARN = a} :: CreateRule)
{-# DEPRECATED crListenerARN "Use generic-lens or generic-optics with 'listenerARN' instead." #-}

-- | The conditions.
--
-- /Note:/ Consider using 'conditions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crConditions :: Lens.Lens' CreateRule [RuleCondition]
crConditions = Lens.lens (conditions :: CreateRule -> [RuleCondition]) (\s a -> s {conditions = a} :: CreateRule)
{-# DEPRECATED crConditions "Use generic-lens or generic-optics with 'conditions' instead." #-}

-- | The tags to assign to the rule.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTags :: Lens.Lens' CreateRule (Lude.Maybe (Lude.NonEmpty Tag))
crTags = Lens.lens (tags :: CreateRule -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: CreateRule)
{-# DEPRECATED crTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateRule where
  type Rs CreateRule = CreateRuleResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "CreateRuleResult"
      ( \s h x ->
          CreateRuleResponse'
            Lude.<$> ( x Lude..@? "Rules" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateRule where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateRule where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateRule where
  toQuery CreateRule' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateRule" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "Priority" Lude.=: priority,
        "Actions" Lude.=: Lude.toQueryList "member" actions,
        "ListenerArn" Lude.=: listenerARN,
        "Conditions" Lude.=: Lude.toQueryList "member" conditions,
        "Tags"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> tags)
      ]

-- | /See:/ 'mkCreateRuleResponse' smart constructor.
data CreateRuleResponse = CreateRuleResponse'
  { -- | Information about the rule.
    rules :: Lude.Maybe [Rule],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRuleResponse' with the minimum fields required to make a request.
--
-- * 'rules' - Information about the rule.
-- * 'responseStatus' - The response status code.
mkCreateRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateRuleResponse
mkCreateRuleResponse pResponseStatus_ =
  CreateRuleResponse'
    { rules = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the rule.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsRules :: Lens.Lens' CreateRuleResponse (Lude.Maybe [Rule])
crrsRules = Lens.lens (rules :: CreateRuleResponse -> Lude.Maybe [Rule]) (\s a -> s {rules = a} :: CreateRuleResponse)
{-# DEPRECATED crrsRules "Use generic-lens or generic-optics with 'rules' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsResponseStatus :: Lens.Lens' CreateRuleResponse Lude.Int
crrsResponseStatus = Lens.lens (responseStatus :: CreateRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateRuleResponse)
{-# DEPRECATED crrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
