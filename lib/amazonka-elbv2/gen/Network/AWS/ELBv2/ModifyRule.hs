{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.ModifyRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the specified properties of the specified rule. Any properties that you do not specify are unchanged.
--
-- To add an item to a list, remove an item from a list, or update an item in a list, you must provide the entire list. For example, to add an action, specify a list with the current actions plus the new action.
module Network.AWS.ELBv2.ModifyRule
  ( -- * Creating a request
    ModifyRule (..),
    mkModifyRule,

    -- ** Request lenses
    mrActions,
    mrConditions,
    mrRuleARN,

    -- * Destructuring the response
    ModifyRuleResponse (..),
    mkModifyRuleResponse,

    -- ** Response lenses
    mrrsRules,
    mrrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyRule' smart constructor.
data ModifyRule = ModifyRule'
  { -- | The actions.
    actions :: Lude.Maybe [Action],
    -- | The conditions.
    conditions :: Lude.Maybe [RuleCondition],
    -- | The Amazon Resource Name (ARN) of the rule.
    ruleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyRule' with the minimum fields required to make a request.
--
-- * 'actions' - The actions.
-- * 'conditions' - The conditions.
-- * 'ruleARN' - The Amazon Resource Name (ARN) of the rule.
mkModifyRule ::
  -- | 'ruleARN'
  Lude.Text ->
  ModifyRule
mkModifyRule pRuleARN_ =
  ModifyRule'
    { actions = Lude.Nothing,
      conditions = Lude.Nothing,
      ruleARN = pRuleARN_
    }

-- | The actions.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrActions :: Lens.Lens' ModifyRule (Lude.Maybe [Action])
mrActions = Lens.lens (actions :: ModifyRule -> Lude.Maybe [Action]) (\s a -> s {actions = a} :: ModifyRule)
{-# DEPRECATED mrActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | The conditions.
--
-- /Note:/ Consider using 'conditions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrConditions :: Lens.Lens' ModifyRule (Lude.Maybe [RuleCondition])
mrConditions = Lens.lens (conditions :: ModifyRule -> Lude.Maybe [RuleCondition]) (\s a -> s {conditions = a} :: ModifyRule)
{-# DEPRECATED mrConditions "Use generic-lens or generic-optics with 'conditions' instead." #-}

-- | The Amazon Resource Name (ARN) of the rule.
--
-- /Note:/ Consider using 'ruleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrRuleARN :: Lens.Lens' ModifyRule Lude.Text
mrRuleARN = Lens.lens (ruleARN :: ModifyRule -> Lude.Text) (\s a -> s {ruleARN = a} :: ModifyRule)
{-# DEPRECATED mrRuleARN "Use generic-lens or generic-optics with 'ruleARN' instead." #-}

instance Lude.AWSRequest ModifyRule where
  type Rs ModifyRule = ModifyRuleResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "ModifyRuleResult"
      ( \s h x ->
          ModifyRuleResponse'
            Lude.<$> ( x Lude..@? "Rules" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyRule where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyRule where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyRule where
  toQuery ModifyRule' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyRule" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "Actions"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> actions),
        "Conditions"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> conditions),
        "RuleArn" Lude.=: ruleARN
      ]

-- | /See:/ 'mkModifyRuleResponse' smart constructor.
data ModifyRuleResponse = ModifyRuleResponse'
  { -- | Information about the modified rule.
    rules :: Lude.Maybe [Rule],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyRuleResponse' with the minimum fields required to make a request.
--
-- * 'rules' - Information about the modified rule.
-- * 'responseStatus' - The response status code.
mkModifyRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyRuleResponse
mkModifyRuleResponse pResponseStatus_ =
  ModifyRuleResponse'
    { rules = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the modified rule.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrrsRules :: Lens.Lens' ModifyRuleResponse (Lude.Maybe [Rule])
mrrsRules = Lens.lens (rules :: ModifyRuleResponse -> Lude.Maybe [Rule]) (\s a -> s {rules = a} :: ModifyRuleResponse)
{-# DEPRECATED mrrsRules "Use generic-lens or generic-optics with 'rules' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrrsResponseStatus :: Lens.Lens' ModifyRuleResponse Lude.Int
mrrsResponseStatus = Lens.lens (responseStatus :: ModifyRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyRuleResponse)
{-# DEPRECATED mrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
