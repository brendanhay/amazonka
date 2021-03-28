{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ModifyRule (..)
    , mkModifyRule
    -- ** Request lenses
    , mrRuleArn
    , mrActions
    , mrConditions

    -- * Destructuring the response
    , ModifyRuleResponse (..)
    , mkModifyRuleResponse
    -- ** Response lenses
    , mrrrsRules
    , mrrrsResponseStatus
    ) where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyRule' smart constructor.
data ModifyRule = ModifyRule'
  { ruleArn :: Types.RuleArn
    -- ^ The Amazon Resource Name (ARN) of the rule.
  , actions :: Core.Maybe [Types.Action]
    -- ^ The actions.
  , conditions :: Core.Maybe [Types.RuleCondition]
    -- ^ The conditions.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyRule' value with any optional fields omitted.
mkModifyRule
    :: Types.RuleArn -- ^ 'ruleArn'
    -> ModifyRule
mkModifyRule ruleArn
  = ModifyRule'{ruleArn, actions = Core.Nothing,
                conditions = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the rule.
--
-- /Note:/ Consider using 'ruleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrRuleArn :: Lens.Lens' ModifyRule Types.RuleArn
mrRuleArn = Lens.field @"ruleArn"
{-# INLINEABLE mrRuleArn #-}
{-# DEPRECATED ruleArn "Use generic-lens or generic-optics with 'ruleArn' instead"  #-}

-- | The actions.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrActions :: Lens.Lens' ModifyRule (Core.Maybe [Types.Action])
mrActions = Lens.field @"actions"
{-# INLINEABLE mrActions #-}
{-# DEPRECATED actions "Use generic-lens or generic-optics with 'actions' instead"  #-}

-- | The conditions.
--
-- /Note:/ Consider using 'conditions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrConditions :: Lens.Lens' ModifyRule (Core.Maybe [Types.RuleCondition])
mrConditions = Lens.field @"conditions"
{-# INLINEABLE mrConditions #-}
{-# DEPRECATED conditions "Use generic-lens or generic-optics with 'conditions' instead"  #-}

instance Core.ToQuery ModifyRule where
        toQuery ModifyRule{..}
          = Core.toQueryPair "Action" ("ModifyRule" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2015-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "RuleArn" ruleArn
              Core.<>
              Core.toQueryPair "Actions"
                (Core.maybe Core.mempty (Core.toQueryList "member") actions)
              Core.<>
              Core.toQueryPair "Conditions"
                (Core.maybe Core.mempty (Core.toQueryList "member") conditions)

instance Core.ToHeaders ModifyRule where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyRule where
        type Rs ModifyRule = ModifyRuleResponse
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
          = Response.receiveXMLWrapper "ModifyRuleResult"
              (\ s h x ->
                 ModifyRuleResponse' Core.<$>
                   (x Core..@? "Rules" Core..<@> Core.parseXMLList "member") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyRuleResponse' smart constructor.
data ModifyRuleResponse = ModifyRuleResponse'
  { rules :: Core.Maybe [Types.Rule]
    -- ^ Information about the modified rule.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyRuleResponse' value with any optional fields omitted.
mkModifyRuleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyRuleResponse
mkModifyRuleResponse responseStatus
  = ModifyRuleResponse'{rules = Core.Nothing, responseStatus}

-- | Information about the modified rule.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrrrsRules :: Lens.Lens' ModifyRuleResponse (Core.Maybe [Types.Rule])
mrrrsRules = Lens.field @"rules"
{-# INLINEABLE mrrrsRules #-}
{-# DEPRECATED rules "Use generic-lens or generic-optics with 'rules' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrrrsResponseStatus :: Lens.Lens' ModifyRuleResponse Core.Int
mrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
