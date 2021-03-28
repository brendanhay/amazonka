{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DeleteReceiptRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified receipt rule.
--
-- For information about managing receipt rules, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rules.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DeleteReceiptRule
    (
    -- * Creating a request
      DeleteReceiptRule (..)
    , mkDeleteReceiptRule
    -- ** Request lenses
    , drrfRuleSetName
    , drrfRuleName

    -- * Destructuring the response
    , DeleteReceiptRuleResponse (..)
    , mkDeleteReceiptRuleResponse
    -- ** Response lenses
    , drrrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to delete a receipt rule. You use receipt rules to receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkDeleteReceiptRule' smart constructor.
data DeleteReceiptRule = DeleteReceiptRule'
  { ruleSetName :: Types.ReceiptRuleSetName
    -- ^ The name of the receipt rule set that contains the receipt rule to delete.
  , ruleName :: Types.ReceiptRuleName
    -- ^ The name of the receipt rule to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteReceiptRule' value with any optional fields omitted.
mkDeleteReceiptRule
    :: Types.ReceiptRuleSetName -- ^ 'ruleSetName'
    -> Types.ReceiptRuleName -- ^ 'ruleName'
    -> DeleteReceiptRule
mkDeleteReceiptRule ruleSetName ruleName
  = DeleteReceiptRule'{ruleSetName, ruleName}

-- | The name of the receipt rule set that contains the receipt rule to delete.
--
-- /Note:/ Consider using 'ruleSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfRuleSetName :: Lens.Lens' DeleteReceiptRule Types.ReceiptRuleSetName
drrfRuleSetName = Lens.field @"ruleSetName"
{-# INLINEABLE drrfRuleSetName #-}
{-# DEPRECATED ruleSetName "Use generic-lens or generic-optics with 'ruleSetName' instead"  #-}

-- | The name of the receipt rule to delete.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfRuleName :: Lens.Lens' DeleteReceiptRule Types.ReceiptRuleName
drrfRuleName = Lens.field @"ruleName"
{-# INLINEABLE drrfRuleName #-}
{-# DEPRECATED ruleName "Use generic-lens or generic-optics with 'ruleName' instead"  #-}

instance Core.ToQuery DeleteReceiptRule where
        toQuery DeleteReceiptRule{..}
          = Core.toQueryPair "Action" ("DeleteReceiptRule" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "RuleSetName" ruleSetName
              Core.<> Core.toQueryPair "RuleName" ruleName

instance Core.ToHeaders DeleteReceiptRule where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteReceiptRule where
        type Rs DeleteReceiptRule = DeleteReceiptRuleResponse
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
          = Response.receiveXMLWrapper "DeleteReceiptRuleResult"
              (\ s h x ->
                 DeleteReceiptRuleResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkDeleteReceiptRuleResponse' smart constructor.
newtype DeleteReceiptRuleResponse = DeleteReceiptRuleResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteReceiptRuleResponse' value with any optional fields omitted.
mkDeleteReceiptRuleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteReceiptRuleResponse
mkDeleteReceiptRuleResponse responseStatus
  = DeleteReceiptRuleResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrfrsResponseStatus :: Lens.Lens' DeleteReceiptRuleResponse Core.Int
drrrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drrrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
