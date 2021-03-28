{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DescribeReceiptRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of the specified receipt rule.
--
-- For information about setting up receipt rules, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rules.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DescribeReceiptRule
    (
    -- * Creating a request
      DescribeReceiptRule (..)
    , mkDescribeReceiptRule
    -- ** Request lenses
    , drrRuleSetName
    , drrRuleName

    -- * Destructuring the response
    , DescribeReceiptRuleResponse (..)
    , mkDescribeReceiptRuleResponse
    -- ** Response lenses
    , drrrrsRule
    , drrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to return the details of a receipt rule. You use receipt rules to receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkDescribeReceiptRule' smart constructor.
data DescribeReceiptRule = DescribeReceiptRule'
  { ruleSetName :: Types.ReceiptRuleSetName
    -- ^ The name of the receipt rule set that the receipt rule belongs to.
  , ruleName :: Types.ReceiptRuleName
    -- ^ The name of the receipt rule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReceiptRule' value with any optional fields omitted.
mkDescribeReceiptRule
    :: Types.ReceiptRuleSetName -- ^ 'ruleSetName'
    -> Types.ReceiptRuleName -- ^ 'ruleName'
    -> DescribeReceiptRule
mkDescribeReceiptRule ruleSetName ruleName
  = DescribeReceiptRule'{ruleSetName, ruleName}

-- | The name of the receipt rule set that the receipt rule belongs to.
--
-- /Note:/ Consider using 'ruleSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrRuleSetName :: Lens.Lens' DescribeReceiptRule Types.ReceiptRuleSetName
drrRuleSetName = Lens.field @"ruleSetName"
{-# INLINEABLE drrRuleSetName #-}
{-# DEPRECATED ruleSetName "Use generic-lens or generic-optics with 'ruleSetName' instead"  #-}

-- | The name of the receipt rule.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrRuleName :: Lens.Lens' DescribeReceiptRule Types.ReceiptRuleName
drrRuleName = Lens.field @"ruleName"
{-# INLINEABLE drrRuleName #-}
{-# DEPRECATED ruleName "Use generic-lens or generic-optics with 'ruleName' instead"  #-}

instance Core.ToQuery DescribeReceiptRule where
        toQuery DescribeReceiptRule{..}
          = Core.toQueryPair "Action" ("DescribeReceiptRule" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "RuleSetName" ruleSetName
              Core.<> Core.toQueryPair "RuleName" ruleName

instance Core.ToHeaders DescribeReceiptRule where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeReceiptRule where
        type Rs DescribeReceiptRule = DescribeReceiptRuleResponse
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
          = Response.receiveXMLWrapper "DescribeReceiptRuleResult"
              (\ s h x ->
                 DescribeReceiptRuleResponse' Core.<$>
                   (x Core..@? "Rule") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the details of a receipt rule.
--
-- /See:/ 'mkDescribeReceiptRuleResponse' smart constructor.
data DescribeReceiptRuleResponse = DescribeReceiptRuleResponse'
  { rule :: Core.Maybe Types.ReceiptRule
    -- ^ A data structure that contains the specified receipt rule's name, actions, recipients, domains, enabled status, scan status, and Transport Layer Security (TLS) policy.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReceiptRuleResponse' value with any optional fields omitted.
mkDescribeReceiptRuleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeReceiptRuleResponse
mkDescribeReceiptRuleResponse responseStatus
  = DescribeReceiptRuleResponse'{rule = Core.Nothing, responseStatus}

-- | A data structure that contains the specified receipt rule's name, actions, recipients, domains, enabled status, scan status, and Transport Layer Security (TLS) policy.
--
-- /Note:/ Consider using 'rule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrrsRule :: Lens.Lens' DescribeReceiptRuleResponse (Core.Maybe Types.ReceiptRule)
drrrrsRule = Lens.field @"rule"
{-# INLINEABLE drrrrsRule #-}
{-# DEPRECATED rule "Use generic-lens or generic-optics with 'rule' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrrsResponseStatus :: Lens.Lens' DescribeReceiptRuleResponse Core.Int
drrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
