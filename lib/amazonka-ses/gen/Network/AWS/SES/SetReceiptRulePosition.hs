{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.SetReceiptRulePosition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the position of the specified receipt rule in the receipt rule set.
--
-- For information about managing receipt rules, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rules.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.SetReceiptRulePosition
    (
    -- * Creating a request
      SetReceiptRulePosition (..)
    , mkSetReceiptRulePosition
    -- ** Request lenses
    , srrpRuleSetName
    , srrpRuleName
    , srrpAfter

    -- * Destructuring the response
    , SetReceiptRulePositionResponse (..)
    , mkSetReceiptRulePositionResponse
    -- ** Response lenses
    , srrprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to set the position of a receipt rule in a receipt rule set. You use receipt rule sets to receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkSetReceiptRulePosition' smart constructor.
data SetReceiptRulePosition = SetReceiptRulePosition'
  { ruleSetName :: Types.ReceiptRuleSetName
    -- ^ The name of the receipt rule set that contains the receipt rule to reposition.
  , ruleName :: Types.ReceiptRuleName
    -- ^ The name of the receipt rule to reposition.
  , after :: Core.Maybe Types.ReceiptRuleName
    -- ^ The name of the receipt rule after which to place the specified receipt rule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetReceiptRulePosition' value with any optional fields omitted.
mkSetReceiptRulePosition
    :: Types.ReceiptRuleSetName -- ^ 'ruleSetName'
    -> Types.ReceiptRuleName -- ^ 'ruleName'
    -> SetReceiptRulePosition
mkSetReceiptRulePosition ruleSetName ruleName
  = SetReceiptRulePosition'{ruleSetName, ruleName,
                            after = Core.Nothing}

-- | The name of the receipt rule set that contains the receipt rule to reposition.
--
-- /Note:/ Consider using 'ruleSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrpRuleSetName :: Lens.Lens' SetReceiptRulePosition Types.ReceiptRuleSetName
srrpRuleSetName = Lens.field @"ruleSetName"
{-# INLINEABLE srrpRuleSetName #-}
{-# DEPRECATED ruleSetName "Use generic-lens or generic-optics with 'ruleSetName' instead"  #-}

-- | The name of the receipt rule to reposition.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrpRuleName :: Lens.Lens' SetReceiptRulePosition Types.ReceiptRuleName
srrpRuleName = Lens.field @"ruleName"
{-# INLINEABLE srrpRuleName #-}
{-# DEPRECATED ruleName "Use generic-lens or generic-optics with 'ruleName' instead"  #-}

-- | The name of the receipt rule after which to place the specified receipt rule.
--
-- /Note:/ Consider using 'after' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrpAfter :: Lens.Lens' SetReceiptRulePosition (Core.Maybe Types.ReceiptRuleName)
srrpAfter = Lens.field @"after"
{-# INLINEABLE srrpAfter #-}
{-# DEPRECATED after "Use generic-lens or generic-optics with 'after' instead"  #-}

instance Core.ToQuery SetReceiptRulePosition where
        toQuery SetReceiptRulePosition{..}
          = Core.toQueryPair "Action" ("SetReceiptRulePosition" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "RuleSetName" ruleSetName
              Core.<> Core.toQueryPair "RuleName" ruleName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "After") after

instance Core.ToHeaders SetReceiptRulePosition where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest SetReceiptRulePosition where
        type Rs SetReceiptRulePosition = SetReceiptRulePositionResponse
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
          = Response.receiveXMLWrapper "SetReceiptRulePositionResult"
              (\ s h x ->
                 SetReceiptRulePositionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkSetReceiptRulePositionResponse' smart constructor.
newtype SetReceiptRulePositionResponse = SetReceiptRulePositionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SetReceiptRulePositionResponse' value with any optional fields omitted.
mkSetReceiptRulePositionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SetReceiptRulePositionResponse
mkSetReceiptRulePositionResponse responseStatus
  = SetReceiptRulePositionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrprrsResponseStatus :: Lens.Lens' SetReceiptRulePositionResponse Core.Int
srrprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srrprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
