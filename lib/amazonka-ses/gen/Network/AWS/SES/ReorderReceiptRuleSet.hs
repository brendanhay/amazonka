{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.ReorderReceiptRuleSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reorders the receipt rules within a receipt rule set.
--
-- For information about managing receipt rule sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.ReorderReceiptRuleSet
  ( -- * Creating a request
    ReorderReceiptRuleSet (..),
    mkReorderReceiptRuleSet,

    -- ** Request lenses
    rrrsRuleSetName,
    rrrsRuleNames,

    -- * Destructuring the response
    ReorderReceiptRuleSetResponse (..),
    mkReorderReceiptRuleSetResponse,

    -- ** Response lenses
    rrrsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to reorder the receipt rules within a receipt rule set. You use receipt rule sets to receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkReorderReceiptRuleSet' smart constructor.
data ReorderReceiptRuleSet = ReorderReceiptRuleSet'
  { -- | The name of the receipt rule set to reorder.
    ruleSetName :: Types.ReceiptRuleSetName,
    -- | A list of the specified receipt rule set's receipt rules in the order that you want to put them.
    ruleNames :: [Types.ReceiptRuleName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReorderReceiptRuleSet' value with any optional fields omitted.
mkReorderReceiptRuleSet ::
  -- | 'ruleSetName'
  Types.ReceiptRuleSetName ->
  ReorderReceiptRuleSet
mkReorderReceiptRuleSet ruleSetName =
  ReorderReceiptRuleSet' {ruleSetName, ruleNames = Core.mempty}

-- | The name of the receipt rule set to reorder.
--
-- /Note:/ Consider using 'ruleSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrrsRuleSetName :: Lens.Lens' ReorderReceiptRuleSet Types.ReceiptRuleSetName
rrrsRuleSetName = Lens.field @"ruleSetName"
{-# DEPRECATED rrrsRuleSetName "Use generic-lens or generic-optics with 'ruleSetName' instead." #-}

-- | A list of the specified receipt rule set's receipt rules in the order that you want to put them.
--
-- /Note:/ Consider using 'ruleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrrsRuleNames :: Lens.Lens' ReorderReceiptRuleSet [Types.ReceiptRuleName]
rrrsRuleNames = Lens.field @"ruleNames"
{-# DEPRECATED rrrsRuleNames "Use generic-lens or generic-optics with 'ruleNames' instead." #-}

instance Core.AWSRequest ReorderReceiptRuleSet where
  type Rs ReorderReceiptRuleSet = ReorderReceiptRuleSetResponse
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
            ( Core.pure ("Action", "ReorderReceiptRuleSet")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "RuleSetName" ruleSetName)
                Core.<> ( Core.toQueryValue
                            "RuleNames"
                            (Core.toQueryList "member" ruleNames)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ReorderReceiptRuleSetResult"
      ( \s h x ->
          ReorderReceiptRuleSetResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkReorderReceiptRuleSetResponse' smart constructor.
newtype ReorderReceiptRuleSetResponse = ReorderReceiptRuleSetResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ReorderReceiptRuleSetResponse' value with any optional fields omitted.
mkReorderReceiptRuleSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ReorderReceiptRuleSetResponse
mkReorderReceiptRuleSetResponse responseStatus =
  ReorderReceiptRuleSetResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrrsrrsResponseStatus :: Lens.Lens' ReorderReceiptRuleSetResponse Core.Int
rrrsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rrrsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
