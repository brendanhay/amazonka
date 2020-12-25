{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    SetReceiptRulePosition (..),
    mkSetReceiptRulePosition,

    -- ** Request lenses
    srrpRuleSetName,
    srrpRuleName,
    srrpAfter,

    -- * Destructuring the response
    SetReceiptRulePositionResponse (..),
    mkSetReceiptRulePositionResponse,

    -- ** Response lenses
    srrprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to set the position of a receipt rule in a receipt rule set. You use receipt rule sets to receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkSetReceiptRulePosition' smart constructor.
data SetReceiptRulePosition = SetReceiptRulePosition'
  { -- | The name of the receipt rule set that contains the receipt rule to reposition.
    ruleSetName :: Types.ReceiptRuleSetName,
    -- | The name of the receipt rule to reposition.
    ruleName :: Types.ReceiptRuleName,
    -- | The name of the receipt rule after which to place the specified receipt rule.
    after :: Core.Maybe Types.ReceiptRuleName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetReceiptRulePosition' value with any optional fields omitted.
mkSetReceiptRulePosition ::
  -- | 'ruleSetName'
  Types.ReceiptRuleSetName ->
  -- | 'ruleName'
  Types.ReceiptRuleName ->
  SetReceiptRulePosition
mkSetReceiptRulePosition ruleSetName ruleName =
  SetReceiptRulePosition'
    { ruleSetName,
      ruleName,
      after = Core.Nothing
    }

-- | The name of the receipt rule set that contains the receipt rule to reposition.
--
-- /Note:/ Consider using 'ruleSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrpRuleSetName :: Lens.Lens' SetReceiptRulePosition Types.ReceiptRuleSetName
srrpRuleSetName = Lens.field @"ruleSetName"
{-# DEPRECATED srrpRuleSetName "Use generic-lens or generic-optics with 'ruleSetName' instead." #-}

-- | The name of the receipt rule to reposition.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrpRuleName :: Lens.Lens' SetReceiptRulePosition Types.ReceiptRuleName
srrpRuleName = Lens.field @"ruleName"
{-# DEPRECATED srrpRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

-- | The name of the receipt rule after which to place the specified receipt rule.
--
-- /Note:/ Consider using 'after' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrpAfter :: Lens.Lens' SetReceiptRulePosition (Core.Maybe Types.ReceiptRuleName)
srrpAfter = Lens.field @"after"
{-# DEPRECATED srrpAfter "Use generic-lens or generic-optics with 'after' instead." #-}

instance Core.AWSRequest SetReceiptRulePosition where
  type Rs SetReceiptRulePosition = SetReceiptRulePositionResponse
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
            ( Core.pure ("Action", "SetReceiptRulePosition")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "RuleSetName" ruleSetName)
                Core.<> (Core.toQueryValue "RuleName" ruleName)
                Core.<> (Core.toQueryValue "After" Core.<$> after)
            )
      }
  response =
    Response.receiveXMLWrapper
      "SetReceiptRulePositionResult"
      ( \s h x ->
          SetReceiptRulePositionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkSetReceiptRulePositionResponse' smart constructor.
newtype SetReceiptRulePositionResponse = SetReceiptRulePositionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SetReceiptRulePositionResponse' value with any optional fields omitted.
mkSetReceiptRulePositionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SetReceiptRulePositionResponse
mkSetReceiptRulePositionResponse responseStatus =
  SetReceiptRulePositionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrprrsResponseStatus :: Lens.Lens' SetReceiptRulePositionResponse Core.Int
srrprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED srrprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
