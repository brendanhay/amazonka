{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.UpdateReceiptRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a receipt rule.
--
-- For information about managing receipt rules, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rules.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.UpdateReceiptRule
  ( -- * Creating a request
    UpdateReceiptRule (..),
    mkUpdateReceiptRule,

    -- ** Request lenses
    urrRuleSetName,
    urrRule,

    -- * Destructuring the response
    UpdateReceiptRuleResponse (..),
    mkUpdateReceiptRuleResponse,

    -- ** Response lenses
    urrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to update a receipt rule. You use receipt rules to receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkUpdateReceiptRule' smart constructor.
data UpdateReceiptRule = UpdateReceiptRule'
  { -- | The name of the receipt rule set that the receipt rule belongs to.
    ruleSetName :: Types.ReceiptRuleSetName,
    -- | A data structure that contains the updated receipt rule information.
    rule :: Types.ReceiptRule
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateReceiptRule' value with any optional fields omitted.
mkUpdateReceiptRule ::
  -- | 'ruleSetName'
  Types.ReceiptRuleSetName ->
  -- | 'rule'
  Types.ReceiptRule ->
  UpdateReceiptRule
mkUpdateReceiptRule ruleSetName rule =
  UpdateReceiptRule' {ruleSetName, rule}

-- | The name of the receipt rule set that the receipt rule belongs to.
--
-- /Note:/ Consider using 'ruleSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrRuleSetName :: Lens.Lens' UpdateReceiptRule Types.ReceiptRuleSetName
urrRuleSetName = Lens.field @"ruleSetName"
{-# DEPRECATED urrRuleSetName "Use generic-lens or generic-optics with 'ruleSetName' instead." #-}

-- | A data structure that contains the updated receipt rule information.
--
-- /Note:/ Consider using 'rule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrRule :: Lens.Lens' UpdateReceiptRule Types.ReceiptRule
urrRule = Lens.field @"rule"
{-# DEPRECATED urrRule "Use generic-lens or generic-optics with 'rule' instead." #-}

instance Core.AWSRequest UpdateReceiptRule where
  type Rs UpdateReceiptRule = UpdateReceiptRuleResponse
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
            ( Core.pure ("Action", "UpdateReceiptRule")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "RuleSetName" ruleSetName)
                Core.<> (Core.toQueryValue "Rule" rule)
            )
      }
  response =
    Response.receiveXMLWrapper
      "UpdateReceiptRuleResult"
      ( \s h x ->
          UpdateReceiptRuleResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkUpdateReceiptRuleResponse' smart constructor.
newtype UpdateReceiptRuleResponse = UpdateReceiptRuleResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateReceiptRuleResponse' value with any optional fields omitted.
mkUpdateReceiptRuleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateReceiptRuleResponse
mkUpdateReceiptRuleResponse responseStatus =
  UpdateReceiptRuleResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrrsResponseStatus :: Lens.Lens' UpdateReceiptRuleResponse Core.Int
urrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED urrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
