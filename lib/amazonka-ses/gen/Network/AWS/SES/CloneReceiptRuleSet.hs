{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.CloneReceiptRuleSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a receipt rule set by cloning an existing one. All receipt rules and configurations are copied to the new receipt rule set and are completely independent of the source rule set.
--
-- For information about setting up rule sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rule-set.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.CloneReceiptRuleSet
  ( -- * Creating a request
    CloneReceiptRuleSet (..),
    mkCloneReceiptRuleSet,

    -- ** Request lenses
    cRuleSetName,
    cOriginalRuleSetName,

    -- * Destructuring the response
    CloneReceiptRuleSetResponse (..),
    mkCloneReceiptRuleSetResponse,

    -- ** Response lenses
    crsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to create a receipt rule set by cloning an existing one. You use receipt rule sets to receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkCloneReceiptRuleSet' smart constructor.
data CloneReceiptRuleSet = CloneReceiptRuleSet'
  { -- | The name of the rule set to create. The name must:
    --
    --
    --     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
    --
    --
    --     * Start and end with a letter or number.
    --
    --
    --     * Contain less than 64 characters.
    ruleSetName :: Types.ReceiptRuleSetName,
    -- | The name of the rule set to clone.
    originalRuleSetName :: Types.ReceiptRuleSetName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CloneReceiptRuleSet' value with any optional fields omitted.
mkCloneReceiptRuleSet ::
  -- | 'ruleSetName'
  Types.ReceiptRuleSetName ->
  -- | 'originalRuleSetName'
  Types.ReceiptRuleSetName ->
  CloneReceiptRuleSet
mkCloneReceiptRuleSet ruleSetName originalRuleSetName =
  CloneReceiptRuleSet' {ruleSetName, originalRuleSetName}

-- | The name of the rule set to create. The name must:
--
--
--     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
--
--
--     * Start and end with a letter or number.
--
--
--     * Contain less than 64 characters.
--
--
--
-- /Note:/ Consider using 'ruleSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRuleSetName :: Lens.Lens' CloneReceiptRuleSet Types.ReceiptRuleSetName
cRuleSetName = Lens.field @"ruleSetName"
{-# DEPRECATED cRuleSetName "Use generic-lens or generic-optics with 'ruleSetName' instead." #-}

-- | The name of the rule set to clone.
--
-- /Note:/ Consider using 'originalRuleSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cOriginalRuleSetName :: Lens.Lens' CloneReceiptRuleSet Types.ReceiptRuleSetName
cOriginalRuleSetName = Lens.field @"originalRuleSetName"
{-# DEPRECATED cOriginalRuleSetName "Use generic-lens or generic-optics with 'originalRuleSetName' instead." #-}

instance Core.AWSRequest CloneReceiptRuleSet where
  type Rs CloneReceiptRuleSet = CloneReceiptRuleSetResponse
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
            ( Core.pure ("Action", "CloneReceiptRuleSet")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "RuleSetName" ruleSetName)
                Core.<> (Core.toQueryValue "OriginalRuleSetName" originalRuleSetName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "CloneReceiptRuleSetResult"
      ( \s h x ->
          CloneReceiptRuleSetResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkCloneReceiptRuleSetResponse' smart constructor.
newtype CloneReceiptRuleSetResponse = CloneReceiptRuleSetResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CloneReceiptRuleSetResponse' value with any optional fields omitted.
mkCloneReceiptRuleSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CloneReceiptRuleSetResponse
mkCloneReceiptRuleSetResponse responseStatus =
  CloneReceiptRuleSetResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CloneReceiptRuleSetResponse Core.Int
crsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
