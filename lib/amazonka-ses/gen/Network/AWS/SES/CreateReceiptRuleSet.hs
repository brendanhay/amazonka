{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.CreateReceiptRuleSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an empty receipt rule set.
--
-- For information about setting up receipt rule sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rule-set.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.CreateReceiptRuleSet
    (
    -- * Creating a request
      CreateReceiptRuleSet (..)
    , mkCreateReceiptRuleSet
    -- ** Request lenses
    , crrsRuleSetName

    -- * Destructuring the response
    , CreateReceiptRuleSetResponse (..)
    , mkCreateReceiptRuleSetResponse
    -- ** Response lenses
    , crrsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to create an empty receipt rule set. You use receipt rule sets to receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkCreateReceiptRuleSet' smart constructor.
newtype CreateReceiptRuleSet = CreateReceiptRuleSet'
  { ruleSetName :: Types.ReceiptRuleSetName
    -- ^ The name of the rule set to create. The name must:
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateReceiptRuleSet' value with any optional fields omitted.
mkCreateReceiptRuleSet
    :: Types.ReceiptRuleSetName -- ^ 'ruleSetName'
    -> CreateReceiptRuleSet
mkCreateReceiptRuleSet ruleSetName
  = CreateReceiptRuleSet'{ruleSetName}

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
crrsRuleSetName :: Lens.Lens' CreateReceiptRuleSet Types.ReceiptRuleSetName
crrsRuleSetName = Lens.field @"ruleSetName"
{-# INLINEABLE crrsRuleSetName #-}
{-# DEPRECATED ruleSetName "Use generic-lens or generic-optics with 'ruleSetName' instead"  #-}

instance Core.ToQuery CreateReceiptRuleSet where
        toQuery CreateReceiptRuleSet{..}
          = Core.toQueryPair "Action" ("CreateReceiptRuleSet" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "RuleSetName" ruleSetName

instance Core.ToHeaders CreateReceiptRuleSet where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateReceiptRuleSet where
        type Rs CreateReceiptRuleSet = CreateReceiptRuleSetResponse
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
          = Response.receiveXMLWrapper "CreateReceiptRuleSetResult"
              (\ s h x ->
                 CreateReceiptRuleSetResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkCreateReceiptRuleSetResponse' smart constructor.
newtype CreateReceiptRuleSetResponse = CreateReceiptRuleSetResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateReceiptRuleSetResponse' value with any optional fields omitted.
mkCreateReceiptRuleSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateReceiptRuleSetResponse
mkCreateReceiptRuleSetResponse responseStatus
  = CreateReceiptRuleSetResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsrrsResponseStatus :: Lens.Lens' CreateReceiptRuleSetResponse Core.Int
crrsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crrsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
