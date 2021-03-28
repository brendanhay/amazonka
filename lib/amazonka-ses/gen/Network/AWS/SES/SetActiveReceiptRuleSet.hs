{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.SetActiveReceiptRuleSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the specified receipt rule set as the active receipt rule set.
--
-- For information about managing receipt rule sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.SetActiveReceiptRuleSet
    (
    -- * Creating a request
      SetActiveReceiptRuleSet (..)
    , mkSetActiveReceiptRuleSet
    -- ** Request lenses
    , sarrsRuleSetName

    -- * Destructuring the response
    , SetActiveReceiptRuleSetResponse (..)
    , mkSetActiveReceiptRuleSetResponse
    -- ** Response lenses
    , sarrsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to set a receipt rule set as the active receipt rule set. You use receipt rule sets to receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkSetActiveReceiptRuleSet' smart constructor.
newtype SetActiveReceiptRuleSet = SetActiveReceiptRuleSet'
  { ruleSetName :: Core.Maybe Types.ReceiptRuleSetName
    -- ^ The name of the receipt rule set to make active. Setting this value to null disables all email receiving.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SetActiveReceiptRuleSet' value with any optional fields omitted.
mkSetActiveReceiptRuleSet
    :: SetActiveReceiptRuleSet
mkSetActiveReceiptRuleSet
  = SetActiveReceiptRuleSet'{ruleSetName = Core.Nothing}

-- | The name of the receipt rule set to make active. Setting this value to null disables all email receiving.
--
-- /Note:/ Consider using 'ruleSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarrsRuleSetName :: Lens.Lens' SetActiveReceiptRuleSet (Core.Maybe Types.ReceiptRuleSetName)
sarrsRuleSetName = Lens.field @"ruleSetName"
{-# INLINEABLE sarrsRuleSetName #-}
{-# DEPRECATED ruleSetName "Use generic-lens or generic-optics with 'ruleSetName' instead"  #-}

instance Core.ToQuery SetActiveReceiptRuleSet where
        toQuery SetActiveReceiptRuleSet{..}
          = Core.toQueryPair "Action"
              ("SetActiveReceiptRuleSet" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "RuleSetName") ruleSetName

instance Core.ToHeaders SetActiveReceiptRuleSet where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest SetActiveReceiptRuleSet where
        type Rs SetActiveReceiptRuleSet = SetActiveReceiptRuleSetResponse
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
          = Response.receiveXMLWrapper "SetActiveReceiptRuleSetResult"
              (\ s h x ->
                 SetActiveReceiptRuleSetResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkSetActiveReceiptRuleSetResponse' smart constructor.
newtype SetActiveReceiptRuleSetResponse = SetActiveReceiptRuleSetResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SetActiveReceiptRuleSetResponse' value with any optional fields omitted.
mkSetActiveReceiptRuleSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SetActiveReceiptRuleSetResponse
mkSetActiveReceiptRuleSetResponse responseStatus
  = SetActiveReceiptRuleSetResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarrsrrsResponseStatus :: Lens.Lens' SetActiveReceiptRuleSetResponse Core.Int
sarrsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sarrsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
