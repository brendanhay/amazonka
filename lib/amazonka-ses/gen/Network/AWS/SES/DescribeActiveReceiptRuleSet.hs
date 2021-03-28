{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DescribeActiveReceiptRuleSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the metadata and receipt rules for the receipt rule set that is currently active.
--
-- For information about setting up receipt rule sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rule-set.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DescribeActiveReceiptRuleSet
    (
    -- * Creating a request
      DescribeActiveReceiptRuleSet (..)
    , mkDescribeActiveReceiptRuleSet

    -- * Destructuring the response
    , DescribeActiveReceiptRuleSetResponse (..)
    , mkDescribeActiveReceiptRuleSetResponse
    -- ** Response lenses
    , darrsrrsMetadata
    , darrsrrsRules
    , darrsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to return the metadata and receipt rules for the receipt rule set that is currently active. You use receipt rule sets to receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkDescribeActiveReceiptRuleSet' smart constructor.
data DescribeActiveReceiptRuleSet = DescribeActiveReceiptRuleSet'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeActiveReceiptRuleSet' value with any optional fields omitted.
mkDescribeActiveReceiptRuleSet
    :: DescribeActiveReceiptRuleSet
mkDescribeActiveReceiptRuleSet = DescribeActiveReceiptRuleSet'

instance Core.ToQuery DescribeActiveReceiptRuleSet where
        toQuery DescribeActiveReceiptRuleSet{..}
          = Core.toQueryPair "Action"
              ("DescribeActiveReceiptRuleSet" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)

instance Core.ToHeaders DescribeActiveReceiptRuleSet where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeActiveReceiptRuleSet where
        type Rs DescribeActiveReceiptRuleSet =
             DescribeActiveReceiptRuleSetResponse
        toRequest x@_
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
          = Response.receiveXMLWrapper "DescribeActiveReceiptRuleSetResult"
              (\ s h x ->
                 DescribeActiveReceiptRuleSetResponse' Core.<$>
                   (x Core..@? "Metadata") Core.<*>
                     x Core..@? "Rules" Core..<@> Core.parseXMLList "member"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the metadata and receipt rules for the receipt rule set that is currently active.
--
-- /See:/ 'mkDescribeActiveReceiptRuleSetResponse' smart constructor.
data DescribeActiveReceiptRuleSetResponse = DescribeActiveReceiptRuleSetResponse'
  { metadata :: Core.Maybe Types.ReceiptRuleSetMetadata
    -- ^ The metadata for the currently active receipt rule set. The metadata consists of the rule set name and a timestamp of when the rule set was created.
  , rules :: Core.Maybe [Types.ReceiptRule]
    -- ^ The receipt rules that belong to the active rule set.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeActiveReceiptRuleSetResponse' value with any optional fields omitted.
mkDescribeActiveReceiptRuleSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeActiveReceiptRuleSetResponse
mkDescribeActiveReceiptRuleSetResponse responseStatus
  = DescribeActiveReceiptRuleSetResponse'{metadata = Core.Nothing,
                                          rules = Core.Nothing, responseStatus}

-- | The metadata for the currently active receipt rule set. The metadata consists of the rule set name and a timestamp of when the rule set was created.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsrrsMetadata :: Lens.Lens' DescribeActiveReceiptRuleSetResponse (Core.Maybe Types.ReceiptRuleSetMetadata)
darrsrrsMetadata = Lens.field @"metadata"
{-# INLINEABLE darrsrrsMetadata #-}
{-# DEPRECATED metadata "Use generic-lens or generic-optics with 'metadata' instead"  #-}

-- | The receipt rules that belong to the active rule set.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsrrsRules :: Lens.Lens' DescribeActiveReceiptRuleSetResponse (Core.Maybe [Types.ReceiptRule])
darrsrrsRules = Lens.field @"rules"
{-# INLINEABLE darrsrrsRules #-}
{-# DEPRECATED rules "Use generic-lens or generic-optics with 'rules' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsrrsResponseStatus :: Lens.Lens' DescribeActiveReceiptRuleSetResponse Core.Int
darrsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE darrsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
