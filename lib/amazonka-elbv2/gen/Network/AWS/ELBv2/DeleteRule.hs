{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.DeleteRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified rule.
--
-- You can't delete the default rule.
module Network.AWS.ELBv2.DeleteRule
    (
    -- * Creating a request
      DeleteRule (..)
    , mkDeleteRule
    -- ** Request lenses
    , drRuleArn

    -- * Destructuring the response
    , DeleteRuleResponse (..)
    , mkDeleteRuleResponse
    -- ** Response lenses
    , drrrsResponseStatus
    ) where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteRule' smart constructor.
newtype DeleteRule = DeleteRule'
  { ruleArn :: Types.RuleArn
    -- ^ The Amazon Resource Name (ARN) of the rule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRule' value with any optional fields omitted.
mkDeleteRule
    :: Types.RuleArn -- ^ 'ruleArn'
    -> DeleteRule
mkDeleteRule ruleArn = DeleteRule'{ruleArn}

-- | The Amazon Resource Name (ARN) of the rule.
--
-- /Note:/ Consider using 'ruleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drRuleArn :: Lens.Lens' DeleteRule Types.RuleArn
drRuleArn = Lens.field @"ruleArn"
{-# INLINEABLE drRuleArn #-}
{-# DEPRECATED ruleArn "Use generic-lens or generic-optics with 'ruleArn' instead"  #-}

instance Core.ToQuery DeleteRule where
        toQuery DeleteRule{..}
          = Core.toQueryPair "Action" ("DeleteRule" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2015-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "RuleArn" ruleArn

instance Core.ToHeaders DeleteRule where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteRule where
        type Rs DeleteRule = DeleteRuleResponse
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
          = Response.receiveXMLWrapper "DeleteRuleResult"
              (\ s h x ->
                 DeleteRuleResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteRuleResponse' smart constructor.
newtype DeleteRuleResponse = DeleteRuleResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRuleResponse' value with any optional fields omitted.
mkDeleteRuleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteRuleResponse
mkDeleteRuleResponse responseStatus
  = DeleteRuleResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsResponseStatus :: Lens.Lens' DeleteRuleResponse Core.Int
drrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
