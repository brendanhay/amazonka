{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteTopicRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the rule.
module Network.AWS.IoT.DeleteTopicRule
    (
    -- * Creating a request
      DeleteTopicRule (..)
    , mkDeleteTopicRule
    -- ** Request lenses
    , dRuleName

    -- * Destructuring the response
    , DeleteTopicRuleResponse (..)
    , mkDeleteTopicRuleResponse
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DeleteTopicRule operation.
--
-- /See:/ 'mkDeleteTopicRule' smart constructor.
newtype DeleteTopicRule = DeleteTopicRule'
  { ruleName :: Types.RuleName
    -- ^ The name of the rule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTopicRule' value with any optional fields omitted.
mkDeleteTopicRule
    :: Types.RuleName -- ^ 'ruleName'
    -> DeleteTopicRule
mkDeleteTopicRule ruleName = DeleteTopicRule'{ruleName}

-- | The name of the rule.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRuleName :: Lens.Lens' DeleteTopicRule Types.RuleName
dRuleName = Lens.field @"ruleName"
{-# INLINEABLE dRuleName #-}
{-# DEPRECATED ruleName "Use generic-lens or generic-optics with 'ruleName' instead"  #-}

instance Core.ToQuery DeleteTopicRule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteTopicRule where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteTopicRule where
        type Rs DeleteTopicRule = DeleteTopicRuleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/rules/" Core.<> Core.toText ruleName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteTopicRuleResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteTopicRuleResponse' smart constructor.
data DeleteTopicRuleResponse = DeleteTopicRuleResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTopicRuleResponse' value with any optional fields omitted.
mkDeleteTopicRuleResponse
    :: DeleteTopicRuleResponse
mkDeleteTopicRuleResponse = DeleteTopicRuleResponse'
