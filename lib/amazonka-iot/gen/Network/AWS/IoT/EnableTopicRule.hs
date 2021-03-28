{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.EnableTopicRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the rule.
module Network.AWS.IoT.EnableTopicRule
    (
    -- * Creating a request
      EnableTopicRule (..)
    , mkEnableTopicRule
    -- ** Request lenses
    , etrRuleName

    -- * Destructuring the response
    , EnableTopicRuleResponse (..)
    , mkEnableTopicRuleResponse
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the EnableTopicRuleRequest operation.
--
-- /See:/ 'mkEnableTopicRule' smart constructor.
newtype EnableTopicRule = EnableTopicRule'
  { ruleName :: Types.RuleName
    -- ^ The name of the topic rule to enable.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EnableTopicRule' value with any optional fields omitted.
mkEnableTopicRule
    :: Types.RuleName -- ^ 'ruleName'
    -> EnableTopicRule
mkEnableTopicRule ruleName = EnableTopicRule'{ruleName}

-- | The name of the topic rule to enable.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etrRuleName :: Lens.Lens' EnableTopicRule Types.RuleName
etrRuleName = Lens.field @"ruleName"
{-# INLINEABLE etrRuleName #-}
{-# DEPRECATED ruleName "Use generic-lens or generic-optics with 'ruleName' instead"  #-}

instance Core.ToQuery EnableTopicRule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders EnableTopicRule where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON EnableTopicRule where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest EnableTopicRule where
        type Rs EnableTopicRule = EnableTopicRuleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/rules/" Core.<> Core.toText ruleName Core.<> "/enable",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull EnableTopicRuleResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkEnableTopicRuleResponse' smart constructor.
data EnableTopicRuleResponse = EnableTopicRuleResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableTopicRuleResponse' value with any optional fields omitted.
mkEnableTopicRuleResponse
    :: EnableTopicRuleResponse
mkEnableTopicRuleResponse = EnableTopicRuleResponse'
