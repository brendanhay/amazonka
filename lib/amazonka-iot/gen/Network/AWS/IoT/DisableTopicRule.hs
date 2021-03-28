{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DisableTopicRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the rule.
module Network.AWS.IoT.DisableTopicRule
    (
    -- * Creating a request
      DisableTopicRule (..)
    , mkDisableTopicRule
    -- ** Request lenses
    , dtrRuleName

    -- * Destructuring the response
    , DisableTopicRuleResponse (..)
    , mkDisableTopicRuleResponse
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DisableTopicRuleRequest operation.
--
-- /See:/ 'mkDisableTopicRule' smart constructor.
newtype DisableTopicRule = DisableTopicRule'
  { ruleName :: Types.RuleName
    -- ^ The name of the rule to disable.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisableTopicRule' value with any optional fields omitted.
mkDisableTopicRule
    :: Types.RuleName -- ^ 'ruleName'
    -> DisableTopicRule
mkDisableTopicRule ruleName = DisableTopicRule'{ruleName}

-- | The name of the rule to disable.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrRuleName :: Lens.Lens' DisableTopicRule Types.RuleName
dtrRuleName = Lens.field @"ruleName"
{-# INLINEABLE dtrRuleName #-}
{-# DEPRECATED ruleName "Use generic-lens or generic-optics with 'ruleName' instead"  #-}

instance Core.ToQuery DisableTopicRule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisableTopicRule where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON DisableTopicRule where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DisableTopicRule where
        type Rs DisableTopicRule = DisableTopicRuleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/rules/" Core.<> Core.toText ruleName Core.<> "/disable",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DisableTopicRuleResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisableTopicRuleResponse' smart constructor.
data DisableTopicRuleResponse = DisableTopicRuleResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableTopicRuleResponse' value with any optional fields omitted.
mkDisableTopicRuleResponse
    :: DisableTopicRuleResponse
mkDisableTopicRuleResponse = DisableTopicRuleResponse'
