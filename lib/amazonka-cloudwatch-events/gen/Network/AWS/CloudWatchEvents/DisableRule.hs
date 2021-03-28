{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.DisableRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the specified rule. A disabled rule won't match any events, and won't self-trigger if it has a schedule expression.
--
-- When you disable a rule, incoming events might continue to match to the disabled rule. Allow a short period of time for changes to take effect.
module Network.AWS.CloudWatchEvents.DisableRule
    (
    -- * Creating a request
      DisableRule (..)
    , mkDisableRule
    -- ** Request lenses
    , dName
    , dEventBusName

    -- * Destructuring the response
    , DisableRuleResponse (..)
    , mkDisableRuleResponse
    ) where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisableRule' smart constructor.
data DisableRule = DisableRule'
  { name :: Types.RuleName
    -- ^ The name of the rule.
  , eventBusName :: Core.Maybe Types.EventBusNameOrArn
    -- ^ The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableRule' value with any optional fields omitted.
mkDisableRule
    :: Types.RuleName -- ^ 'name'
    -> DisableRule
mkDisableRule name
  = DisableRule'{name, eventBusName = Core.Nothing}

-- | The name of the rule.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' DisableRule Types.RuleName
dName = Lens.field @"name"
{-# INLINEABLE dName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
--
-- /Note:/ Consider using 'eventBusName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dEventBusName :: Lens.Lens' DisableRule (Core.Maybe Types.EventBusNameOrArn)
dEventBusName = Lens.field @"eventBusName"
{-# INLINEABLE dEventBusName #-}
{-# DEPRECATED eventBusName "Use generic-lens or generic-optics with 'eventBusName' instead"  #-}

instance Core.ToQuery DisableRule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisableRule where
        toHeaders DisableRule{..}
          = Core.pure ("X-Amz-Target", "AWSEvents.DisableRule") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DisableRule where
        toJSON DisableRule{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  ("EventBusName" Core..=) Core.<$> eventBusName])

instance Core.AWSRequest DisableRule where
        type Rs DisableRule = DisableRuleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DisableRuleResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisableRuleResponse' smart constructor.
data DisableRuleResponse = DisableRuleResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableRuleResponse' value with any optional fields omitted.
mkDisableRuleResponse
    :: DisableRuleResponse
mkDisableRuleResponse = DisableRuleResponse'
