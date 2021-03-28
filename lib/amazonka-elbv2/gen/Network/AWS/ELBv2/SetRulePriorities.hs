{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.SetRulePriorities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the priorities of the specified rules.
--
-- You can reorder the rules as long as there are no priority conflicts in the new order. Any existing rules that you do not specify retain their current priority.
module Network.AWS.ELBv2.SetRulePriorities
    (
    -- * Creating a request
      SetRulePriorities (..)
    , mkSetRulePriorities
    -- ** Request lenses
    , srpRulePriorities

    -- * Destructuring the response
    , SetRulePrioritiesResponse (..)
    , mkSetRulePrioritiesResponse
    -- ** Response lenses
    , srprrsRules
    , srprrsResponseStatus
    ) where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSetRulePriorities' smart constructor.
newtype SetRulePriorities = SetRulePriorities'
  { rulePriorities :: [Types.RulePriorityPair]
    -- ^ The rule priorities.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SetRulePriorities' value with any optional fields omitted.
mkSetRulePriorities
    :: SetRulePriorities
mkSetRulePriorities
  = SetRulePriorities'{rulePriorities = Core.mempty}

-- | The rule priorities.
--
-- /Note:/ Consider using 'rulePriorities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srpRulePriorities :: Lens.Lens' SetRulePriorities [Types.RulePriorityPair]
srpRulePriorities = Lens.field @"rulePriorities"
{-# INLINEABLE srpRulePriorities #-}
{-# DEPRECATED rulePriorities "Use generic-lens or generic-optics with 'rulePriorities' instead"  #-}

instance Core.ToQuery SetRulePriorities where
        toQuery SetRulePriorities{..}
          = Core.toQueryPair "Action" ("SetRulePriorities" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-12-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "RulePriorities"
                (Core.toQueryList "member" rulePriorities)

instance Core.ToHeaders SetRulePriorities where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest SetRulePriorities where
        type Rs SetRulePriorities = SetRulePrioritiesResponse
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
          = Response.receiveXMLWrapper "SetRulePrioritiesResult"
              (\ s h x ->
                 SetRulePrioritiesResponse' Core.<$>
                   (x Core..@? "Rules" Core..<@> Core.parseXMLList "member") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetRulePrioritiesResponse' smart constructor.
data SetRulePrioritiesResponse = SetRulePrioritiesResponse'
  { rules :: Core.Maybe [Types.Rule]
    -- ^ Information about the rules.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetRulePrioritiesResponse' value with any optional fields omitted.
mkSetRulePrioritiesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SetRulePrioritiesResponse
mkSetRulePrioritiesResponse responseStatus
  = SetRulePrioritiesResponse'{rules = Core.Nothing, responseStatus}

-- | Information about the rules.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srprrsRules :: Lens.Lens' SetRulePrioritiesResponse (Core.Maybe [Types.Rule])
srprrsRules = Lens.field @"rules"
{-# INLINEABLE srprrsRules #-}
{-# DEPRECATED rules "Use generic-lens or generic-optics with 'rules' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srprrsResponseStatus :: Lens.Lens' SetRulePrioritiesResponse Core.Int
srprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
