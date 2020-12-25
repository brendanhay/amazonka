{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    SetRulePriorities (..),
    mkSetRulePriorities,

    -- ** Request lenses
    srpRulePriorities,

    -- * Destructuring the response
    SetRulePrioritiesResponse (..),
    mkSetRulePrioritiesResponse,

    -- ** Response lenses
    srprrsRules,
    srprrsResponseStatus,
  )
where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSetRulePriorities' smart constructor.
newtype SetRulePriorities = SetRulePriorities'
  { -- | The rule priorities.
    rulePriorities :: [Types.RulePriorityPair]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SetRulePriorities' value with any optional fields omitted.
mkSetRulePriorities ::
  SetRulePriorities
mkSetRulePriorities =
  SetRulePriorities' {rulePriorities = Core.mempty}

-- | The rule priorities.
--
-- /Note:/ Consider using 'rulePriorities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srpRulePriorities :: Lens.Lens' SetRulePriorities [Types.RulePriorityPair]
srpRulePriorities = Lens.field @"rulePriorities"
{-# DEPRECATED srpRulePriorities "Use generic-lens or generic-optics with 'rulePriorities' instead." #-}

instance Core.AWSRequest SetRulePriorities where
  type Rs SetRulePriorities = SetRulePrioritiesResponse
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
            ( Core.pure ("Action", "SetRulePriorities")
                Core.<> (Core.pure ("Version", "2015-12-01"))
                Core.<> ( Core.toQueryValue
                            "RulePriorities"
                            (Core.toQueryList "member" rulePriorities)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "SetRulePrioritiesResult"
      ( \s h x ->
          SetRulePrioritiesResponse'
            Core.<$> (x Core..@? "Rules" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkSetRulePrioritiesResponse' smart constructor.
data SetRulePrioritiesResponse = SetRulePrioritiesResponse'
  { -- | Information about the rules.
    rules :: Core.Maybe [Types.Rule],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetRulePrioritiesResponse' value with any optional fields omitted.
mkSetRulePrioritiesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SetRulePrioritiesResponse
mkSetRulePrioritiesResponse responseStatus =
  SetRulePrioritiesResponse' {rules = Core.Nothing, responseStatus}

-- | Information about the rules.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srprrsRules :: Lens.Lens' SetRulePrioritiesResponse (Core.Maybe [Types.Rule])
srprrsRules = Lens.field @"rules"
{-# DEPRECATED srprrsRules "Use generic-lens or generic-optics with 'rules' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srprrsResponseStatus :: Lens.Lens' SetRulePrioritiesResponse Core.Int
srprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED srprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
