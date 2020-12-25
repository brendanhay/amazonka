{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.GetRuleGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'RuleGroup' that is specified by the @RuleGroupId@ that you included in the @GetRuleGroup@ request.
--
-- To view the rules in a rule group, use 'ListActivatedRulesInRuleGroup' .
module Network.AWS.WAFRegional.GetRuleGroup
  ( -- * Creating a request
    GetRuleGroup (..),
    mkGetRuleGroup,

    -- ** Request lenses
    grgRuleGroupId,

    -- * Destructuring the response
    GetRuleGroupResponse (..),
    mkGetRuleGroupResponse,

    -- ** Response lenses
    grgrrsRuleGroup,
    grgrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkGetRuleGroup' smart constructor.
newtype GetRuleGroup = GetRuleGroup'
  { -- | The @RuleGroupId@ of the 'RuleGroup' that you want to get. @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
    ruleGroupId :: Types.ResourceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetRuleGroup' value with any optional fields omitted.
mkGetRuleGroup ::
  -- | 'ruleGroupId'
  Types.ResourceId ->
  GetRuleGroup
mkGetRuleGroup ruleGroupId = GetRuleGroup' {ruleGroupId}

-- | The @RuleGroupId@ of the 'RuleGroup' that you want to get. @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
--
-- /Note:/ Consider using 'ruleGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgRuleGroupId :: Lens.Lens' GetRuleGroup Types.ResourceId
grgRuleGroupId = Lens.field @"ruleGroupId"
{-# DEPRECATED grgRuleGroupId "Use generic-lens or generic-optics with 'ruleGroupId' instead." #-}

instance Core.FromJSON GetRuleGroup where
  toJSON GetRuleGroup {..} =
    Core.object
      (Core.catMaybes [Core.Just ("RuleGroupId" Core..= ruleGroupId)])

instance Core.AWSRequest GetRuleGroup where
  type Rs GetRuleGroup = GetRuleGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSWAF_Regional_20161128.GetRuleGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRuleGroupResponse'
            Core.<$> (x Core..:? "RuleGroup") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetRuleGroupResponse' smart constructor.
data GetRuleGroupResponse = GetRuleGroupResponse'
  { -- | Information about the 'RuleGroup' that you specified in the @GetRuleGroup@ request.
    ruleGroup :: Core.Maybe Types.RuleGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRuleGroupResponse' value with any optional fields omitted.
mkGetRuleGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetRuleGroupResponse
mkGetRuleGroupResponse responseStatus =
  GetRuleGroupResponse' {ruleGroup = Core.Nothing, responseStatus}

-- | Information about the 'RuleGroup' that you specified in the @GetRuleGroup@ request.
--
-- /Note:/ Consider using 'ruleGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgrrsRuleGroup :: Lens.Lens' GetRuleGroupResponse (Core.Maybe Types.RuleGroup)
grgrrsRuleGroup = Lens.field @"ruleGroup"
{-# DEPRECATED grgrrsRuleGroup "Use generic-lens or generic-optics with 'ruleGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgrrsResponseStatus :: Lens.Lens' GetRuleGroupResponse Core.Int
grgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
