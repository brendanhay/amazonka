{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.GetRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'Rule' that is specified by the @RuleId@ that you included in the @GetRule@ request.
module Network.AWS.WAFRegional.GetRule
  ( -- * Creating a request
    GetRule (..),
    mkGetRule,

    -- ** Request lenses
    grRuleId,

    -- * Destructuring the response
    GetRuleResponse (..),
    mkGetRuleResponse,

    -- ** Response lenses
    grrrsRule,
    grrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkGetRule' smart constructor.
newtype GetRule = GetRule'
  { -- | The @RuleId@ of the 'Rule' that you want to get. @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
    ruleId :: Types.ResourceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetRule' value with any optional fields omitted.
mkGetRule ::
  -- | 'ruleId'
  Types.ResourceId ->
  GetRule
mkGetRule ruleId = GetRule' {ruleId}

-- | The @RuleId@ of the 'Rule' that you want to get. @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
--
-- /Note:/ Consider using 'ruleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grRuleId :: Lens.Lens' GetRule Types.ResourceId
grRuleId = Lens.field @"ruleId"
{-# DEPRECATED grRuleId "Use generic-lens or generic-optics with 'ruleId' instead." #-}

instance Core.FromJSON GetRule where
  toJSON GetRule {..} =
    Core.object
      (Core.catMaybes [Core.Just ("RuleId" Core..= ruleId)])

instance Core.AWSRequest GetRule where
  type Rs GetRule = GetRuleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSWAF_Regional_20161128.GetRule")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRuleResponse'
            Core.<$> (x Core..:? "Rule") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetRuleResponse' smart constructor.
data GetRuleResponse = GetRuleResponse'
  { -- | Information about the 'Rule' that you specified in the @GetRule@ request. For more information, see the following topics:
    --
    --
    --     * 'Rule' : Contains @MetricName@ , @Name@ , an array of @Predicate@ objects, and @RuleId@
    --
    --
    --     * 'Predicate' : Each @Predicate@ object contains @DataId@ , @Negated@ , and @Type@
    rule :: Core.Maybe Types.Rule,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRuleResponse' value with any optional fields omitted.
mkGetRuleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetRuleResponse
mkGetRuleResponse responseStatus =
  GetRuleResponse' {rule = Core.Nothing, responseStatus}

-- | Information about the 'Rule' that you specified in the @GetRule@ request. For more information, see the following topics:
--
--
--     * 'Rule' : Contains @MetricName@ , @Name@ , an array of @Predicate@ objects, and @RuleId@
--
--
--     * 'Predicate' : Each @Predicate@ object contains @DataId@ , @Negated@ , and @Type@
--
--
--
-- /Note:/ Consider using 'rule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsRule :: Lens.Lens' GetRuleResponse (Core.Maybe Types.Rule)
grrrsRule = Lens.field @"rule"
{-# DEPRECATED grrrsRule "Use generic-lens or generic-optics with 'rule' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsResponseStatus :: Lens.Lens' GetRuleResponse Core.Int
grrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
