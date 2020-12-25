{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.DeleteSamplingRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a sampling rule.
module Network.AWS.XRay.DeleteSamplingRule
  ( -- * Creating a request
    DeleteSamplingRule (..),
    mkDeleteSamplingRule,

    -- ** Request lenses
    dsrRuleARN,
    dsrRuleName,

    -- * Destructuring the response
    DeleteSamplingRuleResponse (..),
    mkDeleteSamplingRuleResponse,

    -- ** Response lenses
    dsrrrsSamplingRuleRecord,
    dsrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.XRay.Types as Types

-- | /See:/ 'mkDeleteSamplingRule' smart constructor.
data DeleteSamplingRule = DeleteSamplingRule'
  { -- | The ARN of the sampling rule. Specify a rule by either name or ARN, but not both.
    ruleARN :: Core.Maybe Types.String,
    -- | The name of the sampling rule. Specify a rule by either name or ARN, but not both.
    ruleName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSamplingRule' value with any optional fields omitted.
mkDeleteSamplingRule ::
  DeleteSamplingRule
mkDeleteSamplingRule =
  DeleteSamplingRule'
    { ruleARN = Core.Nothing,
      ruleName = Core.Nothing
    }

-- | The ARN of the sampling rule. Specify a rule by either name or ARN, but not both.
--
-- /Note:/ Consider using 'ruleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrRuleARN :: Lens.Lens' DeleteSamplingRule (Core.Maybe Types.String)
dsrRuleARN = Lens.field @"ruleARN"
{-# DEPRECATED dsrRuleARN "Use generic-lens or generic-optics with 'ruleARN' instead." #-}

-- | The name of the sampling rule. Specify a rule by either name or ARN, but not both.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrRuleName :: Lens.Lens' DeleteSamplingRule (Core.Maybe Types.String)
dsrRuleName = Lens.field @"ruleName"
{-# DEPRECATED dsrRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

instance Core.FromJSON DeleteSamplingRule where
  toJSON DeleteSamplingRule {..} =
    Core.object
      ( Core.catMaybes
          [ ("RuleARN" Core..=) Core.<$> ruleARN,
            ("RuleName" Core..=) Core.<$> ruleName
          ]
      )

instance Core.AWSRequest DeleteSamplingRule where
  type Rs DeleteSamplingRule = DeleteSamplingRuleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/DeleteSamplingRule",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSamplingRuleResponse'
            Core.<$> (x Core..:? "SamplingRuleRecord")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteSamplingRuleResponse' smart constructor.
data DeleteSamplingRuleResponse = DeleteSamplingRuleResponse'
  { -- | The deleted rule definition and metadata.
    samplingRuleRecord :: Core.Maybe Types.SamplingRuleRecord,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteSamplingRuleResponse' value with any optional fields omitted.
mkDeleteSamplingRuleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteSamplingRuleResponse
mkDeleteSamplingRuleResponse responseStatus =
  DeleteSamplingRuleResponse'
    { samplingRuleRecord = Core.Nothing,
      responseStatus
    }

-- | The deleted rule definition and metadata.
--
-- /Note:/ Consider using 'samplingRuleRecord' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrrsSamplingRuleRecord :: Lens.Lens' DeleteSamplingRuleResponse (Core.Maybe Types.SamplingRuleRecord)
dsrrrsSamplingRuleRecord = Lens.field @"samplingRuleRecord"
{-# DEPRECATED dsrrrsSamplingRuleRecord "Use generic-lens or generic-optics with 'samplingRuleRecord' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrrsResponseStatus :: Lens.Lens' DeleteSamplingRuleResponse Core.Int
dsrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
