{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DisableInsightRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the specified Contributor Insights rules. When rules are disabled, they do not analyze log groups and do not incur costs.
module Network.AWS.CloudWatch.DisableInsightRules
  ( -- * Creating a request
    DisableInsightRules (..),
    mkDisableInsightRules,

    -- ** Request lenses
    dirRuleNames,

    -- * Destructuring the response
    DisableInsightRulesResponse (..),
    mkDisableInsightRulesResponse,

    -- ** Response lenses
    dirrrsFailures,
    dirrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisableInsightRules' smart constructor.
newtype DisableInsightRules = DisableInsightRules'
  { -- | An array of the rule names to disable. If you need to find out the names of your rules, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules> .
    ruleNames :: [Types.InsightRuleName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisableInsightRules' value with any optional fields omitted.
mkDisableInsightRules ::
  DisableInsightRules
mkDisableInsightRules =
  DisableInsightRules' {ruleNames = Core.mempty}

-- | An array of the rule names to disable. If you need to find out the names of your rules, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules> .
--
-- /Note:/ Consider using 'ruleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirRuleNames :: Lens.Lens' DisableInsightRules [Types.InsightRuleName]
dirRuleNames = Lens.field @"ruleNames"
{-# DEPRECATED dirRuleNames "Use generic-lens or generic-optics with 'ruleNames' instead." #-}

instance Core.AWSRequest DisableInsightRules where
  type Rs DisableInsightRules = DisableInsightRulesResponse
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
            ( Core.pure ("Action", "DisableInsightRules")
                Core.<> (Core.pure ("Version", "2010-08-01"))
                Core.<> ( Core.toQueryValue
                            "RuleNames"
                            (Core.toQueryList "member" ruleNames)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DisableInsightRulesResult"
      ( \s h x ->
          DisableInsightRulesResponse'
            Core.<$> (x Core..@? "Failures" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisableInsightRulesResponse' smart constructor.
data DisableInsightRulesResponse = DisableInsightRulesResponse'
  { -- | An array listing the rules that could not be disabled. You cannot disable built-in rules.
    failures :: Core.Maybe [Types.PartialFailure],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableInsightRulesResponse' value with any optional fields omitted.
mkDisableInsightRulesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisableInsightRulesResponse
mkDisableInsightRulesResponse responseStatus =
  DisableInsightRulesResponse'
    { failures = Core.Nothing,
      responseStatus
    }

-- | An array listing the rules that could not be disabled. You cannot disable built-in rules.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrrsFailures :: Lens.Lens' DisableInsightRulesResponse (Core.Maybe [Types.PartialFailure])
dirrrsFailures = Lens.field @"failures"
{-# DEPRECATED dirrrsFailures "Use generic-lens or generic-optics with 'failures' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrrsResponseStatus :: Lens.Lens' DisableInsightRulesResponse Core.Int
dirrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dirrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
