{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.EnableInsightRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified Contributor Insights rules. When rules are enabled, they immediately begin analyzing log data.
module Network.AWS.CloudWatch.EnableInsightRules
  ( -- * Creating a request
    EnableInsightRules (..),
    mkEnableInsightRules,

    -- ** Request lenses
    eirRuleNames,

    -- * Destructuring the response
    EnableInsightRulesResponse (..),
    mkEnableInsightRulesResponse,

    -- ** Response lenses
    eirrrsFailures,
    eirrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEnableInsightRules' smart constructor.
newtype EnableInsightRules = EnableInsightRules'
  { -- | An array of the rule names to enable. If you need to find out the names of your rules, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules> .
    ruleNames :: [Types.InsightRuleName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EnableInsightRules' value with any optional fields omitted.
mkEnableInsightRules ::
  EnableInsightRules
mkEnableInsightRules = EnableInsightRules' {ruleNames = Core.mempty}

-- | An array of the rule names to enable. If you need to find out the names of your rules, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules> .
--
-- /Note:/ Consider using 'ruleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirRuleNames :: Lens.Lens' EnableInsightRules [Types.InsightRuleName]
eirRuleNames = Lens.field @"ruleNames"
{-# DEPRECATED eirRuleNames "Use generic-lens or generic-optics with 'ruleNames' instead." #-}

instance Core.AWSRequest EnableInsightRules where
  type Rs EnableInsightRules = EnableInsightRulesResponse
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
            ( Core.pure ("Action", "EnableInsightRules")
                Core.<> (Core.pure ("Version", "2010-08-01"))
                Core.<> ( Core.toQueryValue
                            "RuleNames"
                            (Core.toQueryList "member" ruleNames)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "EnableInsightRulesResult"
      ( \s h x ->
          EnableInsightRulesResponse'
            Core.<$> (x Core..@? "Failures" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkEnableInsightRulesResponse' smart constructor.
data EnableInsightRulesResponse = EnableInsightRulesResponse'
  { -- | An array listing the rules that could not be enabled. You cannot disable or enable built-in rules.
    failures :: Core.Maybe [Types.PartialFailure],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableInsightRulesResponse' value with any optional fields omitted.
mkEnableInsightRulesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  EnableInsightRulesResponse
mkEnableInsightRulesResponse responseStatus =
  EnableInsightRulesResponse'
    { failures = Core.Nothing,
      responseStatus
    }

-- | An array listing the rules that could not be enabled. You cannot disable or enable built-in rules.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirrrsFailures :: Lens.Lens' EnableInsightRulesResponse (Core.Maybe [Types.PartialFailure])
eirrrsFailures = Lens.field @"failures"
{-# DEPRECATED eirrrsFailures "Use generic-lens or generic-optics with 'failures' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirrrsResponseStatus :: Lens.Lens' EnableInsightRulesResponse Core.Int
eirrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED eirrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
