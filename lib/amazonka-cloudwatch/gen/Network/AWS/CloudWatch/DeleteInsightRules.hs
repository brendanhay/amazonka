{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DeleteInsightRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes the specified Contributor Insights rules.
--
-- If you create a rule, delete it, and then re-create it with the same name, historical data from the first time the rule was created might not be available.
module Network.AWS.CloudWatch.DeleteInsightRules
  ( -- * Creating a request
    DeleteInsightRules (..),
    mkDeleteInsightRules,

    -- ** Request lenses
    dRuleNames,

    -- * Destructuring the response
    DeleteInsightRulesResponse (..),
    mkDeleteInsightRulesResponse,

    -- ** Response lenses
    drsFailures,
    drsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteInsightRules' smart constructor.
newtype DeleteInsightRules = DeleteInsightRules'
  { -- | An array of the rule names to delete. If you need to find out the names of your rules, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules> .
    ruleNames :: [Types.InsightRuleName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInsightRules' value with any optional fields omitted.
mkDeleteInsightRules ::
  DeleteInsightRules
mkDeleteInsightRules = DeleteInsightRules' {ruleNames = Core.mempty}

-- | An array of the rule names to delete. If you need to find out the names of your rules, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules> .
--
-- /Note:/ Consider using 'ruleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRuleNames :: Lens.Lens' DeleteInsightRules [Types.InsightRuleName]
dRuleNames = Lens.field @"ruleNames"
{-# DEPRECATED dRuleNames "Use generic-lens or generic-optics with 'ruleNames' instead." #-}

instance Core.AWSRequest DeleteInsightRules where
  type Rs DeleteInsightRules = DeleteInsightRulesResponse
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
            ( Core.pure ("Action", "DeleteInsightRules")
                Core.<> (Core.pure ("Version", "2010-08-01"))
                Core.<> ( Core.toQueryValue
                            "RuleNames"
                            (Core.toQueryList "member" ruleNames)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteInsightRulesResult"
      ( \s h x ->
          DeleteInsightRulesResponse'
            Core.<$> (x Core..@? "Failures" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteInsightRulesResponse' smart constructor.
data DeleteInsightRulesResponse = DeleteInsightRulesResponse'
  { -- | An array listing the rules that could not be deleted. You cannot delete built-in rules.
    failures :: Core.Maybe [Types.PartialFailure],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInsightRulesResponse' value with any optional fields omitted.
mkDeleteInsightRulesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteInsightRulesResponse
mkDeleteInsightRulesResponse responseStatus =
  DeleteInsightRulesResponse'
    { failures = Core.Nothing,
      responseStatus
    }

-- | An array listing the rules that could not be deleted. You cannot delete built-in rules.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsFailures :: Lens.Lens' DeleteInsightRulesResponse (Core.Maybe [Types.PartialFailure])
drsFailures = Lens.field @"failures"
{-# DEPRECATED drsFailures "Use generic-lens or generic-optics with 'failures' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteInsightRulesResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
