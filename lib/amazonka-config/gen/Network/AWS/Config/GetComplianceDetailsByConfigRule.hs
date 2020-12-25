{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.GetComplianceDetailsByConfigRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the evaluation results for the specified AWS Config rule. The results indicate which AWS resources were evaluated by the rule, when each resource was last evaluated, and whether each resource complies with the rule.
--
-- This operation returns paginated results.
module Network.AWS.Config.GetComplianceDetailsByConfigRule
  ( -- * Creating a request
    GetComplianceDetailsByConfigRule (..),
    mkGetComplianceDetailsByConfigRule,

    -- ** Request lenses
    gcdbcrConfigRuleName,
    gcdbcrComplianceTypes,
    gcdbcrLimit,
    gcdbcrNextToken,

    -- * Destructuring the response
    GetComplianceDetailsByConfigRuleResponse (..),
    mkGetComplianceDetailsByConfigRuleResponse,

    -- ** Response lenses
    gcdbcrrrsEvaluationResults,
    gcdbcrrrsNextToken,
    gcdbcrrrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkGetComplianceDetailsByConfigRule' smart constructor.
data GetComplianceDetailsByConfigRule = GetComplianceDetailsByConfigRule'
  { -- | The name of the AWS Config rule for which you want compliance information.
    configRuleName :: Types.StringWithCharLimit64,
    -- | Filters the results by compliance.
    --
    -- The allowed values are @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ .
    complianceTypes :: Core.Maybe [Types.ComplianceType],
    -- | The maximum number of evaluation results returned on each page. The default is 10. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
    limit :: Core.Maybe Core.Natural,
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetComplianceDetailsByConfigRule' value with any optional fields omitted.
mkGetComplianceDetailsByConfigRule ::
  -- | 'configRuleName'
  Types.StringWithCharLimit64 ->
  GetComplianceDetailsByConfigRule
mkGetComplianceDetailsByConfigRule configRuleName =
  GetComplianceDetailsByConfigRule'
    { configRuleName,
      complianceTypes = Core.Nothing,
      limit = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The name of the AWS Config rule for which you want compliance information.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbcrConfigRuleName :: Lens.Lens' GetComplianceDetailsByConfigRule Types.StringWithCharLimit64
gcdbcrConfigRuleName = Lens.field @"configRuleName"
{-# DEPRECATED gcdbcrConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

-- | Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ .
--
-- /Note:/ Consider using 'complianceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbcrComplianceTypes :: Lens.Lens' GetComplianceDetailsByConfigRule (Core.Maybe [Types.ComplianceType])
gcdbcrComplianceTypes = Lens.field @"complianceTypes"
{-# DEPRECATED gcdbcrComplianceTypes "Use generic-lens or generic-optics with 'complianceTypes' instead." #-}

-- | The maximum number of evaluation results returned on each page. The default is 10. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbcrLimit :: Lens.Lens' GetComplianceDetailsByConfigRule (Core.Maybe Core.Natural)
gcdbcrLimit = Lens.field @"limit"
{-# DEPRECATED gcdbcrLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbcrNextToken :: Lens.Lens' GetComplianceDetailsByConfigRule (Core.Maybe Types.NextToken)
gcdbcrNextToken = Lens.field @"nextToken"
{-# DEPRECATED gcdbcrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON GetComplianceDetailsByConfigRule where
  toJSON GetComplianceDetailsByConfigRule {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ConfigRuleName" Core..= configRuleName),
            ("ComplianceTypes" Core..=) Core.<$> complianceTypes,
            ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest GetComplianceDetailsByConfigRule where
  type
    Rs GetComplianceDetailsByConfigRule =
      GetComplianceDetailsByConfigRuleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StarlingDoveService.GetComplianceDetailsByConfigRule"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetComplianceDetailsByConfigRuleResponse'
            Core.<$> (x Core..:? "EvaluationResults")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetComplianceDetailsByConfigRule where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"evaluationResults" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- |
--
-- /See:/ 'mkGetComplianceDetailsByConfigRuleResponse' smart constructor.
data GetComplianceDetailsByConfigRuleResponse = GetComplianceDetailsByConfigRuleResponse'
  { -- | Indicates whether the AWS resource complies with the specified AWS Config rule.
    evaluationResults :: Core.Maybe [Types.EvaluationResult],
    -- | The string that you use in a subsequent request to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetComplianceDetailsByConfigRuleResponse' value with any optional fields omitted.
mkGetComplianceDetailsByConfigRuleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetComplianceDetailsByConfigRuleResponse
mkGetComplianceDetailsByConfigRuleResponse responseStatus =
  GetComplianceDetailsByConfigRuleResponse'
    { evaluationResults =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Indicates whether the AWS resource complies with the specified AWS Config rule.
--
-- /Note:/ Consider using 'evaluationResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbcrrrsEvaluationResults :: Lens.Lens' GetComplianceDetailsByConfigRuleResponse (Core.Maybe [Types.EvaluationResult])
gcdbcrrrsEvaluationResults = Lens.field @"evaluationResults"
{-# DEPRECATED gcdbcrrrsEvaluationResults "Use generic-lens or generic-optics with 'evaluationResults' instead." #-}

-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbcrrrsNextToken :: Lens.Lens' GetComplianceDetailsByConfigRuleResponse (Core.Maybe Types.NextToken)
gcdbcrrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gcdbcrrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbcrrrsResponseStatus :: Lens.Lens' GetComplianceDetailsByConfigRuleResponse Core.Int
gcdbcrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcdbcrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
