{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.GetAggregateComplianceDetailsByConfigRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the evaluation results for the specified AWS Config rule for a specific resource in a rule. The results indicate which AWS resources were evaluated by the rule, when each resource was last evaluated, and whether each resource complies with the rule.
--
-- This operation returns paginated results.
module Network.AWS.Config.GetAggregateComplianceDetailsByConfigRule
  ( -- * Creating a request
    GetAggregateComplianceDetailsByConfigRule (..),
    mkGetAggregateComplianceDetailsByConfigRule,

    -- ** Request lenses
    gacdbcrConfigurationAggregatorName,
    gacdbcrConfigRuleName,
    gacdbcrAccountId,
    gacdbcrAwsRegion,
    gacdbcrComplianceType,
    gacdbcrLimit,
    gacdbcrNextToken,

    -- * Destructuring the response
    GetAggregateComplianceDetailsByConfigRuleResponse (..),
    mkGetAggregateComplianceDetailsByConfigRuleResponse,

    -- ** Response lenses
    gacdbcrrrsAggregateEvaluationResults,
    gacdbcrrrsNextToken,
    gacdbcrrrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAggregateComplianceDetailsByConfigRule' smart constructor.
data GetAggregateComplianceDetailsByConfigRule = GetAggregateComplianceDetailsByConfigRule'
  { -- | The name of the configuration aggregator.
    configurationAggregatorName :: Types.ConfigurationAggregatorName,
    -- | The name of the AWS Config rule for which you want compliance information.
    configRuleName :: Types.ConfigRuleName,
    -- | The 12-digit account ID of the source account.
    accountId :: Types.AccountId,
    -- | The source region from where the data is aggregated.
    awsRegion :: Types.AwsRegion,
    -- | The resource compliance status.
    complianceType :: Core.Maybe Types.ComplianceType,
    -- | The maximum number of evaluation results returned on each page. The default is 50. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
    limit :: Core.Maybe Core.Natural,
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAggregateComplianceDetailsByConfigRule' value with any optional fields omitted.
mkGetAggregateComplianceDetailsByConfigRule ::
  -- | 'configurationAggregatorName'
  Types.ConfigurationAggregatorName ->
  -- | 'configRuleName'
  Types.ConfigRuleName ->
  -- | 'accountId'
  Types.AccountId ->
  -- | 'awsRegion'
  Types.AwsRegion ->
  GetAggregateComplianceDetailsByConfigRule
mkGetAggregateComplianceDetailsByConfigRule
  configurationAggregatorName
  configRuleName
  accountId
  awsRegion =
    GetAggregateComplianceDetailsByConfigRule'
      { configurationAggregatorName,
        configRuleName,
        accountId,
        awsRegion,
        complianceType = Core.Nothing,
        limit = Core.Nothing,
        nextToken = Core.Nothing
      }

-- | The name of the configuration aggregator.
--
-- /Note:/ Consider using 'configurationAggregatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacdbcrConfigurationAggregatorName :: Lens.Lens' GetAggregateComplianceDetailsByConfigRule Types.ConfigurationAggregatorName
gacdbcrConfigurationAggregatorName = Lens.field @"configurationAggregatorName"
{-# DEPRECATED gacdbcrConfigurationAggregatorName "Use generic-lens or generic-optics with 'configurationAggregatorName' instead." #-}

-- | The name of the AWS Config rule for which you want compliance information.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacdbcrConfigRuleName :: Lens.Lens' GetAggregateComplianceDetailsByConfigRule Types.ConfigRuleName
gacdbcrConfigRuleName = Lens.field @"configRuleName"
{-# DEPRECATED gacdbcrConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

-- | The 12-digit account ID of the source account.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacdbcrAccountId :: Lens.Lens' GetAggregateComplianceDetailsByConfigRule Types.AccountId
gacdbcrAccountId = Lens.field @"accountId"
{-# DEPRECATED gacdbcrAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The source region from where the data is aggregated.
--
-- /Note:/ Consider using 'awsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacdbcrAwsRegion :: Lens.Lens' GetAggregateComplianceDetailsByConfigRule Types.AwsRegion
gacdbcrAwsRegion = Lens.field @"awsRegion"
{-# DEPRECATED gacdbcrAwsRegion "Use generic-lens or generic-optics with 'awsRegion' instead." #-}

-- | The resource compliance status.
--
-- /Note:/ Consider using 'complianceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacdbcrComplianceType :: Lens.Lens' GetAggregateComplianceDetailsByConfigRule (Core.Maybe Types.ComplianceType)
gacdbcrComplianceType = Lens.field @"complianceType"
{-# DEPRECATED gacdbcrComplianceType "Use generic-lens or generic-optics with 'complianceType' instead." #-}

-- | The maximum number of evaluation results returned on each page. The default is 50. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacdbcrLimit :: Lens.Lens' GetAggregateComplianceDetailsByConfigRule (Core.Maybe Core.Natural)
gacdbcrLimit = Lens.field @"limit"
{-# DEPRECATED gacdbcrLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacdbcrNextToken :: Lens.Lens' GetAggregateComplianceDetailsByConfigRule (Core.Maybe Types.NextToken)
gacdbcrNextToken = Lens.field @"nextToken"
{-# DEPRECATED gacdbcrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON GetAggregateComplianceDetailsByConfigRule where
  toJSON GetAggregateComplianceDetailsByConfigRule {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "ConfigurationAggregatorName"
                  Core..= configurationAggregatorName
              ),
            Core.Just ("ConfigRuleName" Core..= configRuleName),
            Core.Just ("AccountId" Core..= accountId),
            Core.Just ("AwsRegion" Core..= awsRegion),
            ("ComplianceType" Core..=) Core.<$> complianceType,
            ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest GetAggregateComplianceDetailsByConfigRule where
  type
    Rs GetAggregateComplianceDetailsByConfigRule =
      GetAggregateComplianceDetailsByConfigRuleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StarlingDoveService.GetAggregateComplianceDetailsByConfigRule"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAggregateComplianceDetailsByConfigRuleResponse'
            Core.<$> (x Core..:? "AggregateEvaluationResults")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetAggregateComplianceDetailsByConfigRule where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"aggregateEvaluationResults" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetAggregateComplianceDetailsByConfigRuleResponse' smart constructor.
data GetAggregateComplianceDetailsByConfigRuleResponse = GetAggregateComplianceDetailsByConfigRuleResponse'
  { -- | Returns an AggregateEvaluationResults object.
    aggregateEvaluationResults :: Core.Maybe [Types.AggregateEvaluationResult],
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetAggregateComplianceDetailsByConfigRuleResponse' value with any optional fields omitted.
mkGetAggregateComplianceDetailsByConfigRuleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetAggregateComplianceDetailsByConfigRuleResponse
mkGetAggregateComplianceDetailsByConfigRuleResponse responseStatus =
  GetAggregateComplianceDetailsByConfigRuleResponse'
    { aggregateEvaluationResults =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Returns an AggregateEvaluationResults object.
--
-- /Note:/ Consider using 'aggregateEvaluationResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacdbcrrrsAggregateEvaluationResults :: Lens.Lens' GetAggregateComplianceDetailsByConfigRuleResponse (Core.Maybe [Types.AggregateEvaluationResult])
gacdbcrrrsAggregateEvaluationResults = Lens.field @"aggregateEvaluationResults"
{-# DEPRECATED gacdbcrrrsAggregateEvaluationResults "Use generic-lens or generic-optics with 'aggregateEvaluationResults' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacdbcrrrsNextToken :: Lens.Lens' GetAggregateComplianceDetailsByConfigRuleResponse (Core.Maybe Types.NextToken)
gacdbcrrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gacdbcrrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacdbcrrrsResponseStatus :: Lens.Lens' GetAggregateComplianceDetailsByConfigRuleResponse Core.Int
gacdbcrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gacdbcrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
