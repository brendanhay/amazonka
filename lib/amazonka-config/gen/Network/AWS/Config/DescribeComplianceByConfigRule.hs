{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeComplianceByConfigRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Indicates whether the specified AWS Config rules are compliant. If a rule is noncompliant, this action returns the number of AWS resources that do not comply with the rule.
--
-- A rule is compliant if all of the evaluated resources comply with it. It is noncompliant if any of these resources do not comply.
-- If AWS Config has no current evaluation results for the rule, it returns @INSUFFICIENT_DATA@ . This result might indicate one of the following conditions:
--
--     * AWS Config has never invoked an evaluation for the rule. To check whether it has, use the @DescribeConfigRuleEvaluationStatus@ action to get the @LastSuccessfulInvocationTime@ and @LastFailedInvocationTime@ .
--
--
--     * The rule's AWS Lambda function is failing to send evaluation results to AWS Config. Verify that the role you assigned to your configuration recorder includes the @config:PutEvaluations@ permission. If the rule is a custom rule, verify that the AWS Lambda execution role includes the @config:PutEvaluations@ permission.
--
--
--     * The rule's AWS Lambda function has returned @NOT_APPLICABLE@ for all evaluation results. This can occur if the resources were deleted or removed from the rule's scope.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeComplianceByConfigRule
  ( -- * Creating a request
    DescribeComplianceByConfigRule (..),
    mkDescribeComplianceByConfigRule,

    -- ** Request lenses
    dcbcrComplianceTypes,
    dcbcrConfigRuleNames,
    dcbcrNextToken,

    -- * Destructuring the response
    DescribeComplianceByConfigRuleResponse (..),
    mkDescribeComplianceByConfigRuleResponse,

    -- ** Response lenses
    dcbcrrrsComplianceByConfigRules,
    dcbcrrrsNextToken,
    dcbcrrrsResponseStatus,
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
-- /See:/ 'mkDescribeComplianceByConfigRule' smart constructor.
data DescribeComplianceByConfigRule = DescribeComplianceByConfigRule'
  { -- | Filters the results by compliance.
    --
    -- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ .
    complianceTypes :: Core.Maybe [Types.ComplianceType],
    -- | Specify one or more AWS Config rule names to filter the results by rule.
    configRuleNames :: Core.Maybe [Types.ConfigRuleName],
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeComplianceByConfigRule' value with any optional fields omitted.
mkDescribeComplianceByConfigRule ::
  DescribeComplianceByConfigRule
mkDescribeComplianceByConfigRule =
  DescribeComplianceByConfigRule'
    { complianceTypes = Core.Nothing,
      configRuleNames = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ .
--
-- /Note:/ Consider using 'complianceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbcrComplianceTypes :: Lens.Lens' DescribeComplianceByConfigRule (Core.Maybe [Types.ComplianceType])
dcbcrComplianceTypes = Lens.field @"complianceTypes"
{-# DEPRECATED dcbcrComplianceTypes "Use generic-lens or generic-optics with 'complianceTypes' instead." #-}

-- | Specify one or more AWS Config rule names to filter the results by rule.
--
-- /Note:/ Consider using 'configRuleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbcrConfigRuleNames :: Lens.Lens' DescribeComplianceByConfigRule (Core.Maybe [Types.ConfigRuleName])
dcbcrConfigRuleNames = Lens.field @"configRuleNames"
{-# DEPRECATED dcbcrConfigRuleNames "Use generic-lens or generic-optics with 'configRuleNames' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbcrNextToken :: Lens.Lens' DescribeComplianceByConfigRule (Core.Maybe Types.String)
dcbcrNextToken = Lens.field @"nextToken"
{-# DEPRECATED dcbcrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeComplianceByConfigRule where
  toJSON DescribeComplianceByConfigRule {..} =
    Core.object
      ( Core.catMaybes
          [ ("ComplianceTypes" Core..=) Core.<$> complianceTypes,
            ("ConfigRuleNames" Core..=) Core.<$> configRuleNames,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeComplianceByConfigRule where
  type
    Rs DescribeComplianceByConfigRule =
      DescribeComplianceByConfigRuleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StarlingDoveService.DescribeComplianceByConfigRule"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeComplianceByConfigRuleResponse'
            Core.<$> (x Core..:? "ComplianceByConfigRules")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeComplianceByConfigRule where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"complianceByConfigRules" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- |
--
-- /See:/ 'mkDescribeComplianceByConfigRuleResponse' smart constructor.
data DescribeComplianceByConfigRuleResponse = DescribeComplianceByConfigRuleResponse'
  { -- | Indicates whether each of the specified AWS Config rules is compliant.
    complianceByConfigRules :: Core.Maybe [Types.ComplianceByConfigRule],
    -- | The string that you use in a subsequent request to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeComplianceByConfigRuleResponse' value with any optional fields omitted.
mkDescribeComplianceByConfigRuleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeComplianceByConfigRuleResponse
mkDescribeComplianceByConfigRuleResponse responseStatus =
  DescribeComplianceByConfigRuleResponse'
    { complianceByConfigRules =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Indicates whether each of the specified AWS Config rules is compliant.
--
-- /Note:/ Consider using 'complianceByConfigRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbcrrrsComplianceByConfigRules :: Lens.Lens' DescribeComplianceByConfigRuleResponse (Core.Maybe [Types.ComplianceByConfigRule])
dcbcrrrsComplianceByConfigRules = Lens.field @"complianceByConfigRules"
{-# DEPRECATED dcbcrrrsComplianceByConfigRules "Use generic-lens or generic-optics with 'complianceByConfigRules' instead." #-}

-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbcrrrsNextToken :: Lens.Lens' DescribeComplianceByConfigRuleResponse (Core.Maybe Types.NextToken)
dcbcrrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dcbcrrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbcrrrsResponseStatus :: Lens.Lens' DescribeComplianceByConfigRuleResponse Core.Int
dcbcrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcbcrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
