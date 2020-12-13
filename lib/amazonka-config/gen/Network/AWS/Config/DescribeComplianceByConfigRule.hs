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
    dcbcrConfigRuleNames,
    dcbcrComplianceTypes,
    dcbcrNextToken,

    -- * Destructuring the response
    DescribeComplianceByConfigRuleResponse (..),
    mkDescribeComplianceByConfigRuleResponse,

    -- ** Response lenses
    dcbcrrsComplianceByConfigRules,
    dcbcrrsNextToken,
    dcbcrrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeComplianceByConfigRule' smart constructor.
data DescribeComplianceByConfigRule = DescribeComplianceByConfigRule'
  { -- | Specify one or more AWS Config rule names to filter the results by rule.
    configRuleNames :: Lude.Maybe [Lude.Text],
    -- | Filters the results by compliance.
    --
    -- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ .
    complianceTypes :: Lude.Maybe [ComplianceType],
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeComplianceByConfigRule' with the minimum fields required to make a request.
--
-- * 'configRuleNames' - Specify one or more AWS Config rule names to filter the results by rule.
-- * 'complianceTypes' - Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ .
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
mkDescribeComplianceByConfigRule ::
  DescribeComplianceByConfigRule
mkDescribeComplianceByConfigRule =
  DescribeComplianceByConfigRule'
    { configRuleNames = Lude.Nothing,
      complianceTypes = Lude.Nothing,
      nextToken = Lude.Nothing
    }

-- | Specify one or more AWS Config rule names to filter the results by rule.
--
-- /Note:/ Consider using 'configRuleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbcrConfigRuleNames :: Lens.Lens' DescribeComplianceByConfigRule (Lude.Maybe [Lude.Text])
dcbcrConfigRuleNames = Lens.lens (configRuleNames :: DescribeComplianceByConfigRule -> Lude.Maybe [Lude.Text]) (\s a -> s {configRuleNames = a} :: DescribeComplianceByConfigRule)
{-# DEPRECATED dcbcrConfigRuleNames "Use generic-lens or generic-optics with 'configRuleNames' instead." #-}

-- | Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ .
--
-- /Note:/ Consider using 'complianceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbcrComplianceTypes :: Lens.Lens' DescribeComplianceByConfigRule (Lude.Maybe [ComplianceType])
dcbcrComplianceTypes = Lens.lens (complianceTypes :: DescribeComplianceByConfigRule -> Lude.Maybe [ComplianceType]) (\s a -> s {complianceTypes = a} :: DescribeComplianceByConfigRule)
{-# DEPRECATED dcbcrComplianceTypes "Use generic-lens or generic-optics with 'complianceTypes' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbcrNextToken :: Lens.Lens' DescribeComplianceByConfigRule (Lude.Maybe Lude.Text)
dcbcrNextToken = Lens.lens (nextToken :: DescribeComplianceByConfigRule -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeComplianceByConfigRule)
{-# DEPRECATED dcbcrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager DescribeComplianceByConfigRule where
  page rq rs
    | Page.stop (rs Lens.^. dcbcrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dcbcrrsComplianceByConfigRules) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dcbcrNextToken Lens..~ rs Lens.^. dcbcrrsNextToken

instance Lude.AWSRequest DescribeComplianceByConfigRule where
  type
    Rs DescribeComplianceByConfigRule =
      DescribeComplianceByConfigRuleResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeComplianceByConfigRuleResponse'
            Lude.<$> (x Lude..?> "ComplianceByConfigRules" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeComplianceByConfigRule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DescribeComplianceByConfigRule" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeComplianceByConfigRule where
  toJSON DescribeComplianceByConfigRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ConfigRuleNames" Lude..=) Lude.<$> configRuleNames,
            ("ComplianceTypes" Lude..=) Lude.<$> complianceTypes,
            ("NextToken" Lude..=) Lude.<$> nextToken
          ]
      )

instance Lude.ToPath DescribeComplianceByConfigRule where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeComplianceByConfigRule where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDescribeComplianceByConfigRuleResponse' smart constructor.
data DescribeComplianceByConfigRuleResponse = DescribeComplianceByConfigRuleResponse'
  { -- | Indicates whether each of the specified AWS Config rules is compliant.
    complianceByConfigRules :: Lude.Maybe [ComplianceByConfigRule],
    -- | The string that you use in a subsequent request to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeComplianceByConfigRuleResponse' with the minimum fields required to make a request.
--
-- * 'complianceByConfigRules' - Indicates whether each of the specified AWS Config rules is compliant.
-- * 'nextToken' - The string that you use in a subsequent request to get the next page of results in a paginated response.
-- * 'responseStatus' - The response status code.
mkDescribeComplianceByConfigRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeComplianceByConfigRuleResponse
mkDescribeComplianceByConfigRuleResponse pResponseStatus_ =
  DescribeComplianceByConfigRuleResponse'
    { complianceByConfigRules =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Indicates whether each of the specified AWS Config rules is compliant.
--
-- /Note:/ Consider using 'complianceByConfigRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbcrrsComplianceByConfigRules :: Lens.Lens' DescribeComplianceByConfigRuleResponse (Lude.Maybe [ComplianceByConfigRule])
dcbcrrsComplianceByConfigRules = Lens.lens (complianceByConfigRules :: DescribeComplianceByConfigRuleResponse -> Lude.Maybe [ComplianceByConfigRule]) (\s a -> s {complianceByConfigRules = a} :: DescribeComplianceByConfigRuleResponse)
{-# DEPRECATED dcbcrrsComplianceByConfigRules "Use generic-lens or generic-optics with 'complianceByConfigRules' instead." #-}

-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbcrrsNextToken :: Lens.Lens' DescribeComplianceByConfigRuleResponse (Lude.Maybe Lude.Text)
dcbcrrsNextToken = Lens.lens (nextToken :: DescribeComplianceByConfigRuleResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeComplianceByConfigRuleResponse)
{-# DEPRECATED dcbcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbcrrsResponseStatus :: Lens.Lens' DescribeComplianceByConfigRuleResponse Lude.Int
dcbcrrsResponseStatus = Lens.lens (responseStatus :: DescribeComplianceByConfigRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeComplianceByConfigRuleResponse)
{-# DEPRECATED dcbcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
