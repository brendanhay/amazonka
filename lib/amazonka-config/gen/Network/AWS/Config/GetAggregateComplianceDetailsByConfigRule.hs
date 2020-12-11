{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    gacdbcrNextToken,
    gacdbcrLimit,
    gacdbcrComplianceType,
    gacdbcrConfigurationAggregatorName,
    gacdbcrConfigRuleName,
    gacdbcrAccountId,
    gacdbcrAWSRegion,

    -- * Destructuring the response
    GetAggregateComplianceDetailsByConfigRuleResponse (..),
    mkGetAggregateComplianceDetailsByConfigRuleResponse,

    -- ** Response lenses
    gacdbcrrsNextToken,
    gacdbcrrsAggregateEvaluationResults,
    gacdbcrrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAggregateComplianceDetailsByConfigRule' smart constructor.
data GetAggregateComplianceDetailsByConfigRule = GetAggregateComplianceDetailsByConfigRule'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    limit ::
      Lude.Maybe
        Lude.Natural,
    complianceType ::
      Lude.Maybe
        ComplianceType,
    configurationAggregatorName ::
      Lude.Text,
    configRuleName ::
      Lude.Text,
    accountId ::
      Lude.Text,
    awsRegion ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAggregateComplianceDetailsByConfigRule' with the minimum fields required to make a request.
--
-- * 'accountId' - The 12-digit account ID of the source account.
-- * 'awsRegion' - The source region from where the data is aggregated.
-- * 'complianceType' - The resource compliance status.
-- * 'configRuleName' - The name of the AWS Config rule for which you want compliance information.
-- * 'configurationAggregatorName' - The name of the configuration aggregator.
-- * 'limit' - The maximum number of evaluation results returned on each page. The default is 50. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
mkGetAggregateComplianceDetailsByConfigRule ::
  -- | 'configurationAggregatorName'
  Lude.Text ->
  -- | 'configRuleName'
  Lude.Text ->
  -- | 'accountId'
  Lude.Text ->
  -- | 'awsRegion'
  Lude.Text ->
  GetAggregateComplianceDetailsByConfigRule
mkGetAggregateComplianceDetailsByConfigRule
  pConfigurationAggregatorName_
  pConfigRuleName_
  pAccountId_
  pAWSRegion_ =
    GetAggregateComplianceDetailsByConfigRule'
      { nextToken =
          Lude.Nothing,
        limit = Lude.Nothing,
        complianceType = Lude.Nothing,
        configurationAggregatorName =
          pConfigurationAggregatorName_,
        configRuleName = pConfigRuleName_,
        accountId = pAccountId_,
        awsRegion = pAWSRegion_
      }

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacdbcrNextToken :: Lens.Lens' GetAggregateComplianceDetailsByConfigRule (Lude.Maybe Lude.Text)
gacdbcrNextToken = Lens.lens (nextToken :: GetAggregateComplianceDetailsByConfigRule -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetAggregateComplianceDetailsByConfigRule)
{-# DEPRECATED gacdbcrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of evaluation results returned on each page. The default is 50. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacdbcrLimit :: Lens.Lens' GetAggregateComplianceDetailsByConfigRule (Lude.Maybe Lude.Natural)
gacdbcrLimit = Lens.lens (limit :: GetAggregateComplianceDetailsByConfigRule -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: GetAggregateComplianceDetailsByConfigRule)
{-# DEPRECATED gacdbcrLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The resource compliance status.
--
-- /Note:/ Consider using 'complianceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacdbcrComplianceType :: Lens.Lens' GetAggregateComplianceDetailsByConfigRule (Lude.Maybe ComplianceType)
gacdbcrComplianceType = Lens.lens (complianceType :: GetAggregateComplianceDetailsByConfigRule -> Lude.Maybe ComplianceType) (\s a -> s {complianceType = a} :: GetAggregateComplianceDetailsByConfigRule)
{-# DEPRECATED gacdbcrComplianceType "Use generic-lens or generic-optics with 'complianceType' instead." #-}

-- | The name of the configuration aggregator.
--
-- /Note:/ Consider using 'configurationAggregatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacdbcrConfigurationAggregatorName :: Lens.Lens' GetAggregateComplianceDetailsByConfigRule Lude.Text
gacdbcrConfigurationAggregatorName = Lens.lens (configurationAggregatorName :: GetAggregateComplianceDetailsByConfigRule -> Lude.Text) (\s a -> s {configurationAggregatorName = a} :: GetAggregateComplianceDetailsByConfigRule)
{-# DEPRECATED gacdbcrConfigurationAggregatorName "Use generic-lens or generic-optics with 'configurationAggregatorName' instead." #-}

-- | The name of the AWS Config rule for which you want compliance information.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacdbcrConfigRuleName :: Lens.Lens' GetAggregateComplianceDetailsByConfigRule Lude.Text
gacdbcrConfigRuleName = Lens.lens (configRuleName :: GetAggregateComplianceDetailsByConfigRule -> Lude.Text) (\s a -> s {configRuleName = a} :: GetAggregateComplianceDetailsByConfigRule)
{-# DEPRECATED gacdbcrConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

-- | The 12-digit account ID of the source account.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacdbcrAccountId :: Lens.Lens' GetAggregateComplianceDetailsByConfigRule Lude.Text
gacdbcrAccountId = Lens.lens (accountId :: GetAggregateComplianceDetailsByConfigRule -> Lude.Text) (\s a -> s {accountId = a} :: GetAggregateComplianceDetailsByConfigRule)
{-# DEPRECATED gacdbcrAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The source region from where the data is aggregated.
--
-- /Note:/ Consider using 'awsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacdbcrAWSRegion :: Lens.Lens' GetAggregateComplianceDetailsByConfigRule Lude.Text
gacdbcrAWSRegion = Lens.lens (awsRegion :: GetAggregateComplianceDetailsByConfigRule -> Lude.Text) (\s a -> s {awsRegion = a} :: GetAggregateComplianceDetailsByConfigRule)
{-# DEPRECATED gacdbcrAWSRegion "Use generic-lens or generic-optics with 'awsRegion' instead." #-}

instance Page.AWSPager GetAggregateComplianceDetailsByConfigRule where
  page rq rs
    | Page.stop (rs Lens.^. gacdbcrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gacdbcrrsAggregateEvaluationResults) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gacdbcrNextToken Lens..~ rs Lens.^. gacdbcrrsNextToken

instance Lude.AWSRequest GetAggregateComplianceDetailsByConfigRule where
  type
    Rs GetAggregateComplianceDetailsByConfigRule =
      GetAggregateComplianceDetailsByConfigRuleResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAggregateComplianceDetailsByConfigRuleResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "AggregateEvaluationResults" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAggregateComplianceDetailsByConfigRule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.GetAggregateComplianceDetailsByConfigRule" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetAggregateComplianceDetailsByConfigRule where
  toJSON GetAggregateComplianceDetailsByConfigRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            ("ComplianceType" Lude..=) Lude.<$> complianceType,
            Lude.Just
              ( "ConfigurationAggregatorName"
                  Lude..= configurationAggregatorName
              ),
            Lude.Just ("ConfigRuleName" Lude..= configRuleName),
            Lude.Just ("AccountId" Lude..= accountId),
            Lude.Just ("AwsRegion" Lude..= awsRegion)
          ]
      )

instance Lude.ToPath GetAggregateComplianceDetailsByConfigRule where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAggregateComplianceDetailsByConfigRule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAggregateComplianceDetailsByConfigRuleResponse' smart constructor.
data GetAggregateComplianceDetailsByConfigRuleResponse = GetAggregateComplianceDetailsByConfigRuleResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    aggregateEvaluationResults ::
      Lude.Maybe
        [AggregateEvaluationResult],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'GetAggregateComplianceDetailsByConfigRuleResponse' with the minimum fields required to make a request.
--
-- * 'aggregateEvaluationResults' - Returns an AggregateEvaluationResults object.
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'responseStatus' - The response status code.
mkGetAggregateComplianceDetailsByConfigRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAggregateComplianceDetailsByConfigRuleResponse
mkGetAggregateComplianceDetailsByConfigRuleResponse
  pResponseStatus_ =
    GetAggregateComplianceDetailsByConfigRuleResponse'
      { nextToken =
          Lude.Nothing,
        aggregateEvaluationResults = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacdbcrrsNextToken :: Lens.Lens' GetAggregateComplianceDetailsByConfigRuleResponse (Lude.Maybe Lude.Text)
gacdbcrrsNextToken = Lens.lens (nextToken :: GetAggregateComplianceDetailsByConfigRuleResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetAggregateComplianceDetailsByConfigRuleResponse)
{-# DEPRECATED gacdbcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Returns an AggregateEvaluationResults object.
--
-- /Note:/ Consider using 'aggregateEvaluationResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacdbcrrsAggregateEvaluationResults :: Lens.Lens' GetAggregateComplianceDetailsByConfigRuleResponse (Lude.Maybe [AggregateEvaluationResult])
gacdbcrrsAggregateEvaluationResults = Lens.lens (aggregateEvaluationResults :: GetAggregateComplianceDetailsByConfigRuleResponse -> Lude.Maybe [AggregateEvaluationResult]) (\s a -> s {aggregateEvaluationResults = a} :: GetAggregateComplianceDetailsByConfigRuleResponse)
{-# DEPRECATED gacdbcrrsAggregateEvaluationResults "Use generic-lens or generic-optics with 'aggregateEvaluationResults' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacdbcrrsResponseStatus :: Lens.Lens' GetAggregateComplianceDetailsByConfigRuleResponse Lude.Int
gacdbcrrsResponseStatus = Lens.lens (responseStatus :: GetAggregateComplianceDetailsByConfigRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAggregateComplianceDetailsByConfigRuleResponse)
{-# DEPRECATED gacdbcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
