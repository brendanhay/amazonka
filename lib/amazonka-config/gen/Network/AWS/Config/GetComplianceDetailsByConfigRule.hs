{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    gcdbcrComplianceTypes,
    gcdbcrNextToken,
    gcdbcrLimit,
    gcdbcrConfigRuleName,

    -- * Destructuring the response
    GetComplianceDetailsByConfigRuleResponse (..),
    mkGetComplianceDetailsByConfigRuleResponse,

    -- ** Response lenses
    gcdbcrrsEvaluationResults,
    gcdbcrrsNextToken,
    gcdbcrrsResponseStatus,
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
-- /See:/ 'mkGetComplianceDetailsByConfigRule' smart constructor.
data GetComplianceDetailsByConfigRule = GetComplianceDetailsByConfigRule'
  { complianceTypes ::
      Lude.Maybe
        [ComplianceType],
    nextToken ::
      Lude.Maybe Lude.Text,
    limit ::
      Lude.Maybe Lude.Natural,
    configRuleName ::
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

-- | Creates a value of 'GetComplianceDetailsByConfigRule' with the minimum fields required to make a request.
--
-- * 'complianceTypes' - Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ .
-- * 'configRuleName' - The name of the AWS Config rule for which you want compliance information.
-- * 'limit' - The maximum number of evaluation results returned on each page. The default is 10. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
mkGetComplianceDetailsByConfigRule ::
  -- | 'configRuleName'
  Lude.Text ->
  GetComplianceDetailsByConfigRule
mkGetComplianceDetailsByConfigRule pConfigRuleName_ =
  GetComplianceDetailsByConfigRule'
    { complianceTypes = Lude.Nothing,
      nextToken = Lude.Nothing,
      limit = Lude.Nothing,
      configRuleName = pConfigRuleName_
    }

-- | Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ .
--
-- /Note:/ Consider using 'complianceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbcrComplianceTypes :: Lens.Lens' GetComplianceDetailsByConfigRule (Lude.Maybe [ComplianceType])
gcdbcrComplianceTypes = Lens.lens (complianceTypes :: GetComplianceDetailsByConfigRule -> Lude.Maybe [ComplianceType]) (\s a -> s {complianceTypes = a} :: GetComplianceDetailsByConfigRule)
{-# DEPRECATED gcdbcrComplianceTypes "Use generic-lens or generic-optics with 'complianceTypes' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbcrNextToken :: Lens.Lens' GetComplianceDetailsByConfigRule (Lude.Maybe Lude.Text)
gcdbcrNextToken = Lens.lens (nextToken :: GetComplianceDetailsByConfigRule -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetComplianceDetailsByConfigRule)
{-# DEPRECATED gcdbcrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of evaluation results returned on each page. The default is 10. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbcrLimit :: Lens.Lens' GetComplianceDetailsByConfigRule (Lude.Maybe Lude.Natural)
gcdbcrLimit = Lens.lens (limit :: GetComplianceDetailsByConfigRule -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: GetComplianceDetailsByConfigRule)
{-# DEPRECATED gcdbcrLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The name of the AWS Config rule for which you want compliance information.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbcrConfigRuleName :: Lens.Lens' GetComplianceDetailsByConfigRule Lude.Text
gcdbcrConfigRuleName = Lens.lens (configRuleName :: GetComplianceDetailsByConfigRule -> Lude.Text) (\s a -> s {configRuleName = a} :: GetComplianceDetailsByConfigRule)
{-# DEPRECATED gcdbcrConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

instance Page.AWSPager GetComplianceDetailsByConfigRule where
  page rq rs
    | Page.stop (rs Lens.^. gcdbcrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gcdbcrrsEvaluationResults) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gcdbcrNextToken Lens..~ rs Lens.^. gcdbcrrsNextToken

instance Lude.AWSRequest GetComplianceDetailsByConfigRule where
  type
    Rs GetComplianceDetailsByConfigRule =
      GetComplianceDetailsByConfigRuleResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetComplianceDetailsByConfigRuleResponse'
            Lude.<$> (x Lude..?> "EvaluationResults" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetComplianceDetailsByConfigRule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.GetComplianceDetailsByConfigRule" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetComplianceDetailsByConfigRule where
  toJSON GetComplianceDetailsByConfigRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ComplianceTypes" Lude..=) Lude.<$> complianceTypes,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            Lude.Just ("ConfigRuleName" Lude..= configRuleName)
          ]
      )

instance Lude.ToPath GetComplianceDetailsByConfigRule where
  toPath = Lude.const "/"

instance Lude.ToQuery GetComplianceDetailsByConfigRule where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkGetComplianceDetailsByConfigRuleResponse' smart constructor.
data GetComplianceDetailsByConfigRuleResponse = GetComplianceDetailsByConfigRuleResponse'
  { evaluationResults ::
      Lude.Maybe
        [EvaluationResult],
    nextToken ::
      Lude.Maybe
        Lude.Text,
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
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetComplianceDetailsByConfigRuleResponse' with the minimum fields required to make a request.
--
-- * 'evaluationResults' - Indicates whether the AWS resource complies with the specified AWS Config rule.
-- * 'nextToken' - The string that you use in a subsequent request to get the next page of results in a paginated response.
-- * 'responseStatus' - The response status code.
mkGetComplianceDetailsByConfigRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetComplianceDetailsByConfigRuleResponse
mkGetComplianceDetailsByConfigRuleResponse pResponseStatus_ =
  GetComplianceDetailsByConfigRuleResponse'
    { evaluationResults =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Indicates whether the AWS resource complies with the specified AWS Config rule.
--
-- /Note:/ Consider using 'evaluationResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbcrrsEvaluationResults :: Lens.Lens' GetComplianceDetailsByConfigRuleResponse (Lude.Maybe [EvaluationResult])
gcdbcrrsEvaluationResults = Lens.lens (evaluationResults :: GetComplianceDetailsByConfigRuleResponse -> Lude.Maybe [EvaluationResult]) (\s a -> s {evaluationResults = a} :: GetComplianceDetailsByConfigRuleResponse)
{-# DEPRECATED gcdbcrrsEvaluationResults "Use generic-lens or generic-optics with 'evaluationResults' instead." #-}

-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbcrrsNextToken :: Lens.Lens' GetComplianceDetailsByConfigRuleResponse (Lude.Maybe Lude.Text)
gcdbcrrsNextToken = Lens.lens (nextToken :: GetComplianceDetailsByConfigRuleResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetComplianceDetailsByConfigRuleResponse)
{-# DEPRECATED gcdbcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbcrrsResponseStatus :: Lens.Lens' GetComplianceDetailsByConfigRuleResponse Lude.Int
gcdbcrrsResponseStatus = Lens.lens (responseStatus :: GetComplianceDetailsByConfigRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetComplianceDetailsByConfigRuleResponse)
{-# DEPRECATED gcdbcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
