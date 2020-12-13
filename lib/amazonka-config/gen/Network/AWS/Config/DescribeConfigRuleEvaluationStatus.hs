{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeConfigRuleEvaluationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns status information for each of your AWS managed Config rules. The status includes information such as the last time AWS Config invoked the rule, the last time AWS Config failed to invoke the rule, and the related error for the last failure.
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeConfigRuleEvaluationStatus
  ( -- * Creating a request
    DescribeConfigRuleEvaluationStatus (..),
    mkDescribeConfigRuleEvaluationStatus,

    -- ** Request lenses
    dcresConfigRuleNames,
    dcresNextToken,
    dcresLimit,

    -- * Destructuring the response
    DescribeConfigRuleEvaluationStatusResponse (..),
    mkDescribeConfigRuleEvaluationStatusResponse,

    -- ** Response lenses
    dcresrsConfigRulesEvaluationStatus,
    dcresrsNextToken,
    dcresrsResponseStatus,
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
-- /See:/ 'mkDescribeConfigRuleEvaluationStatus' smart constructor.
data DescribeConfigRuleEvaluationStatus = DescribeConfigRuleEvaluationStatus'
  { -- | The name of the AWS managed Config rules for which you want status information. If you do not specify any names, AWS Config returns status information for all AWS managed Config rules that you use.
    configRuleNames :: Lude.Maybe [Lude.Text],
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The number of rule evaluation results that you want returned.
    --
    -- This parameter is required if the rule limit for your account is more than the default of 150 rules.
    -- For information about requesting a rule limit increase, see <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_config AWS Config Limits> in the /AWS General Reference Guide/ .
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConfigRuleEvaluationStatus' with the minimum fields required to make a request.
--
-- * 'configRuleNames' - The name of the AWS managed Config rules for which you want status information. If you do not specify any names, AWS Config returns status information for all AWS managed Config rules that you use.
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'limit' - The number of rule evaluation results that you want returned.
--
-- This parameter is required if the rule limit for your account is more than the default of 150 rules.
-- For information about requesting a rule limit increase, see <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_config AWS Config Limits> in the /AWS General Reference Guide/ .
mkDescribeConfigRuleEvaluationStatus ::
  DescribeConfigRuleEvaluationStatus
mkDescribeConfigRuleEvaluationStatus =
  DescribeConfigRuleEvaluationStatus'
    { configRuleNames =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The name of the AWS managed Config rules for which you want status information. If you do not specify any names, AWS Config returns status information for all AWS managed Config rules that you use.
--
-- /Note:/ Consider using 'configRuleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcresConfigRuleNames :: Lens.Lens' DescribeConfigRuleEvaluationStatus (Lude.Maybe [Lude.Text])
dcresConfigRuleNames = Lens.lens (configRuleNames :: DescribeConfigRuleEvaluationStatus -> Lude.Maybe [Lude.Text]) (\s a -> s {configRuleNames = a} :: DescribeConfigRuleEvaluationStatus)
{-# DEPRECATED dcresConfigRuleNames "Use generic-lens or generic-optics with 'configRuleNames' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcresNextToken :: Lens.Lens' DescribeConfigRuleEvaluationStatus (Lude.Maybe Lude.Text)
dcresNextToken = Lens.lens (nextToken :: DescribeConfigRuleEvaluationStatus -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeConfigRuleEvaluationStatus)
{-# DEPRECATED dcresNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The number of rule evaluation results that you want returned.
--
-- This parameter is required if the rule limit for your account is more than the default of 150 rules.
-- For information about requesting a rule limit increase, see <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_config AWS Config Limits> in the /AWS General Reference Guide/ .
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcresLimit :: Lens.Lens' DescribeConfigRuleEvaluationStatus (Lude.Maybe Lude.Natural)
dcresLimit = Lens.lens (limit :: DescribeConfigRuleEvaluationStatus -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeConfigRuleEvaluationStatus)
{-# DEPRECATED dcresLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager DescribeConfigRuleEvaluationStatus where
  page rq rs
    | Page.stop (rs Lens.^. dcresrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dcresrsConfigRulesEvaluationStatus) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dcresNextToken Lens..~ rs Lens.^. dcresrsNextToken

instance Lude.AWSRequest DescribeConfigRuleEvaluationStatus where
  type
    Rs DescribeConfigRuleEvaluationStatus =
      DescribeConfigRuleEvaluationStatusResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeConfigRuleEvaluationStatusResponse'
            Lude.<$> (x Lude..?> "ConfigRulesEvaluationStatus" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeConfigRuleEvaluationStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DescribeConfigRuleEvaluationStatus" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeConfigRuleEvaluationStatus where
  toJSON DescribeConfigRuleEvaluationStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ConfigRuleNames" Lude..=) Lude.<$> configRuleNames,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath DescribeConfigRuleEvaluationStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeConfigRuleEvaluationStatus where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDescribeConfigRuleEvaluationStatusResponse' smart constructor.
data DescribeConfigRuleEvaluationStatusResponse = DescribeConfigRuleEvaluationStatusResponse'
  { -- | Status information about your AWS managed Config rules.
    configRulesEvaluationStatus :: Lude.Maybe [ConfigRuleEvaluationStatus],
    -- | The string that you use in a subsequent request to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConfigRuleEvaluationStatusResponse' with the minimum fields required to make a request.
--
-- * 'configRulesEvaluationStatus' - Status information about your AWS managed Config rules.
-- * 'nextToken' - The string that you use in a subsequent request to get the next page of results in a paginated response.
-- * 'responseStatus' - The response status code.
mkDescribeConfigRuleEvaluationStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeConfigRuleEvaluationStatusResponse
mkDescribeConfigRuleEvaluationStatusResponse pResponseStatus_ =
  DescribeConfigRuleEvaluationStatusResponse'
    { configRulesEvaluationStatus =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Status information about your AWS managed Config rules.
--
-- /Note:/ Consider using 'configRulesEvaluationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcresrsConfigRulesEvaluationStatus :: Lens.Lens' DescribeConfigRuleEvaluationStatusResponse (Lude.Maybe [ConfigRuleEvaluationStatus])
dcresrsConfigRulesEvaluationStatus = Lens.lens (configRulesEvaluationStatus :: DescribeConfigRuleEvaluationStatusResponse -> Lude.Maybe [ConfigRuleEvaluationStatus]) (\s a -> s {configRulesEvaluationStatus = a} :: DescribeConfigRuleEvaluationStatusResponse)
{-# DEPRECATED dcresrsConfigRulesEvaluationStatus "Use generic-lens or generic-optics with 'configRulesEvaluationStatus' instead." #-}

-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcresrsNextToken :: Lens.Lens' DescribeConfigRuleEvaluationStatusResponse (Lude.Maybe Lude.Text)
dcresrsNextToken = Lens.lens (nextToken :: DescribeConfigRuleEvaluationStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeConfigRuleEvaluationStatusResponse)
{-# DEPRECATED dcresrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcresrsResponseStatus :: Lens.Lens' DescribeConfigRuleEvaluationStatusResponse Lude.Int
dcresrsResponseStatus = Lens.lens (responseStatus :: DescribeConfigRuleEvaluationStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeConfigRuleEvaluationStatusResponse)
{-# DEPRECATED dcresrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
