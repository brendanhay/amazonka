{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeAggregateComplianceByConfigRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of compliant and noncompliant rules with the number of resources for compliant and noncompliant rules.
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeAggregateComplianceByConfigRules
  ( -- * Creating a request
    DescribeAggregateComplianceByConfigRules (..),
    mkDescribeAggregateComplianceByConfigRules,

    -- ** Request lenses
    dacbcrFilters,
    dacbcrNextToken,
    dacbcrLimit,
    dacbcrConfigurationAggregatorName,

    -- * Destructuring the response
    DescribeAggregateComplianceByConfigRulesResponse (..),
    mkDescribeAggregateComplianceByConfigRulesResponse,

    -- ** Response lenses
    dacbcrrsNextToken,
    dacbcrrsAggregateComplianceByConfigRules,
    dacbcrrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAggregateComplianceByConfigRules' smart constructor.
data DescribeAggregateComplianceByConfigRules = DescribeAggregateComplianceByConfigRules'
  { -- | Filters the results by ConfigRuleComplianceFilters object.
    filters :: Lude.Maybe ConfigRuleComplianceFilters,
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of evaluation results returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
    limit :: Lude.Maybe Lude.Natural,
    -- | The name of the configuration aggregator.
    configurationAggregatorName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAggregateComplianceByConfigRules' with the minimum fields required to make a request.
--
-- * 'filters' - Filters the results by ConfigRuleComplianceFilters object.
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'limit' - The maximum number of evaluation results returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
-- * 'configurationAggregatorName' - The name of the configuration aggregator.
mkDescribeAggregateComplianceByConfigRules ::
  -- | 'configurationAggregatorName'
  Lude.Text ->
  DescribeAggregateComplianceByConfigRules
mkDescribeAggregateComplianceByConfigRules
  pConfigurationAggregatorName_ =
    DescribeAggregateComplianceByConfigRules'
      { filters = Lude.Nothing,
        nextToken = Lude.Nothing,
        limit = Lude.Nothing,
        configurationAggregatorName =
          pConfigurationAggregatorName_
      }

-- | Filters the results by ConfigRuleComplianceFilters object.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacbcrFilters :: Lens.Lens' DescribeAggregateComplianceByConfigRules (Lude.Maybe ConfigRuleComplianceFilters)
dacbcrFilters = Lens.lens (filters :: DescribeAggregateComplianceByConfigRules -> Lude.Maybe ConfigRuleComplianceFilters) (\s a -> s {filters = a} :: DescribeAggregateComplianceByConfigRules)
{-# DEPRECATED dacbcrFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacbcrNextToken :: Lens.Lens' DescribeAggregateComplianceByConfigRules (Lude.Maybe Lude.Text)
dacbcrNextToken = Lens.lens (nextToken :: DescribeAggregateComplianceByConfigRules -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAggregateComplianceByConfigRules)
{-# DEPRECATED dacbcrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of evaluation results returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacbcrLimit :: Lens.Lens' DescribeAggregateComplianceByConfigRules (Lude.Maybe Lude.Natural)
dacbcrLimit = Lens.lens (limit :: DescribeAggregateComplianceByConfigRules -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeAggregateComplianceByConfigRules)
{-# DEPRECATED dacbcrLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The name of the configuration aggregator.
--
-- /Note:/ Consider using 'configurationAggregatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacbcrConfigurationAggregatorName :: Lens.Lens' DescribeAggregateComplianceByConfigRules Lude.Text
dacbcrConfigurationAggregatorName = Lens.lens (configurationAggregatorName :: DescribeAggregateComplianceByConfigRules -> Lude.Text) (\s a -> s {configurationAggregatorName = a} :: DescribeAggregateComplianceByConfigRules)
{-# DEPRECATED dacbcrConfigurationAggregatorName "Use generic-lens or generic-optics with 'configurationAggregatorName' instead." #-}

instance Page.AWSPager DescribeAggregateComplianceByConfigRules where
  page rq rs
    | Page.stop (rs Lens.^. dacbcrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dacbcrrsAggregateComplianceByConfigRules) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dacbcrNextToken Lens..~ rs Lens.^. dacbcrrsNextToken

instance Lude.AWSRequest DescribeAggregateComplianceByConfigRules where
  type
    Rs DescribeAggregateComplianceByConfigRules =
      DescribeAggregateComplianceByConfigRulesResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAggregateComplianceByConfigRulesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> ( x Lude..?> "AggregateComplianceByConfigRules"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAggregateComplianceByConfigRules where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DescribeAggregateComplianceByConfigRules" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeAggregateComplianceByConfigRules where
  toJSON DescribeAggregateComplianceByConfigRules' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            Lude.Just
              ( "ConfigurationAggregatorName"
                  Lude..= configurationAggregatorName
              )
          ]
      )

instance Lude.ToPath DescribeAggregateComplianceByConfigRules where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAggregateComplianceByConfigRules where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAggregateComplianceByConfigRulesResponse' smart constructor.
data DescribeAggregateComplianceByConfigRulesResponse = DescribeAggregateComplianceByConfigRulesResponse'
  { -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Returns a list of AggregateComplianceByConfigRule object.
    aggregateComplianceByConfigRules :: Lude.Maybe [AggregateComplianceByConfigRule],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAggregateComplianceByConfigRulesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'aggregateComplianceByConfigRules' - Returns a list of AggregateComplianceByConfigRule object.
-- * 'responseStatus' - The response status code.
mkDescribeAggregateComplianceByConfigRulesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAggregateComplianceByConfigRulesResponse
mkDescribeAggregateComplianceByConfigRulesResponse pResponseStatus_ =
  DescribeAggregateComplianceByConfigRulesResponse'
    { nextToken =
        Lude.Nothing,
      aggregateComplianceByConfigRules =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacbcrrsNextToken :: Lens.Lens' DescribeAggregateComplianceByConfigRulesResponse (Lude.Maybe Lude.Text)
dacbcrrsNextToken = Lens.lens (nextToken :: DescribeAggregateComplianceByConfigRulesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAggregateComplianceByConfigRulesResponse)
{-# DEPRECATED dacbcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Returns a list of AggregateComplianceByConfigRule object.
--
-- /Note:/ Consider using 'aggregateComplianceByConfigRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacbcrrsAggregateComplianceByConfigRules :: Lens.Lens' DescribeAggregateComplianceByConfigRulesResponse (Lude.Maybe [AggregateComplianceByConfigRule])
dacbcrrsAggregateComplianceByConfigRules = Lens.lens (aggregateComplianceByConfigRules :: DescribeAggregateComplianceByConfigRulesResponse -> Lude.Maybe [AggregateComplianceByConfigRule]) (\s a -> s {aggregateComplianceByConfigRules = a} :: DescribeAggregateComplianceByConfigRulesResponse)
{-# DEPRECATED dacbcrrsAggregateComplianceByConfigRules "Use generic-lens or generic-optics with 'aggregateComplianceByConfigRules' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacbcrrsResponseStatus :: Lens.Lens' DescribeAggregateComplianceByConfigRulesResponse Lude.Int
dacbcrrsResponseStatus = Lens.lens (responseStatus :: DescribeAggregateComplianceByConfigRulesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAggregateComplianceByConfigRulesResponse)
{-# DEPRECATED dacbcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
