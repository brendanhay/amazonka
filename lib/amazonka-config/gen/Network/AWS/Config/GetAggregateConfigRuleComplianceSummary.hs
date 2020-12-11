{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.GetAggregateConfigRuleComplianceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of compliant and noncompliant rules for one or more accounts and regions in an aggregator.
module Network.AWS.Config.GetAggregateConfigRuleComplianceSummary
  ( -- * Creating a request
    GetAggregateConfigRuleComplianceSummary (..),
    mkGetAggregateConfigRuleComplianceSummary,

    -- ** Request lenses
    gacrcsFilters,
    gacrcsNextToken,
    gacrcsLimit,
    gacrcsGroupByKey,
    gacrcsConfigurationAggregatorName,

    -- * Destructuring the response
    GetAggregateConfigRuleComplianceSummaryResponse (..),
    mkGetAggregateConfigRuleComplianceSummaryResponse,

    -- ** Response lenses
    gacrcsrsAggregateComplianceCounts,
    gacrcsrsNextToken,
    gacrcsrsGroupByKey,
    gacrcsrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAggregateConfigRuleComplianceSummary' smart constructor.
data GetAggregateConfigRuleComplianceSummary = GetAggregateConfigRuleComplianceSummary'
  { filters ::
      Lude.Maybe
        ConfigRuleComplianceSummaryFilters,
    nextToken ::
      Lude.Maybe
        Lude.Text,
    limit ::
      Lude.Maybe
        Lude.Natural,
    groupByKey ::
      Lude.Maybe
        ConfigRuleComplianceSummaryGroupKey,
    configurationAggregatorName ::
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

-- | Creates a value of 'GetAggregateConfigRuleComplianceSummary' with the minimum fields required to make a request.
--
-- * 'configurationAggregatorName' - The name of the configuration aggregator.
-- * 'filters' - Filters the results based on the ConfigRuleComplianceSummaryFilters object.
-- * 'groupByKey' - Groups the result based on ACCOUNT_ID or AWS_REGION.
-- * 'limit' - The maximum number of evaluation results returned on each page. The default is 1000. You cannot specify a number greater than 1000. If you specify 0, AWS Config uses the default.
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
mkGetAggregateConfigRuleComplianceSummary ::
  -- | 'configurationAggregatorName'
  Lude.Text ->
  GetAggregateConfigRuleComplianceSummary
mkGetAggregateConfigRuleComplianceSummary
  pConfigurationAggregatorName_ =
    GetAggregateConfigRuleComplianceSummary'
      { filters = Lude.Nothing,
        nextToken = Lude.Nothing,
        limit = Lude.Nothing,
        groupByKey = Lude.Nothing,
        configurationAggregatorName =
          pConfigurationAggregatorName_
      }

-- | Filters the results based on the ConfigRuleComplianceSummaryFilters object.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacrcsFilters :: Lens.Lens' GetAggregateConfigRuleComplianceSummary (Lude.Maybe ConfigRuleComplianceSummaryFilters)
gacrcsFilters = Lens.lens (filters :: GetAggregateConfigRuleComplianceSummary -> Lude.Maybe ConfigRuleComplianceSummaryFilters) (\s a -> s {filters = a} :: GetAggregateConfigRuleComplianceSummary)
{-# DEPRECATED gacrcsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacrcsNextToken :: Lens.Lens' GetAggregateConfigRuleComplianceSummary (Lude.Maybe Lude.Text)
gacrcsNextToken = Lens.lens (nextToken :: GetAggregateConfigRuleComplianceSummary -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetAggregateConfigRuleComplianceSummary)
{-# DEPRECATED gacrcsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of evaluation results returned on each page. The default is 1000. You cannot specify a number greater than 1000. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacrcsLimit :: Lens.Lens' GetAggregateConfigRuleComplianceSummary (Lude.Maybe Lude.Natural)
gacrcsLimit = Lens.lens (limit :: GetAggregateConfigRuleComplianceSummary -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: GetAggregateConfigRuleComplianceSummary)
{-# DEPRECATED gacrcsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | Groups the result based on ACCOUNT_ID or AWS_REGION.
--
-- /Note:/ Consider using 'groupByKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacrcsGroupByKey :: Lens.Lens' GetAggregateConfigRuleComplianceSummary (Lude.Maybe ConfigRuleComplianceSummaryGroupKey)
gacrcsGroupByKey = Lens.lens (groupByKey :: GetAggregateConfigRuleComplianceSummary -> Lude.Maybe ConfigRuleComplianceSummaryGroupKey) (\s a -> s {groupByKey = a} :: GetAggregateConfigRuleComplianceSummary)
{-# DEPRECATED gacrcsGroupByKey "Use generic-lens or generic-optics with 'groupByKey' instead." #-}

-- | The name of the configuration aggregator.
--
-- /Note:/ Consider using 'configurationAggregatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacrcsConfigurationAggregatorName :: Lens.Lens' GetAggregateConfigRuleComplianceSummary Lude.Text
gacrcsConfigurationAggregatorName = Lens.lens (configurationAggregatorName :: GetAggregateConfigRuleComplianceSummary -> Lude.Text) (\s a -> s {configurationAggregatorName = a} :: GetAggregateConfigRuleComplianceSummary)
{-# DEPRECATED gacrcsConfigurationAggregatorName "Use generic-lens or generic-optics with 'configurationAggregatorName' instead." #-}

instance Lude.AWSRequest GetAggregateConfigRuleComplianceSummary where
  type
    Rs GetAggregateConfigRuleComplianceSummary =
      GetAggregateConfigRuleComplianceSummaryResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAggregateConfigRuleComplianceSummaryResponse'
            Lude.<$> (x Lude..?> "AggregateComplianceCounts" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "GroupByKey")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAggregateConfigRuleComplianceSummary where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.GetAggregateConfigRuleComplianceSummary" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetAggregateConfigRuleComplianceSummary where
  toJSON GetAggregateConfigRuleComplianceSummary' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            ("GroupByKey" Lude..=) Lude.<$> groupByKey,
            Lude.Just
              ( "ConfigurationAggregatorName"
                  Lude..= configurationAggregatorName
              )
          ]
      )

instance Lude.ToPath GetAggregateConfigRuleComplianceSummary where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAggregateConfigRuleComplianceSummary where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAggregateConfigRuleComplianceSummaryResponse' smart constructor.
data GetAggregateConfigRuleComplianceSummaryResponse = GetAggregateConfigRuleComplianceSummaryResponse'
  { aggregateComplianceCounts ::
      Lude.Maybe
        [AggregateComplianceCount],
    nextToken ::
      Lude.Maybe
        Lude.Text,
    groupByKey ::
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
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'GetAggregateConfigRuleComplianceSummaryResponse' with the minimum fields required to make a request.
--
-- * 'aggregateComplianceCounts' - Returns a list of AggregateComplianceCounts object.
-- * 'groupByKey' - Groups the result based on ACCOUNT_ID or AWS_REGION.
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'responseStatus' - The response status code.
mkGetAggregateConfigRuleComplianceSummaryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAggregateConfigRuleComplianceSummaryResponse
mkGetAggregateConfigRuleComplianceSummaryResponse pResponseStatus_ =
  GetAggregateConfigRuleComplianceSummaryResponse'
    { aggregateComplianceCounts =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      groupByKey = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns a list of AggregateComplianceCounts object.
--
-- /Note:/ Consider using 'aggregateComplianceCounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacrcsrsAggregateComplianceCounts :: Lens.Lens' GetAggregateConfigRuleComplianceSummaryResponse (Lude.Maybe [AggregateComplianceCount])
gacrcsrsAggregateComplianceCounts = Lens.lens (aggregateComplianceCounts :: GetAggregateConfigRuleComplianceSummaryResponse -> Lude.Maybe [AggregateComplianceCount]) (\s a -> s {aggregateComplianceCounts = a} :: GetAggregateConfigRuleComplianceSummaryResponse)
{-# DEPRECATED gacrcsrsAggregateComplianceCounts "Use generic-lens or generic-optics with 'aggregateComplianceCounts' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacrcsrsNextToken :: Lens.Lens' GetAggregateConfigRuleComplianceSummaryResponse (Lude.Maybe Lude.Text)
gacrcsrsNextToken = Lens.lens (nextToken :: GetAggregateConfigRuleComplianceSummaryResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetAggregateConfigRuleComplianceSummaryResponse)
{-# DEPRECATED gacrcsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Groups the result based on ACCOUNT_ID or AWS_REGION.
--
-- /Note:/ Consider using 'groupByKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacrcsrsGroupByKey :: Lens.Lens' GetAggregateConfigRuleComplianceSummaryResponse (Lude.Maybe Lude.Text)
gacrcsrsGroupByKey = Lens.lens (groupByKey :: GetAggregateConfigRuleComplianceSummaryResponse -> Lude.Maybe Lude.Text) (\s a -> s {groupByKey = a} :: GetAggregateConfigRuleComplianceSummaryResponse)
{-# DEPRECATED gacrcsrsGroupByKey "Use generic-lens or generic-optics with 'groupByKey' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacrcsrsResponseStatus :: Lens.Lens' GetAggregateConfigRuleComplianceSummaryResponse Lude.Int
gacrcsrsResponseStatus = Lens.lens (responseStatus :: GetAggregateConfigRuleComplianceSummaryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAggregateConfigRuleComplianceSummaryResponse)
{-# DEPRECATED gacrcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
