{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.GetAggregateDiscoveredResourceCounts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the resource counts across accounts and regions that are present in your AWS Config aggregator. You can request the resource counts by providing filters and GroupByKey.
--
-- For example, if the input contains accountID 12345678910 and region us-east-1 in filters, the API returns the count of resources in account ID 12345678910 and region us-east-1. If the input contains ACCOUNT_ID as a GroupByKey, the API returns resource counts for all source accounts that are present in your aggregator.
module Network.AWS.Config.GetAggregateDiscoveredResourceCounts
  ( -- * Creating a request
    GetAggregateDiscoveredResourceCounts (..),
    mkGetAggregateDiscoveredResourceCounts,

    -- ** Request lenses
    gadrcFilters,
    gadrcNextToken,
    gadrcLimit,
    gadrcGroupByKey,
    gadrcConfigurationAggregatorName,

    -- * Destructuring the response
    GetAggregateDiscoveredResourceCountsResponse (..),
    mkGetAggregateDiscoveredResourceCountsResponse,

    -- ** Response lenses
    gadrcrsGroupedResourceCounts,
    gadrcrsTotalDiscoveredResources,
    gadrcrsNextToken,
    gadrcrsGroupByKey,
    gadrcrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAggregateDiscoveredResourceCounts' smart constructor.
data GetAggregateDiscoveredResourceCounts = GetAggregateDiscoveredResourceCounts'
  { -- | Filters the results based on the @ResourceCountFilters@ object.
    filters :: Lude.Maybe ResourceCountFilters,
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of 'GroupedResourceCount' objects returned on each page. The default is 1000. You cannot specify a number greater than 1000. If you specify 0, AWS Config uses the default.
    limit :: Lude.Maybe Lude.Natural,
    -- | The key to group the resource counts.
    groupByKey :: Lude.Maybe ResourceCountGroupKey,
    -- | The name of the configuration aggregator.
    configurationAggregatorName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAggregateDiscoveredResourceCounts' with the minimum fields required to make a request.
--
-- * 'filters' - Filters the results based on the @ResourceCountFilters@ object.
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'limit' - The maximum number of 'GroupedResourceCount' objects returned on each page. The default is 1000. You cannot specify a number greater than 1000. If you specify 0, AWS Config uses the default.
-- * 'groupByKey' - The key to group the resource counts.
-- * 'configurationAggregatorName' - The name of the configuration aggregator.
mkGetAggregateDiscoveredResourceCounts ::
  -- | 'configurationAggregatorName'
  Lude.Text ->
  GetAggregateDiscoveredResourceCounts
mkGetAggregateDiscoveredResourceCounts
  pConfigurationAggregatorName_ =
    GetAggregateDiscoveredResourceCounts'
      { filters = Lude.Nothing,
        nextToken = Lude.Nothing,
        limit = Lude.Nothing,
        groupByKey = Lude.Nothing,
        configurationAggregatorName =
          pConfigurationAggregatorName_
      }

-- | Filters the results based on the @ResourceCountFilters@ object.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcFilters :: Lens.Lens' GetAggregateDiscoveredResourceCounts (Lude.Maybe ResourceCountFilters)
gadrcFilters = Lens.lens (filters :: GetAggregateDiscoveredResourceCounts -> Lude.Maybe ResourceCountFilters) (\s a -> s {filters = a} :: GetAggregateDiscoveredResourceCounts)
{-# DEPRECATED gadrcFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcNextToken :: Lens.Lens' GetAggregateDiscoveredResourceCounts (Lude.Maybe Lude.Text)
gadrcNextToken = Lens.lens (nextToken :: GetAggregateDiscoveredResourceCounts -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetAggregateDiscoveredResourceCounts)
{-# DEPRECATED gadrcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of 'GroupedResourceCount' objects returned on each page. The default is 1000. You cannot specify a number greater than 1000. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcLimit :: Lens.Lens' GetAggregateDiscoveredResourceCounts (Lude.Maybe Lude.Natural)
gadrcLimit = Lens.lens (limit :: GetAggregateDiscoveredResourceCounts -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: GetAggregateDiscoveredResourceCounts)
{-# DEPRECATED gadrcLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The key to group the resource counts.
--
-- /Note:/ Consider using 'groupByKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcGroupByKey :: Lens.Lens' GetAggregateDiscoveredResourceCounts (Lude.Maybe ResourceCountGroupKey)
gadrcGroupByKey = Lens.lens (groupByKey :: GetAggregateDiscoveredResourceCounts -> Lude.Maybe ResourceCountGroupKey) (\s a -> s {groupByKey = a} :: GetAggregateDiscoveredResourceCounts)
{-# DEPRECATED gadrcGroupByKey "Use generic-lens or generic-optics with 'groupByKey' instead." #-}

-- | The name of the configuration aggregator.
--
-- /Note:/ Consider using 'configurationAggregatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcConfigurationAggregatorName :: Lens.Lens' GetAggregateDiscoveredResourceCounts Lude.Text
gadrcConfigurationAggregatorName = Lens.lens (configurationAggregatorName :: GetAggregateDiscoveredResourceCounts -> Lude.Text) (\s a -> s {configurationAggregatorName = a} :: GetAggregateDiscoveredResourceCounts)
{-# DEPRECATED gadrcConfigurationAggregatorName "Use generic-lens or generic-optics with 'configurationAggregatorName' instead." #-}

instance Lude.AWSRequest GetAggregateDiscoveredResourceCounts where
  type
    Rs GetAggregateDiscoveredResourceCounts =
      GetAggregateDiscoveredResourceCountsResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAggregateDiscoveredResourceCountsResponse'
            Lude.<$> (x Lude..?> "GroupedResourceCounts" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..:> "TotalDiscoveredResources")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "GroupByKey")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAggregateDiscoveredResourceCounts where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.GetAggregateDiscoveredResourceCounts" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetAggregateDiscoveredResourceCounts where
  toJSON GetAggregateDiscoveredResourceCounts' {..} =
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

instance Lude.ToPath GetAggregateDiscoveredResourceCounts where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAggregateDiscoveredResourceCounts where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAggregateDiscoveredResourceCountsResponse' smart constructor.
data GetAggregateDiscoveredResourceCountsResponse = GetAggregateDiscoveredResourceCountsResponse'
  { -- | Returns a list of GroupedResourceCount objects.
    groupedResourceCounts :: Lude.Maybe [GroupedResourceCount],
    -- | The total number of resources that are present in an aggregator with the filters that you provide.
    totalDiscoveredResources :: Lude.Integer,
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The key passed into the request object. If @GroupByKey@ is not provided, the result will be empty.
    groupByKey :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAggregateDiscoveredResourceCountsResponse' with the minimum fields required to make a request.
--
-- * 'groupedResourceCounts' - Returns a list of GroupedResourceCount objects.
-- * 'totalDiscoveredResources' - The total number of resources that are present in an aggregator with the filters that you provide.
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'groupByKey' - The key passed into the request object. If @GroupByKey@ is not provided, the result will be empty.
-- * 'responseStatus' - The response status code.
mkGetAggregateDiscoveredResourceCountsResponse ::
  -- | 'totalDiscoveredResources'
  Lude.Integer ->
  -- | 'responseStatus'
  Lude.Int ->
  GetAggregateDiscoveredResourceCountsResponse
mkGetAggregateDiscoveredResourceCountsResponse
  pTotalDiscoveredResources_
  pResponseStatus_ =
    GetAggregateDiscoveredResourceCountsResponse'
      { groupedResourceCounts =
          Lude.Nothing,
        totalDiscoveredResources =
          pTotalDiscoveredResources_,
        nextToken = Lude.Nothing,
        groupByKey = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Returns a list of GroupedResourceCount objects.
--
-- /Note:/ Consider using 'groupedResourceCounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcrsGroupedResourceCounts :: Lens.Lens' GetAggregateDiscoveredResourceCountsResponse (Lude.Maybe [GroupedResourceCount])
gadrcrsGroupedResourceCounts = Lens.lens (groupedResourceCounts :: GetAggregateDiscoveredResourceCountsResponse -> Lude.Maybe [GroupedResourceCount]) (\s a -> s {groupedResourceCounts = a} :: GetAggregateDiscoveredResourceCountsResponse)
{-# DEPRECATED gadrcrsGroupedResourceCounts "Use generic-lens or generic-optics with 'groupedResourceCounts' instead." #-}

-- | The total number of resources that are present in an aggregator with the filters that you provide.
--
-- /Note:/ Consider using 'totalDiscoveredResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcrsTotalDiscoveredResources :: Lens.Lens' GetAggregateDiscoveredResourceCountsResponse Lude.Integer
gadrcrsTotalDiscoveredResources = Lens.lens (totalDiscoveredResources :: GetAggregateDiscoveredResourceCountsResponse -> Lude.Integer) (\s a -> s {totalDiscoveredResources = a} :: GetAggregateDiscoveredResourceCountsResponse)
{-# DEPRECATED gadrcrsTotalDiscoveredResources "Use generic-lens or generic-optics with 'totalDiscoveredResources' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcrsNextToken :: Lens.Lens' GetAggregateDiscoveredResourceCountsResponse (Lude.Maybe Lude.Text)
gadrcrsNextToken = Lens.lens (nextToken :: GetAggregateDiscoveredResourceCountsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetAggregateDiscoveredResourceCountsResponse)
{-# DEPRECATED gadrcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The key passed into the request object. If @GroupByKey@ is not provided, the result will be empty.
--
-- /Note:/ Consider using 'groupByKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcrsGroupByKey :: Lens.Lens' GetAggregateDiscoveredResourceCountsResponse (Lude.Maybe Lude.Text)
gadrcrsGroupByKey = Lens.lens (groupByKey :: GetAggregateDiscoveredResourceCountsResponse -> Lude.Maybe Lude.Text) (\s a -> s {groupByKey = a} :: GetAggregateDiscoveredResourceCountsResponse)
{-# DEPRECATED gadrcrsGroupByKey "Use generic-lens or generic-optics with 'groupByKey' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcrsResponseStatus :: Lens.Lens' GetAggregateDiscoveredResourceCountsResponse Lude.Int
gadrcrsResponseStatus = Lens.lens (responseStatus :: GetAggregateDiscoveredResourceCountsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAggregateDiscoveredResourceCountsResponse)
{-# DEPRECATED gadrcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
