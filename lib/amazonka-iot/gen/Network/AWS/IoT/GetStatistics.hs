{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.GetStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the count, average, sum, minimum, maximum, sum of squares, variance, and standard deviation for the specified aggregated field. If the aggregation field is of type @String@ , only the count statistic is returned.
module Network.AWS.IoT.GetStatistics
  ( -- * Creating a request
    GetStatistics (..),
    mkGetStatistics,

    -- ** Request lenses
    gsQueryVersion,
    gsAggregationField,
    gsQueryString,
    gsIndexName,

    -- * Destructuring the response
    GetStatisticsResponse (..),
    mkGetStatisticsResponse,

    -- ** Response lenses
    gsrsStatistics,
    gsrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetStatistics' smart constructor.
data GetStatistics = GetStatistics'
  { -- | The version of the query used to search.
    queryVersion :: Lude.Maybe Lude.Text,
    -- | The aggregation field name.
    aggregationField :: Lude.Maybe Lude.Text,
    -- | The query used to search. You can specify "*" for the query string to get the count of all indexed things in your AWS account.
    queryString :: Lude.Text,
    -- | The name of the index to search. The default value is @AWS_Things@ .
    indexName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetStatistics' with the minimum fields required to make a request.
--
-- * 'queryVersion' - The version of the query used to search.
-- * 'aggregationField' - The aggregation field name.
-- * 'queryString' - The query used to search. You can specify "*" for the query string to get the count of all indexed things in your AWS account.
-- * 'indexName' - The name of the index to search. The default value is @AWS_Things@ .
mkGetStatistics ::
  -- | 'queryString'
  Lude.Text ->
  GetStatistics
mkGetStatistics pQueryString_ =
  GetStatistics'
    { queryVersion = Lude.Nothing,
      aggregationField = Lude.Nothing,
      queryString = pQueryString_,
      indexName = Lude.Nothing
    }

-- | The version of the query used to search.
--
-- /Note:/ Consider using 'queryVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsQueryVersion :: Lens.Lens' GetStatistics (Lude.Maybe Lude.Text)
gsQueryVersion = Lens.lens (queryVersion :: GetStatistics -> Lude.Maybe Lude.Text) (\s a -> s {queryVersion = a} :: GetStatistics)
{-# DEPRECATED gsQueryVersion "Use generic-lens or generic-optics with 'queryVersion' instead." #-}

-- | The aggregation field name.
--
-- /Note:/ Consider using 'aggregationField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsAggregationField :: Lens.Lens' GetStatistics (Lude.Maybe Lude.Text)
gsAggregationField = Lens.lens (aggregationField :: GetStatistics -> Lude.Maybe Lude.Text) (\s a -> s {aggregationField = a} :: GetStatistics)
{-# DEPRECATED gsAggregationField "Use generic-lens or generic-optics with 'aggregationField' instead." #-}

-- | The query used to search. You can specify "*" for the query string to get the count of all indexed things in your AWS account.
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsQueryString :: Lens.Lens' GetStatistics Lude.Text
gsQueryString = Lens.lens (queryString :: GetStatistics -> Lude.Text) (\s a -> s {queryString = a} :: GetStatistics)
{-# DEPRECATED gsQueryString "Use generic-lens or generic-optics with 'queryString' instead." #-}

-- | The name of the index to search. The default value is @AWS_Things@ .
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsIndexName :: Lens.Lens' GetStatistics (Lude.Maybe Lude.Text)
gsIndexName = Lens.lens (indexName :: GetStatistics -> Lude.Maybe Lude.Text) (\s a -> s {indexName = a} :: GetStatistics)
{-# DEPRECATED gsIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

instance Lude.AWSRequest GetStatistics where
  type Rs GetStatistics = GetStatisticsResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetStatisticsResponse'
            Lude.<$> (x Lude..?> "statistics") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetStatistics where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON GetStatistics where
  toJSON GetStatistics' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("queryVersion" Lude..=) Lude.<$> queryVersion,
            ("aggregationField" Lude..=) Lude.<$> aggregationField,
            Lude.Just ("queryString" Lude..= queryString),
            ("indexName" Lude..=) Lude.<$> indexName
          ]
      )

instance Lude.ToPath GetStatistics where
  toPath = Lude.const "/indices/statistics"

instance Lude.ToQuery GetStatistics where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetStatisticsResponse' smart constructor.
data GetStatisticsResponse = GetStatisticsResponse'
  { -- | The statistics returned by the Fleet Indexing service based on the query and aggregation field.
    statistics :: Lude.Maybe Statistics,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetStatisticsResponse' with the minimum fields required to make a request.
--
-- * 'statistics' - The statistics returned by the Fleet Indexing service based on the query and aggregation field.
-- * 'responseStatus' - The response status code.
mkGetStatisticsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetStatisticsResponse
mkGetStatisticsResponse pResponseStatus_ =
  GetStatisticsResponse'
    { statistics = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The statistics returned by the Fleet Indexing service based on the query and aggregation field.
--
-- /Note:/ Consider using 'statistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsStatistics :: Lens.Lens' GetStatisticsResponse (Lude.Maybe Statistics)
gsrsStatistics = Lens.lens (statistics :: GetStatisticsResponse -> Lude.Maybe Statistics) (\s a -> s {statistics = a} :: GetStatisticsResponse)
{-# DEPRECATED gsrsStatistics "Use generic-lens or generic-optics with 'statistics' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsResponseStatus :: Lens.Lens' GetStatisticsResponse Lude.Int
gsrsResponseStatus = Lens.lens (responseStatus :: GetStatisticsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetStatisticsResponse)
{-# DEPRECATED gsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
