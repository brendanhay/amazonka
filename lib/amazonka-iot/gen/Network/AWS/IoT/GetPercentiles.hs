{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.GetPercentiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Groups the aggregated values that match the query into percentile groupings. The default percentile groupings are: 1,5,25,50,75,95,99, although you can specify your own when you call @GetPercentiles@ . This function returns a value for each percentile group specified (or the default percentile groupings). The percentile group "1" contains the aggregated field value that occurs in approximately one percent of the values that match the query. The percentile group "5" contains the aggregated field value that occurs in approximately five percent of the values that match the query, and so on. The result is an approximation, the more values that match the query, the more accurate the percentile values.
module Network.AWS.IoT.GetPercentiles
  ( -- * Creating a request
    GetPercentiles (..),
    mkGetPercentiles,

    -- ** Request lenses
    gpPercents,
    gpQueryVersion,
    gpAggregationField,
    gpIndexName,
    gpQueryString,

    -- * Destructuring the response
    GetPercentilesResponse (..),
    mkGetPercentilesResponse,

    -- ** Response lenses
    grsPercentiles,
    grsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetPercentiles' smart constructor.
data GetPercentiles = GetPercentiles'
  { percents ::
      Lude.Maybe [Lude.Double],
    queryVersion :: Lude.Maybe Lude.Text,
    aggregationField :: Lude.Maybe Lude.Text,
    indexName :: Lude.Maybe Lude.Text,
    queryString :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPercentiles' with the minimum fields required to make a request.
--
-- * 'aggregationField' - The field to aggregate.
-- * 'indexName' - The name of the index to search.
-- * 'percents' - The percentile groups returned.
-- * 'queryString' - The query string.
-- * 'queryVersion' - The query version.
mkGetPercentiles ::
  -- | 'queryString'
  Lude.Text ->
  GetPercentiles
mkGetPercentiles pQueryString_ =
  GetPercentiles'
    { percents = Lude.Nothing,
      queryVersion = Lude.Nothing,
      aggregationField = Lude.Nothing,
      indexName = Lude.Nothing,
      queryString = pQueryString_
    }

-- | The percentile groups returned.
--
-- /Note:/ Consider using 'percents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpPercents :: Lens.Lens' GetPercentiles (Lude.Maybe [Lude.Double])
gpPercents = Lens.lens (percents :: GetPercentiles -> Lude.Maybe [Lude.Double]) (\s a -> s {percents = a} :: GetPercentiles)
{-# DEPRECATED gpPercents "Use generic-lens or generic-optics with 'percents' instead." #-}

-- | The query version.
--
-- /Note:/ Consider using 'queryVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpQueryVersion :: Lens.Lens' GetPercentiles (Lude.Maybe Lude.Text)
gpQueryVersion = Lens.lens (queryVersion :: GetPercentiles -> Lude.Maybe Lude.Text) (\s a -> s {queryVersion = a} :: GetPercentiles)
{-# DEPRECATED gpQueryVersion "Use generic-lens or generic-optics with 'queryVersion' instead." #-}

-- | The field to aggregate.
--
-- /Note:/ Consider using 'aggregationField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpAggregationField :: Lens.Lens' GetPercentiles (Lude.Maybe Lude.Text)
gpAggregationField = Lens.lens (aggregationField :: GetPercentiles -> Lude.Maybe Lude.Text) (\s a -> s {aggregationField = a} :: GetPercentiles)
{-# DEPRECATED gpAggregationField "Use generic-lens or generic-optics with 'aggregationField' instead." #-}

-- | The name of the index to search.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpIndexName :: Lens.Lens' GetPercentiles (Lude.Maybe Lude.Text)
gpIndexName = Lens.lens (indexName :: GetPercentiles -> Lude.Maybe Lude.Text) (\s a -> s {indexName = a} :: GetPercentiles)
{-# DEPRECATED gpIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | The query string.
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpQueryString :: Lens.Lens' GetPercentiles Lude.Text
gpQueryString = Lens.lens (queryString :: GetPercentiles -> Lude.Text) (\s a -> s {queryString = a} :: GetPercentiles)
{-# DEPRECATED gpQueryString "Use generic-lens or generic-optics with 'queryString' instead." #-}

instance Lude.AWSRequest GetPercentiles where
  type Rs GetPercentiles = GetPercentilesResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetPercentilesResponse'
            Lude.<$> (x Lude..?> "percentiles" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetPercentiles where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON GetPercentiles where
  toJSON GetPercentiles' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("percents" Lude..=) Lude.<$> percents,
            ("queryVersion" Lude..=) Lude.<$> queryVersion,
            ("aggregationField" Lude..=) Lude.<$> aggregationField,
            ("indexName" Lude..=) Lude.<$> indexName,
            Lude.Just ("queryString" Lude..= queryString)
          ]
      )

instance Lude.ToPath GetPercentiles where
  toPath = Lude.const "/indices/percentiles"

instance Lude.ToQuery GetPercentiles where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetPercentilesResponse' smart constructor.
data GetPercentilesResponse = GetPercentilesResponse'
  { percentiles ::
      Lude.Maybe [PercentPair],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPercentilesResponse' with the minimum fields required to make a request.
--
-- * 'percentiles' - The percentile values of the aggregated fields.
-- * 'responseStatus' - The response status code.
mkGetPercentilesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetPercentilesResponse
mkGetPercentilesResponse pResponseStatus_ =
  GetPercentilesResponse'
    { percentiles = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The percentile values of the aggregated fields.
--
-- /Note:/ Consider using 'percentiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsPercentiles :: Lens.Lens' GetPercentilesResponse (Lude.Maybe [PercentPair])
grsPercentiles = Lens.lens (percentiles :: GetPercentilesResponse -> Lude.Maybe [PercentPair]) (\s a -> s {percentiles = a} :: GetPercentilesResponse)
{-# DEPRECATED grsPercentiles "Use generic-lens or generic-optics with 'percentiles' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetPercentilesResponse Lude.Int
grsResponseStatus = Lens.lens (responseStatus :: GetPercentilesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPercentilesResponse)
{-# DEPRECATED grsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
