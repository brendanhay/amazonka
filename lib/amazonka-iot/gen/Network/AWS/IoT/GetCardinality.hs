{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.GetCardinality
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the approximate count of unique values that match the query.
module Network.AWS.IoT.GetCardinality
  ( -- * Creating a request
    GetCardinality (..),
    mkGetCardinality,

    -- ** Request lenses
    gcQueryVersion,
    gcAggregationField,
    gcQueryString,
    gcIndexName,

    -- * Destructuring the response
    GetCardinalityResponse (..),
    mkGetCardinalityResponse,

    -- ** Response lenses
    gcrsCardinality,
    gcrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCardinality' smart constructor.
data GetCardinality = GetCardinality'
  { -- | The query version.
    queryVersion :: Lude.Maybe Lude.Text,
    -- | The field to aggregate.
    aggregationField :: Lude.Maybe Lude.Text,
    -- | The search query.
    queryString :: Lude.Text,
    -- | The name of the index to search.
    indexName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCardinality' with the minimum fields required to make a request.
--
-- * 'queryVersion' - The query version.
-- * 'aggregationField' - The field to aggregate.
-- * 'queryString' - The search query.
-- * 'indexName' - The name of the index to search.
mkGetCardinality ::
  -- | 'queryString'
  Lude.Text ->
  GetCardinality
mkGetCardinality pQueryString_ =
  GetCardinality'
    { queryVersion = Lude.Nothing,
      aggregationField = Lude.Nothing,
      queryString = pQueryString_,
      indexName = Lude.Nothing
    }

-- | The query version.
--
-- /Note:/ Consider using 'queryVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcQueryVersion :: Lens.Lens' GetCardinality (Lude.Maybe Lude.Text)
gcQueryVersion = Lens.lens (queryVersion :: GetCardinality -> Lude.Maybe Lude.Text) (\s a -> s {queryVersion = a} :: GetCardinality)
{-# DEPRECATED gcQueryVersion "Use generic-lens or generic-optics with 'queryVersion' instead." #-}

-- | The field to aggregate.
--
-- /Note:/ Consider using 'aggregationField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcAggregationField :: Lens.Lens' GetCardinality (Lude.Maybe Lude.Text)
gcAggregationField = Lens.lens (aggregationField :: GetCardinality -> Lude.Maybe Lude.Text) (\s a -> s {aggregationField = a} :: GetCardinality)
{-# DEPRECATED gcAggregationField "Use generic-lens or generic-optics with 'aggregationField' instead." #-}

-- | The search query.
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcQueryString :: Lens.Lens' GetCardinality Lude.Text
gcQueryString = Lens.lens (queryString :: GetCardinality -> Lude.Text) (\s a -> s {queryString = a} :: GetCardinality)
{-# DEPRECATED gcQueryString "Use generic-lens or generic-optics with 'queryString' instead." #-}

-- | The name of the index to search.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcIndexName :: Lens.Lens' GetCardinality (Lude.Maybe Lude.Text)
gcIndexName = Lens.lens (indexName :: GetCardinality -> Lude.Maybe Lude.Text) (\s a -> s {indexName = a} :: GetCardinality)
{-# DEPRECATED gcIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

instance Lude.AWSRequest GetCardinality where
  type Rs GetCardinality = GetCardinalityResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCardinalityResponse'
            Lude.<$> (x Lude..?> "cardinality") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCardinality where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON GetCardinality where
  toJSON GetCardinality' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("queryVersion" Lude..=) Lude.<$> queryVersion,
            ("aggregationField" Lude..=) Lude.<$> aggregationField,
            Lude.Just ("queryString" Lude..= queryString),
            ("indexName" Lude..=) Lude.<$> indexName
          ]
      )

instance Lude.ToPath GetCardinality where
  toPath = Lude.const "/indices/cardinality"

instance Lude.ToQuery GetCardinality where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCardinalityResponse' smart constructor.
data GetCardinalityResponse = GetCardinalityResponse'
  { -- | The approximate count of unique values that match the query.
    cardinality :: Lude.Maybe Lude.Int,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCardinalityResponse' with the minimum fields required to make a request.
--
-- * 'cardinality' - The approximate count of unique values that match the query.
-- * 'responseStatus' - The response status code.
mkGetCardinalityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCardinalityResponse
mkGetCardinalityResponse pResponseStatus_ =
  GetCardinalityResponse'
    { cardinality = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The approximate count of unique values that match the query.
--
-- /Note:/ Consider using 'cardinality' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsCardinality :: Lens.Lens' GetCardinalityResponse (Lude.Maybe Lude.Int)
gcrsCardinality = Lens.lens (cardinality :: GetCardinalityResponse -> Lude.Maybe Lude.Int) (\s a -> s {cardinality = a} :: GetCardinalityResponse)
{-# DEPRECATED gcrsCardinality "Use generic-lens or generic-optics with 'cardinality' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsResponseStatus :: Lens.Lens' GetCardinalityResponse Lude.Int
gcrsResponseStatus = Lens.lens (responseStatus :: GetCardinalityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCardinalityResponse)
{-# DEPRECATED gcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
