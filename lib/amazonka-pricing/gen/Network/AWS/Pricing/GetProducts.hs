{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pricing.GetProducts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all products that match the filter criteria.
--
-- This operation returns paginated results.
module Network.AWS.Pricing.GetProducts
  ( -- * Creating a request
    GetProducts (..),
    mkGetProducts,

    -- ** Request lenses
    gpFilters,
    gpFormatVersion,
    gpNextToken,
    gpServiceCode,
    gpMaxResults,

    -- * Destructuring the response
    GetProductsResponse (..),
    mkGetProductsResponse,

    -- ** Response lenses
    gprsFormatVersion,
    gprsNextToken,
    gprsPriceList,
    gprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Pricing.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetProducts' smart constructor.
data GetProducts = GetProducts'
  { -- | The list of filters that limit the returned products. only products that match all filters are returned.
    filters :: Lude.Maybe [Filter],
    -- | The format version that you want the response to be in.
    --
    -- Valid values are: @aws_v1@
    formatVersion :: Lude.Maybe Lude.Text,
    -- | The pagination token that indicates the next set of results that you want to retrieve.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The code for the service whose products you want to retrieve.
    serviceCode :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return in the response.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetProducts' with the minimum fields required to make a request.
--
-- * 'filters' - The list of filters that limit the returned products. only products that match all filters are returned.
-- * 'formatVersion' - The format version that you want the response to be in.
--
-- Valid values are: @aws_v1@
-- * 'nextToken' - The pagination token that indicates the next set of results that you want to retrieve.
-- * 'serviceCode' - The code for the service whose products you want to retrieve.
-- * 'maxResults' - The maximum number of results to return in the response.
mkGetProducts ::
  GetProducts
mkGetProducts =
  GetProducts'
    { filters = Lude.Nothing,
      formatVersion = Lude.Nothing,
      nextToken = Lude.Nothing,
      serviceCode = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The list of filters that limit the returned products. only products that match all filters are returned.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpFilters :: Lens.Lens' GetProducts (Lude.Maybe [Filter])
gpFilters = Lens.lens (filters :: GetProducts -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: GetProducts)
{-# DEPRECATED gpFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The format version that you want the response to be in.
--
-- Valid values are: @aws_v1@
--
-- /Note:/ Consider using 'formatVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpFormatVersion :: Lens.Lens' GetProducts (Lude.Maybe Lude.Text)
gpFormatVersion = Lens.lens (formatVersion :: GetProducts -> Lude.Maybe Lude.Text) (\s a -> s {formatVersion = a} :: GetProducts)
{-# DEPRECATED gpFormatVersion "Use generic-lens or generic-optics with 'formatVersion' instead." #-}

-- | The pagination token that indicates the next set of results that you want to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpNextToken :: Lens.Lens' GetProducts (Lude.Maybe Lude.Text)
gpNextToken = Lens.lens (nextToken :: GetProducts -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetProducts)
{-# DEPRECATED gpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The code for the service whose products you want to retrieve.
--
-- /Note:/ Consider using 'serviceCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpServiceCode :: Lens.Lens' GetProducts (Lude.Maybe Lude.Text)
gpServiceCode = Lens.lens (serviceCode :: GetProducts -> Lude.Maybe Lude.Text) (\s a -> s {serviceCode = a} :: GetProducts)
{-# DEPRECATED gpServiceCode "Use generic-lens or generic-optics with 'serviceCode' instead." #-}

-- | The maximum number of results to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpMaxResults :: Lens.Lens' GetProducts (Lude.Maybe Lude.Natural)
gpMaxResults = Lens.lens (maxResults :: GetProducts -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetProducts)
{-# DEPRECATED gpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetProducts where
  page rq rs
    | Page.stop (rs Lens.^. gprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gprsPriceList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gpNextToken Lens..~ rs Lens.^. gprsNextToken

instance Lude.AWSRequest GetProducts where
  type Rs GetProducts = GetProductsResponse
  request = Req.postJSON pricingService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetProductsResponse'
            Lude.<$> (x Lude..?> "FormatVersion")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "PriceList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetProducts where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSPriceListService.GetProducts" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetProducts where
  toJSON GetProducts' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("FormatVersion" Lude..=) Lude.<$> formatVersion,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("ServiceCode" Lude..=) Lude.<$> serviceCode,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath GetProducts where
  toPath = Lude.const "/"

instance Lude.ToQuery GetProducts where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetProductsResponse' smart constructor.
data GetProductsResponse = GetProductsResponse'
  { -- | The format version of the response. For example, aws_v1.
    formatVersion :: Lude.Maybe Lude.Text,
    -- | The pagination token that indicates the next set of results to retrieve.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The list of products that match your filters. The list contains both the product metadata and the price information.
    priceList :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetProductsResponse' with the minimum fields required to make a request.
--
-- * 'formatVersion' - The format version of the response. For example, aws_v1.
-- * 'nextToken' - The pagination token that indicates the next set of results to retrieve.
-- * 'priceList' - The list of products that match your filters. The list contains both the product metadata and the price information.
-- * 'responseStatus' - The response status code.
mkGetProductsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetProductsResponse
mkGetProductsResponse pResponseStatus_ =
  GetProductsResponse'
    { formatVersion = Lude.Nothing,
      nextToken = Lude.Nothing,
      priceList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The format version of the response. For example, aws_v1.
--
-- /Note:/ Consider using 'formatVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsFormatVersion :: Lens.Lens' GetProductsResponse (Lude.Maybe Lude.Text)
gprsFormatVersion = Lens.lens (formatVersion :: GetProductsResponse -> Lude.Maybe Lude.Text) (\s a -> s {formatVersion = a} :: GetProductsResponse)
{-# DEPRECATED gprsFormatVersion "Use generic-lens or generic-optics with 'formatVersion' instead." #-}

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsNextToken :: Lens.Lens' GetProductsResponse (Lude.Maybe Lude.Text)
gprsNextToken = Lens.lens (nextToken :: GetProductsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetProductsResponse)
{-# DEPRECATED gprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of products that match your filters. The list contains both the product metadata and the price information.
--
-- /Note:/ Consider using 'priceList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsPriceList :: Lens.Lens' GetProductsResponse (Lude.Maybe [Lude.Text])
gprsPriceList = Lens.lens (priceList :: GetProductsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {priceList = a} :: GetProductsResponse)
{-# DEPRECATED gprsPriceList "Use generic-lens or generic-optics with 'priceList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsResponseStatus :: Lens.Lens' GetProductsResponse Lude.Int
gprsResponseStatus = Lens.lens (responseStatus :: GetProductsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetProductsResponse)
{-# DEPRECATED gprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
