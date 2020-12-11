{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pricing.DescribeServices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the metadata for one service or a list of the metadata for all services. Use this without a service code to get the service codes for all services. Use it with a service code, such as @AmazonEC2@ , to get information specific to that service, such as the attribute names available for that service. For example, some of the attribute names available for EC2 are @volumeType@ , @maxIopsVolume@ , @operation@ , @locationType@ , and @instanceCapacity10xlarge@ .
--
-- This operation returns paginated results.
module Network.AWS.Pricing.DescribeServices
  ( -- * Creating a request
    DescribeServices (..),
    mkDescribeServices,

    -- ** Request lenses
    dsFormatVersion,
    dsNextToken,
    dsServiceCode,
    dsMaxResults,

    -- * Destructuring the response
    DescribeServicesResponse (..),
    mkDescribeServicesResponse,

    -- ** Response lenses
    dsrsFormatVersion,
    dsrsNextToken,
    dsrsServices,
    dsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Pricing.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeServices' smart constructor.
data DescribeServices = DescribeServices'
  { formatVersion ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    serviceCode :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeServices' with the minimum fields required to make a request.
--
-- * 'formatVersion' - The format version that you want the response to be in.
--
-- Valid values are: @aws_v1@
-- * 'maxResults' - The maximum number of results that you want returned in the response.
-- * 'nextToken' - The pagination token that indicates the next set of results that you want to retrieve.
-- * 'serviceCode' - The code for the service whose information you want to retrieve, such as @AmazonEC2@ . You can use the @ServiceCode@ to filter the results in a @GetProducts@ call. To retrieve a list of all services, leave this blank.
mkDescribeServices ::
  DescribeServices
mkDescribeServices =
  DescribeServices'
    { formatVersion = Lude.Nothing,
      nextToken = Lude.Nothing,
      serviceCode = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The format version that you want the response to be in.
--
-- Valid values are: @aws_v1@
--
-- /Note:/ Consider using 'formatVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsFormatVersion :: Lens.Lens' DescribeServices (Lude.Maybe Lude.Text)
dsFormatVersion = Lens.lens (formatVersion :: DescribeServices -> Lude.Maybe Lude.Text) (\s a -> s {formatVersion = a} :: DescribeServices)
{-# DEPRECATED dsFormatVersion "Use generic-lens or generic-optics with 'formatVersion' instead." #-}

-- | The pagination token that indicates the next set of results that you want to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsNextToken :: Lens.Lens' DescribeServices (Lude.Maybe Lude.Text)
dsNextToken = Lens.lens (nextToken :: DescribeServices -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeServices)
{-# DEPRECATED dsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The code for the service whose information you want to retrieve, such as @AmazonEC2@ . You can use the @ServiceCode@ to filter the results in a @GetProducts@ call. To retrieve a list of all services, leave this blank.
--
-- /Note:/ Consider using 'serviceCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsServiceCode :: Lens.Lens' DescribeServices (Lude.Maybe Lude.Text)
dsServiceCode = Lens.lens (serviceCode :: DescribeServices -> Lude.Maybe Lude.Text) (\s a -> s {serviceCode = a} :: DescribeServices)
{-# DEPRECATED dsServiceCode "Use generic-lens or generic-optics with 'serviceCode' instead." #-}

-- | The maximum number of results that you want returned in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsMaxResults :: Lens.Lens' DescribeServices (Lude.Maybe Lude.Natural)
dsMaxResults = Lens.lens (maxResults :: DescribeServices -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeServices)
{-# DEPRECATED dsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeServices where
  page rq rs
    | Page.stop (rs Lens.^. dsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dsrsServices) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dsNextToken Lens..~ rs Lens.^. dsrsNextToken

instance Lude.AWSRequest DescribeServices where
  type Rs DescribeServices = DescribeServicesResponse
  request = Req.postJSON pricingService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeServicesResponse'
            Lude.<$> (x Lude..?> "FormatVersion")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Services" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeServices where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSPriceListService.DescribeServices" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeServices where
  toJSON DescribeServices' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("FormatVersion" Lude..=) Lude.<$> formatVersion,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("ServiceCode" Lude..=) Lude.<$> serviceCode,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeServices where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeServices where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeServicesResponse' smart constructor.
data DescribeServicesResponse = DescribeServicesResponse'
  { formatVersion ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    services :: Lude.Maybe [PricingService],
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

-- | Creates a value of 'DescribeServicesResponse' with the minimum fields required to make a request.
--
-- * 'formatVersion' - The format version of the response. For example, @aws_v1@ .
-- * 'nextToken' - The pagination token for the next set of retreivable results.
-- * 'responseStatus' - The response status code.
-- * 'services' - The service metadata for the service or services in the response.
mkDescribeServicesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeServicesResponse
mkDescribeServicesResponse pResponseStatus_ =
  DescribeServicesResponse'
    { formatVersion = Lude.Nothing,
      nextToken = Lude.Nothing,
      services = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The format version of the response. For example, @aws_v1@ .
--
-- /Note:/ Consider using 'formatVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsFormatVersion :: Lens.Lens' DescribeServicesResponse (Lude.Maybe Lude.Text)
dsrsFormatVersion = Lens.lens (formatVersion :: DescribeServicesResponse -> Lude.Maybe Lude.Text) (\s a -> s {formatVersion = a} :: DescribeServicesResponse)
{-# DEPRECATED dsrsFormatVersion "Use generic-lens or generic-optics with 'formatVersion' instead." #-}

-- | The pagination token for the next set of retreivable results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsNextToken :: Lens.Lens' DescribeServicesResponse (Lude.Maybe Lude.Text)
dsrsNextToken = Lens.lens (nextToken :: DescribeServicesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeServicesResponse)
{-# DEPRECATED dsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The service metadata for the service or services in the response.
--
-- /Note:/ Consider using 'services' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsServices :: Lens.Lens' DescribeServicesResponse (Lude.Maybe [PricingService])
dsrsServices = Lens.lens (services :: DescribeServicesResponse -> Lude.Maybe [PricingService]) (\s a -> s {services = a} :: DescribeServicesResponse)
{-# DEPRECATED dsrsServices "Use generic-lens or generic-optics with 'services' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsResponseStatus :: Lens.Lens' DescribeServicesResponse Lude.Int
dsrsResponseStatus = Lens.lens (responseStatus :: DescribeServicesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeServicesResponse)
{-# DEPRECATED dsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
