{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pricing.GetAttributeValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of attribute values. Attibutes are similar to the details in a Price List API offer file. For a list of available attributes, see <http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/reading-an-offer.html#pps-defs Offer File Definitions> in the <http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/billing-what-is.html AWS Billing and Cost Management User Guide> .
--
-- This operation returns paginated results.
module Network.AWS.Pricing.GetAttributeValues
  ( -- * Creating a request
    GetAttributeValues (..),
    mkGetAttributeValues,

    -- ** Request lenses
    gavNextToken,
    gavServiceCode,
    gavAttributeName,
    gavMaxResults,

    -- * Destructuring the response
    GetAttributeValuesResponse (..),
    mkGetAttributeValuesResponse,

    -- ** Response lenses
    gavrsAttributeValues,
    gavrsNextToken,
    gavrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Pricing.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAttributeValues' smart constructor.
data GetAttributeValues = GetAttributeValues'
  { -- | The pagination token that indicates the next set of results that you want to retrieve.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The service code for the service whose attributes you want to retrieve. For example, if you want the retrieve an EC2 attribute, use @AmazonEC2@ .
    serviceCode :: Lude.Text,
    -- | The name of the attribute that you want to retrieve the values for, such as @volumeType@ .
    attributeName :: Lude.Text,
    -- | The maximum number of results to return in response.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAttributeValues' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token that indicates the next set of results that you want to retrieve.
-- * 'serviceCode' - The service code for the service whose attributes you want to retrieve. For example, if you want the retrieve an EC2 attribute, use @AmazonEC2@ .
-- * 'attributeName' - The name of the attribute that you want to retrieve the values for, such as @volumeType@ .
-- * 'maxResults' - The maximum number of results to return in response.
mkGetAttributeValues ::
  -- | 'serviceCode'
  Lude.Text ->
  -- | 'attributeName'
  Lude.Text ->
  GetAttributeValues
mkGetAttributeValues pServiceCode_ pAttributeName_ =
  GetAttributeValues'
    { nextToken = Lude.Nothing,
      serviceCode = pServiceCode_,
      attributeName = pAttributeName_,
      maxResults = Lude.Nothing
    }

-- | The pagination token that indicates the next set of results that you want to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavNextToken :: Lens.Lens' GetAttributeValues (Lude.Maybe Lude.Text)
gavNextToken = Lens.lens (nextToken :: GetAttributeValues -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetAttributeValues)
{-# DEPRECATED gavNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The service code for the service whose attributes you want to retrieve. For example, if you want the retrieve an EC2 attribute, use @AmazonEC2@ .
--
-- /Note:/ Consider using 'serviceCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavServiceCode :: Lens.Lens' GetAttributeValues Lude.Text
gavServiceCode = Lens.lens (serviceCode :: GetAttributeValues -> Lude.Text) (\s a -> s {serviceCode = a} :: GetAttributeValues)
{-# DEPRECATED gavServiceCode "Use generic-lens or generic-optics with 'serviceCode' instead." #-}

-- | The name of the attribute that you want to retrieve the values for, such as @volumeType@ .
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavAttributeName :: Lens.Lens' GetAttributeValues Lude.Text
gavAttributeName = Lens.lens (attributeName :: GetAttributeValues -> Lude.Text) (\s a -> s {attributeName = a} :: GetAttributeValues)
{-# DEPRECATED gavAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

-- | The maximum number of results to return in response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavMaxResults :: Lens.Lens' GetAttributeValues (Lude.Maybe Lude.Natural)
gavMaxResults = Lens.lens (maxResults :: GetAttributeValues -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetAttributeValues)
{-# DEPRECATED gavMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetAttributeValues where
  page rq rs
    | Page.stop (rs Lens.^. gavrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gavrsAttributeValues) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gavNextToken Lens..~ rs Lens.^. gavrsNextToken

instance Lude.AWSRequest GetAttributeValues where
  type Rs GetAttributeValues = GetAttributeValuesResponse
  request = Req.postJSON pricingService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAttributeValuesResponse'
            Lude.<$> (x Lude..?> "AttributeValues" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAttributeValues where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSPriceListService.GetAttributeValues" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetAttributeValues where
  toJSON GetAttributeValues' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("ServiceCode" Lude..= serviceCode),
            Lude.Just ("AttributeName" Lude..= attributeName),
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath GetAttributeValues where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAttributeValues where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAttributeValuesResponse' smart constructor.
data GetAttributeValuesResponse = GetAttributeValuesResponse'
  { -- | The list of values for an attribute. For example, @Throughput Optimized HDD@ and @Provisioned IOPS@ are two available values for the @AmazonEC2@ @volumeType@ .
    attributeValues :: Lude.Maybe [AttributeValue],
    -- | The pagination token that indicates the next set of results to retrieve.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAttributeValuesResponse' with the minimum fields required to make a request.
--
-- * 'attributeValues' - The list of values for an attribute. For example, @Throughput Optimized HDD@ and @Provisioned IOPS@ are two available values for the @AmazonEC2@ @volumeType@ .
-- * 'nextToken' - The pagination token that indicates the next set of results to retrieve.
-- * 'responseStatus' - The response status code.
mkGetAttributeValuesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAttributeValuesResponse
mkGetAttributeValuesResponse pResponseStatus_ =
  GetAttributeValuesResponse'
    { attributeValues = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of values for an attribute. For example, @Throughput Optimized HDD@ and @Provisioned IOPS@ are two available values for the @AmazonEC2@ @volumeType@ .
--
-- /Note:/ Consider using 'attributeValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavrsAttributeValues :: Lens.Lens' GetAttributeValuesResponse (Lude.Maybe [AttributeValue])
gavrsAttributeValues = Lens.lens (attributeValues :: GetAttributeValuesResponse -> Lude.Maybe [AttributeValue]) (\s a -> s {attributeValues = a} :: GetAttributeValuesResponse)
{-# DEPRECATED gavrsAttributeValues "Use generic-lens or generic-optics with 'attributeValues' instead." #-}

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavrsNextToken :: Lens.Lens' GetAttributeValuesResponse (Lude.Maybe Lude.Text)
gavrsNextToken = Lens.lens (nextToken :: GetAttributeValuesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetAttributeValuesResponse)
{-# DEPRECATED gavrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavrsResponseStatus :: Lens.Lens' GetAttributeValuesResponse Lude.Int
gavrsResponseStatus = Lens.lens (responseStatus :: GetAttributeValuesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAttributeValuesResponse)
{-# DEPRECATED gavrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
