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
    gavServiceCode,
    gavAttributeName,
    gavMaxResults,
    gavNextToken,

    -- * Destructuring the response
    GetAttributeValuesResponse (..),
    mkGetAttributeValuesResponse,

    -- ** Response lenses
    gavrrsAttributeValues,
    gavrrsNextToken,
    gavrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Pricing.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAttributeValues' smart constructor.
data GetAttributeValues = GetAttributeValues'
  { -- | The service code for the service whose attributes you want to retrieve. For example, if you want the retrieve an EC2 attribute, use @AmazonEC2@ .
    serviceCode :: Types.String,
    -- | The name of the attribute that you want to retrieve the values for, such as @volumeType@ .
    attributeName :: Types.String,
    -- | The maximum number of results to return in response.
    maxResults :: Core.Maybe Core.Natural,
    -- | The pagination token that indicates the next set of results that you want to retrieve.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAttributeValues' value with any optional fields omitted.
mkGetAttributeValues ::
  -- | 'serviceCode'
  Types.String ->
  -- | 'attributeName'
  Types.String ->
  GetAttributeValues
mkGetAttributeValues serviceCode attributeName =
  GetAttributeValues'
    { serviceCode,
      attributeName,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The service code for the service whose attributes you want to retrieve. For example, if you want the retrieve an EC2 attribute, use @AmazonEC2@ .
--
-- /Note:/ Consider using 'serviceCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavServiceCode :: Lens.Lens' GetAttributeValues Types.String
gavServiceCode = Lens.field @"serviceCode"
{-# DEPRECATED gavServiceCode "Use generic-lens or generic-optics with 'serviceCode' instead." #-}

-- | The name of the attribute that you want to retrieve the values for, such as @volumeType@ .
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavAttributeName :: Lens.Lens' GetAttributeValues Types.String
gavAttributeName = Lens.field @"attributeName"
{-# DEPRECATED gavAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

-- | The maximum number of results to return in response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavMaxResults :: Lens.Lens' GetAttributeValues (Core.Maybe Core.Natural)
gavMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gavMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The pagination token that indicates the next set of results that you want to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavNextToken :: Lens.Lens' GetAttributeValues (Core.Maybe Types.String)
gavNextToken = Lens.field @"nextToken"
{-# DEPRECATED gavNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON GetAttributeValues where
  toJSON GetAttributeValues {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ServiceCode" Core..= serviceCode),
            Core.Just ("AttributeName" Core..= attributeName),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest GetAttributeValues where
  type Rs GetAttributeValues = GetAttributeValuesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSPriceListService.GetAttributeValues")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAttributeValuesResponse'
            Core.<$> (x Core..:? "AttributeValues")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetAttributeValues where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"attributeValues" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetAttributeValuesResponse' smart constructor.
data GetAttributeValuesResponse = GetAttributeValuesResponse'
  { -- | The list of values for an attribute. For example, @Throughput Optimized HDD@ and @Provisioned IOPS@ are two available values for the @AmazonEC2@ @volumeType@ .
    attributeValues :: Core.Maybe [Types.AttributeValue],
    -- | The pagination token that indicates the next set of results to retrieve.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAttributeValuesResponse' value with any optional fields omitted.
mkGetAttributeValuesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetAttributeValuesResponse
mkGetAttributeValuesResponse responseStatus =
  GetAttributeValuesResponse'
    { attributeValues = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The list of values for an attribute. For example, @Throughput Optimized HDD@ and @Provisioned IOPS@ are two available values for the @AmazonEC2@ @volumeType@ .
--
-- /Note:/ Consider using 'attributeValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavrrsAttributeValues :: Lens.Lens' GetAttributeValuesResponse (Core.Maybe [Types.AttributeValue])
gavrrsAttributeValues = Lens.field @"attributeValues"
{-# DEPRECATED gavrrsAttributeValues "Use generic-lens or generic-optics with 'attributeValues' instead." #-}

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavrrsNextToken :: Lens.Lens' GetAttributeValuesResponse (Core.Maybe Types.String)
gavrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gavrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavrrsResponseStatus :: Lens.Lens' GetAttributeValuesResponse Core.Int
gavrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gavrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
