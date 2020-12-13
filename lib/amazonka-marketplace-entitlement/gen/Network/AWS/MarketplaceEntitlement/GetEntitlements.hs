{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceEntitlement.GetEntitlements
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- GetEntitlements retrieves entitlement values for a given product. The results can be filtered based on customer identifier or product dimensions.
--
-- This operation returns paginated results.
module Network.AWS.MarketplaceEntitlement.GetEntitlements
  ( -- * Creating a request
    GetEntitlements (..),
    mkGetEntitlements,

    -- ** Request lenses
    geNextToken,
    geFilter,
    geProductCode,
    geMaxResults,

    -- * Destructuring the response
    GetEntitlementsResponse (..),
    mkGetEntitlementsResponse,

    -- ** Response lenses
    gersNextToken,
    gersEntitlements,
    gersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MarketplaceEntitlement.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The GetEntitlementsRequest contains parameters for the GetEntitlements operation.
--
-- /See:/ 'mkGetEntitlements' smart constructor.
data GetEntitlements = GetEntitlements'
  { -- | For paginated calls to GetEntitlements, pass the NextToken from the previous GetEntitlementsResult.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Filter is used to return entitlements for a specific customer or for a specific dimension. Filters are described as keys mapped to a lists of values. Filtered requests are /unioned/ for each value in the value list, and then /intersected/ for each filter key.
    filter :: Lude.Maybe (Lude.HashMap GetEntitlementFilterName (Lude.NonEmpty Lude.Text)),
    -- | Product code is used to uniquely identify a product in AWS Marketplace. The product code will be provided by AWS Marketplace when the product listing is created.
    productCode :: Lude.Text,
    -- | The maximum number of items to retrieve from the GetEntitlements operation. For pagination, use the NextToken field in subsequent calls to GetEntitlements.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetEntitlements' with the minimum fields required to make a request.
--
-- * 'nextToken' - For paginated calls to GetEntitlements, pass the NextToken from the previous GetEntitlementsResult.
-- * 'filter' - Filter is used to return entitlements for a specific customer or for a specific dimension. Filters are described as keys mapped to a lists of values. Filtered requests are /unioned/ for each value in the value list, and then /intersected/ for each filter key.
-- * 'productCode' - Product code is used to uniquely identify a product in AWS Marketplace. The product code will be provided by AWS Marketplace when the product listing is created.
-- * 'maxResults' - The maximum number of items to retrieve from the GetEntitlements operation. For pagination, use the NextToken field in subsequent calls to GetEntitlements.
mkGetEntitlements ::
  -- | 'productCode'
  Lude.Text ->
  GetEntitlements
mkGetEntitlements pProductCode_ =
  GetEntitlements'
    { nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      productCode = pProductCode_,
      maxResults = Lude.Nothing
    }

-- | For paginated calls to GetEntitlements, pass the NextToken from the previous GetEntitlementsResult.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geNextToken :: Lens.Lens' GetEntitlements (Lude.Maybe Lude.Text)
geNextToken = Lens.lens (nextToken :: GetEntitlements -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetEntitlements)
{-# DEPRECATED geNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filter is used to return entitlements for a specific customer or for a specific dimension. Filters are described as keys mapped to a lists of values. Filtered requests are /unioned/ for each value in the value list, and then /intersected/ for each filter key.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geFilter :: Lens.Lens' GetEntitlements (Lude.Maybe (Lude.HashMap GetEntitlementFilterName (Lude.NonEmpty Lude.Text)))
geFilter = Lens.lens (filter :: GetEntitlements -> Lude.Maybe (Lude.HashMap GetEntitlementFilterName (Lude.NonEmpty Lude.Text))) (\s a -> s {filter = a} :: GetEntitlements)
{-# DEPRECATED geFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | Product code is used to uniquely identify a product in AWS Marketplace. The product code will be provided by AWS Marketplace when the product listing is created.
--
-- /Note:/ Consider using 'productCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geProductCode :: Lens.Lens' GetEntitlements Lude.Text
geProductCode = Lens.lens (productCode :: GetEntitlements -> Lude.Text) (\s a -> s {productCode = a} :: GetEntitlements)
{-# DEPRECATED geProductCode "Use generic-lens or generic-optics with 'productCode' instead." #-}

-- | The maximum number of items to retrieve from the GetEntitlements operation. For pagination, use the NextToken field in subsequent calls to GetEntitlements.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geMaxResults :: Lens.Lens' GetEntitlements (Lude.Maybe Lude.Int)
geMaxResults = Lens.lens (maxResults :: GetEntitlements -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: GetEntitlements)
{-# DEPRECATED geMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetEntitlements where
  page rq rs
    | Page.stop (rs Lens.^. gersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gersEntitlements) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& geNextToken Lens..~ rs Lens.^. gersNextToken

instance Lude.AWSRequest GetEntitlements where
  type Rs GetEntitlements = GetEntitlementsResponse
  request = Req.postJSON marketplaceEntitlementService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetEntitlementsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Entitlements" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetEntitlements where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSMPEntitlementService.GetEntitlements" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetEntitlements where
  toJSON GetEntitlements' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Filter" Lude..=) Lude.<$> filter,
            Lude.Just ("ProductCode" Lude..= productCode),
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath GetEntitlements where
  toPath = Lude.const "/"

instance Lude.ToQuery GetEntitlements where
  toQuery = Lude.const Lude.mempty

-- | The GetEntitlementsRequest contains results from the GetEntitlements operation.
--
-- /See:/ 'mkGetEntitlementsResponse' smart constructor.
data GetEntitlementsResponse = GetEntitlementsResponse'
  { -- | For paginated results, use NextToken in subsequent calls to GetEntitlements. If the result contains an empty set of entitlements, NextToken might still be present and should be used.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The set of entitlements found through the GetEntitlements operation. If the result contains an empty set of entitlements, NextToken might still be present and should be used.
    entitlements :: Lude.Maybe [Entitlement],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetEntitlementsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - For paginated results, use NextToken in subsequent calls to GetEntitlements. If the result contains an empty set of entitlements, NextToken might still be present and should be used.
-- * 'entitlements' - The set of entitlements found through the GetEntitlements operation. If the result contains an empty set of entitlements, NextToken might still be present and should be used.
-- * 'responseStatus' - The response status code.
mkGetEntitlementsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetEntitlementsResponse
mkGetEntitlementsResponse pResponseStatus_ =
  GetEntitlementsResponse'
    { nextToken = Lude.Nothing,
      entitlements = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | For paginated results, use NextToken in subsequent calls to GetEntitlements. If the result contains an empty set of entitlements, NextToken might still be present and should be used.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersNextToken :: Lens.Lens' GetEntitlementsResponse (Lude.Maybe Lude.Text)
gersNextToken = Lens.lens (nextToken :: GetEntitlementsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetEntitlementsResponse)
{-# DEPRECATED gersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The set of entitlements found through the GetEntitlements operation. If the result contains an empty set of entitlements, NextToken might still be present and should be used.
--
-- /Note:/ Consider using 'entitlements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersEntitlements :: Lens.Lens' GetEntitlementsResponse (Lude.Maybe [Entitlement])
gersEntitlements = Lens.lens (entitlements :: GetEntitlementsResponse -> Lude.Maybe [Entitlement]) (\s a -> s {entitlements = a} :: GetEntitlementsResponse)
{-# DEPRECATED gersEntitlements "Use generic-lens or generic-optics with 'entitlements' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersResponseStatus :: Lens.Lens' GetEntitlementsResponse Lude.Int
gersResponseStatus = Lens.lens (responseStatus :: GetEntitlementsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetEntitlementsResponse)
{-# DEPRECATED gersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
