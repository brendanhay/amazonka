{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListStackInstancesForProvisionedProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about stack instances that are associated with the specified @CFN_STACKSET@ type provisioned product. You can filter for stack instances that are associated with a specific AWS account name or region.
module Network.AWS.ServiceCatalog.ListStackInstancesForProvisionedProduct
  ( -- * Creating a request
    ListStackInstancesForProvisionedProduct (..),
    mkListStackInstancesForProvisionedProduct,

    -- ** Request lenses
    lsifppAcceptLanguage,
    lsifppPageToken,
    lsifppPageSize,
    lsifppProvisionedProductId,

    -- * Destructuring the response
    ListStackInstancesForProvisionedProductResponse (..),
    mkListStackInstancesForProvisionedProductResponse,

    -- ** Response lenses
    lsifpprsNextPageToken,
    lsifpprsStackInstances,
    lsifpprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkListStackInstancesForProvisionedProduct' smart constructor.
data ListStackInstancesForProvisionedProduct = ListStackInstancesForProvisionedProduct'
  { acceptLanguage ::
      Lude.Maybe
        Lude.Text,
    pageToken ::
      Lude.Maybe
        Lude.Text,
    pageSize ::
      Lude.Maybe
        Lude.Natural,
    provisionedProductId ::
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

-- | Creates a value of 'ListStackInstancesForProvisionedProduct' with the minimum fields required to make a request.
--
-- * 'acceptLanguage' - The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
-- * 'pageSize' - The maximum number of items to return with this call.
-- * 'pageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
-- * 'provisionedProductId' - The identifier of the provisioned product.
mkListStackInstancesForProvisionedProduct ::
  -- | 'provisionedProductId'
  Lude.Text ->
  ListStackInstancesForProvisionedProduct
mkListStackInstancesForProvisionedProduct pProvisionedProductId_ =
  ListStackInstancesForProvisionedProduct'
    { acceptLanguage =
        Lude.Nothing,
      pageToken = Lude.Nothing,
      pageSize = Lude.Nothing,
      provisionedProductId = pProvisionedProductId_
    }

-- | The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
--
-- /Note:/ Consider using 'acceptLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsifppAcceptLanguage :: Lens.Lens' ListStackInstancesForProvisionedProduct (Lude.Maybe Lude.Text)
lsifppAcceptLanguage = Lens.lens (acceptLanguage :: ListStackInstancesForProvisionedProduct -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: ListStackInstancesForProvisionedProduct)
{-# DEPRECATED lsifppAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsifppPageToken :: Lens.Lens' ListStackInstancesForProvisionedProduct (Lude.Maybe Lude.Text)
lsifppPageToken = Lens.lens (pageToken :: ListStackInstancesForProvisionedProduct -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: ListStackInstancesForProvisionedProduct)
{-# DEPRECATED lsifppPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsifppPageSize :: Lens.Lens' ListStackInstancesForProvisionedProduct (Lude.Maybe Lude.Natural)
lsifppPageSize = Lens.lens (pageSize :: ListStackInstancesForProvisionedProduct -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: ListStackInstancesForProvisionedProduct)
{-# DEPRECATED lsifppPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The identifier of the provisioned product.
--
-- /Note:/ Consider using 'provisionedProductId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsifppProvisionedProductId :: Lens.Lens' ListStackInstancesForProvisionedProduct Lude.Text
lsifppProvisionedProductId = Lens.lens (provisionedProductId :: ListStackInstancesForProvisionedProduct -> Lude.Text) (\s a -> s {provisionedProductId = a} :: ListStackInstancesForProvisionedProduct)
{-# DEPRECATED lsifppProvisionedProductId "Use generic-lens or generic-optics with 'provisionedProductId' instead." #-}

instance Lude.AWSRequest ListStackInstancesForProvisionedProduct where
  type
    Rs ListStackInstancesForProvisionedProduct =
      ListStackInstancesForProvisionedProductResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListStackInstancesForProvisionedProductResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "StackInstances" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListStackInstancesForProvisionedProduct where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.ListStackInstancesForProvisionedProduct" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListStackInstancesForProvisionedProduct where
  toJSON ListStackInstancesForProvisionedProduct' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("PageToken" Lude..=) Lude.<$> pageToken,
            ("PageSize" Lude..=) Lude.<$> pageSize,
            Lude.Just ("ProvisionedProductId" Lude..= provisionedProductId)
          ]
      )

instance Lude.ToPath ListStackInstancesForProvisionedProduct where
  toPath = Lude.const "/"

instance Lude.ToQuery ListStackInstancesForProvisionedProduct where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListStackInstancesForProvisionedProductResponse' smart constructor.
data ListStackInstancesForProvisionedProductResponse = ListStackInstancesForProvisionedProductResponse'
  { nextPageToken ::
      Lude.Maybe
        Lude.Text,
    stackInstances ::
      Lude.Maybe
        [StackInstance],
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

-- | Creates a value of 'ListStackInstancesForProvisionedProductResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
-- * 'responseStatus' - The response status code.
-- * 'stackInstances' - List of stack instances.
mkListStackInstancesForProvisionedProductResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListStackInstancesForProvisionedProductResponse
mkListStackInstancesForProvisionedProductResponse pResponseStatus_ =
  ListStackInstancesForProvisionedProductResponse'
    { nextPageToken =
        Lude.Nothing,
      stackInstances = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsifpprsNextPageToken :: Lens.Lens' ListStackInstancesForProvisionedProductResponse (Lude.Maybe Lude.Text)
lsifpprsNextPageToken = Lens.lens (nextPageToken :: ListStackInstancesForProvisionedProductResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: ListStackInstancesForProvisionedProductResponse)
{-# DEPRECATED lsifpprsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | List of stack instances.
--
-- /Note:/ Consider using 'stackInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsifpprsStackInstances :: Lens.Lens' ListStackInstancesForProvisionedProductResponse (Lude.Maybe [StackInstance])
lsifpprsStackInstances = Lens.lens (stackInstances :: ListStackInstancesForProvisionedProductResponse -> Lude.Maybe [StackInstance]) (\s a -> s {stackInstances = a} :: ListStackInstancesForProvisionedProductResponse)
{-# DEPRECATED lsifpprsStackInstances "Use generic-lens or generic-optics with 'stackInstances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsifpprsResponseStatus :: Lens.Lens' ListStackInstancesForProvisionedProductResponse Lude.Int
lsifpprsResponseStatus = Lens.lens (responseStatus :: ListStackInstancesForProvisionedProductResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListStackInstancesForProvisionedProductResponse)
{-# DEPRECATED lsifpprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
