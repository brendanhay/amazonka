{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeProvisionedProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified provisioned product.
module Network.AWS.ServiceCatalog.DescribeProvisionedProduct
  ( -- * Creating a request
    DescribeProvisionedProduct (..),
    mkDescribeProvisionedProduct,

    -- ** Request lenses
    deseName,
    deseAcceptLanguage,
    deseId,

    -- * Destructuring the response
    DescribeProvisionedProductResponse (..),
    mkDescribeProvisionedProductResponse,

    -- ** Response lenses
    drsProvisionedProductDetail,
    drsCloudWatchDashboards,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | DescribeProvisionedProductAPI input structure. AcceptLanguage - [Optional] The language code for localization. Id - [Optional] The provisioned product identifier. Name - [Optional] Another provisioned product identifier. Customers must provide either Id or Name.
--
-- /See:/ 'mkDescribeProvisionedProduct' smart constructor.
data DescribeProvisionedProduct = DescribeProvisionedProduct'
  { name ::
      Lude.Maybe Lude.Text,
    acceptLanguage ::
      Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeProvisionedProduct' with the minimum fields required to make a request.
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
-- * 'id' - The provisioned product identifier. You must provide the name or ID, but not both.
--
-- If you do not provide a name or ID, or you provide both name and ID, an @InvalidParametersException@ will occur.
-- * 'name' - The name of the provisioned product. You must provide the name or ID, but not both.
--
-- If you do not provide a name or ID, or you provide both name and ID, an @InvalidParametersException@ will occur.
mkDescribeProvisionedProduct ::
  DescribeProvisionedProduct
mkDescribeProvisionedProduct =
  DescribeProvisionedProduct'
    { name = Lude.Nothing,
      acceptLanguage = Lude.Nothing,
      id = Lude.Nothing
    }

-- | The name of the provisioned product. You must provide the name or ID, but not both.
--
-- If you do not provide a name or ID, or you provide both name and ID, an @InvalidParametersException@ will occur.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deseName :: Lens.Lens' DescribeProvisionedProduct (Lude.Maybe Lude.Text)
deseName = Lens.lens (name :: DescribeProvisionedProduct -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DescribeProvisionedProduct)
{-# DEPRECATED deseName "Use generic-lens or generic-optics with 'name' instead." #-}

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
deseAcceptLanguage :: Lens.Lens' DescribeProvisionedProduct (Lude.Maybe Lude.Text)
deseAcceptLanguage = Lens.lens (acceptLanguage :: DescribeProvisionedProduct -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: DescribeProvisionedProduct)
{-# DEPRECATED deseAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The provisioned product identifier. You must provide the name or ID, but not both.
--
-- If you do not provide a name or ID, or you provide both name and ID, an @InvalidParametersException@ will occur.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deseId :: Lens.Lens' DescribeProvisionedProduct (Lude.Maybe Lude.Text)
deseId = Lens.lens (id :: DescribeProvisionedProduct -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: DescribeProvisionedProduct)
{-# DEPRECATED deseId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DescribeProvisionedProduct where
  type
    Rs DescribeProvisionedProduct =
      DescribeProvisionedProductResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeProvisionedProductResponse'
            Lude.<$> (x Lude..?> "ProvisionedProductDetail")
            Lude.<*> (x Lude..?> "CloudWatchDashboards" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeProvisionedProduct where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.DescribeProvisionedProduct" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeProvisionedProduct where
  toJSON DescribeProvisionedProduct' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Name" Lude..=) Lude.<$> name,
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("Id" Lude..=) Lude.<$> id
          ]
      )

instance Lude.ToPath DescribeProvisionedProduct where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeProvisionedProduct where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeProvisionedProductResponse' smart constructor.
data DescribeProvisionedProductResponse = DescribeProvisionedProductResponse'
  { provisionedProductDetail ::
      Lude.Maybe
        ProvisionedProductDetail,
    cloudWatchDashboards ::
      Lude.Maybe
        [CloudWatchDashboard],
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
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeProvisionedProductResponse' with the minimum fields required to make a request.
--
-- * 'cloudWatchDashboards' - Any CloudWatch dashboards that were created when provisioning the product.
-- * 'provisionedProductDetail' - Information about the provisioned product.
-- * 'responseStatus' - The response status code.
mkDescribeProvisionedProductResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeProvisionedProductResponse
mkDescribeProvisionedProductResponse pResponseStatus_ =
  DescribeProvisionedProductResponse'
    { provisionedProductDetail =
        Lude.Nothing,
      cloudWatchDashboards = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the provisioned product.
--
-- /Note:/ Consider using 'provisionedProductDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsProvisionedProductDetail :: Lens.Lens' DescribeProvisionedProductResponse (Lude.Maybe ProvisionedProductDetail)
drsProvisionedProductDetail = Lens.lens (provisionedProductDetail :: DescribeProvisionedProductResponse -> Lude.Maybe ProvisionedProductDetail) (\s a -> s {provisionedProductDetail = a} :: DescribeProvisionedProductResponse)
{-# DEPRECATED drsProvisionedProductDetail "Use generic-lens or generic-optics with 'provisionedProductDetail' instead." #-}

-- | Any CloudWatch dashboards that were created when provisioning the product.
--
-- /Note:/ Consider using 'cloudWatchDashboards' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCloudWatchDashboards :: Lens.Lens' DescribeProvisionedProductResponse (Lude.Maybe [CloudWatchDashboard])
drsCloudWatchDashboards = Lens.lens (cloudWatchDashboards :: DescribeProvisionedProductResponse -> Lude.Maybe [CloudWatchDashboard]) (\s a -> s {cloudWatchDashboards = a} :: DescribeProvisionedProductResponse)
{-# DEPRECATED drsCloudWatchDashboards "Use generic-lens or generic-optics with 'cloudWatchDashboards' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeProvisionedProductResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeProvisionedProductResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeProvisionedProductResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
