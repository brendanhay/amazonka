{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeProductView
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified product.
module Network.AWS.ServiceCatalog.DescribeProductView
  ( -- * Creating a request
    DescribeProductView (..),
    mkDescribeProductView,

    -- ** Request lenses
    dpvAcceptLanguage,
    dpvId,

    -- * Destructuring the response
    DescribeProductViewResponse (..),
    mkDescribeProductViewResponse,

    -- ** Response lenses
    dpvrsProductViewSummary,
    dpvrsProvisioningArtifacts,
    dpvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkDescribeProductView' smart constructor.
data DescribeProductView = DescribeProductView'
  { acceptLanguage ::
      Lude.Maybe Lude.Text,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeProductView' with the minimum fields required to make a request.
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
-- * 'id' - The product view identifier.
mkDescribeProductView ::
  -- | 'id'
  Lude.Text ->
  DescribeProductView
mkDescribeProductView pId_ =
  DescribeProductView' {acceptLanguage = Lude.Nothing, id = pId_}

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
dpvAcceptLanguage :: Lens.Lens' DescribeProductView (Lude.Maybe Lude.Text)
dpvAcceptLanguage = Lens.lens (acceptLanguage :: DescribeProductView -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: DescribeProductView)
{-# DEPRECATED dpvAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The product view identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvId :: Lens.Lens' DescribeProductView Lude.Text
dpvId = Lens.lens (id :: DescribeProductView -> Lude.Text) (\s a -> s {id = a} :: DescribeProductView)
{-# DEPRECATED dpvId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DescribeProductView where
  type Rs DescribeProductView = DescribeProductViewResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeProductViewResponse'
            Lude.<$> (x Lude..?> "ProductViewSummary")
            Lude.<*> (x Lude..?> "ProvisioningArtifacts" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeProductView where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.DescribeProductView" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeProductView where
  toJSON DescribeProductView' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            Lude.Just ("Id" Lude..= id)
          ]
      )

instance Lude.ToPath DescribeProductView where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeProductView where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeProductViewResponse' smart constructor.
data DescribeProductViewResponse = DescribeProductViewResponse'
  { productViewSummary ::
      Lude.Maybe ProductViewSummary,
    provisioningArtifacts ::
      Lude.Maybe [ProvisioningArtifact],
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

-- | Creates a value of 'DescribeProductViewResponse' with the minimum fields required to make a request.
--
-- * 'productViewSummary' - Summary information about the product.
-- * 'provisioningArtifacts' - Information about the provisioning artifacts for the product.
-- * 'responseStatus' - The response status code.
mkDescribeProductViewResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeProductViewResponse
mkDescribeProductViewResponse pResponseStatus_ =
  DescribeProductViewResponse'
    { productViewSummary = Lude.Nothing,
      provisioningArtifacts = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Summary information about the product.
--
-- /Note:/ Consider using 'productViewSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvrsProductViewSummary :: Lens.Lens' DescribeProductViewResponse (Lude.Maybe ProductViewSummary)
dpvrsProductViewSummary = Lens.lens (productViewSummary :: DescribeProductViewResponse -> Lude.Maybe ProductViewSummary) (\s a -> s {productViewSummary = a} :: DescribeProductViewResponse)
{-# DEPRECATED dpvrsProductViewSummary "Use generic-lens or generic-optics with 'productViewSummary' instead." #-}

-- | Information about the provisioning artifacts for the product.
--
-- /Note:/ Consider using 'provisioningArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvrsProvisioningArtifacts :: Lens.Lens' DescribeProductViewResponse (Lude.Maybe [ProvisioningArtifact])
dpvrsProvisioningArtifacts = Lens.lens (provisioningArtifacts :: DescribeProductViewResponse -> Lude.Maybe [ProvisioningArtifact]) (\s a -> s {provisioningArtifacts = a} :: DescribeProductViewResponse)
{-# DEPRECATED dpvrsProvisioningArtifacts "Use generic-lens or generic-optics with 'provisioningArtifacts' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvrsResponseStatus :: Lens.Lens' DescribeProductViewResponse Lude.Int
dpvrsResponseStatus = Lens.lens (responseStatus :: DescribeProductViewResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeProductViewResponse)
{-# DEPRECATED dpvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
