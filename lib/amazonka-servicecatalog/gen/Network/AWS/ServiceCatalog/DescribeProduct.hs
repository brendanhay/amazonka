{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified product.
module Network.AWS.ServiceCatalog.DescribeProduct
  ( -- * Creating a request
    DescribeProduct (..),
    mkDescribeProduct,

    -- ** Request lenses
    dpName,
    dpAcceptLanguage,
    dpId,

    -- * Destructuring the response
    DescribeProductResponse (..),
    mkDescribeProductResponse,

    -- ** Response lenses
    ddrsProductViewSummary,
    ddrsProvisioningArtifacts,
    ddrsLaunchPaths,
    ddrsBudgets,
    ddrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkDescribeProduct' smart constructor.
data DescribeProduct = DescribeProduct'
  { name ::
      Lude.Maybe Lude.Text,
    acceptLanguage :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeProduct' with the minimum fields required to make a request.
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
-- * 'id' - The product identifier.
-- * 'name' - The product name.
mkDescribeProduct ::
  DescribeProduct
mkDescribeProduct =
  DescribeProduct'
    { name = Lude.Nothing,
      acceptLanguage = Lude.Nothing,
      id = Lude.Nothing
    }

-- | The product name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpName :: Lens.Lens' DescribeProduct (Lude.Maybe Lude.Text)
dpName = Lens.lens (name :: DescribeProduct -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DescribeProduct)
{-# DEPRECATED dpName "Use generic-lens or generic-optics with 'name' instead." #-}

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
dpAcceptLanguage :: Lens.Lens' DescribeProduct (Lude.Maybe Lude.Text)
dpAcceptLanguage = Lens.lens (acceptLanguage :: DescribeProduct -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: DescribeProduct)
{-# DEPRECATED dpAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpId :: Lens.Lens' DescribeProduct (Lude.Maybe Lude.Text)
dpId = Lens.lens (id :: DescribeProduct -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: DescribeProduct)
{-# DEPRECATED dpId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DescribeProduct where
  type Rs DescribeProduct = DescribeProductResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeProductResponse'
            Lude.<$> (x Lude..?> "ProductViewSummary")
            Lude.<*> (x Lude..?> "ProvisioningArtifacts" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "LaunchPaths" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Budgets" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeProduct where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWS242ServiceCatalogService.DescribeProduct" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeProduct where
  toJSON DescribeProduct' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Name" Lude..=) Lude.<$> name,
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("Id" Lude..=) Lude.<$> id
          ]
      )

instance Lude.ToPath DescribeProduct where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeProduct where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeProductResponse' smart constructor.
data DescribeProductResponse = DescribeProductResponse'
  { productViewSummary ::
      Lude.Maybe ProductViewSummary,
    provisioningArtifacts ::
      Lude.Maybe [ProvisioningArtifact],
    launchPaths :: Lude.Maybe [LaunchPath],
    budgets :: Lude.Maybe [BudgetDetail],
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

-- | Creates a value of 'DescribeProductResponse' with the minimum fields required to make a request.
--
-- * 'budgets' - Information about the associated budgets.
-- * 'launchPaths' - Information about the associated launch paths.
-- * 'productViewSummary' - Summary information about the product view.
-- * 'provisioningArtifacts' - Information about the provisioning artifacts for the specified product.
-- * 'responseStatus' - The response status code.
mkDescribeProductResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeProductResponse
mkDescribeProductResponse pResponseStatus_ =
  DescribeProductResponse'
    { productViewSummary = Lude.Nothing,
      provisioningArtifacts = Lude.Nothing,
      launchPaths = Lude.Nothing,
      budgets = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Summary information about the product view.
--
-- /Note:/ Consider using 'productViewSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsProductViewSummary :: Lens.Lens' DescribeProductResponse (Lude.Maybe ProductViewSummary)
ddrsProductViewSummary = Lens.lens (productViewSummary :: DescribeProductResponse -> Lude.Maybe ProductViewSummary) (\s a -> s {productViewSummary = a} :: DescribeProductResponse)
{-# DEPRECATED ddrsProductViewSummary "Use generic-lens or generic-optics with 'productViewSummary' instead." #-}

-- | Information about the provisioning artifacts for the specified product.
--
-- /Note:/ Consider using 'provisioningArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsProvisioningArtifacts :: Lens.Lens' DescribeProductResponse (Lude.Maybe [ProvisioningArtifact])
ddrsProvisioningArtifacts = Lens.lens (provisioningArtifacts :: DescribeProductResponse -> Lude.Maybe [ProvisioningArtifact]) (\s a -> s {provisioningArtifacts = a} :: DescribeProductResponse)
{-# DEPRECATED ddrsProvisioningArtifacts "Use generic-lens or generic-optics with 'provisioningArtifacts' instead." #-}

-- | Information about the associated launch paths.
--
-- /Note:/ Consider using 'launchPaths' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsLaunchPaths :: Lens.Lens' DescribeProductResponse (Lude.Maybe [LaunchPath])
ddrsLaunchPaths = Lens.lens (launchPaths :: DescribeProductResponse -> Lude.Maybe [LaunchPath]) (\s a -> s {launchPaths = a} :: DescribeProductResponse)
{-# DEPRECATED ddrsLaunchPaths "Use generic-lens or generic-optics with 'launchPaths' instead." #-}

-- | Information about the associated budgets.
--
-- /Note:/ Consider using 'budgets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsBudgets :: Lens.Lens' DescribeProductResponse (Lude.Maybe [BudgetDetail])
ddrsBudgets = Lens.lens (budgets :: DescribeProductResponse -> Lude.Maybe [BudgetDetail]) (\s a -> s {budgets = a} :: DescribeProductResponse)
{-# DEPRECATED ddrsBudgets "Use generic-lens or generic-optics with 'budgets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsResponseStatus :: Lens.Lens' DescribeProductResponse Lude.Int
ddrsResponseStatus = Lens.lens (responseStatus :: DescribeProductResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeProductResponse)
{-# DEPRECATED ddrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
