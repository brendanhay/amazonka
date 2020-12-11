{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeProductAsAdmin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified product. This operation is run with administrator access.
module Network.AWS.ServiceCatalog.DescribeProductAsAdmin
  ( -- * Creating a request
    DescribeProductAsAdmin (..),
    mkDescribeProductAsAdmin,

    -- ** Request lenses
    dpaaName,
    dpaaAcceptLanguage,
    dpaaId,

    -- * Destructuring the response
    DescribeProductAsAdminResponse (..),
    mkDescribeProductAsAdminResponse,

    -- ** Response lenses
    dpaarsProductViewDetail,
    dpaarsTagOptions,
    dpaarsProvisioningArtifactSummaries,
    dpaarsBudgets,
    dpaarsTags,
    dpaarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkDescribeProductAsAdmin' smart constructor.
data DescribeProductAsAdmin = DescribeProductAsAdmin'
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

-- | Creates a value of 'DescribeProductAsAdmin' with the minimum fields required to make a request.
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
mkDescribeProductAsAdmin ::
  DescribeProductAsAdmin
mkDescribeProductAsAdmin =
  DescribeProductAsAdmin'
    { name = Lude.Nothing,
      acceptLanguage = Lude.Nothing,
      id = Lude.Nothing
    }

-- | The product name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpaaName :: Lens.Lens' DescribeProductAsAdmin (Lude.Maybe Lude.Text)
dpaaName = Lens.lens (name :: DescribeProductAsAdmin -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DescribeProductAsAdmin)
{-# DEPRECATED dpaaName "Use generic-lens or generic-optics with 'name' instead." #-}

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
dpaaAcceptLanguage :: Lens.Lens' DescribeProductAsAdmin (Lude.Maybe Lude.Text)
dpaaAcceptLanguage = Lens.lens (acceptLanguage :: DescribeProductAsAdmin -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: DescribeProductAsAdmin)
{-# DEPRECATED dpaaAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpaaId :: Lens.Lens' DescribeProductAsAdmin (Lude.Maybe Lude.Text)
dpaaId = Lens.lens (id :: DescribeProductAsAdmin -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: DescribeProductAsAdmin)
{-# DEPRECATED dpaaId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DescribeProductAsAdmin where
  type Rs DescribeProductAsAdmin = DescribeProductAsAdminResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeProductAsAdminResponse'
            Lude.<$> (x Lude..?> "ProductViewDetail")
            Lude.<*> (x Lude..?> "TagOptions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "ProvisioningArtifactSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Budgets" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeProductAsAdmin where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.DescribeProductAsAdmin" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeProductAsAdmin where
  toJSON DescribeProductAsAdmin' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Name" Lude..=) Lude.<$> name,
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("Id" Lude..=) Lude.<$> id
          ]
      )

instance Lude.ToPath DescribeProductAsAdmin where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeProductAsAdmin where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeProductAsAdminResponse' smart constructor.
data DescribeProductAsAdminResponse = DescribeProductAsAdminResponse'
  { productViewDetail ::
      Lude.Maybe ProductViewDetail,
    tagOptions ::
      Lude.Maybe [TagOptionDetail],
    provisioningArtifactSummaries ::
      Lude.Maybe
        [ProvisioningArtifactSummary],
    budgets ::
      Lude.Maybe [BudgetDetail],
    tags :: Lude.Maybe [Tag],
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

-- | Creates a value of 'DescribeProductAsAdminResponse' with the minimum fields required to make a request.
--
-- * 'budgets' - Information about the associated budgets.
-- * 'productViewDetail' - Information about the product view.
-- * 'provisioningArtifactSummaries' - Information about the provisioning artifacts (also known as versions) for the specified product.
-- * 'responseStatus' - The response status code.
-- * 'tagOptions' - Information about the TagOptions associated with the product.
-- * 'tags' - Information about the tags associated with the product.
mkDescribeProductAsAdminResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeProductAsAdminResponse
mkDescribeProductAsAdminResponse pResponseStatus_ =
  DescribeProductAsAdminResponse'
    { productViewDetail = Lude.Nothing,
      tagOptions = Lude.Nothing,
      provisioningArtifactSummaries = Lude.Nothing,
      budgets = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the product view.
--
-- /Note:/ Consider using 'productViewDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpaarsProductViewDetail :: Lens.Lens' DescribeProductAsAdminResponse (Lude.Maybe ProductViewDetail)
dpaarsProductViewDetail = Lens.lens (productViewDetail :: DescribeProductAsAdminResponse -> Lude.Maybe ProductViewDetail) (\s a -> s {productViewDetail = a} :: DescribeProductAsAdminResponse)
{-# DEPRECATED dpaarsProductViewDetail "Use generic-lens or generic-optics with 'productViewDetail' instead." #-}

-- | Information about the TagOptions associated with the product.
--
-- /Note:/ Consider using 'tagOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpaarsTagOptions :: Lens.Lens' DescribeProductAsAdminResponse (Lude.Maybe [TagOptionDetail])
dpaarsTagOptions = Lens.lens (tagOptions :: DescribeProductAsAdminResponse -> Lude.Maybe [TagOptionDetail]) (\s a -> s {tagOptions = a} :: DescribeProductAsAdminResponse)
{-# DEPRECATED dpaarsTagOptions "Use generic-lens or generic-optics with 'tagOptions' instead." #-}

-- | Information about the provisioning artifacts (also known as versions) for the specified product.
--
-- /Note:/ Consider using 'provisioningArtifactSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpaarsProvisioningArtifactSummaries :: Lens.Lens' DescribeProductAsAdminResponse (Lude.Maybe [ProvisioningArtifactSummary])
dpaarsProvisioningArtifactSummaries = Lens.lens (provisioningArtifactSummaries :: DescribeProductAsAdminResponse -> Lude.Maybe [ProvisioningArtifactSummary]) (\s a -> s {provisioningArtifactSummaries = a} :: DescribeProductAsAdminResponse)
{-# DEPRECATED dpaarsProvisioningArtifactSummaries "Use generic-lens or generic-optics with 'provisioningArtifactSummaries' instead." #-}

-- | Information about the associated budgets.
--
-- /Note:/ Consider using 'budgets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpaarsBudgets :: Lens.Lens' DescribeProductAsAdminResponse (Lude.Maybe [BudgetDetail])
dpaarsBudgets = Lens.lens (budgets :: DescribeProductAsAdminResponse -> Lude.Maybe [BudgetDetail]) (\s a -> s {budgets = a} :: DescribeProductAsAdminResponse)
{-# DEPRECATED dpaarsBudgets "Use generic-lens or generic-optics with 'budgets' instead." #-}

-- | Information about the tags associated with the product.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpaarsTags :: Lens.Lens' DescribeProductAsAdminResponse (Lude.Maybe [Tag])
dpaarsTags = Lens.lens (tags :: DescribeProductAsAdminResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: DescribeProductAsAdminResponse)
{-# DEPRECATED dpaarsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpaarsResponseStatus :: Lens.Lens' DescribeProductAsAdminResponse Lude.Int
dpaarsResponseStatus = Lens.lens (responseStatus :: DescribeProductAsAdminResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeProductAsAdminResponse)
{-# DEPRECATED dpaarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
