{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.AssociateProductWithPortfolio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified product with the specified portfolio.
--
-- A delegated admin is authorized to invoke this command.
module Network.AWS.ServiceCatalog.AssociateProductWithPortfolio
  ( -- * Creating a request
    AssociateProductWithPortfolio (..),
    mkAssociateProductWithPortfolio,

    -- ** Request lenses
    aPortfolioId,
    aSourcePortfolioId,
    aAcceptLanguage,
    aProductId,

    -- * Destructuring the response
    AssociateProductWithPortfolioResponse (..),
    mkAssociateProductWithPortfolioResponse,

    -- ** Response lenses
    apwprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkAssociateProductWithPortfolio' smart constructor.
data AssociateProductWithPortfolio = AssociateProductWithPortfolio'
  { -- | The portfolio identifier.
    portfolioId :: Lude.Text,
    -- | The identifier of the source portfolio.
    sourcePortfolioId :: Lude.Maybe Lude.Text,
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
    acceptLanguage :: Lude.Maybe Lude.Text,
    -- | The product identifier.
    productId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateProductWithPortfolio' with the minimum fields required to make a request.
--
-- * 'portfolioId' - The portfolio identifier.
-- * 'sourcePortfolioId' - The identifier of the source portfolio.
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
-- * 'productId' - The product identifier.
mkAssociateProductWithPortfolio ::
  -- | 'portfolioId'
  Lude.Text ->
  -- | 'productId'
  Lude.Text ->
  AssociateProductWithPortfolio
mkAssociateProductWithPortfolio pPortfolioId_ pProductId_ =
  AssociateProductWithPortfolio'
    { portfolioId = pPortfolioId_,
      sourcePortfolioId = Lude.Nothing,
      acceptLanguage = Lude.Nothing,
      productId = pProductId_
    }

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPortfolioId :: Lens.Lens' AssociateProductWithPortfolio Lude.Text
aPortfolioId = Lens.lens (portfolioId :: AssociateProductWithPortfolio -> Lude.Text) (\s a -> s {portfolioId = a} :: AssociateProductWithPortfolio)
{-# DEPRECATED aPortfolioId "Use generic-lens or generic-optics with 'portfolioId' instead." #-}

-- | The identifier of the source portfolio.
--
-- /Note:/ Consider using 'sourcePortfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSourcePortfolioId :: Lens.Lens' AssociateProductWithPortfolio (Lude.Maybe Lude.Text)
aSourcePortfolioId = Lens.lens (sourcePortfolioId :: AssociateProductWithPortfolio -> Lude.Maybe Lude.Text) (\s a -> s {sourcePortfolioId = a} :: AssociateProductWithPortfolio)
{-# DEPRECATED aSourcePortfolioId "Use generic-lens or generic-optics with 'sourcePortfolioId' instead." #-}

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
aAcceptLanguage :: Lens.Lens' AssociateProductWithPortfolio (Lude.Maybe Lude.Text)
aAcceptLanguage = Lens.lens (acceptLanguage :: AssociateProductWithPortfolio -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: AssociateProductWithPortfolio)
{-# DEPRECATED aAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aProductId :: Lens.Lens' AssociateProductWithPortfolio Lude.Text
aProductId = Lens.lens (productId :: AssociateProductWithPortfolio -> Lude.Text) (\s a -> s {productId = a} :: AssociateProductWithPortfolio)
{-# DEPRECATED aProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

instance Lude.AWSRequest AssociateProductWithPortfolio where
  type
    Rs AssociateProductWithPortfolio =
      AssociateProductWithPortfolioResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AssociateProductWithPortfolioResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateProductWithPortfolio where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.AssociateProductWithPortfolio" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateProductWithPortfolio where
  toJSON AssociateProductWithPortfolio' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("PortfolioId" Lude..= portfolioId),
            ("SourcePortfolioId" Lude..=) Lude.<$> sourcePortfolioId,
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            Lude.Just ("ProductId" Lude..= productId)
          ]
      )

instance Lude.ToPath AssociateProductWithPortfolio where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateProductWithPortfolio where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateProductWithPortfolioResponse' smart constructor.
newtype AssociateProductWithPortfolioResponse = AssociateProductWithPortfolioResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateProductWithPortfolioResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAssociateProductWithPortfolioResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateProductWithPortfolioResponse
mkAssociateProductWithPortfolioResponse pResponseStatus_ =
  AssociateProductWithPortfolioResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apwprsResponseStatus :: Lens.Lens' AssociateProductWithPortfolioResponse Lude.Int
apwprsResponseStatus = Lens.lens (responseStatus :: AssociateProductWithPortfolioResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateProductWithPortfolioResponse)
{-# DEPRECATED apwprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
