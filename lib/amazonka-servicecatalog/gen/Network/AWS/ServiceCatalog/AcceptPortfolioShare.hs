{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.AcceptPortfolioShare
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts an offer to share the specified portfolio.
module Network.AWS.ServiceCatalog.AcceptPortfolioShare
  ( -- * Creating a request
    AcceptPortfolioShare (..),
    mkAcceptPortfolioShare,

    -- ** Request lenses
    apsPortfolioShareType,
    apsAcceptLanguage,
    apsPortfolioId,

    -- * Destructuring the response
    AcceptPortfolioShareResponse (..),
    mkAcceptPortfolioShareResponse,

    -- ** Response lenses
    apsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkAcceptPortfolioShare' smart constructor.
data AcceptPortfolioShare = AcceptPortfolioShare'
  { portfolioShareType ::
      Lude.Maybe PortfolioShareType,
    acceptLanguage :: Lude.Maybe Lude.Text,
    portfolioId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AcceptPortfolioShare' with the minimum fields required to make a request.
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
-- * 'portfolioId' - The portfolio identifier.
-- * 'portfolioShareType' - The type of shared portfolios to accept. The default is to accept imported portfolios.
--
--
--     * @AWS_ORGANIZATIONS@ - Accept portfolios shared by the management account of your organization.
--
--
--     * @IMPORTED@ - Accept imported portfolios.
--
--
--     * @AWS_SERVICECATALOG@ - Not supported. (Throws ResourceNotFoundException.)
--
--
-- For example, @aws servicecatalog accept-portfolio-share --portfolio-id "port-2qwzkwxt3y5fk" --portfolio-share-type AWS_ORGANIZATIONS@
mkAcceptPortfolioShare ::
  -- | 'portfolioId'
  Lude.Text ->
  AcceptPortfolioShare
mkAcceptPortfolioShare pPortfolioId_ =
  AcceptPortfolioShare'
    { portfolioShareType = Lude.Nothing,
      acceptLanguage = Lude.Nothing,
      portfolioId = pPortfolioId_
    }

-- | The type of shared portfolios to accept. The default is to accept imported portfolios.
--
--
--     * @AWS_ORGANIZATIONS@ - Accept portfolios shared by the management account of your organization.
--
--
--     * @IMPORTED@ - Accept imported portfolios.
--
--
--     * @AWS_SERVICECATALOG@ - Not supported. (Throws ResourceNotFoundException.)
--
--
-- For example, @aws servicecatalog accept-portfolio-share --portfolio-id "port-2qwzkwxt3y5fk" --portfolio-share-type AWS_ORGANIZATIONS@
--
-- /Note:/ Consider using 'portfolioShareType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apsPortfolioShareType :: Lens.Lens' AcceptPortfolioShare (Lude.Maybe PortfolioShareType)
apsPortfolioShareType = Lens.lens (portfolioShareType :: AcceptPortfolioShare -> Lude.Maybe PortfolioShareType) (\s a -> s {portfolioShareType = a} :: AcceptPortfolioShare)
{-# DEPRECATED apsPortfolioShareType "Use generic-lens or generic-optics with 'portfolioShareType' instead." #-}

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
apsAcceptLanguage :: Lens.Lens' AcceptPortfolioShare (Lude.Maybe Lude.Text)
apsAcceptLanguage = Lens.lens (acceptLanguage :: AcceptPortfolioShare -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: AcceptPortfolioShare)
{-# DEPRECATED apsAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apsPortfolioId :: Lens.Lens' AcceptPortfolioShare Lude.Text
apsPortfolioId = Lens.lens (portfolioId :: AcceptPortfolioShare -> Lude.Text) (\s a -> s {portfolioId = a} :: AcceptPortfolioShare)
{-# DEPRECATED apsPortfolioId "Use generic-lens or generic-optics with 'portfolioId' instead." #-}

instance Lude.AWSRequest AcceptPortfolioShare where
  type Rs AcceptPortfolioShare = AcceptPortfolioShareResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AcceptPortfolioShareResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AcceptPortfolioShare where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.AcceptPortfolioShare" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AcceptPortfolioShare where
  toJSON AcceptPortfolioShare' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PortfolioShareType" Lude..=) Lude.<$> portfolioShareType,
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            Lude.Just ("PortfolioId" Lude..= portfolioId)
          ]
      )

instance Lude.ToPath AcceptPortfolioShare where
  toPath = Lude.const "/"

instance Lude.ToQuery AcceptPortfolioShare where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAcceptPortfolioShareResponse' smart constructor.
newtype AcceptPortfolioShareResponse = AcceptPortfolioShareResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AcceptPortfolioShareResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAcceptPortfolioShareResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AcceptPortfolioShareResponse
mkAcceptPortfolioShareResponse pResponseStatus_ =
  AcceptPortfolioShareResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apsrsResponseStatus :: Lens.Lens' AcceptPortfolioShareResponse Lude.Int
apsrsResponseStatus = Lens.lens (responseStatus :: AcceptPortfolioShareResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AcceptPortfolioShareResponse)
{-# DEPRECATED apsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
