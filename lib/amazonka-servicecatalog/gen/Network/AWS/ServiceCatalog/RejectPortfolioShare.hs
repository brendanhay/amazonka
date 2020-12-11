{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.RejectPortfolioShare
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects an offer to share the specified portfolio.
module Network.AWS.ServiceCatalog.RejectPortfolioShare
  ( -- * Creating a request
    RejectPortfolioShare (..),
    mkRejectPortfolioShare,

    -- ** Request lenses
    rpsPortfolioShareType,
    rpsAcceptLanguage,
    rpsPortfolioId,

    -- * Destructuring the response
    RejectPortfolioShareResponse (..),
    mkRejectPortfolioShareResponse,

    -- ** Response lenses
    rpsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkRejectPortfolioShare' smart constructor.
data RejectPortfolioShare = RejectPortfolioShare'
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

-- | Creates a value of 'RejectPortfolioShare' with the minimum fields required to make a request.
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
-- * 'portfolioShareType' - The type of shared portfolios to reject. The default is to reject imported portfolios.
--
--
--     * @AWS_ORGANIZATIONS@ - Reject portfolios shared by the management account of your organization.
--
--
--     * @IMPORTED@ - Reject imported portfolios.
--
--
--     * @AWS_SERVICECATALOG@ - Not supported. (Throws ResourceNotFoundException.)
--
--
-- For example, @aws servicecatalog reject-portfolio-share --portfolio-id "port-2qwzkwxt3y5fk" --portfolio-share-type AWS_ORGANIZATIONS@
mkRejectPortfolioShare ::
  -- | 'portfolioId'
  Lude.Text ->
  RejectPortfolioShare
mkRejectPortfolioShare pPortfolioId_ =
  RejectPortfolioShare'
    { portfolioShareType = Lude.Nothing,
      acceptLanguage = Lude.Nothing,
      portfolioId = pPortfolioId_
    }

-- | The type of shared portfolios to reject. The default is to reject imported portfolios.
--
--
--     * @AWS_ORGANIZATIONS@ - Reject portfolios shared by the management account of your organization.
--
--
--     * @IMPORTED@ - Reject imported portfolios.
--
--
--     * @AWS_SERVICECATALOG@ - Not supported. (Throws ResourceNotFoundException.)
--
--
-- For example, @aws servicecatalog reject-portfolio-share --portfolio-id "port-2qwzkwxt3y5fk" --portfolio-share-type AWS_ORGANIZATIONS@
--
-- /Note:/ Consider using 'portfolioShareType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpsPortfolioShareType :: Lens.Lens' RejectPortfolioShare (Lude.Maybe PortfolioShareType)
rpsPortfolioShareType = Lens.lens (portfolioShareType :: RejectPortfolioShare -> Lude.Maybe PortfolioShareType) (\s a -> s {portfolioShareType = a} :: RejectPortfolioShare)
{-# DEPRECATED rpsPortfolioShareType "Use generic-lens or generic-optics with 'portfolioShareType' instead." #-}

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
rpsAcceptLanguage :: Lens.Lens' RejectPortfolioShare (Lude.Maybe Lude.Text)
rpsAcceptLanguage = Lens.lens (acceptLanguage :: RejectPortfolioShare -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: RejectPortfolioShare)
{-# DEPRECATED rpsAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpsPortfolioId :: Lens.Lens' RejectPortfolioShare Lude.Text
rpsPortfolioId = Lens.lens (portfolioId :: RejectPortfolioShare -> Lude.Text) (\s a -> s {portfolioId = a} :: RejectPortfolioShare)
{-# DEPRECATED rpsPortfolioId "Use generic-lens or generic-optics with 'portfolioId' instead." #-}

instance Lude.AWSRequest RejectPortfolioShare where
  type Rs RejectPortfolioShare = RejectPortfolioShareResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveEmpty
      ( \s h x ->
          RejectPortfolioShareResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RejectPortfolioShare where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.RejectPortfolioShare" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RejectPortfolioShare where
  toJSON RejectPortfolioShare' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PortfolioShareType" Lude..=) Lude.<$> portfolioShareType,
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            Lude.Just ("PortfolioId" Lude..= portfolioId)
          ]
      )

instance Lude.ToPath RejectPortfolioShare where
  toPath = Lude.const "/"

instance Lude.ToQuery RejectPortfolioShare where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRejectPortfolioShareResponse' smart constructor.
newtype RejectPortfolioShareResponse = RejectPortfolioShareResponse'
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

-- | Creates a value of 'RejectPortfolioShareResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRejectPortfolioShareResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RejectPortfolioShareResponse
mkRejectPortfolioShareResponse pResponseStatus_ =
  RejectPortfolioShareResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpsrsResponseStatus :: Lens.Lens' RejectPortfolioShareResponse Lude.Int
rpsrsResponseStatus = Lens.lens (responseStatus :: RejectPortfolioShareResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RejectPortfolioShareResponse)
{-# DEPRECATED rpsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
