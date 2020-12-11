{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DeletePortfolioShare
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops sharing the specified portfolio with the specified account or organization node. Shares to an organization node can only be deleted by the management account of an organization or by a delegated administrator.
--
-- Note that if a delegated admin is de-registered, portfolio shares created from that account are removed.
module Network.AWS.ServiceCatalog.DeletePortfolioShare
  ( -- * Creating a request
    DeletePortfolioShare (..),
    mkDeletePortfolioShare,

    -- ** Request lenses
    dpsAccountId,
    dpsAcceptLanguage,
    dpsOrganizationNode,
    dpsPortfolioId,

    -- * Destructuring the response
    DeletePortfolioShareResponse (..),
    mkDeletePortfolioShareResponse,

    -- ** Response lenses
    dpsrsPortfolioShareToken,
    dpsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkDeletePortfolioShare' smart constructor.
data DeletePortfolioShare = DeletePortfolioShare'
  { accountId ::
      Lude.Maybe Lude.Text,
    acceptLanguage :: Lude.Maybe Lude.Text,
    organizationNode :: Lude.Maybe OrganizationNode,
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

-- | Creates a value of 'DeletePortfolioShare' with the minimum fields required to make a request.
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
-- * 'accountId' - The AWS account ID.
-- * 'organizationNode' - The organization node to whom you are going to stop sharing.
-- * 'portfolioId' - The portfolio identifier.
mkDeletePortfolioShare ::
  -- | 'portfolioId'
  Lude.Text ->
  DeletePortfolioShare
mkDeletePortfolioShare pPortfolioId_ =
  DeletePortfolioShare'
    { accountId = Lude.Nothing,
      acceptLanguage = Lude.Nothing,
      organizationNode = Lude.Nothing,
      portfolioId = pPortfolioId_
    }

-- | The AWS account ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsAccountId :: Lens.Lens' DeletePortfolioShare (Lude.Maybe Lude.Text)
dpsAccountId = Lens.lens (accountId :: DeletePortfolioShare -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: DeletePortfolioShare)
{-# DEPRECATED dpsAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

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
dpsAcceptLanguage :: Lens.Lens' DeletePortfolioShare (Lude.Maybe Lude.Text)
dpsAcceptLanguage = Lens.lens (acceptLanguage :: DeletePortfolioShare -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: DeletePortfolioShare)
{-# DEPRECATED dpsAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The organization node to whom you are going to stop sharing.
--
-- /Note:/ Consider using 'organizationNode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsOrganizationNode :: Lens.Lens' DeletePortfolioShare (Lude.Maybe OrganizationNode)
dpsOrganizationNode = Lens.lens (organizationNode :: DeletePortfolioShare -> Lude.Maybe OrganizationNode) (\s a -> s {organizationNode = a} :: DeletePortfolioShare)
{-# DEPRECATED dpsOrganizationNode "Use generic-lens or generic-optics with 'organizationNode' instead." #-}

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsPortfolioId :: Lens.Lens' DeletePortfolioShare Lude.Text
dpsPortfolioId = Lens.lens (portfolioId :: DeletePortfolioShare -> Lude.Text) (\s a -> s {portfolioId = a} :: DeletePortfolioShare)
{-# DEPRECATED dpsPortfolioId "Use generic-lens or generic-optics with 'portfolioId' instead." #-}

instance Lude.AWSRequest DeletePortfolioShare where
  type Rs DeletePortfolioShare = DeletePortfolioShareResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeletePortfolioShareResponse'
            Lude.<$> (x Lude..?> "PortfolioShareToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeletePortfolioShare where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.DeletePortfolioShare" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeletePortfolioShare where
  toJSON DeletePortfolioShare' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AccountId" Lude..=) Lude.<$> accountId,
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("OrganizationNode" Lude..=) Lude.<$> organizationNode,
            Lude.Just ("PortfolioId" Lude..= portfolioId)
          ]
      )

instance Lude.ToPath DeletePortfolioShare where
  toPath = Lude.const "/"

instance Lude.ToQuery DeletePortfolioShare where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeletePortfolioShareResponse' smart constructor.
data DeletePortfolioShareResponse = DeletePortfolioShareResponse'
  { portfolioShareToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DeletePortfolioShareResponse' with the minimum fields required to make a request.
--
-- * 'portfolioShareToken' - The portfolio share unique identifier. This will only be returned if delete is made to an organization node.
-- * 'responseStatus' - The response status code.
mkDeletePortfolioShareResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeletePortfolioShareResponse
mkDeletePortfolioShareResponse pResponseStatus_ =
  DeletePortfolioShareResponse'
    { portfolioShareToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The portfolio share unique identifier. This will only be returned if delete is made to an organization node.
--
-- /Note:/ Consider using 'portfolioShareToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsrsPortfolioShareToken :: Lens.Lens' DeletePortfolioShareResponse (Lude.Maybe Lude.Text)
dpsrsPortfolioShareToken = Lens.lens (portfolioShareToken :: DeletePortfolioShareResponse -> Lude.Maybe Lude.Text) (\s a -> s {portfolioShareToken = a} :: DeletePortfolioShareResponse)
{-# DEPRECATED dpsrsPortfolioShareToken "Use generic-lens or generic-optics with 'portfolioShareToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsrsResponseStatus :: Lens.Lens' DeletePortfolioShareResponse Lude.Int
dpsrsResponseStatus = Lens.lens (responseStatus :: DeletePortfolioShareResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeletePortfolioShareResponse)
{-# DEPRECATED dpsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
