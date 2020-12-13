{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.CreatePortfolioShare
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shares the specified portfolio with the specified account or organization node. Shares to an organization node can only be created by the management account of an organization or by a delegated administrator. You can share portfolios to an organization, an organizational unit, or a specific account.
--
-- Note that if a delegated admin is de-registered, they can no longer create portfolio shares.
-- @AWSOrganizationsAccess@ must be enabled in order to create a portfolio share to an organization node.
-- You can't share a shared resource. This includes portfolios that contain a shared product.
module Network.AWS.ServiceCatalog.CreatePortfolioShare
  ( -- * Creating a request
    CreatePortfolioShare (..),
    mkCreatePortfolioShare,

    -- ** Request lenses
    cpsPortfolioId,
    cpsAccountId,
    cpsAcceptLanguage,
    cpsOrganizationNode,

    -- * Destructuring the response
    CreatePortfolioShareResponse (..),
    mkCreatePortfolioShareResponse,

    -- ** Response lenses
    cpsrsPortfolioShareToken,
    cpsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkCreatePortfolioShare' smart constructor.
data CreatePortfolioShare = CreatePortfolioShare'
  { -- | The portfolio identifier.
    portfolioId :: Lude.Text,
    -- | The AWS account ID. For example, @123456789012@ .
    accountId :: Lude.Maybe Lude.Text,
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
    -- | The organization node to whom you are going to share. If @OrganizationNode@ is passed in, @PortfolioShare@ will be created for the node an ListOrganizationPortfolioAccessd its children (when applies), and a @PortfolioShareToken@ will be returned in the output in order for the administrator to monitor the status of the @PortfolioShare@ creation process.
    organizationNode :: Lude.Maybe OrganizationNode
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePortfolioShare' with the minimum fields required to make a request.
--
-- * 'portfolioId' - The portfolio identifier.
-- * 'accountId' - The AWS account ID. For example, @123456789012@ .
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
-- * 'organizationNode' - The organization node to whom you are going to share. If @OrganizationNode@ is passed in, @PortfolioShare@ will be created for the node an ListOrganizationPortfolioAccessd its children (when applies), and a @PortfolioShareToken@ will be returned in the output in order for the administrator to monitor the status of the @PortfolioShare@ creation process.
mkCreatePortfolioShare ::
  -- | 'portfolioId'
  Lude.Text ->
  CreatePortfolioShare
mkCreatePortfolioShare pPortfolioId_ =
  CreatePortfolioShare'
    { portfolioId = pPortfolioId_,
      accountId = Lude.Nothing,
      acceptLanguage = Lude.Nothing,
      organizationNode = Lude.Nothing
    }

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsPortfolioId :: Lens.Lens' CreatePortfolioShare Lude.Text
cpsPortfolioId = Lens.lens (portfolioId :: CreatePortfolioShare -> Lude.Text) (\s a -> s {portfolioId = a} :: CreatePortfolioShare)
{-# DEPRECATED cpsPortfolioId "Use generic-lens or generic-optics with 'portfolioId' instead." #-}

-- | The AWS account ID. For example, @123456789012@ .
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsAccountId :: Lens.Lens' CreatePortfolioShare (Lude.Maybe Lude.Text)
cpsAccountId = Lens.lens (accountId :: CreatePortfolioShare -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: CreatePortfolioShare)
{-# DEPRECATED cpsAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

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
cpsAcceptLanguage :: Lens.Lens' CreatePortfolioShare (Lude.Maybe Lude.Text)
cpsAcceptLanguage = Lens.lens (acceptLanguage :: CreatePortfolioShare -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: CreatePortfolioShare)
{-# DEPRECATED cpsAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The organization node to whom you are going to share. If @OrganizationNode@ is passed in, @PortfolioShare@ will be created for the node an ListOrganizationPortfolioAccessd its children (when applies), and a @PortfolioShareToken@ will be returned in the output in order for the administrator to monitor the status of the @PortfolioShare@ creation process.
--
-- /Note:/ Consider using 'organizationNode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsOrganizationNode :: Lens.Lens' CreatePortfolioShare (Lude.Maybe OrganizationNode)
cpsOrganizationNode = Lens.lens (organizationNode :: CreatePortfolioShare -> Lude.Maybe OrganizationNode) (\s a -> s {organizationNode = a} :: CreatePortfolioShare)
{-# DEPRECATED cpsOrganizationNode "Use generic-lens or generic-optics with 'organizationNode' instead." #-}

instance Lude.AWSRequest CreatePortfolioShare where
  type Rs CreatePortfolioShare = CreatePortfolioShareResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreatePortfolioShareResponse'
            Lude.<$> (x Lude..?> "PortfolioShareToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePortfolioShare where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.CreatePortfolioShare" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreatePortfolioShare where
  toJSON CreatePortfolioShare' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("PortfolioId" Lude..= portfolioId),
            ("AccountId" Lude..=) Lude.<$> accountId,
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("OrganizationNode" Lude..=) Lude.<$> organizationNode
          ]
      )

instance Lude.ToPath CreatePortfolioShare where
  toPath = Lude.const "/"

instance Lude.ToQuery CreatePortfolioShare where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreatePortfolioShareResponse' smart constructor.
data CreatePortfolioShareResponse = CreatePortfolioShareResponse'
  { -- | The portfolio shares a unique identifier that only returns if the portfolio is shared to an organization node.
    portfolioShareToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePortfolioShareResponse' with the minimum fields required to make a request.
--
-- * 'portfolioShareToken' - The portfolio shares a unique identifier that only returns if the portfolio is shared to an organization node.
-- * 'responseStatus' - The response status code.
mkCreatePortfolioShareResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreatePortfolioShareResponse
mkCreatePortfolioShareResponse pResponseStatus_ =
  CreatePortfolioShareResponse'
    { portfolioShareToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The portfolio shares a unique identifier that only returns if the portfolio is shared to an organization node.
--
-- /Note:/ Consider using 'portfolioShareToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsrsPortfolioShareToken :: Lens.Lens' CreatePortfolioShareResponse (Lude.Maybe Lude.Text)
cpsrsPortfolioShareToken = Lens.lens (portfolioShareToken :: CreatePortfolioShareResponse -> Lude.Maybe Lude.Text) (\s a -> s {portfolioShareToken = a} :: CreatePortfolioShareResponse)
{-# DEPRECATED cpsrsPortfolioShareToken "Use generic-lens or generic-optics with 'portfolioShareToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsrsResponseStatus :: Lens.Lens' CreatePortfolioShareResponse Lude.Int
cpsrsResponseStatus = Lens.lens (responseStatus :: CreatePortfolioShareResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePortfolioShareResponse)
{-# DEPRECATED cpsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
