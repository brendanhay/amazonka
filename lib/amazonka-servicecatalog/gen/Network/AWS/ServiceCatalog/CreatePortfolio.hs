{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.CreatePortfolio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a portfolio.
--
-- A delegated admin is authorized to invoke this command.
module Network.AWS.ServiceCatalog.CreatePortfolio
  ( -- * Creating a request
    CreatePortfolio (..),
    mkCreatePortfolio,

    -- ** Request lenses
    cpfIdempotencyToken,
    cpfAcceptLanguage,
    cpfDisplayName,
    cpfDescription,
    cpfProviderName,
    cpfTags,

    -- * Destructuring the response
    CreatePortfolioResponse (..),
    mkCreatePortfolioResponse,

    -- ** Response lenses
    crsPortfolioDetail,
    crsTags,
    crsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkCreatePortfolio' smart constructor.
data CreatePortfolio = CreatePortfolio'
  { -- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
    idempotencyToken :: Lude.Text,
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
    -- | The name to use for display purposes.
    displayName :: Lude.Text,
    -- | The description of the portfolio.
    description :: Lude.Maybe Lude.Text,
    -- | The name of the portfolio provider.
    providerName :: Lude.Text,
    -- | One or more tags.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePortfolio' with the minimum fields required to make a request.
--
-- * 'idempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
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
-- * 'displayName' - The name to use for display purposes.
-- * 'description' - The description of the portfolio.
-- * 'providerName' - The name of the portfolio provider.
-- * 'tags' - One or more tags.
mkCreatePortfolio ::
  -- | 'idempotencyToken'
  Lude.Text ->
  -- | 'displayName'
  Lude.Text ->
  -- | 'providerName'
  Lude.Text ->
  CreatePortfolio
mkCreatePortfolio pIdempotencyToken_ pDisplayName_ pProviderName_ =
  CreatePortfolio'
    { idempotencyToken = pIdempotencyToken_,
      acceptLanguage = Lude.Nothing,
      displayName = pDisplayName_,
      description = Lude.Nothing,
      providerName = pProviderName_,
      tags = Lude.Nothing
    }

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpfIdempotencyToken :: Lens.Lens' CreatePortfolio Lude.Text
cpfIdempotencyToken = Lens.lens (idempotencyToken :: CreatePortfolio -> Lude.Text) (\s a -> s {idempotencyToken = a} :: CreatePortfolio)
{-# DEPRECATED cpfIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

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
cpfAcceptLanguage :: Lens.Lens' CreatePortfolio (Lude.Maybe Lude.Text)
cpfAcceptLanguage = Lens.lens (acceptLanguage :: CreatePortfolio -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: CreatePortfolio)
{-# DEPRECATED cpfAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The name to use for display purposes.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpfDisplayName :: Lens.Lens' CreatePortfolio Lude.Text
cpfDisplayName = Lens.lens (displayName :: CreatePortfolio -> Lude.Text) (\s a -> s {displayName = a} :: CreatePortfolio)
{-# DEPRECATED cpfDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The description of the portfolio.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpfDescription :: Lens.Lens' CreatePortfolio (Lude.Maybe Lude.Text)
cpfDescription = Lens.lens (description :: CreatePortfolio -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreatePortfolio)
{-# DEPRECATED cpfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the portfolio provider.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpfProviderName :: Lens.Lens' CreatePortfolio Lude.Text
cpfProviderName = Lens.lens (providerName :: CreatePortfolio -> Lude.Text) (\s a -> s {providerName = a} :: CreatePortfolio)
{-# DEPRECATED cpfProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

-- | One or more tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpfTags :: Lens.Lens' CreatePortfolio (Lude.Maybe [Tag])
cpfTags = Lens.lens (tags :: CreatePortfolio -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreatePortfolio)
{-# DEPRECATED cpfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreatePortfolio where
  type Rs CreatePortfolio = CreatePortfolioResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreatePortfolioResponse'
            Lude.<$> (x Lude..?> "PortfolioDetail")
            Lude.<*> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePortfolio where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWS242ServiceCatalogService.CreatePortfolio" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreatePortfolio where
  toJSON CreatePortfolio' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("IdempotencyToken" Lude..= idempotencyToken),
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            Lude.Just ("DisplayName" Lude..= displayName),
            ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("ProviderName" Lude..= providerName),
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreatePortfolio where
  toPath = Lude.const "/"

instance Lude.ToQuery CreatePortfolio where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreatePortfolioResponse' smart constructor.
data CreatePortfolioResponse = CreatePortfolioResponse'
  { -- | Information about the portfolio.
    portfolioDetail :: Lude.Maybe PortfolioDetail,
    -- | Information about the tags associated with the portfolio.
    tags :: Lude.Maybe [Tag],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePortfolioResponse' with the minimum fields required to make a request.
--
-- * 'portfolioDetail' - Information about the portfolio.
-- * 'tags' - Information about the tags associated with the portfolio.
-- * 'responseStatus' - The response status code.
mkCreatePortfolioResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreatePortfolioResponse
mkCreatePortfolioResponse pResponseStatus_ =
  CreatePortfolioResponse'
    { portfolioDetail = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the portfolio.
--
-- /Note:/ Consider using 'portfolioDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsPortfolioDetail :: Lens.Lens' CreatePortfolioResponse (Lude.Maybe PortfolioDetail)
crsPortfolioDetail = Lens.lens (portfolioDetail :: CreatePortfolioResponse -> Lude.Maybe PortfolioDetail) (\s a -> s {portfolioDetail = a} :: CreatePortfolioResponse)
{-# DEPRECATED crsPortfolioDetail "Use generic-lens or generic-optics with 'portfolioDetail' instead." #-}

-- | Information about the tags associated with the portfolio.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsTags :: Lens.Lens' CreatePortfolioResponse (Lude.Maybe [Tag])
crsTags = Lens.lens (tags :: CreatePortfolioResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreatePortfolioResponse)
{-# DEPRECATED crsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreatePortfolioResponse Lude.Int
crsResponseStatus = Lens.lens (responseStatus :: CreatePortfolioResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePortfolioResponse)
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
